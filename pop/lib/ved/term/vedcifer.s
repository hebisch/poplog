/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 * File:            C.all/lib/ved/term/vedcifer.s
 * Purpose:         Assembly code for driving Cifer 2634 terminals
 * Author:          Aled Morris, 13 Dec 1985
 * Documentation:
 * Related Files:   LIB * VEDCIFERSCREEN
 */

/*

Assembler version of code to drive Cifer 2634 terminals:
the code is downloaded to the terminal as machine code by procedure
-vedciferscreen-

/*  The machine code given in this file performs two jobs:
	1)  Alters the way Cifer terminals scroll, such that ALL scrolling is
		performed in software by these routines.
	2)  Gives a form of purely-graphics mode (like the Visual 200).

	The functions are accessed by the transmission of new escape sequences,
	namely:
			<ESC> { <top> <bot>   scroll region down (includes <top> and <bot>)
			<ESC> } <top> <bot>                 up
			<ESC> .               graphics on
			<ESC> ,               graphics off

		<<  line numbers are in form `\s`+N, zero-origin >>

	The program also acts on the reciept of the newline code (ASCII 10), so
	that it can test whether to scroll the screen (which it does).
*/

/*                            SYMBOL DEFINITIONS.

	The source workspace area symbols are detailed in the CIFER 2634 manual
	pages 3-126 & 3-127.

		  <<<    Addresses listed here are offsets from IY.      >>>
		  <<< Flags are bit positions of bytes in the workspace. >>>
*/

/*  VCFLGS (VDU Control Flags) Bit set when corresponding states are ON.  */

	vcflgs      equ     2              ;;; Cifer VDU control flags address
									   ;;; bits:
	vcfesc      equ     0              ;;;  Prev. Char. was ESC flag
	vcfparm     equ     2              ;;;  Command awaits parameters flag
	vcftpn      equ     4              ;;;  Source is transparent flag

/*  PARMSR Re-entry address of parameterised subroutine.  Command procedures
	requiring parameters set their reentry address here, and set bit VCFPARM
	of VCFLGS to indicate that parameters are required.

	Parameterised routines are explained in the CIFER 2634 manual page 3-97.

	Notice that there is an error in the manual at this point - Firmware
	routines which require one parameter DO NOT use the re-entry vector.
	Instead, the C register is used to pass the single parameter.
*/
	parmsr      equ     16:12              ;;; Parameter re-entry address

/*  WS1 Workspace useable while processing one command and its parameters, if
	any.  4 bytes are provided, though only 2 are required for the routine
	below.  This workspace is only available while processing command
	parameters using the re-entry vector technique.
*/
	savescrl    equ     16:18              ;;; store scroll direction character
	savetopline equ     16:19              ;;;   "   scrolling region top line

/*  SCRN0 points to HOME position in refresh RAM (line 0).  Used to calculate
	the location in memory of any line on the screen.  CURY holds the cursor Y
	ordinate (in the range 0-23).
*/
	scrn0       equ     16:4288            ;;; pointer to top left of screen
	cury        equ     16:0053            ;;; cursor line number

/*  The following are rountines in the firmware.  The symbol value represents
	an offset in a table of entry addresses, so the address of the "highlight
	select graphics" routine will be entry 16:1B (the 27th) in the table.

	The full list of routines can be found in appendix 3/Z of the Cifer
	manual, pages 138-142, and the calling process is explained in section
	3.4.5 ("Screen Branch Table") on pages 3-96 to 3-97.  Remember that the
	manual incorrectly states that the C register is immaterial on entry, and
	a further call via the parameters vector is required to pass a value to a
	routine.  This is incorrect, the routines used here (where there is only
	one parameter) use the C register to pass the parameter.

	SCRNBTBL holds the address of the base of the table of addresses.
*/
	scrnbtbl    equ     16:4290            ;;; firmware routine vectors

	hgsel       equ     16:1B              ;;; select graphics
	hbegin      equ     16:29              ;;; highlights begin
	hend        equ     16:18              ;;; highlights end
	wrtctos     equ     16:25              ;;; write character to screen

/*  Terminal characteristics.  Screen size is 80 chars by 24 lines.
	A space character is used to blank a character position, while bit pattern
	2:11111111 is used to switch off all highlights.
	The bit pattern 2:01011111 is used to convert lower case characters to
	upper case (using AND to reset bit 5).
*/
	linelen     equ     80             ;;; 80 characters/line
	scrntopln   equ     0              ;;; screen top line is line number 0
	scrnbotln   equ     23             ;;; screen bottom line is number 23
	space       equ     32             ;;; ASCII code space to clear characters
	nohilite    equ     16:FF          ;;; Bits set to clear highlights
	ucasebitset equ     2:01011111     ;;; mask to convert lower to upper case

/*  The cifer screen memory is split into two sections - the text area (which
	resides between locations 16:0800 and 16:0FFF) and the highlights area
	in which the graphics characters are stored, which occupies a similar
	chunk of memory between locations 16:4800 and 16:4FFF.  Therefore, to map
	between them, all that need be done is to set/reset bit 6 of the high byte
	of the address.  That is what the symbol "hilitebit" represents.
*/
	hilitebit   equ     6              ;;; converts text to highlight memory

/*  bit 0 of location 16:3FFF is used to keep a flag of graphics mode.
*/
	grflag      equ     16:3FFF        ;;; flag location
	gronbit     equ     0              ;;; flag bit
	gronbitset  equ     1              ;;; flag bit when set (for ORing)

/*  The scrolling region is defined by two parameters - top and bottom line
	number.  These are sent with an offset of 32 decimal (bit 6 set) so a mask
	is used to filter out this value, by subtraction.
*/
	masklineno  equ     32

/*  These characters are used to cause the scrolling or graphics modes to be
	set.  They must be preceded by the ESC character.
*/
	scrldnch    equ     "{"            ;;; scroll down
	scrlupch    equ     "}"            ;;; scroll up
	graphonch   equ     "."            ;;; graphics mode on
	graphoffch  equ     ","            ;;; graphics mode off

/*  Characters SCRLDNCH and SCRLUPCH can be distinguished by testing a single
	bit - in the case of the above characters, bit 1 will be set if an down
	scroll is needed, and reset for up scroll.
*/
	scrlupbit   equ     1



						  /*  WINDOW 0 INTERCEPT  */

/*  The cifer provides numerous points for user code to intercept the
	processing of characters.  These are known as WINDOWS.  The window used by
	this routine is Window 0, ther start of the destination-screen en-route
	process, so it is entered for each character which is accepted by the
	terminal.

	On entry, C  holds the character
			  IY points to the CIFER internal workspace

	The window process is described in detail in the Cifer manual page 3-94.

	The routine is held in the RAM area 16:3D50 onwards, with location 16:3FFF
	being used as a flag for graphics mode.
*/

							org     16:3D50     ;;; assemble to 16:3D50

/*  If transparent mode is active then ignore the character, return to the
	firmware
*/
3D50 FD7E02         w0:     ld      a,(iy+vcflgs)
3D53 CB67                   bit     vcftpn,a
3D55 2032                   jr      nz,retfw

/*  If the previous character was ESC then the current character might be one
	of the control characters which we are trapping.
*/
3D57 CB47                   bit     vcfesc,a
3D59 2010                   jr      nz,escs

/*  The current character is not part of an escape sequence, but it might be
	the ASCII control code NewLine, which must be trapped in order that
	software scrolling is implemented.
*/
3D5B 79                     ld      a,c
3D5C FE0A                   cp      nl
3D5E CA553E                 jp      z, donl

/*  The character is not a control code we are trapping, but it may be a
	character which translates to a graphics character if graphics mode is
	active.
*/
3D61 21FF3F                 ld      hl,grflag       ;;; load graphics flag
3D64 CB46                   bit     gronbit,(hl)    ;;; test graphics mode bit
3D66 2821                   jr      z,retfw         ;;; graphics off - exit
3D68 C3853E                 jp      outg            ;;; graphics on - deal with
													;;;     graphics characters

/*  Escape sequence initiated, so test current character for scrolling or
	graphics control.
*/
3D6B 79             escs:   ld      a,c          ;;; C holds the character
3D6C FE7D                   cp      scrlupch     ;;; scroll up ?
3D6E 2804                   jr      z,scrl       ;;; yes - get parameters
3D70 FE7B                   cp      scrldnch     ;;; no - scroll down char ?
3D72 2016                   jr      nz,ntscrl    ;;; no - try graphics codes
												 ;;; yes - get parameters

/*  The scrolling routine has been called, and parameters are required.
	In order to generalise the routines which follow, the scroll direction is
	stored and the same routines used for both up and down scrolling.

	The flag "prev. char. was ESC" is reset, the parameters required flag
	is set, and the re-entry vector set up.

	The instruction POP HL is included before returning to the firmware in
	order to avoid the Cifer routines interpreting the scroll character
	sequence.  This is described in the Cifer manual page 3-96.

	All routines in the window 0 trap mechanism exit via the RET statement
	here.
*/
3D74 FD7718         scrl:   ld      (iy+savescrl),a     ;;; save this symbol
3D77 FDCB0286               res     vcfesc,(iy+vcflgs)  ;;; ESC flag
3D7B FDCB02D6               set     vcfparm,(iy+vcflgs) ;;; parameters flag
3D7F 21963D                 ld      hl,gottop           ;;; re-entry vector
3D82 FD7512                 ld      (iy+parmsr),l       ;;; set up re-entry
3D85 FD7413                 ld      (iy+parmsr+1),h     ;;;     vector
3D88 E1                     pop     hl
3D89 C9             retfw:  ret                        ;;; continue in firmware

/*  Here the character may be one which toggles the graphics mode flag, since
	it is part of an escape sequence and not one of the scroll characters
*/
3D8A FE2E           ntscrl: cp      graphonch     ;;; graphics on ?
3D8C CA6D3E                 jp      z,gron        ;;; set up graphics
3D8F FE2C                   cp      graphoffch    ;;; graphics off ?
3D91 CA743E                 jp      z,groff       ;;; switch off graphics
3D94 18F3                   jr      retfw         ;;; give up on this character

/*  This routine is entered when the first parameter for the scrolling routine
	is recieved by the Cifer.  This represents the top line number of the
	scrolling region, and is saved while the second (and final) parameter is
	sought.
*/
3D96 79             gottop: ld      a,c                ;;; C holds top line no.
3D97 D620                   sub     masklineno         ;;; mask top bits out
3D99 FD7719                 ld      (iy+savetopline),a ;;; save top line number
3D9C 21A63D                 ld      hl,scroll          ;;; scroll routine entry
3D9F FD7512                 ld      (iy+parmsr),l      ;;; re-entry vector
3DA2 FD7413                 ld      (iy+parmsr+1),h
3DA5 C9                     ret                        ;;; continue in firmware



							/*  SCROLL ROUTINE  */

/*  On entry, which is via the parameters vector PARMSR, the C register will
	hold the line number of the bottom of the region which is to scroll.  The
	top line number will already have been stored in the workspace (offset
	SAVETOPLINE), and the direction to scroll can be found by testing bit
	SCRLUPBIT of workspace byte (offset) SAVESCRL.

	Firstly, the flag for Parameters Required must be reset, then the value
	in the C register must be adjusted to correspond to a line number.

	The register pair BC is conserved, see Cifer manual page 3-95 "Rules for
	User Code", point (iv).
*/
3DA6 FDCB0296       scroll: res     vcfparm,(iy+vcflgs)  ;;; no more parms flag
3DAA C5                     push    bc
3DAB 79                     ld      a,c               ;;; sub only works on A
3DAC D620                   sub     masklineno        ;;; convert C to line no.

/*  Register E is used to hold the bottom line number, since the multiply
	routine will destroy the BC and HL registers.
*/
3DAE 5F                     ld      e,a               ;;; keep bottom line in E

/*  Now it is necessary to test for numbers being out of range - either the
	line numbers are not in the range 0 - 24, or that the bottom line number is
	greater than the top line number.
*/
3DAF FE18                   cp      scrnbotln + 1       ;;; screen bottom line
3DB1 305D                   jr      nc,error            ;;; if A >= 24, jump
3DB3 FD7E19                 ld      a,(iy+savetopline)  ;;; get top line number
3DB6 FE18                   cp      scrnbotln + 1       ;;; max value for this
3DB8 3056                   jr      nc,error            ;;; if A >= 24, jump

/*  At this point, both top and bottom line numbers will be within the range
	0 to 23.  The current state of the relevent registers is:
				A       top line number
			  C & E     bottom line number
	We now test to see if the top line number is greater (ie lower on the
	screen) than the bottom line number.  Subtracting the bottom line from the
	top line number will yield a carry if all is well, since subtracting a
	larger number from a smaller number will set the carry flag.  If there is
	no carry flag, then there is a possibility that the two line numbers are
	the same, which this scroll routine does not cater for.  A simple jump on
	zero is needed in this case.
*/
3DBA BB                     cp      e                   ;;; subtract bot line
3DBB 3808                   jr      c,bggt              ;;; line numbers ok.
3DBD 2851                   jr      z,error             ;;; top = bottom, jump
3DBF FD7319                 ld      (iy+savetopline),e  ;;; swap top and bottom
3DC2 5F                     ld      e,a                 ;;;       line numbers
3DC3 1801                   jr      parmsok
3DC5 7B             bggt:   ld      a,e                 ;;; restore A reg.

/*  The number of bytes which need to be scrolled is calculated here.  The
	accumulator holds the bottom line number, while the top line number is
	held in the workspace.  After calculating the number of lines to scroll,
	the multiply routine is called, which returns the number of butes to scroll
	by multipling the number of lines by the width of the screen (80
	characters).  This number is returned in HL, which is saved for the time
	being on the stack.
*/
3DC6 FD9619         parmsok:sub     (iy+savetopline) ;;; no of lines to scroll
3DC9 4F                     ld      c,a              ;;; for multiply ...
3DCA CD303E                 call    mult             ;;; get no.bytes to scroll
3DCD E5                     push    hl               ;;; save to restore later

/*  The line number of the bottom line to scroll is converted firstly into an
	offset from the top of the screen in bytes (using the multiply routine)
	and then into a memory location by calling the routine CALCS.
*/
3DCE 4B                     ld      c,e        ;;; get bottom line number
3DCF CD303E                 call    mult       ;;; returns C multiplied by 80
3DD2 CD4E3E                 call    calcs      ;;; get address on screen

/*  HL now holds the memory location of the bottom of the scroll region.

	This is kept in the DE pair while the location of the top line to scroll
	is calculated.
*/
3DD5 EB                     ex      de,hl               ;;; save HL

3DD6 FD4E19                 ld      c,(iy+savetopline)  ;;; get top line number
3DD9 CD303E                 call    mult          ;;; calculate offset in bytes
3DDC CD4E3E                 call    calcs         ;;; convert to memory loc.

/*  The number of bytes to move was previously calculated and stacked, so this
	is restored into the BC registers.

	The register DE now holds the location of the leftmost character on the
	bottom line which is to be scrolled.  HL holds the leftmost character on
	the top line to be scrolled.
*/
3DDF C1                     pop     bc            ;;; number of bytes to move

/*  If an up scroll is called for, then we want to move bytes downwards in
	memory, proceeding from the top of the screen onwards. A down scroll
	operation requires that bytes are moved upwards in memory proceeding from
	the right hand edge of the screen at the bottom, going towards the top.
*/
3DE0 FDCB184E               bit     scrlupbit,(iy+savescrl);;; up scroll ?
3DE4 2003                   jr      nz,scrldn     ;;; if not - down scroll
3DE6 EB                     ex      de,hl         ;;; if so - top line to DE
3DE7 1801                   jr      doscrl        ;;; avoid down scroll stuff
3DE9 1B             scrldn: dec     de            ;;; start at screen right

/*  At this point HL will hold the top of the region for a downwards scroll,
	or the bottom of the region for an upwards scroll.

	This value is not required in the actual scrolling process, but it is
	needed for creating the blank line at the end of the scrolling operation.
	It must be saved so that it can be used later for this purpose.
*/
3DEA E5             doscrl: push    hl            ;;; addr for blanking

/*  The scroll process involves the z80 block load instructions.  These cause
	bytes to be moved from (HL) to (DE).  Therefore, HL should point 80 bytes
	ahead of DE (scroll up) or 80 bytes less than DE (scroll down).  This is
	accomplished here:
*/
3DEB 215000                 ld      hl,linelen   ;;; 80 characters/screen line
3DEE 19                     add     hl,de        ;;; HL point to line below DE

/*  As previously mentioned, HL needs to be 80 bytes less than DE for a down
	scroll, and this is the code which achieves this:
*/
3DEF FDCB184E               bit     scrlupbit,(iy+savescrl) ;;; down scroll ?
3DF3 2806                   jr      z,goup         ;;; no - do up scroll
3DF5 EB                     ex      de,hl          ;;; yes - make DE > HL

/*  For speed, two different scroll routines are provided (one for up scroll,
	and one for down scroll) as opposed to having one routine with checks for
	scrolling direction.  At this point, a down scroll is required.
*/
3DF6 CD213E                 call    movdown     ;;; scroll routine
3DF9 1803                   jr      out         ;;; avoid up scroll as well !

/*  An up scroll is needed, so call the subroutine.
*/
3DFB CD123E         goup:   call    movup       ;;; move a line up

/*  Having scrolled, blank a line.
	The address of the left hand edge of the line to blank has already been
	calculated, and stacked.
*/
3DFE E1             out:    pop     hl             ;;; get location of line
3DFF 54                     ld      d,h            ;;; copy to DE
3E00 5D                     ld      e,l
3E01 13                     inc     de             ;;; point at second char.
3E02 3620                   ld      (hl),space     ;;; put space over 1st char
3E04 014F00                 ld      bc,linelen-1   ;;; number of positions
3E07 CBF4                   set     6,h            ;;; same to highlights RAM
3E09 36FF                   ld      (hl),nohilite  ;;; clear highlights
3E0B CBB4                   res     6,h            ;;; back to text RAM

/*  use the scroll up subroutine since it has the nescessary block load
	instruction.
*/
3E0D CD123E                 call    movup          ;;; blank line

/*  Finally restore BC (as directed by manual, see above) and return to
	firmware.
*/
3E10 C1             error:  pop     bc             ;;; restore BC
3E11 C9                     ret                    ;;; process next character



/*  SCROLLING SUBROUTINES  */

/*  This subroutine causes scrolling up, by the use of the LDIR (load with
	increment and repeat) instruction.  It is otherwise identical to the
	scroll down routine.

	It has two LDIR instructions, one for the text area and one for the
	highlights area of memory.  This requires that the registers HL, DE and BC
	are saved.  As they are restored off the stack, the appropriate bit is
	set which reflects the text area onto the highlights area.
*/
3E12 E5             movup:  push    hl
3E13 D5                     push    de            ;;; save registers
3E14 C5                     push    bc
3E15 EDB0                   ldir                  ;;; load one line/text area
3E17 C1                     pop     bc            ;;; restore registers
3E18 D1                     pop     de
3E19 CBF2                   set     hilitebit,d   ;;; map text to hilite area
3E1B E1                     pop     hl
3E1C CBF4                   set     hilitebit,h
3E1E EDB0                   ldir                  ;;; load one line/hilite area
3E20 C9                     ret                   ;;; exit move routine

/*  This is the same routine as "movup" but it incorporates the LDDR
	instruction.
*/
3E21 E5             movdown:push    hl
3E22 D5                     push    de            ;;; save registers
3E23 C5                     push    bc
3E24 EDB8                   lddr                  ;;; load one line/text area
3E26 C1                     pop     bc            ;;; restore registers
3E27 D1                     pop     de
3E28 CBF2                   set     hilitebit,d   ;;; map text to hilite area
3E2A E1                     pop     hl
3E2B CBF4                   set     hilitebit,h
3E2D EDB8                   lddr                  ;;; load one line/hilite area
3E2F C9                     ret                   ;;; exit move routine



/*  UTILITY SUBROUTINES FOR SCROLLING  */

/*  Given a number in the C register, this routine returns that number
	multiplied by 80 in the HL register pair.
	Each [sla l/rl h] will shift HL left once, effectively multiplying HL by
	two.
*/
3E30 0600           mult:   ld      b,0          ;;; make sure B = 0
3E32 60                     ld      h,b          ;;; copy BC to HL
3E33 69                     ld      l,c
3E34 CB25                   sla     l            ;;; mult BC * 2
3E36 CB14                   rl      h
3E38 CB25                   sla     l            ;;; gives BC * 4
3E3A CB14                   rl      h
3E3C 09                     add     hl,bc        ;;; leaves BC * 5
3E3D CB25                   sla     l
3E3F CB14                   rl      h            ;;; gives BC * 10
3E41 CB25                   sla     l
3E43 CB14                   rl      h            ;;; leaves BC * 20
3E45 CB25                   sla     l
3E47 CB14                   rl      h            ;;; gives BC * 40
3E49 CB25                   sla     l
3E4B CB14                   rl      h            ;;; HL now holds BC * 80
3E4D C9                     ret

/*  In order to work with screen locations, it is necessary to translate the
	offset from the top left hand corner as returned by the multiply routine
	into actual screen coordinates.  This is accomplished by adding the
	location of the top left hand corner of the screen.
*/
3E4E 44             calcs:  ld      b,h          ;;; copy HL to BC (there is
3E4F 4D                     ld      c,l          ;;;           no ADD BC,HL !)
3E50 2A8842                 ld      hl,(scrn0)   ;;; get location of HOME
3E53 09                     add     hl,bc        ;;; calculate actual location
3E54 C9                     ret                  ;;; exit subroutine




/*  DEALING WITH NEWLINE CHARACTERS  */

/*  This section deals with newline characters issued to the terminal.  If the
	current line is the last line on the screen then instead of allowing the
	firmware to deal with it, it calls the scroll routine above to move the
	entire screen up one line.

	The problem with letting the firmware cope with scrolling is that it will
	use a hardware scrolling technique.  This is achieved by moving the
	"virtual" screen over the physical memory.  For instance, before a scroll
	the home position will be addressed to location 16:0800. After a scroll,
	location 16:0800 might be somewhere in the middle of the bottom line,
	while the screen home position is mapped onto (say) location 16:0880, and
	so as scrolling occurs, the screen locations become wrapped more and more.

	This routine cures the problem by invoking a software scroll when
	attempting to linefeed at the bottom of a page.
*/
3E55 3A5300         donl:   ld      a,(cury)            ;;; get line number
3E58 FE17                   cp      scrnbotln           ;;; bottom of screen?
3E5A 2010                   jr      nz,noscrl           ;;; no - exit

/*  At this point, the whole screen needs to be scrolled via software, so the
	parameters are set up to simulate a "scroll up" escape sequence for the
	routine detailed above.
*/
3E5C FD36187D               ld      (iy+savescrl),scrlupch     ;;; force scrlup
3E60 FD361900               ld      (iy+savetopline),scrntopln ;;; top line #
3E64 0E37                   ld      c,scrnbotln + masklineno   ;;; bottom #
3E66 CDA63D                 call    scroll                     ;;; and scroll.

/*  If we have scrolled the screen then drop the return addresses on the stack
	to stop the Cifer handling the newline character.
	If no scrolling has taken place, then let the Cifer perform a linefeed by
	exiting at label "noscrl".
*/
3E69 E1                     pop     hl              ;;; drop return addresses
3E6A E1                     pop     hl              ;;; (Cifer manual page
3E6B E1                     pop     hl              ;;;     3-96 point (v)d )
3E6C C9             noscrl: ret                     ;;; exit to firmware




/*  HANDLING GRAPHICS CHARACTERS  */

/*  Firstly deal with those characters which toggle the graphics mode flag.
	The flag is held at location "grflag" (defined above as 16:3FFF).
	Graphics mode is indicated by bit "gronbit" of "grflag" being set.
	As a convenience, "gronbitset" is a constant which is defined above, in
	order that OR and AND can be used to set or reset the flag.  Notice that
	NOT is a directive to the assembler in use, its effect is to return the
	ones-complement of its argument.
*/
3E6D 3AFF3F         gron:   ld      a,(grflag)         ;;; flag graphics mode
3E70 F601                   or      gronbitset         ;;; set the bit
3E72 1806                   jr      exunt              ;;; back to firmware

3E74 FDCB1786       groff:  ld      a,(grflag)         ;;; out of graphics mode
3E78 E6FE                   and     NOT(gronbitset)    ;;; reset the bit

/*  Here the "prev. char. was ESC" flag is reset, and return addresses popped
	off the stack in order that the graphics on/off sequence is not passed to
	the fimware.
*/
3E7A 32FF3F         exunt:  ld      (flags),a
3E7D FDCB0286               res     vcfesc,(iy+vcflgs)     ;;; ESC flag
3E81 E1                     pop     hl
3E82 E1                     pop     hl
3E83 E1                     pop     hl
3E84 C9                     ret                            ;;; finished



/*  DISPLAY GRAPHICS CHARACTERS  */

/*  Here is the heart of the graphics routine, that which tests the characters
	recieved during graphics mode, and should they fall between "a" and "p",
	cause the appropriate graphics character to be displayed instead.
	Characters which do not correspond to graphics characters are printed as
	normal.

	Cifer graphics are handled as highlights, in the following way: In order
	to display a graphics character, it is nescessary to select that
	particular graphics character as a highlight.  Then (with highlights
	active) all characters printed have the selected graphic (highlight)
	superimposed on it.  Thus in order to print a graphics character (such as
	the vertical bar symbol) it is nescessary to select this as the current
	graphics highlight (by calling the HIGHLIGHT GRAPHICS SELECT routine with
	argument "J"), then activating the highlights with a call to HIGHLIGHTS
	ON, then in order that the highlight os printed, it is necessary to output
	some character to the screen (a space is the best, since the highlight is
	superimposed on the character).  This must be followed by HIGHLIGHTS OFF
	or else all following characters are displayed with the graphics
	superimposed on them.
*/
3E85 79             outg:   ld      a,c            ;;; transfer char to A
3E86 FE61                   cp      "a"            ;;; carry set = not graphic
3E88 3822                   jr      c,ntgcode      ;;; no carry, no graphic
3E8A FE70                   cp      "p"            ;;; carry set = graphic code
3E8C 301E                   jr      nc,ntgcode     ;;; if not graphic, print it

/*  At this point, all non graphics (ie characters which are not in the range
	"a" to "p", those that do not translate to graphics characters) have been
	filtered off, and via "ntgcode" they will be dealt with in the firmware.
*/
3E8E E1                     pop     hl
3E8F E1                     pop     hl          ;;; avoid firmware clash
3E90 E1                     pop     hl
3E91 C5                     push    bc       ;;; save BC as outlined previously

/*  Characters enter as lower case, but the firmware prefers them to be in
	upper case, therefore some conversion is required.
*/
3E92 E65F                   and     ucasebitset ;;; convert to upper case equiv

/*  Set the character in th accumulator (which will be in the range "A" to
	"O") as the current graphics highlight.
*/
3E94 4F                     ld      c,a         ;;; keep the graphic code
3E95 1E1B                   ld      e,hgsel     ;;; highlight graphics option
3E97 CDAD3E                 call    fwcall      ;;; .... in firmware

/*  Make highlights active.
*/
3E9A 1E29                   ld      e,hbegin    ;;; highlights begin routine
3E9C CDAD3E                 call    fwcall      ;;; .... in the firmware

/*  Write a space to the screen so that the highlight is displayed.
*/
3E9F 0E20                   ld      c,20h       ;;; space - used to give graph. char.
3EA1 1E25                   ld      e,wrtctos   ;;; write character to screen
3EA3 CDAD3E                 call    fwcall      ;;; call firmware routine

/*  Restore BC now C is no longer required, and will not be changed.
*/
3EA6 C1                     pop     bc          ;;; for politeness

/*  Switch off highlights.
*/
3EA7 1E18                   ld      e,hend      ;;; highlights off routine
3EA9 CDAD3E                 call    fwcall

/*  Exit for both cases - ie when graphics have been printed, this will return
	after 3 POPs have been executed (terminate processing of current
	character) or when the current character is not a graphics character, in
	which case the firmware will take care of printing it on the screen in the
	normal way.
*/
3EAC C9             ntgcode:ret


/*  This is the generalised firmware subroutine entry procedure, and it is
	taken directly from the cifer manual, part 3.4.5 "Screen Branch Table",
	page 3-97.  It is also used in their example program in appendix 3/H.

	It's effect is to call the routine whose address is in SCRNTBL (a linear
	type table of two byte addresses) indexed by the E register.
*/
3EAD 1600           fwcall: ld      d,0         ;;; for addition of 2 byte regs
3EAF 2A9042                 ld      hl,(scrnbtbl) ;;; base address of table
3EB2 19                     add     hl,de       ;;; calculate offset
3EB3 19                     add     hl,de
3EB4 5E                     ld      e,(hl)      ;;; get low byte of address
3EB5 23                     inc     hl
3EB6 56                     ld      d,(hl)      ;;; fetch high byte
3EB7 EB                     ex      de,hl       ;;; no JP (DE) is provided !
3EB8 E9                     jp      (hl)        ;;; jump to the routine.

/*  end of code  */

*/
