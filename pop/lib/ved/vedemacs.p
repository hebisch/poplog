/* --- Copyright University of Sussex 1990.  All rights reserved. ---------
 > File:           C.all/lib/ved/vedemacs.p
 > Purpose:        Modify VED to simulate UNIX EMACS editor key controls.
 > Author:         Mark Rubinstein, May  7 1986 (see revisions)
 > Documentation:  HELP * VEDEMACS
 > Related Files:  image made by $usepop/pop/com/mkvedemacs
 */

#_TERMIN_IF DEF POPC_COMPILING

include sysdefs.ph;       ;;; for changing terminal control characters
uses vedset;

section;

lvars next_yank_command = ved_y;    ;;; the most appropriate yank command

/*
    ***********************************************************
                    VEDEMACS PROCEDURES
    ===========================================================
*/

;;; ---- MISCELLANEOUS STUFF ----------------------------------

;;;this is used to construct closures
define lconstant enterandinsert(string);
    lvars string;
    vedenter();
    vedinsertstring(string)
enddefine;

;;;vedemacs commands that use closures on enterandinsert
lconstant vedenterw         = enterandinsert(%'w '%);
lconstant veddoved          = enterandinsert(%'ved '%);
lconstant veddoqved         = enterandinsert(%'qved '%);
lconstant vedemacsreadfile  = enterandinsert(%'r '%);
lconstant ved_smallhelp     = enterandinsert(%'vh '%);
lconstant vedsearchforward  = enterandinsert(% '/' %);
lconstant vedsearchbackward = enterandinsert(% '\\' %);
lconstant vedglobalsr       = enterandinsert(%'gs/'%);
lconstant vedquerysr        = enterandinsert(%'s/'%);
lconstant veddesckey        = enterandinsert(%'hkey'%);

;;;insert literal character
define global ved_qc;
    ;;; quote character
    vedcharinsert(vedinascii())
enddefine;


;;; ---- CHARACTER OPERATIONS ---------------------------------

;;;repeat the next keystroke 4 times
define lconstant procedure vedemacs_count with_props emacs_count;
    lvars char, numtimes = 4;
    if isnumbercode(vedinascii() ->> char) then
        char - `0 -> numtimes;
        vedinascii() -> char;
    else
        while char == `\^U` do
            numtimes * 2 -> numtimes;
            vedinascii() -> char
        endwhile
    endif;
    vedgetproctable(char) -> char;
    repeat numtimes times char() endrepeat;
enddefine;

;;; ---- WORD OPERATIONS -------------------------------------------

;;;the next two procedures package up the normal vedworddelete procedures
;;;and -set next_yank_command- to -yankw-
define lconstant procedure vedrightworddel;
    vedwordrightdelete();
    ved_yankw -> next_yank_command;
enddefine;

define lconstant procedure vedleftworddel;
    vedwordleftdelete();
    ved_yankw -> next_yank_command;
enddefine;


;;; ---- LINE OPERATIONS ----------------------------------------

define lconstant procedure vedemacs_deol with_props emacs_deol;
    if  vedcolumn > vvedlinesize then
        vednextline();
        vedchardelete()
    else
        vedcleartail();
        ved_yankw -> next_yank_command;
    endif
enddefine;

;;;*** added A.S. Jan 88. ^X-^K deletes whole line
define lconstant procedure vedemacs_dline with_props emacs_dline;
    if vvedlinesize > 0 then
        ved_yankl -> next_yank_command;
    endif;
    vedlinedelete();
enddefine;

define lconstant procedure vedemacs_dbegline;
    vedclearhead();
    ved_yankw -> next_yank_command;
enddefine;

;;; ---- Facilities for tracking end of line (EMACS track-eol) -------------

/*
EMACS commands ^P and ^N for going up or down a line are defined so
that if the cursor starts beyond the end of a line it thereafter
"hugs" the line.
    <ENTER> track_eol
or
    <ENTER> track_eol off

will return to standard VED mode - no hugging.
*/

;;; variables used by end of line hugging facility
lvars
    emacs_was_eol = false,      ;;; go to eol if true
    emacs_hug_started = false,  ;;; indicates that vedprocesstrap is set
    emacs_hugging = false;      ;;; facility turned off, or on

define lconstant emacshug;
    ;;; go to end of line if necessary
    if emacs_hugging
    and (emacs_was_eol or (vedcolumn > vvedlinesize and vvedlinesize > 0))
    then
        vedtextright();
    endif
enddefine;

define global ved_track_eol;
    ;;; <ENTER> track_eol (or <ENTER> hug) switches hugging
    ;;; on and off. Can be given explicit argument 'on' or 'off'
    unless emacs_hug_started then
        ;;; set vedprocesstrap
        procedure;
            if vvedlinesize /== 0 then
                vedcolumn > vvedlinesize -> emacs_was_eol
            endif
        endprocedure <> vedprocesstrap -> vedprocesstrap;
        true -> emacs_hug_started;
    endunless;
    if vedargument = 'off' or vedargument = 'OFF' then
        false -> emacs_hugging;
    elseif vedargument = 'on' or vedargument = 'ON' then
        true -> emacs_hugging
    else
        not(emacs_hugging) -> emacs_hugging
    endif;
    if emacs_hugging then
        vedcharup <> emacshug   -> vednormaltable(`\^P`);
        vedchardown <> emacshug -> vednormaltable(`\^N`);
        'HUGGING ON'
    else
        vedcharup -> vednormaltable(`\^P`);
        vedchardown -> vednormaltable(`\^N`);
        'HUGGING OFF'
    endif . vedputmessage
enddefine;

syssynonym("ved_hug","ved_track_eol");


;;; ---- SCREEN AND SCREEN OPERATIONS -------------------------------

define lconstant procedure vedemacs_to_top_screen with_props emacs_to_top_screen;
    vedline - 1 -> vedlineoffset;
    vedrefresh()
enddefine;

define lconstant procedure vedemacs_comma with_props emacs_comma;
    unless vedline - 1 == vedlineoffset do
        vedscreenup();
    endunless
enddefine;

define lconstant procedure vedemacs_dot with_props emacs_dot;
    unless vedline == vedlineoffset - 1 + vedwindowlength do
        vedscreendown()
    endunless
enddefine;


;;; ---- BUFFER AND FILE OPERATIONS ----------------------------------

;;; Yank last thing killed, using ^Y. *** Added ^X-y as option Jan 88.
define lconstant procedure vedemacs_yank with_props emacs_yank;
    next_yank_command()
enddefine;

;;;*** added Jan 88
define lconstant procedure vedclear_and_read with_props clear_and_read;
    ved_clear();
    enterandinsert('r ');
enddefine;


;;; ---- SEARCH OPERATIONS -----------------------------------

;;; Next bit required to set EMACS response in Query search,
;;; as described in HELP VEDEMACS
define lconstant emacssearch(oldproc);
    lvars oldproc, procedure oldrawcharin = rawcharin;
    define dlocal rawcharin -> c ;
        lvars c = false;
        until c do
            oldrawcharin() -> c;
            if c == ` ` then `\r`
            elseif c == `n` then `\^?`
            elseif c == `\^G` then `n`
            elseif c == `.` then `y`
            else
                vedscreenbell();
                vedputmessage('OK? one of: <SPACE> n . <CTRL-G> ONLY!');
                vedsetcursor();
                sysflush(poprawdevin);
                false;
            endif -> c;
        enduntil;
    enddefine;
    ;;; now run original proc using special rawcharin
    oldproc();
enddefine;

lconstant oldved_s = ved_s;
emacssearch(%oldved_s%) -> ved_s;


;;; ---- REGION (MARK AND DOT) OPERATIONS ---------------------------

;;;**** added A.S. Jan 87
define lconstant procedure vedemacs_swap_mark with_props emacs_swap_mark;
    if vedpositionstack == [] then vederror('NO MARK SET')
    else
        vedexchangeposition()
    endif
enddefine;

;;; changed A.S. ^W now cuts from last position pushed to current position
define lconstant procedure vedcut_to_here with_props cut_to_here;
    ;;; will produce error if there is not already a stacked position
    vedpushkey();
    ved_cut();
    ved_splice -> next_yank_command;
enddefine;


;;; ---- EMACS MACRO FACILITIES -----------------------------------

lconstant procedure vedemacs_endmacro
    = vederror(% 'you are not defining a key macro' %);

define lconstant procedure veddef_emacs_macro with_props def_emacs_macro;
    lvars c p pdr = identfn;
    vedputmessage('remembering...');
    until (vedgetproctable(vedinascii() ->> c) ->> p) == vedemacs_endmacro do
        if p == undef then
            vederror('undefined key - macro definition aborted')
        elseif p == vedinsertvedchar then
            vedcharinsert(% c %) -> p
        endif;
        pdr <> p -> pdr;    ;;; add to running definition
        p();                ;;; do it
        vedsetcursor();     ;;; make sure cursor is in correct position.
    enduntil;
    vedsetkey('\^Xe',pdr);
    vedputmessage('keyboard macro defined');
enddefine;

lconstant vednlindent       = veddocr <> vedcharinsert(%`\^I`%);
lconstant veddoexclaim      = veddo(%'!'%);                         ;;;used for emacs '^_'


/*
    =========================================================
                            KEY BINDINGS
*/

vedset keys
;;; control operations and POP-11 extras
    enter               = esc X
    enter               = esc x
    marklo              = ^X f
    markhi              = ^X l
    markhi              = ^X @
    ENTER lmr           = ^X ^D
    ENTER lcp           = ^X ^E

;;; character operations
    charleft            = ^B
    charright           = ^F
    charup              = ^P    ;;; to track eol see below
    chardown            = ^N    ;;; to track eol see below
    chardelete          = ^H
    dotdelete           = ^D
    "ENTER sw"          = ^T
    ENTER qc            = ^Q
    emacs_count         = ^U
    nlindent            = ^J

;;; word operations
    wordleft            = esc B
    wordleft            = esc b
    wordright           = esc F
    wordright           = esc f
    rightworddel        = esc D
    rightworddel        = esc d
    leftworddel         = esc H
    leftworddel         = esc h
    "capword"           = esc C
    "capword"           = esc c
    "ENTER lcw"         = esc L
    "ENTER lcw"         = esc l
    "ENTER ucw"         = esc U
    "ENTER ucw"         = esc u
    "ENTER ccw"         = esc ^

;;; sentence operations
    "prevsent"          = esc a
    "prevsent"          = esc A
    "nextsent"          = esc e
    "nextsent"          = esc E

;;; line operations
    screenleft          = ^A
    textright           = ^E
    lineabove           = ^O
    emacs_deol          = ^K
    emacs_dline         = ^X ^K
    emacs_dbegline      = ^X K

;;; paragraph operations
    "prevpara"          = esc [
    "nextpara"          = esc ]
    ENTER jp            = esc J
    ENTER jp            = esc j

;;; getting out
    ENTER w1            = ^X ^S
    enterw              = ^X ^W
    ENTER w             = ^X ^M
    ENTER x             = esc ^X
    ENTER q             = ^X k
    ENTER q             = ^X d
    ENTER q             = ^X D
    ENTER q             = ^X ^Q
    ENTER q             = ^X q
    ENTER q             = ^X Q
    ENTER qq            = ^C
    ENTER qq            = esc ^C
    ENTER qq            = ^X ^C
    doexclaim           = ^_

;;; screen and screen operations
    "nextscreen"        = ^V
    "prevscreen"        = esc v
    "prevscreen"        = esc V
    refresh             = ^L
    scrollup            = ^Z
    scrolldown          = esc z
    scrolldown          = esc Z
    emacs_to_top_screen = esc !
    emacs_comma         = esc ,
    emacs_dot           = esc .
    setwindow           = ^X 2
    setwindow           = ^X 1
    setwindow           = ^X ^Z
    setwindow           = ^X z
    setwindow           = ^X Z
    ENTER rb            = ^X n
    ENTER rb            = ^X N
    ENTER rb            = ^X 4 b
    swapfiles           = ^X p
    swapfiles           = ^X P
    swapfiles           = ^X o
    swapfiles <> setwindow
                        = ^X 0
    "ENTER xdn"         = esc ^V

;;; buffer and file operations
    emacs_yank          = ^Y
    emacs_yank          = ^X y
    emacs_yank          = ^X Y
    doqved              = ^X ^V
    doved               = ^X ^F
    emacsreadfile       = ^X ^I
    clear_and_read      = ^X ^R
    fileselect          = ^X ^O
    fileselect          = ^X B
    fileselect          = ^X b
    ENTER files         = ^X ^B
    topfile             = esc <
    endfile             = esc >


;;; help and helper functions
    desckey <> docr     = ^X ?  ;;;show a given key binding

;;; this function should be mapped to <ESC> ? but that would disable the keypad
;;; for any use, which seems a pity.
    ENTER smallhelp     = esc #

;;; VED's ESC h is changed so replace it with ^X-h
    getsysfile          = ^X h
    getsysfile          = ^X H

;;;search operations
    searchforward       = ^S
    searchforward       = esc s
    searchforward       = esc S
    searchbackward      = ^R
    globalsr            = esc R
    globalsr            = esc r
    querysr             = esc Q
    querysr             = esc q

;;; region (mark and dot) operations
    pushkey             = ^^
    pushkey             = esc @
    emacs_swap_mark     = ^X ^X
    cut_to_here         = ^W

;;; emacs macro facilities
    def_emacs_macro     = ^X (
    emacs_endmacro      = ^X )
endvedset;


/*
    ************************************************************
                    NECESSARY UNIX TERMINAL EXTRAS
*/

;;; The method of changing the control characters is different for
;;; BSD unix and SystemV  unix.
#_IF DEF BERKELEY

uses ioctl_tchars;
uses ioctl_ltchars;

lconstant setpdr =
    procedure(old) with_props emacs_setup;
    lvars old;
    lconstant tchars = inits(6);
        if isprocedure(old) then old -> vedinit; old() endif;
        unless sys_io_control(poprawdevin, TIOCGETC, tchars) do   ;;; get the TCHARS
            mishap('SYS_IO_CONTROL error (using TIOCGETC)', nil);
        endunless;
        `\^G` -> tchars(1);                             ;;; control G becomes interrupt
        255 ->> tchars(2) ->> tchars(3) -> tchars(4);   ;;; disable other special chars
        unless sys_io_control(poprawdevin, TIOCSETC, tchars) do   ;;; set the TCHARS
            mishap('SYS_IO_CONTROL error (using TIOCSETC)', nil);
        endunless;
        unless sys_io_control(poprawdevin, TIOCGLTC, tchars) do   ;;; get lcoal TCHARS
            mishap('SYS_IO_CONTROL error (using TIOCGLTC)', nil);
        endunless;
        255 ->> tchars(2) ->> tchars(4) -> tchars(6);   ;;; disable required chars
        unless sys_io_control(poprawdevin, TIOCSLTC, tchars) do   ;;; set lcoal TCHARS
            mishap('SYS_IO_CONTROL error (using TIOCSLTC)', nil);
        endunless;
    endprocedure;

#_ELSEIF DEF SYSTEM_V

uses termio;

lconstant setpdr =
    procedure(old) with_props emacs_setup;
    lvars old;
    lconstant tstruct = constermio_struct(0,0,0,0,0,0,0,0,0,0,0,0,0,0);
        if isprocedure(old) then old -> vedinit; old() endif;
        unless sys_io_control(poprawdevin, TCGETA, tstruct) do   ;;; set lcoal TCHARS
            mishap(0, 'SYS_IO_CONTROL error (using TCGETA)');
        endunless;
        `\^G` -> termio_vintr(tstruct);             ;;; control G becomes interrupt
        255 -> termio_vquit(tstruct);                 ;;; disable quit.
        ;;; disable XON (control Q control S processing)
        termio_iflag(tstruct) &&~~ IXON -> termio_iflag(tstruct);
        ;;; disable Canonical input, but enable signals.
        (termio_lflag(tstruct) &&~~ ICANON) || ISIG -> termio_lflag(tstruct);
        unless sys_io_control(poprawdevin, TCSETA, tstruct) do   ;;; set lcoal TCHARS
            mishap(0, 'SYS_IO_CONTROL error (using TCSETA)');
        endunless;
    endprocedure;

#_ELSE

;;; Changed so that it works after a fashion without changing the control
;;; characters. (A.S. Jan 1988)
lconstant setpdr =
    procedure(old) with_props emacs_setup;
    lvars old;
        if isprocedure(old) then old -> vedinit; old() endif;
    endprocedure;

#_ENDIF

define procedure ved_setuppdr();
    if vedsetupdone then
        setpdr(false)
    else
        setpdr(% vedinit %) -> vedinit
    endif;
enddefine;

ved_setuppdr();

;;; set the file as having been read

global vars vedemacs=true;  ;;; so that "uses vedemacs" works.

endsection;

/* --- Revision History ---------------------------------------------------
--- Rob Duncan, May  8 1990
        Renamed -vedlastsent-, -vedlastpara- to -vedprevsent-, -vedprevpara-
--- Andreas Schoter, Apr 25 1990
        Implemented ^X 4 b, ^X o, ^X O
--- Rob Duncan, Oct 11 1989
    Replaced -next_page- and -previous_page- with -nextscreen- and
    -prevscreen- (available as libraries).
    Quoted some function names in the vedset commands to reduce autoloading.
    Fixed "with props" in -emacs_comma-.
--- Andreas Schoter, Sept 6 1989
    removed sentence, paragraph, and some word operations and made them
    autoloadable - see:
        vednextsent.p, vedprevsent.p
        vednextpara.p, vedprevpara.p
        ved_ccw.p
        vedcapword.p
--- Andreas Schoter, Jul 11 1989
    Modified to use vedset syntax.
    Paragraph traversal re-writen.
    Word delete modified to set -next_yank_command- correctly.
    Some bindings changed to conform more to emacs.
--- Rob Duncan, Apr  4 1989
    Made to use LIB SYSDEFS instead of doing tests on -sys_os_type-.
    Added -lvars- declaration to -enterandinsert-
--- Aaron Sloman, Aug  9 1988
    removed duplicate definition of vedcapword
--- Aaron Sloman, Aug  9 1988
    Removed error trap for VMS
--- Aaron Sloman, Jun  4 1988
    Fixed vedcapword to reduce all characters after the first to
    lower case
--- Aaron Sloman, Jan  3 1988
    Fixed some minor bugs in move-sentence and move-para.

--- Aaron Sloman, Jan  2 1988
    Many changes, documented in HELP * VEDEMACS

    Several changes to make this conform more closely to standard EMACS,
        and some POPLOG extras provided.
    1. added ved_track_eol with synonym ved_hug to switch tracking of end
       of line with ^P and ^N on and off.
    2. changed query search (ved_s) to use standard EMACS options instead of
       VED options. Old ved_s is oldved_s
    3. Assigned vedpushkey to `\^^` - CTRL-UP-ARROW and
    4. Made ESC-s a substitute for ^S, as the latter does not always work
       across networks
    5. Added ved_qc (Quote Character) to cope with cases where ^Q is
       not transmitted.
    6. Made ^W use ved_cut to "cut" from last POSITION stacked to current
       position, instead of from last line marked. After ^W, ^Y now uses
       ved_splice rather than ved_y.
    7. Added -vedcapword- to capitalise initial letter of current word.
    8. Fixed paragraph moves and added sentence moves.

    Added various other facilities and changes, as described in HELP VEDEMACS
    including various alternatives to control characters that are not normally
    available on VMS etc.

    Moved to C.all so that it becomes available for VMS and other operating
    systems that don't allow change of control characters.
--- John Gibson, Nov 11 1987
        Replaced -popdevraw- with -poprawdevin-

 */
