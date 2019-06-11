/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedciferscreen.p
 > Purpose:         set up the screen for cifer terminals
 > Author:          Aled Morris, 13 Dec, 1985 (see revisions)
 > Related Files:   vedcifer.p vedciferkeys.p
 */
compile_mode :pop11 +strict;

section;

/*  Code to download z80 machine code into the Cifer terminal, to control
 *  the scrolling, and for dealing with some new escape sequences.
 */
define lconstant vedciferprogram();
    lvars cifer_screen_handling_code banner;

    /*  A banner for the status line, must be 80 characters long.  */

    '              Sussex POPLOG ' sys_>< popversion -> banner;
    cons_with consstring {%
        explode(banner),
        repeat 80-datalength(banner) times `\s` endrepeat;
    %} -> banner;


    #_< {16:3D50             ;;; address for code (3D50)
                             ;;; see cifer manual section 3.4.2
        16:FD 16:7E 16:02 16:CB 16:67 16:20 16:32 16:CB 16:47 16:20
        16:10 16:79 16:FE 16:0A 16:CA 16:55 16:3E 16:21 16:FF 16:3F
        16:CB 16:46 16:28 16:21 16:C3 16:85 16:3E 16:79 16:FE 16:7D
        16:28 16:04 16:FE 16:7B 16:20 16:16 16:FD 16:77 16:18 16:FD
        16:CB 16:02 16:86 16:FD 16:CB 16:02 16:D6 16:21 16:96 16:3D
        16:FD 16:75 16:12 16:FD 16:74 16:13 16:E1 16:C9 16:FE 16:2E
        16:CA 16:6D 16:3E 16:FE 16:2C 16:CA 16:74 16:3E 16:18 16:F3
        16:79 16:D6 16:20 16:FD 16:77 16:19 16:21 16:A6 16:3D 16:FD
        16:75 16:12 16:FD 16:74 16:13 16:C9 16:FD 16:CB 16:02 16:96
        16:C5 16:79 16:D6 16:20 16:5F 16:FE 16:18 16:30 16:5D 16:FD
        16:7E 16:19 16:FE 16:18 16:30 16:56 16:BB 16:38 16:08 16:28
        16:51 16:FD 16:73 16:19 16:5F 16:18 16:01 16:7B 16:FD 16:96
        16:19 16:4F 16:CD 16:30 16:3E 16:E5 16:4B 16:CD 16:30 16:3E
        16:CD 16:4E 16:3E 16:EB 16:FD 16:4E 16:19 16:CD 16:30 16:3E
        16:CD 16:4E 16:3E 16:C1 16:FD 16:CB 16:18 16:4E 16:20 16:03
        16:EB 16:18 16:01 16:1B 16:E5 16:21 16:50 16:00 16:19 16:FD
        16:CB 16:18 16:4E 16:28 16:06 16:EB 16:CD 16:21 16:3E 16:18
        16:03 16:CD 16:12 16:3E 16:E1 16:54 16:5D 16:13 16:36 16:20
        16:01 16:4F 16:00 16:CB 16:F4 16:36 16:FF 16:CB 16:B4 16:CD
        16:12 16:3E 16:C1 16:C9 16:E5 16:D5 16:C5 16:ED 16:B0 16:C1
        16:D1 16:CB 16:F2 16:E1 16:CB 16:F4 16:ED 16:B0 16:C9 16:E5
        16:D5 16:C5 16:ED 16:B8 16:C1 16:D1 16:CB 16:F2 16:E1 16:CB
        16:F4 16:ED 16:B8 16:C9 16:06 16:00 16:60 16:69 16:CB 16:25
        16:CB 16:14 16:CB 16:25 16:CB 16:14 16:09 16:CB 16:25 16:CB
        16:14 16:CB 16:25 16:CB 16:14 16:CB 16:25 16:CB 16:14 16:CB
        16:25 16:CB 16:14 16:C9 16:44 16:4D 16:2A 16:88 16:42 16:09
        16:C9 16:3A 16:53 16:00 16:FE 16:17 16:20 16:10 16:FD 16:36
        16:18 16:7D 16:FD 16:36 16:19 16:00 16:0E 16:37 16:CD 16:A6
        16:3D 16:E1 16:E1 16:E1 16:C9 16:3A 16:FF 16:3F 16:F6 16:01
        16:18 16:06 16:FD 16:CB 16:17 16:86 16:E6 16:FE 16:32 16:FF
        16:3F 16:FD 16:CB 16:02 16:86 16:E1 16:E1 16:E1 16:C9 16:79
        16:FE 16:61 16:38 16:22 16:FE 16:70 16:30 16:1E 16:E1 16:E1
        16:E1 16:C5 16:E6 16:5F 16:4F 16:1E 16:1B 16:CD 16:AD 16:3E
        16:1E 16:29 16:CD 16:AD 16:3E 16:0E 16:20 16:1E 16:25 16:CD
        16:AD 16:3E 16:C1 16:1E 16:18 16:CD 16:AD 16:3E 16:C9 16:16
        16:00 16:2A 16:90 16:42 16:19 16:19 16:5E 16:23 16:56 16:EB
        16:E9
    } >_# -> cifer_screen_handling_code;

    define lconstant cifer_escape_seq(string);
        lvars string;
        rawcharout(27);
        appdata(string, rawcharout);
        rawoutflush();
    enddefine;

    define lconstant cifer_out_hex_byte(number);
        lvars number;
        dlocal pop_pr_radix = 16, pop_pr_quotes = false, pr = sys_syspr;
        appdata(if number < 16 then '0' else '' endif >< number, rawcharout)
    enddefine;

    define lconstant cifer_download(hex);
        lvars hex load_address here lim;
        hex(1) -> load_address;
        2 -> here;
        datalength(hex) -> lim;
        until here > lim do
            cifer_escape_seq('*,');
            cifer_out_hex_byte(load_address div 16:100);
            cifer_out_hex_byte(load_address mod 16:100);
            repeat 255 times
                cifer_out_hex_byte(hex(here));
                here+1 -> here;
                if here > lim then
                    rawcharout(`.`);
                    rawoutflush();
                    return
                endif;
                load_address + 1 -> load_address
            endrepeat;
            rawcharout(`.`);
            rawoutflush()
        enduntil;
    enddefine;

    cifer_escape_seq('?!');            ;;; reset terminal

    syssleep(200);                     ;;; wait a couple of seconds

    ;;; change status line to show that code has been downloaded
    cifer_escape_seq('^;  ');           ;;; message write
    rawcharout(80+`\s`);
    appdata(banner, rawcharout);
    cifer_escape_seq('^4');             ;;; message select
    cifer_escape_seq('V');              ;;; blank by default

    cifer_escape_seq('8');              ;;; short scroll mode
    cifer_escape_seq('6');              ;;; line wrap off
    cifer_escape_seq('^@ ');            ;;; insert define spaces off

    cifer_download({16:0800 11});       ;;; show 'download arrow'

    cifer_download(cifer_screen_handling_code); ;;; download the code
    cifer_download({16:3FFF 0});        ;;; reset graphics flag
    cifer_escape_seq('M');              ;;; no inverse video highlights
    cifer_escape_seq('*-3D50');         ;;; intercept

    cifer_download({16:0800 6});        ;;; show 'download okay' tick
enddefine;

;;; Procedure for setting up VED - ie programming the function keys and
;;; numeric keypad on the Cifer to return useful escape sequences.

define lconstant vedciferinit();
    lvars lim n vedcifer_table;

    /*  Set up table of slot numbers and strings to be mapped on to
        keys with CEA (Cifer Extended ASCII) codes in range HEX 80 to EF.

        ESC and stop code will be added to string.

        Mappings of character sequences to VED procedures is assumed to be the
        same as for V200.

        Format for entries is:
        CEA code, SLOT number, STRING to assign to key, DESCRIPTION of key
    */

    #_< {%

    /*  Function keys (top row on keyboard) map  */

    ;;;16:D6, 16:34,        ;;; LOCAL
    ;;;16:B1, 16:5F,        ;;; PRINT MODE
    ;;;16:DB, 16:39,        ;;; RESET key
       16:80, 16:00, `P`,   ;;; F1             delete character
       16:81, 16:01, `Q`,   ;;; F2                    (     / left
       16:82, 16:02, `R`,   ;;; F3                    ( line  current
       16:83, 16:03, ` `,   ;;; F4             delete (     \ right
       16:C4, 16:47, `!`,   ;;; SCROLL DOWN           ( word left
       16:C6, 16:49, `"`,   ;;; SCROLL UP             ( word right
       16:C8, 16:4B, `#`,   ;;; DEL LINE       static mode on/off
       16:C9, 16:46, `$`,   ;;; INS LINE             ( first
       16:CA, 16:4D, `%`,   ;;; DEL CHAR       range ( last
       16:CB, 16:4E, `&`,   ;;; INS CHAR             ( move
       16:B3, 16:64, `\'`,  ;;; HOME           positions stack ( push
       16:C3, 16:55, `(`,   ;;; SEND LINE                      ( pop
       16:C1, 16:58, `)`,   ;;; SEND PAGE      insert line ( above
       16:D2, 16:59, `*`,   ;;; COPY                       ( below
       16:BE, 16:5B, `t`,   ;;; CLEAR SCRN          top of file
       16:CD, 16:50, `H`,   ;;; CLEAR FIELD         end of file
       16:D7, 16:35, `v`,   ;;; BLANK KEY (top right) refresh

    /*  KeyPad mappings  */

       16:B4, 16:63, `D`,   ;;; left arrow : screen left
       16:B5, 16:62, `C`,   ;;; right arrow: text right
       16:B6, 16:61, `B`,   ;;; down arrow : screen down
       16:B7, 16:60, `A`,   ;;; up arrow   : screen up
       16:A1, 16:10, '?q',  ;;; keypad "1" : cursor down & left
       16:A2, 16:11, '?r',  ;;; keypad "2" : cursor down
       16:A3, 16:12, '?s',  ;;; keypad "3" : cursor down & right
       16:A4, 16:13, '?t',  ;;; keypad "4" : cursor left
       16:A5, 16:14, '?u',  ;;; keypad "5" : centre cursor
       16:A6, 16:15, '?v',  ;;; keypad "6" : cursor right
       16:A7, 16:16, '?w',  ;;; keypad "7" : cursor up & left
       16:A8, 16:17, '?x',  ;;; keypad "8" : cursor up
       16:A9, 16:18, '?y',  ;;; keypad "9" : cursor up & right
       16:A0, 16:19, '?l',  ;;; keypad "0" : word left
       16:AA, 16:1A, '?n',  ;;; keypad "." : word right
       16:AC, 16:1B, '?p',  ;;; keypad "," : switch status
       16:AD, 16:1C, '?m',  ;;; keypad "-" : redo last command
       16:BB, 16:1D, '?M',  ;;; ENTER      : go to command line

    /*  control type keys
        (these are not altered, but their codes are shown, should the user wish
        to assign to them)
    */
    ;;;16:B9, (HT),         ;;; TAB
    ;;;16:BA, (ESC),        ;;; ESC
    ;;;16:BC, (LF),         ;;; LF
    ;;;16:DD, (CR),         ;;; RETURN
    ;;;16:DE, (BS),         ;;; BACK SPACE
    ;;;16:DF, (DEL),        ;;; DEL

    ;;;16:DA, 16:38,        ;;; CTRL/SHIFT/SPACE for cursor cycle

    /*  not function keys, but useful just the same. */
       16:B0, 16:65, `z`,   ;;; CTRL TAB  switch hard/soft tabs
       16:E0, 16:20, '?o',  ;;; BLANK KEY bottom left DOWN display status line
       16:E1, 16:21, '?z',  ;;; BLANK KEY bottom left UP   status line off

    /*  key combinations not used by VED, but set up so that the user can
        assign to them using "vedsetkey" (q.v.).
        All the sequences begin ESC ? and this is followed by the character
        shown in the third column of the table here:
    */
       16:E8, 16:45, '?N',  ;;; CTRL/SHIFT/RESET
       16:C5, 16:48, '?O',  ;;; CTRL/SCROLL DOWN
       16:C7, 16:4A, '?P',  ;;; CTRL/SCROLL UP
       16:D8, 16:36, '?Q',  ;;; CTRL/INS CHAR
       16:B8, 16:53, '?R',  ;;; CTRL/HOME
       16:D4, 16:5D, '?S',  ;;; SHIFT/HOME
       16:D5, 16:5E, '?T',  ;;; CTRL/SHIFT/HOME
       16:D1, 16:32, '?U',  ;;; CTRL/COPY
       16:D3, 16:5C, '?V',  ;;; CTRL/CLEAR SCRN
       16:CC, 16:4F, '?W',  ;;; CTRL/CLEAR FIELD
       16:DC, 16:24, '?X',  ;;; CTRL/BLANK KEY (top right)
       16:AF, 16:22, '?Y',  ;;; CTRL/SHIFT/BLANK KEY.        Doesn't work ??
       16:B2, 16:56, '?Z',  ;;; CTRL RETURN
       16:BF, 16:5A, '?a',  ;;; CTRL-LINE FEED
       16:CE, 16:51, '?b',  ;;; SKIP
       16:CF, 16:52, '?c',  ;;; CTRL/SKIP
       16:D9, 16:37, '?d',  ;;; CTRL/ESC
       16:E2, 16:54, '?e',  ;;; CTRL/SHIFT/ESC
       16:E3, 16:23, '?f',  ;;; CTRL/BACK SPACE
       16:EA, 16:43, '?g',  ;;; CTRL/F4 - positionpop

    /*  For completeness, here are all the codes which are not used */

    ;;;16:84 to 16:9F        not used
    ;;;16:AB                 not used
    ;;;16:AE                 not used
    ;;;16:BD                 not used
    ;;;16:C2                 not used
    ;;;16:D0                 not used
    ;;;16:E4 to 16:E7        used for local graphics,
    ;;;16:E9                CTRL/F4 works only with graphics option, E8 without

    %} >_# -> vedcifer_table;

    define lconstant vedcifer_literal(code);
        ;;; send code preceded by space, so that it is taken literally
        lvars code;
        rawcharout(2:100000); rawcharout(code)
    enddefine;

    define lconstant vedcifer_plant(keycode,slotcode,string);
        ;;; store string (preceded by ESC) in slot, mapped onto key
        lvars keycode slotcode string;
        ;;; keycode in range 16:80 (128) to 16:FF (255)
        ;;; send key plant sequence
        vedscreencontrol('\^[^\\');     ;;; send ESC ^ \
        ;;; send flag character with bit 3 set, causing 16:80 to be added
        ;;; to next character to get 8-bit CEA code
        rawcharout(2:1000);
        ;;; send (keycode - 16:80), so that adding 16:80 will get right code
        rawcharout(keycode - 16:80);
        ;;; transfer slot number.
        vedcifer_literal(slotcode);
        ;;; plant stored character or string
        ;;; Transmit characters in string (may be one character)
        vedcifer_literal(`\^[`);    ;;; SPACE ESC
        if isinteger(string) then
            vedcifer_literal(string);   ;;; SPACE ESC
        else
            appdata(string, vedcifer_literal);
        endif;
        rawcharout(`.`);    ;;; termination code
    enddefine;

    vedciferprogram();

    unless (datalength(vedcifer_table)  ->> lim)  rem 3 == 0 then
        mishap(lim, 1, 'CIFER TABLE NOT MULTIPLE OF THREE')
    endunless;

    for n from 1 by 3 to lim do
        vedcifer_plant(
            fast_subscrv(n, vedcifer_table),
            fast_subscrv(n + 1, vedcifer_table),
            fast_subscrv(n + 2, vedcifer_table), )
    endfor;

    rawoutflush();

enddefine;

define vedciferscreenxy(col,row);
    lvars col,row;
    if col == 1 and row == 1 then
        vedscreencontrol('\^[H');
    else
        vedscreencontrol('\^[P');
        vedoutascii(col+31);
        vedoutascii(row+31);
    endif;
    col -> vedscreencolumn;
    row -> vedscreenline;
enddefine;

define lconstant screengraphtrans() with_props vedscreengraphtrans;
    lvars char, c;
    ;;;                123456789ABCDEF0123456789ABCDEF 0
    lconstant trans = 'oooj---j---jbdc~#.____________\s\s';
    fast_subscrs(char fi_- 16:80, trans) -> c;
    ;;; 2nd result true means graphics mode
    if c == `_` then
        char, false
    else
        c, c fi_>= 16:60
    endif
enddefine;

define vedciferscreen();

;;; set flags to control the way ved does things
    "cifer" -> vedterminalname;
    false   -> vedterminalselect;
    true    -> vedscrollscreen;
    false   -> vednokeypad;
    false   -> vednocharinsert;
    false   -> vednochardelete;
    false   -> vednolineinsert;
    false   -> vednolinedelete;

;;;set the new region scroll procedures

    procedure(top,bottom);
        lvars top, bottom;
        vedscreencontrol('\^[{');
        rawcharout(top+vedscreenoffset+31);
        rawcharout(bottom+vedscreenoffset+31);
        rawoutflush();
    endprocedure -> vedscreenscrollregiondown;

    procedure(top,bottom);
        lvars top,bottom;
        vedscreencontrol('\^[}');
        rawcharout(top+vedscreenoffset+31);
        rawcharout(bottom+vedscreenoffset+31);
        rawoutflush();
    endprocedure -> vedscreenscrollregionup;

    vedciferscreenxy -> vedscreenxy;
    screengraphtrans -> vedscreengraphtrans;

;;;redefine screen control variables

    vedset screen
        alpha          = esc ,
        graphic        = esc .
        charleft       = esc D
        charright      = esc C
        charup         = esc A
        chardown       = ^J
        clear          = esc ^ J
        cleartail      = esc ^ K
        deletechar     = esc ^ (
        deleteline     = esc ^ )
        insertline     = esc ^ .
        insertmode     = esc ^ A
        overmode       = esc ^ G
;;;     cursorupscroll = esc :
        setpad         = esc % esc * W B
        resetpad       = esc * W A
    endvedset;

;;; initialise the terminal
    vedciferinit();

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 15 1992
        Commented out cursorupscroll -- no longer exists
--- John Gibson, Feb 14 1992
        Put in definititon for vedscreengraphtrans and removed vedset g*raphic
--- Rob Duncan, Oct 31 1989
        Took initialisation procedure definitions out of -vedciferscreen-
        and made them lconstant at top level; tidied up.
--- Andreas Schoter, Jul 18 1989
        Modified to use seperate key and screen set up with vedset
--- John Williams, Jun 24 1986
        made it sleep for two seconds, not one
*/
