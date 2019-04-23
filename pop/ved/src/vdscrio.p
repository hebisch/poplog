/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/ved/src/vdscrio.p
 > Purpose:         Basic screen facilities for VED
 > Author:          John Gibson, Aaron Sloman 1982? (see revisions)
 > Documentation:   REF * VEDPROCS
 > Related Files:   SRC * vdscreen.p, vdscroll.p
 */

;;; -------------------- BASIC SCREEN I/O ---------------------------------

#_INCLUDE 'vddeclare.ph'

vars
        procedure (vedrestorescreen, vedsetonscreen),
        vednokeypad, vedscreeninasciicursor
    ;


;;; ---------------------------------------------------------------------

section $-Sys$-Ved =>
                vedprintingdone, vedinserting, ved_char_in_stream,
                vedwiggletimes,
                vedscreentrailspacemark, vedscreentrailnewlinemark,
                vedscreenpromptspacemark, vedscreenformatspacemark,
                vedscreennobreakspacemark, vedscreenhairspacemark,

                vedscr_read_input, vedscr_read_ascii,
                vedscr_input_waiting, vedscr_clear_input,
                vedscr_char_out, vedscr_substring_out, vedscr_flush_output,

                vedgetinput, vedinascii, vedinputwaiting, vedclearinput,
                vedscreencontrol, vedscreenescape, vedscreenovermode,
                vedscreengraphtrans, vedscreencharmode, vedscreenoutput,
                vedscreenbell, vedscreenraw, vedscreencooked, vedscreenreset,
                vedmarkset, vedwiggle
            ;

vars
    vedprintingdone     = false,
    vedinserting        = false,
    ved_char_in_stream  = [],
    vedwiggletimes      = 5,

    ;;; display trailing space/newline/hair space chars as a bold S/N/H
    ;;; in colour 3
    vedscreentrailspacemark     = `\[b3]S`,
    vedscreentrailnewlinemark   = `\[b3]N`,
    vedscreenhairspacemark      = `\[b3]H`,

    ;;; default other special spaces to ordinary space
    vedscreenpromptspacemark    = `\s`,
    vedscreenformatspacemark    = `\s`,
    vedscreennobreakspacemark   = `\s`,
    ;

lvars
    screen_is_set       = false,
;


;;; --- REDEFINABLE WINDOW I/O PROCEDURES --------------------------------
;;;     Redefined by xved, etc -- all VED procedures must use these

    /*  Input */

define vars vedscr_read_input() /* -> char_or_procedure */;
    chain(rawcharin)
enddefine;

define vars vedscr_read_ascii = vedscr_read_input enddefine;

define vars vedscr_input_waiting() /* -> bool */;
    chain(poprawdevin, sys_input_waiting)
enddefine;

define vars vedscr_clear_input();
    chain(poprawdevin, sys_clear_input)
enddefine;

    /*  Output */

define vars vedscr_char_out(/* char_or_string */) with_nargs 1;
    chain((), rawcharout)
enddefine;

define vars vedscr_substring_out(/* string, bsub, nbytes */) with_nargs 3;
    chain((), rawsubstringout)
enddefine;

define vars vedscr_flush_output();
    chain(poprawdevout, sysflush)
enddefine;


;;; --- INPUT ACTIONS ---------------------------------------------------

lvars read_ascii = false;

vars
;

define vedgetinput() /* -> char_or_procedure */;
    lvars char;
    _CHECKINTERRUPT;

    while ispair(ved_char_in_stream) do
        fast_destpair(ved_char_in_stream) -> (char, ved_char_in_stream);
        if isstring(char) then
            datalist(char) nc_<> ved_char_in_stream -> ved_char_in_stream
        else
            return(char)
        endif;
    endwhile;

    if read_ascii then
        vedscr_read_ascii,          ;;; a character
        vedscreeninasciicursor
    else
        vedscr_read_input,          ;;; a character or a procedure
        false                       ;;; don't put cursor on if off
    endif;
    Set_wait_cursor((), false);     ;;; 2nd arg means don't set position
    fast_chain()
enddefine;

define vedinascii() -> char;
    lvars char;
    dlocal read_ascii = true;
    until isinteger(vedgetinput() ->> char) do enduntil
enddefine;

define vedinputwaiting();
    ispair(ved_char_in_stream) or vedscr_input_waiting()
enddefine;

define vedclearinput();
    vedscr_flush_output();
    vedscr_clear_input();
    [] -> ved_char_in_stream;
    1000 ->> vedscreenline -> vedscreencolumn   ;;; cursor may be anywhere
enddefine;


;;; --- SCREEN OUTPUT ----------------------------------------------------

;;; vedscreencontrol:
;;;     sends a control sequence to the terminal.
;;;     May be a procedure, character or string.
;;;     The initial test against -nullstring- gives a quick return for
;;;     control sequences initialised to default null values.

define vedscreencontrol(cntrl);
    lvars cntrl;
    unless cntrl == nullstring then
        if isprocedure(cntrl) then
            fast_apply(cntrl);
        else
            vedscr_char_out(cntrl);
        endif;
    endunless;
enddefine;

;;; vedscreenescape:
;;;     old version of -vedscreencontrol- kept for compatability.
;;;     Outputs -char- preceded by -vedescape-

define vedscreenescape(char);
    lvars char;
    if isprocedure(char) then
        fast_apply(char)
    elseunless char = nullstring then
        vedscr_char_out(vedescape);
        vedscr_char_out(char)
    endif
enddefine;

    ;;; Called for graphics chars in the range 16:81 - 16:9D
    ;;; (currently only upto 16:92 are used)
define Default_screengraphtrans(char) with_props vedscreengraphtrans;
    lvars char, c;
    ;;;                123456789ABCDEF0123456789ABCD
    lconstant trans = '---|---|---|||+o#.___________';
    fast_subscrs(char fi_- 16:80, trans) -> c;
    ;;; 2nd result false means not graphics mode
    if c == `_` then char else c endif, false
enddefine;

vars procedure vedscreengraphtrans = Default_screengraphtrans;


lvars char_mode = 0;

define Default_screencharmode() with_props vedscreencharmode;
    char_mode
enddefine;
;;;
define updaterof Default_screencharmode(mode) with_props vedscreencharmode;
    lvars mode, diff, old = char_mode;
    mode ||/& old -> diff;
    returnif(diff == 0);
    mode -> char_mode;

    if diff &&/=_0 VEDCMODE_GRAPHIC then
        vedscreencontrol(if mode &&/=_0 VEDCMODE_GRAPHIC then
                            vvedscreengraphic
                         else
                            vvedscreenalpha
                         endif);
        diff fi_&&~~ VEDCMODE_GRAPHIC -> diff;
        returnif(diff == 0)
    endif;
    mode fi_&&~~ VEDCMODE_GRAPHIC -> mode;

    ;;; Assumes that the other modes accumulate when sent out, i.e. the only
    ;;; way to turn one off is to turn them all off (this is the vt100
    ;;; behaviour)
    if diff fi_&&~~ mode /== 0 then
        ;;; clearing some modes -- clear them all first
        vedscreencontrol(vvedscreencharnormal);
        returnif(mode == 0)
    else
        ;;; only setting new ones
        diff -> mode
    endif;

    if mode &&/=_0 VEDCMODE_BOLD then
        vedscreencontrol(vvedscreencharbold)
    endif;
    if mode &&/=_0 VEDCMODE_ALTFONT then
        if vvedscreencharaltfont == nullstring then
            mode fi_|| VEDCMODE_UNDERLINE -> mode
        else
            vedscreencontrol(vvedscreencharaltfont)
        endif
    endif;
    if mode &&/=_0 VEDCMODE_UNDERLINE then
        vedscreencontrol(vvedscreencharunderline)
    endif;
    if mode &&/=_0 VEDCMODE_HIGHLIGHT then
        vedscreencontrol(vvedscreencharhighlight)
    endif;
    if mode &&/=_0 VEDCMODE_BLINK then
        vedscreencontrol(vvedscreencharblink)
    endif;
enddefine;

vars active vedscreencharmode = Default_screencharmode;


define vedscreenovermode();
    unless vednocharinsert then
        0 -> vedscreencharmode;
        if vedinserting then
            vedscreencontrol(vvedscreenovermode);
            false -> vedinserting
        endif
    endunless
enddefine;


define vedscreenoutput(dchar);
    lvars char, dchar, org_char = dchar, mode;

    if ispair(dchar) then fast_front(dchar) -> dchar endif;
    dchar fi_&&~~ 16:FFFF -> mode;
    dchar fi_&& 16:FFFF -> char;

    if char fi_< `\s` or char == `\^?` then
        if char == `\t` then
            `\s` -> char
        else
            ;;; make other control characters print as vedscreencontrolmark
            vedscreencontrolmark -> dchar;
            (dchar fi_&&~~ 16:FFFF) fi_|| mode -> mode;
            dchar fi_&& 16:FFFF -> char
        endif
    elseif 16:9A fi_<= char and char fi_<= 16:A0 then
        ;;; special spaces etc
        if char == `\St` then
            ;;; trailing space
            vedscreentrailspacemark
        elseif char == `\Sp` then
            ;;; prompt-marker space (vvedpromptchar)
            vedscreenpromptspacemark
        elseif char == 16:9C then
            ;;; format-control space
            vedscreenformatspacemark
        elseif char == 16:9B then
            ;;; trailing newline
            vedscreentrailnewlinemark
        elseif char == 16:9A then
            ;;; hair space (n.b. when vedvarwidthmode is true in XVed,
            ;;; vedscreenhairspacemark will be the same char, and gets picked
            ;;; up as a graphics character below)
            vedscreenhairspacemark
        else
            ;;; ISO Latin and Ved nobreak spaces
            vedscreennobreakspacemark
        endif -> dchar;
        (dchar fi_&&~~ 16:FFFF) fi_|| mode -> mode;
        dchar fi_&& 16:FFFF -> char
    endif;

    if char fi_> 16:7F then
        if char fi_<= 16:9A and char /== 16:80 then
            ;;; Ved standard graphics char -- convert first. Returns boolean
            ;;; saying whether to put into graphics mode, and char to be
            ;;; put out
            if vedscreengraphtrans(char) then
                mode fi_|| VEDCMODE_GRAPHIC -> mode
            endif -> char
        elseunless pop_character_set or char fi_> 16:FF then
            ;;; interpret as old-style graphics char
            mode fi_|| VEDCMODE_GRAPHIC -> mode;
            char fi_&& 16:7F -> char
        endif
    endif;

    if char == `\s` then
        ;;; clear any non-significant attributes on spaces (makes sure
        ;;; things like altfont don't get drawn as underline on spaces)
        if mode &&/=_0 VEDCMODE_SP_SIG_BITS then
            mode fi_&&~~ VEDCMODE_SP_INSIG_BITS
        else
            0
        endif -> mode
    endif;
    mode -> vedscreencharmode;

    ;;; update status line map if on status
    if vedonstatus then
        if vedinserting then
            Shift_string(vedscreencolumn, 1, vedstatusline, false) ->
        endif;
        org_char -> fast_subscrvedstring(vedscreencolumn, vedstatusline)
    endif;
    vedscr_char_out(char);
    if vedscreencolumn == vedscreenwidth then
        1000 -> vedscreenline; 1000 -> vedscreencolumn;
    else
        vedscreencolumn fi_+ 1 -> vedscreencolumn;
    endif;
enddefine;

define vars vedscreenbell();
    vedscreencontrol(vvedscreenbell);
    vedscr_flush_output()
enddefine;

define Default_screenraw() with_props vedscreenraw;
    unless screen_is_set then
        vedscreencontrol(vvedscreeninit);
        unless vednokeypad then
            vedscreencontrol(vvedscreensetpad);
        endunless;
        vedscreenbell();
    endunless;
    1000 ->> vedscreenline -> vedscreencolumn;
    true -> screen_is_set;
enddefine;

vars procedure vedscreenraw = Default_screenraw;

define Default_screencooked() with_props vedscreencooked;
    unless vednocharinsert then
        vedscreencontrol(vvedscreenovermode);
    endunless;
    unless vednokeypad then
        vedscreencontrol(vvedscreenresetpad);
    endunless;
    vedscreencontrol(vvedscreenreset);
    0 -> vedscreencharmode;
    vedscr_flush_output();
    false -> screen_is_set;
enddefine;

vars procedure vedscreencooked = Default_screencooked;

define vars vedscreenreset();
    returnif(XWINDOWS);
    vedscreenxy(1, vedscreenlength);
    vedscr_char_out(`\n`);
    vedscr_flush_output();
    `\n` -> poplastchar;
    vedscreencooked();
    vedscr_flush_output();
    true -> vedprintingdone
enddefine;

    /*  line is in buffer co-ordinates
    */
define vedmarkset(line, mark);
    lvars line, mark;
    if vedediting then
        vedwindowpoint(Window_line(line), 1);
        vedscreenoutput(mark);
        ;;; invalidate screen marks for any stacked marks
        false -> fast_front(marked_range_stack)
    endif;
enddefine;

    ;;; put a mark on left of screen to indicate where cursor is
define Mark_cursor_line();
    dlocal ved_on_status = false;
    if vedediting then
        vedcheck();
        vedmarkset(vedline, if vedmarked(vedline) and vvedmarkprops then
                                vedscreencursorlinerangemark
                            else
                                vedscreencursorlinemark
                            endif)
    endif
enddefine;

    ;;; remove the mark on left of screen
define Unmark_cursor_line();
    dlocal ved_on_status = false;
    if vedediting then
        vedcheck();
        vedmarkset(vedline, if vedmarked(vedline) and vvedmarkprops then
                                vedscreenrangemark
                            else
                                `\s`
                            endif)
    endif
enddefine;

    ;;; Make the cursor wiggle on the screen, e.g. before requesting input
    ;;; line and col in buffer co-ordinates
define vedwiggle(line, col);
    lvars line, col;
    lconstant delay = 5;        ;;; 1/20 sec
    returnunless(vedediting);
    if line == 0 then       ;;; use status line
        vedwindowpoint(1, col)
    else
        procedure(vedline, vedcolumn);
            dlocal vedline, vedcolumn;
            vedcheck();
            vedsetcursor()
        endprocedure(line, col)
    endif;
    unless isinteger(vedwiggletimes) then 5 -> vedwiggletimes endunless;
    repeat vedwiggletimes times
        vedscreencharright();
        vedscr_flush_output();
        syssleep(delay);
        vedscreencharleft();
        vedscr_flush_output();
        syssleep(delay);
    endrepeat;
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 24 1999
        Made vedscreenoutput deal with Ved hair space char (16:9A)
--- John Gibson, Apr 28 1997
        Removed temporary fix for 16-bit chars from vedscreenoutput
--- John Gibson, Feb 15 1997
        String16 changes
--- John Gibson, Jan 22 1996
        Made vedmarkset assign false to front of marked_range_stack
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Jan 19 1995
        Added vedscreentrailnewlinemark and made vedscreenoutput display it
        for character 16:9B (`\Nt` in itemiser)
--- John Gibson, Sep 22 1994
        Fix to vedwiggle
--- John Gibson, Jan 13 1994
        Changed vedgetinput to set cursor before reading
--- John Gibson, Mar  5 1993
        Changed vedscreenoutput to deal with new special spaces
--- John Gibson, May  8 1992
        Made vedscreenreset set vedprintingdone true
--- John Gibson, Feb  6 1992
        Added new variable procedure -vedscreengraphtrans-
--- John Gibson, Jan 23 1992
        Replaced vedscreenm*ark with -vedscreenrangemark-
--- John Gibson, Jan 23 1992
        Improved updater of -Default_screencharmode-
--- John Gibson, Jan 17 1992
        Made updater of -Default_screencharmode- use vvedscreencharunderline
        for ALTFONT mode if vvedscreencharaltfont is nullstring.
--- John Gibson, Jan  4 1992
        Changed -vedscreenoutput- to accept char with attribute bits set
--- John Gibson, Dec 20 1991
        Replaced vedgraphic, vedscreengraphon/off with new active variable
        -vedscreencharmode-
--- John Gibson, Dec 11 1991
        Revised vedwiggle again
--- Robert John Duncan, Dec  6 1991
        Changed -vedwiggle- not to output "non-printing" characters, which
        do show up in XVed
--- John Gibson, Dec  4 1991
        Fixed kluge in vedinascii of redefining vedscr_read_input
        by introducing new lvar that tells vedgetinput what to use.
--- John Gibson, Sep 20 1991
        Split screencooked/screenraw into Default_ procedure and var.
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- John Gibson, Jun  5 1991
        Added -vedgetinput- and -vedscr_read_ascii- etc (-vedinascii- now
        guaranteed to return a char, whereas -vedgetinput- returns a char
        or a procedure).
--- John Gibson, Apr  9 1991
        Added vedscr_ procedures.
        Sectionised and exported -vedinputwaiting- and -vedclearinput-.
--- Aaron Sloman, Oct 12 1990
        changed xved to wved. Added bit for xpop in -vedscreenreset-
--- Aaron Sloman, Oct  8 1990
        Changed guard onxvedinascii
--- Aaron Sloman, Aug 10 1990
        Made vedscreenreset user definable
--- Rob Duncan, Nov  7 1989
        Added -vedscreencontrol- as more rational replacement for
        -vedscreenescape- and replaced all occurrences;
        extended -vedscreenraw/cooked- to output new -vvedscreeninit/reset-
        controls for setting/resetting the terminal; changed -vedkeypadset-
        to -screen_is_set- to reflect this and made it "lvars";
        changed -vedscreenbell- to output -vvedscreenbell- instead of `\^G`
        (could be a visible bell).
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
 */
