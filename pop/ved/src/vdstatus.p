/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/ved/src/vdstatus.p
 > Purpose:         Management of Ved buffer for the status/command line
 > Author:          John Gibson (see revisions)
 */

;;;------------------ STATUS LINE COMMANDS -----------------------------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedencodetabs, veddecodetabs,
        Sys$-Ved$-Mark_cursor_line, Sys$-Ved$-Unmark_cursor_line,
        )
    ;

vars
        procedure (vedvscr_substring_width, vedvscr_space_width,
        vedvscr_set_wline_start),
        vedsetupdone, vedscreenstatusinputmark, vedscreenstatusnumattr,
        vedvarwidthmode
    ;


;;; ----------------------------------------------------------------------

section $-Sys$-Ved =>
                    vedvscr_set_wlinewidth, vedvscr_substring_num_fit,
                    vedonstatus, vedstatusbufferlimit,
                    vedstatusline, vedstatusshowcols,
                    ved_on_status, vedsetstatus, vedputmessage_apply,
                    vedputmessage, vedputcommand, vedredocommand, vedredokey,
                    vedstatusswitch, vedenter, vedenterkey,
                    vederror
                    ;

vars
    vedonstatus             = false,
    vedstatusbufferlimit    = 60,
    vedstatusshowcols       = false,
    vedstatusline,
    ;

lvars
    statusline_globals  = false,
    ;

lconstant macro FIXED_HEADER_LEN = STATUS_HEADER_LEN + 1;


;;; --- VARIABLE-WIDTH PROCEDURES -----------------------------------------
;;; Redefined by XVed in variable-width mode

    /*  Redefined by XVed in variable-width mode
    */
define vars vedvscr_set_wlinewidth();
    vedscreenwidth fi_- vedwcolumnoffset -> vedwlinewidth
enddefine;

define vars vedvscr_substring_num_fit(string, csub, maxchars, width);
    unless maxchars then
        vedusedsize(string) fi_- csub fi_+ 1 -> maxchars
    endunless;
    fi_min(maxchars, width)
enddefine;


;;; -----------------------------------------------------------------------

define active ved_on_status;
    vedonstatus
enddefine;
;;;
define updaterof active ved_on_status val;
    lvars val;

    lconstant macro STATUS_GLOBALS = [
       (vedbuffer,
        vedline,
        vedlineoffset,
        vedcolumn,
        vedcolumnoffset,
        vedpositionstack,
        vvedmarklo,
        vvedmarkhi,
        vvedmarkprops,
        marked_range_stack,
        vvedbuffersize,
        vvedlinesize,
        vedleftmargin,
        vedwindowlength,
        vedstatic,
        vedbreak)
    ];

    if vedonstatus then returnif(val) else returnunless(val) endif;

    if statusline_globals then
        explode(statusline_globals);
        fill(STATUS_GLOBALS, statusline_globals) ->
    else
        ;;; first time
        false -> vedonstatus;
        consvector(#| STATUS_GLOBALS |#) -> statusline_globals;
        ( {% repeat vedstatusbufferlimit times nullstring endrepeat %},
            1, 0, 1, 0, [], 10000, 0, true, conspair(false, []),
                                                0, 0, 0, 2, false, false )
    endif -> STATUS_GLOBALS;

    not(vedonstatus) -> vedonstatus;

    if vedonstatus then
        2 -> vedwindowlength;
        (0, FIXED_HEADER_LEN)
    else
        (1, 1)
    endif -> (vedwlineoffset, vedwcolumnoffset);
    vedvscr_set_wlinewidth()
enddefine;


define Clear_statusline();
    lvars string = vedstatusline;
    Set_subvedstring(0, 1, string, _pint(string!V_LENGTH))
enddefine;

define lconstant Redraw_status(_nchars, _tailblank, flush);
    lvars   vchar, string = vedstatusline, flush, _sindex, _col, _nchars,
            _ntrailsp, _tailblank;

    if vedediting and (not(USEWINDOWS) or wvedwindow) then
        0 -> _ntrailsp;
        if _tailblank then
            _nchars -> _sindex;
            while _sindex /== 0 and dup() == `\s` do
                () -> , _sindex fi_- 1 -> _sindex
            endwhile;
            _nchars fi_- _sindex -> _ntrailsp;
            _sindex -> _nchars
        endif;

        1 -> _col;
        _int(_nchars) _sub _1 -> _sindex;
        while _sindex _sgreq _0 do
            _user_sp()!(w)[_sindex] -> vchar;
            unless fast_subscrvedstring(_col, string) == vchar then
                vedwindowpoint(1, _col);
                vedscreenoutput(vchar)
            endunless;
            _col fi_+ 1 -> _col;
            _sindex _sub _1 -> _sindex
        endwhile;

        if _ntrailsp /== 0 then
            vedwindowpoint(1, _col);
            vedscreencleartail()
        endif;

        if flush then vedscr_flush_output() endif;
    endif;

    erasenum(_nchars)
enddefine;

    /*  Put the status header chars on the stack, including line number
        etc. Leaves ved_on_status true.
    */
define lconstant Stack_status_head_chars();
    lvars   x, n, _c_-, _c_-|, _c3, _c4, _c5, _c6, _c7, _c8,
            _cNought = (vedscreenstatusnumattr fi_&&~~ 16:FFFF) fi_|| `0`;

    ;;; must do this to ensure correct line number is picked up
    false -> ved_on_status;

    if XWINDOWS then
        `\s` ->> _c_- -> _c_-|
    else
        vedscreenstatus_-_mark -> _c_-;
        vedscreenstatus_-|_mark -> _c_-|;
        ;;; char 1 for non-XVed
        _c_-
    endif;

    ;;; char 2 (XVed 1)
    if VDDEV_LOADED and VDDEV_WEAK vedprocswaiting() then
        vedscreenstatusinputmark
    else
        _c_-
    endif;

    ;;; next 6 chars, 3 - 8 (XVed 2 - 7)
    if vedstatusshowcols then
        _cNought fi_||/& `\[1]` -> _cNought;    ;;; toggle bit 0 of colour
        vedcolumn
    else
        vedline
    endif -> x;
    6 -> n;
    while x fi_> 0 and n fi_> 0 do
        (x fi_// 10 -> x) fi_+ _cNought;
        n fi_- 1 -> n
    endwhile;
    if n == 1 then
        _c_-|
    elseif n /== 0 then
        while n fi_> 2 do
            `\s`;
            n fi_- 1 -> n
        endwhile;
        _c_-|, _c_-
    endif -> (_c8, _c7, _c6, _c5, _c4, _c3);
    _c3, _c4, _c5, _c6, _c7, _c8;

    ;;; XVed: char 8
    if XWINDOWS then `\s` endif;

    ;;; char 9 = STATUS_HEADER_LEN
    vedscreenstatus_|-_mark;

    true -> ved_on_status
enddefine;

define Redraw_status_head();
    dlocal ved_on_status;
    if vedstatusline!V_LENGTH _lt _:STATUS_HEADER_LEN then
        initdstring16(STATUS_HEADER_LEN) -> vedstatusline
    endif;
    Redraw_status(Stack_status_head_chars(), STATUS_HEADER_LEN, false, true)
enddefine;

define vedsetstatus(mess, bracket, doredraw);
    lvars   string, procedure (width_p = vedvscr_substring_width,
            space_p = vedvscr_space_width), more, doredraw, mess, bracket,
            vwmode = vedvarwidthmode, ls_diffs, pixrem, wlinewidth,
            _col, _ncols, _len, _spaces, _spwidth, _totwidth, _mswidth,
            _tailblank, _vcol, _pixrem;
    dlocal  ved_on_status;

    returnunless(isstring(vedstatusline));

    unless isstring(mess) then Check_string(mess) endunless;

    ;;; put characters to go on status line onto stack
    #|
        Stack_status_head_chars();
        if vedmarked(vedline) then vedscreenrangemark else `\s` endif;

        Buffer_line(vedline) -> string;
        vvedlinesize -> _ncols;

        ;;; spaces to allow for cursor
        fi_max(vedcolumn fi_- _ncols, 1) fi_+ 1 -> _spaces;
        space_p(_spaces) -> _spwidth;

        ;;; number of first visible col
        vedvscr_set_wline_start(1, string, _ncols) -> (_vcol, pixrem, ls_diffs);
        ;;; total width available on line
        vedwlinewidth fi_+ pixrem -> wlinewidth;
        ;;; cols before first visible
        _vcol fi_- 1 -> _col;
        ;;; number of cols visible
        _ncols fi_- _col -> _ncols;

        ;;; width of message
        width_p(mess, 1, _pint(mess!V_LENGTH)) -> _mswidth;
        if bracket then width_p('()', 1, 2) fi_+ _mswidth -> _mswidth endif;

        ;;; plus width of chars showing
        width_p(string, _vcol, _ncols) fi_+ _mswidth -> _totwidth;
        false -> more;
        if _totwidth fi_+ _spwidth fi_> wlinewidth then
            ;;; won't fit
            wlinewidth fi_- _totwidth -> _totwidth;
            if _totwidth fi_> 0 or _totwidth fi_|| _mswidth == 0
            or _ncols fi_<= 4 then
                fi_max(_totwidth fi_div space_p(1), 1) -> _spaces
            else
                true -> more;
                if _mswidth == 0 then
                    0 -> _spaces;
                    wlinewidth -> _totwidth;
                    if vwmode then
                        false -> more
                    else
                        _totwidth fi_- 1 -> _totwidth   ;;; for more mark
                    endif
                else
                    1 -> _spaces;
                    wlinewidth fi_- _mswidth fi_- space_p(2) -> _totwidth
                endif;
                if _totwidth fi_> 0 then
                    vedvscr_substring_num_fit(string, _vcol, _ncols, _totwidth)
                                                            -> _ncols;
                    _ncols fi_>> 16 -> pixrem;
                    _ncols fi_&& 16:FFFF -> _ncols;
                    if pixrem /== 0 and _mswidth == 0 then
                        _ncols fi_+ 1 -> _ncols
                    endif
                else
                    0 -> _ncols
                endif;
                fi_max(4, _ncols) -> _ncols
            endif
        endif;

        ;;; stack chars from buffer line and message
        _col fi_+ _ncols -> _ncols;
        while _col fi_< _ncols do
            _col fi_+ 1 -> _col;
            fast_subscrvedstring(_col, string)
        endwhile;
        if more then if vwmode then `\s` else vedscreenmoremark endif endif;
        until _spaces == 0 do
            _CHECKUSER;
            `\s`, _spaces fi_- 1 -> _spaces
        enduntil;
        if bracket then
            `(`, Explode_subvedstring(mess), `)`
        else
            Explode_subvedstring(mess)
        endif;
        unless dup() == `\s` then `\s` endunless;
    |# -> _len;

    ;;; This is the only place where vedstatusline is set for each file
    if vwmode then
        _pint(vedstatusline!V_LENGTH) -> _ncols;
        if _len fi_> _ncols then
            _len fi_+ (_len fi_>> 1) -> _ncols;
            initdstring16(_ncols) -> vedstatusline
        endif
    else
        vedscreenwidth -> _ncols;
        if vedstatusline!V_LENGTH /== _int(_ncols) then
            initdstring16(_ncols) -> vedstatusline
        endif
    endif;

    ;;; trim or pack to fit on screen
    if _len fi_>= _ncols then
        erasenum(_len fi_- _ncols)
    else
        if XWINDOWS then `\s` else vedscreenstatus_-_mark endif -> _spaces;
        _ncols fi_- _len -> _len;
        until _len == 0 do
            _CHECKUSER;
            _spaces; _len fi_- 1 -> _len;
        enduntil
    endif;

    if doredraw then
        vedstatusline -> string;
        if vwmode and ls_diffs fi_>> 16 /== 0 then
            ;;; start-of-line pixel offset changed -- force redraw of whole
            ;;; variable-width part of line
            Set_subvedstring(0, #_<FIXED_HEADER_LEN+1>_#, string,
                                        _ncols fi_- FIXED_HEADER_LEN)
        endif;
        true -> _tailblank;
        repeat
            () -> _col;     ;;; the char
            quitif(fast_subscrvedstring(_ncols, string) /== _col);
            returnif((_ncols fi_- 1 ->> _ncols) == 0);
            if _col /== `\s` then false -> _tailblank endif
        endrepeat;
        Redraw_status(_col, _ncols, _tailblank, doredraw /== "undef")
    else
        Fill_vedstring(vedstatusline, false) ->
    endif
enddefine;

define vedputmessage_apply(p);
    lvars p, file, localstatus;

    ;;; Note: vedstatusline must not be dlocal here. This allows it to be
    ;;; reset globally by vedsetstatus above when the length is wrong.
    ;;; (Cases that use a statusline from a 'residual' file below have a
    ;;; separate dlocal.)

    dlocal vedscreenoffset, wvedwindow, vedediting;

    define lconstant basewin_apply(p);
        lvars p;
        dlocal  cucharout = charout,
                cucharerr = charerr,
                pop_charout_device = popdevout,
                pop_charerr_device = popdeverr,
                pop_pr_quotes = false;
        fast_apply(false, p)
    enddefine;


    if XWINDOWS then
        unless vedsetupdone and vedupperfile then
            basewin_apply(p);
            return
        elseunless vedinvedprocess then
            ;;; allow messages from outside vedprocess
            true -> vedediting
        endunless
    elseif vedprintingdone or not(vedsetupdone and vedinvedprocess) then
        basewin_apply(p);
        return
    endif;

    vedcurrentfile -> file;
    false -> localstatus;

    unless file and (file==vedupperfile or file==vedlowerfile) then
        ;;; no current file or it's not on the csreen
        unless vedupperfile or vedlowerfile then
            ;;; no window "current" on the screen
            ;;; Using VDU or xterm, etc. put message at the bottom
            Clear_statusline();
            vedscreenlength fi_- 1
        elseif ispair(vedupperfile) then
            ;;; 'residual' window left over from a quit (or temporarily
            ;;; set by vedreadfile). For XVed, front of pair is the window.
            if XWINDOWS then
                fast_front(vedupperfile) -> wvedwindow;
                0, fast_back(vedupperfile)
            else
                fast_destpair(vedupperfile)
            endif -> localstatus
        elseif ispair(vedlowerfile) then
            ;;; 'residual' file left over from a quit
            fast_destpair(vedlowerfile) -> localstatus
        else
            chain(p, if vedupperfile then vedupperfile else vedlowerfile endif,
                        procedure(p, ved_current_file);
                            lvars p;
                            dlocal ved_current_file;
                            vedputmessage_apply(p)
                        endprocedure)
        endunless -> vedscreenoffset
    endunless;

    if localstatus then
        procedure(p, localstatus);
            lvars p, localstatus;
            dlocal  vedstatusline = localstatus,
                    vedscreenwidth = datalength(vedstatusline);
            MAXSCREENCOL ->> vedscreenline -> vedscreencolumn;
            fast_apply(true, p)
        endprocedure(p, localstatus)
    else
        fast_apply(true, p)
    endif;

    unless vedinvedprocess then
        Set_wait_cursor(true, true)
    endunless
enddefine;

define vars vedputmessage();
    vedputmessage_apply(
        procedure(string, use_ved);
            if isvector(string) then
                ;;; for compatibility with the message arg to sys_pr_message etc
                subscrv(1,string) -> string
            endif;
            unless use_ved then
                printf(string, ';;; %p\n')
            elseif vedediting then
                vedsetstatus(string, string /= nullstring, true)
            endunless;
            string -> vedmessage
        endprocedure)
enddefine;

define vedputcommand(string);
    lvars string, at_end = true;
    dlocal ved_on_status = true;
    if isboolean(string) then ((), string) -> (string, at_end) endif;
    if at_end then vedendfile() endif;
    Copy_vedstring(vedencodetabs(string)) -> vedthisline();
    vedtextright()      ;;; put cursor after command
enddefine;

define Set_vedcommand(copy_to_end);
    lvars copy_to_end, string, used, save, savesize;
    returnunless(ved_on_status);
    vedtrimline();
    vedthisline() -> string;
    veddecodetabs(string) -> vedcommand;
    vedusedsize(vedbuffer) -> used;
    if copy_to_end and vedline fi_< used and string /= nullstring
    and string /= vedbuffer(used) then
        (vedline, vvedlinesize) -> (save, savesize);
        used fi_+ 1 -> vedline;
        Copy_vedstring(string) -> vedthisline();
        (save, savesize) -> (vedline, vvedlinesize);
    endif;
    false -> ved_on_status
enddefine;

define vedredocommand;
    if ved_on_status then
        Set_vedcommand(true);
        Unmark_cursor_line();
    else
        vedtrimline();
        true -> ved_on_status;
        Set_vedcommand(true);
    endif;
    veddocommand();
enddefine;

vars procedure vedredokey = vedredocommand;

define vedstatusswitch;
    if ved_on_status then
        Set_vedcommand(false);
        Unmark_cursor_line();
    else
        Mark_cursor_line();
        vedtrimline();
        true -> ved_on_status;
    endif;
enddefine;

define vedenter();
    unless ved_on_status then vedstatusswitch() endunless;
    vedendfile();
    ;;; lose the first command if buffer too big
    if vedusedsize(vedbuffer) fi_> vedstatusbufferlimit and vvedmarklo /== 1
    then
        Shift_buffer_lines(1, -1)
    endif;
enddefine;

vars procedure vedenterkey = vedenter;

define Do_status_range();
    lvars line, _hi;
    if vvedmarkhi == 0 then
        veddocr()
    else
        vedline -> line;
        vvedmarklo -> vedline;
        fi_min(vvedmarkhi, vvedbuffersize) -> _hi;
        until vedline fi_> _hi do
            vedsetlinesize();
            Set_vedcommand(false);
            veddocommand();
            true -> ved_on_status;
            vedline fi_+ 1 -> vedline
        enduntil;
        line -> vedline;
        Set_vedcommand(false);
        Unmark_cursor_line();
    endif;
enddefine;


;;; --- MESSAGES -------------------------------------------------------

define vars vederror(string);
    lvars string;
    if vedinvedprocess then
        Open_file_window();
        vedputmessage(string);
        vedscreenbell();
        vedinterrupt();
    else
        mishap(0, string)
    endif
enddefine;


endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 26 1999
        Changes to make status line work properly in variable-width mode
--- John Gibson, Sep 28 1998
        Turned the bulk of vedputmessage into a new procedure
        vedputmessage_apply, which is now used by vedputmessage.
--- John Gibson, Aug 15 1997
        Changes for XVed variable-width mode
--- John Gibson, Jan 22 1996
        marked_range_stack now initialised to a pair in ved_on_status.
--- John Gibson, Dec  4 1995
        Made vedsetstatus put out a space after vedscreenmoremark when used.
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Sep 13 1995
        Changed to work with associated char data on status line
--- John Gibson, Jan 18 1995
        Replaced unsatisfactory Set*_status_lengths (which used vedappfiles
        to reset vedstatusline for all files), with ENS*URE_STATUSLINE which
        merely resets it for the current file if necessary. For this to work
        without constantly creating garbage, vedstatusline is now only
        dlocalled by vedputmessage for the 'residual' file cases.
--- John Gibson, Jan 19 1994
        Made vedstatic local to the status line file
--- John Gibson, Jan 10 1994
        Modified layout of status header for XVed.
        Made line number chars take attributes from vedscreenstatusnumattr.
--- John Gibson, Oct  9 1992
        Fixed last change to simply test wvedwindow (since on a qved,
        vedcurrentfile can be false, but vedputmessage assigns window from
        vedupperfile to wvedwindow).
--- Adrian Howard, Sep 22 1992
        Redraw_status now checks that it has a window to draw in
--- John Gibson, Sep 21 1992
        Undid last change (vedediting is supposed to mean don't draw on
        window). Made REF *vedputmessage agree with what it actually does.
--- Adrian Howard, Sep  7 1992
        --- Made vedputmessage put string on standard output if vedediting was
        false so its operation agrees with REF *vedputmessage
        --- Stopped vedputmessage adding quotes around string when it was
        printed to the standard output
--- Adrian Howard, Sep  2 1992
        vedsetstatus now does nothing if vedstatusline is not a string
--- John Gibson, Aug  4 1992
        Stopped updater of ved_on_status from calling vedtrimline. Added
        calls of it in vedstatusswitch and vedredocommand instead.
--- John Gibson, Mar  3 1992
        Got rid of vedst*atusheader. Also got rid of vedsw*itchstatus.
--- John Gibson, Jan 23 1992
        New graphic chars
--- John Gibson, Jan  8 1992
        Changes to cope with dstrings
--- John Gibson, Jun 13 1991
        Added active var ved_on_status
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- John Gibson, Apr  9 1991
        Test for iscaller(vedprocess) changed to vedinvedprocess
--- John Gibson, Mar 21 1991
        Made -vedputmessage- assign standard output devs when using printf.
        Whole file into Sys$-Ved$-.
--- John Gibson, Jan 30 1991
        "undef" 3rd arg to -vedsetstatus- means don't flush output
--- John Gibson, Jan  7 1991
        Removed test for -vedediting- from -vederror- (just leaving test
        for iscaller(vedprocess)
--- John Williams, Nov 14 1990
        Added -vedstatusshowcols-
--- Aaron Sloman, Oct 13 1990
        Altered vedputmess so that if USEWINDOWS then it doesn't
        put message at bottom of screen.
--- Aaron Sloman, Oct 12 1990
        changed xved to wved
--- Aaron Sloman, Sep 18 1990
        Made vedputmessage user definable (for Xved) (fix vddeclare.ph etc.)
--- Rob Duncan, Feb  2 1990
        Removed some redundant declarations
 */
