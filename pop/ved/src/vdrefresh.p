/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/ved/src/vdrefresh.p
 > Purpose:         Refreshing some or all of a VED window
 > Author:          John Gibson, Aaron Sloman 1982? (see revisions)
 > Documentation:   REF * VEDPROCS
 > Related Files:
 */

;;;------------------- BASIC SCREEN REFRESHING -----------------------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedansiscreenxy, vedscreencontrol, vedscreenovermode,
        vedinascii, vedscreensubstringout, Sys$-Ved$-Clear_statusline),
    ;

vars
        procedure (vedscreenreset, vedrestorescreen,
        vedsetonscreen, vedscreenraw, vedscr_substring_out
        ),
        vednokeypad, vedvarwidthmode
    ;

;;; ------------------------------------------------------------------------

section $-Sys$-Ved =>
                    vedvscr_substring_width, vedvscr_set_wline_start,
                    vedvscr_refresh_part_line,
                    vedrefreshrange, vedrefresh,
                    vedrefreshwindow, vedrefreshstatus,
                    vedscreenblankpartline, vedscreensubstringout,
                    vedrefreshpartline, vedrefreshtail, vedrefreshline,
                    vedrestorescreen
                ;


;;; --- VARIABLE-WIDTH PROCEDURES -----------------------------------------
;;; Redefined by XVed in variable-width mode

define vars vedvscr_substring_width(string, csub, nchars);
    unless nchars then
        vedusedsize(string) fi_- csub fi_+ 1 -> nchars
    endunless;
    nchars
enddefine;

define vars vedvscr_set_wline_start(wline, string, usize);
                                        /* -> (index, pixrem, diffs) */ ;
    lvars wline, string, usize;
    vedcolumnoffset fi_+ 1, 0, 0
enddefine;

define vars vedvscr_refresh_part_line(wline, wcol, string, csub, nchars,
                                                            usize, no_opt);
    lvars wline, wcol, string, csub, nchars, usize, no_opt;
    ;;; space on screen
    lvars scrlen = vedscreenwidth fi_- wcol fi_+ 1;
    if usize fi_> scrlen and nchars fi_>= scrlen then
        ;;; more chars available in string than can be displayed
        vedscreensubstringout(string, csub, scrlen fi_- 1, no_opt);
        ;;; indicate that there's more text visible
        vedwindowpoint(wline, vedscreenwidth);
        vedscreenoutput(vedscreenmoremark)
    else
        ;;; use all the characters
        vedscreensubstringout(string, csub, nchars, no_opt);
        if vedscreencolumn fi_>= vedscreenwidth then
            MAXSCREENCOL ->> vedscreenline -> vedscreencolumn
        endif
    endif
enddefine;


;;; --- REFRESHING PARTS OR ALL OF THE SCREEN ------------------------------

define Set_refresh_needed(line);
    lvars line;
    unless vedonstatus then
        if vedrefreshneeded then
            fi_min(vedrefreshneeded, line)
        else
            line
        endif -> vedrefreshneeded
    endunless
enddefine;

    /*  Refresh screen from x to y, marking as necessary
        x and y are buffer line numbers
        if mark is true mark the line. If it is false don't. Otherwise
        mark if within the marked range.
    */
define vedrefreshrange(x, y, mark);
    lvars x, y, mark;
    returnunless(vedediting);
    procedure;
        dlocal vedscreencursoron = false;

        if isboolean(mark) then
            if mark then vedscreenrangemark else `\s` endif
        else
            false
        endif -> mark;
        fi_max(x, vedlineoffset fi_+ 1) -> x;
        fi_min(y, Bottom_of_window()) -> y;
        until x fi_> y do
            vedrefreshline(false, x fi_- vedlineoffset   fi_+  1, Buffer_line(x),
                if mark then mark else vedmarked(x) endif);
            x fi_+ 1 -> x;
        enduntil;
    endprocedure();
enddefine;

    /*  Refresh screen from window line vedrefreshneeded to the end of the
        window, used for DUMB vdus which can't insert or delete lines
    */
define Refresh_lower(setcurs);
    lvars setcurs, save = ved_on_status;
    if save then not(ved_on_status) -> ved_on_status endif;
    vedrefreshrange(vedrefreshneeded fi_+ vedlineoffset fi_- 1,
                                                Bottom_of_window(), undef);
    ;;; undef means mark if necessary
    if save then not(ved_on_status) -> ved_on_status endif;
    false -> vedrefreshneeded;
    if setcurs then Set_wait_cursor(false, true) endif
enddefine;

define vedrefreshwindow(setcurs);
    lvars setcurs;
    if vedediting and not(ved_on_status) then
        2 -> vedrefreshneeded;
        Refresh_lower(setcurs)
    endif
enddefine;

    /*  Refresh only the status line
    */
define vedrefreshstatus();
    returnunless(vedediting);
    false -> vedrefreshneeded;
    vedscreenraw();
    Clear_statusline();
    vedsetstatus(nullstring, false, true)
enddefine;

    /*  Refresh whole window, including status line
    */
define vedrefresh();
    lvars save;
    returnunless(vedediting);
    vedrefreshstatus();
    if ved_on_status ->> save then not(ved_on_status) -> ved_on_status endif;
    vedrefreshwindow(false);
    if save then not(ved_on_status) -> ved_on_status endif;
    Set_wait_cursor(true, true)     ;;; make sure cursor is on
enddefine;


    /*  On window line _wline, starting from window column _wcol blank
        _num spaces. (Leave Ved buffer unchanged).
    */
define vedscreenblankpartline(_wline, _wcol, _num) -> partline;
    lvars _div, _rem, _len, _wline, _wcol, _num, partline;
    lconstant clearstring = '\s\s\s\s\s\s\s\s\s\s\s\s\s\s\s\s\s\s\s\s';

    vedwindowpoint(_wline, _wcol);
    0 -> vedscreencharmode;
    if _num == 0
    or (not(vedvarwidthmode) and _wcol fi_+ _num fi_>= vedscreenwidth) then
        vedscreencleartail();
        false
    else
        vedscreenovermode();    ;;; probably not necessary

        ;;; find out how many times to output clearstring as a whole, and do it
        _num // (datalength(clearstring) ->> _len) -> _div -> _rem;
        until _div == 0 do
            vedscr_char_out(clearstring);
            vedscreencolumn fi_+ _len -> vedscreencolumn;
            _div fi_- 1 -> _div;
        enduntil;

        ;;; now output the remainder
        vedscr_substring_out(clearstring, 1, _rem);
        vedscreencolumn fi_+ _rem -> vedscreencolumn;
        true
    endif -> partline
enddefine;

    /*  Transmit _clen chars in string starting from _index
        process tabs etc.
    */
define vedscreensubstringout(string, _index, _clen, _no_opt);
    lvars   string, Tab, _cptr, _clen, _char, _abptr, _cstart, _wstart,
            _nspace, _index, _index_lim, _isdstr, _no_opt, _ncurpos;

    define :inline lconstant NEXTCHAR(T=item);
        _cptr!(T)++ -> (_char, _cptr);
        if _abptr /== _NULL and _nonzero(_abptr!(b)++ -> _abptr) then
            _0 -> _char
        endif
    enddefine;

    define :inline lconstant OUTSTRING(T=item, _ATTR_PTR=item);
        while _index _lt _index_lim do
            string@(w->T)[_index] -> _cptr;
            if _isdstr then
                _ATTR_PTR(string, _index) -> _abptr
            else
                _NULL -> _abptr
            endif;
            _index_lim _sub _index -> _clen;    ;;; chars to go
            NEXTCHAR(T);
            _cptr -> _cstart;
            ;;; find out how many ordinary printable chars, spaces, etc follow
            repeat
                if (_:`\s` _lt _char and _char _lt _:`\^?`)
                or (_char _gr _:16:A0
                    and (_char _gr _:16:FF or pop_character_set))
                then
                    ;;; ordinary printing char
                    if _zero(_clen _sub _1 ->> _clen) then
                        ;;; stop at end of string
                        ##(T){_cptr, _cstart} _add _1 -> _clen;
                        _0 -> _nspace;
                        quitloop
                    else
                        NEXTCHAR(T)
                    endif
                elseif (_char == _:`\s` or _char == _:`\t`) then
                    ;;; white space _char
                    _cptr -> _wstart;
                    false -> Tab;
                    repeat
                        if _char == _:`\t` then true -> Tab endif;
                        if _zero(_clen _sub _1 ->> _clen) then
                            _cptr@(T)++ -> _cptr;
                            quitloop
                        endif;
                        NEXTCHAR(T);
                        quitif(_char /== _:`\s` and _char /== _:`\t`)
                    endrepeat;
                    ##(T){_cptr, _wstart} -> _nspace;
                    if _nspace _lt _ncurpos and _nonzero(_clen) then
                        ;;; too few spaces to be worth a jump, so if there
                        ;;; were no tabs, just treat them as printing characters
                        nextunless(Tab);
                        _0 -> _nspace
                    endif;
                    ##(T){_wstart, _cstart} -> _clen;
                    quitloop
                else
                    ;;; special char, or one with attributes
                    ##(T){_cptr, _cstart} -> _clen;
                    _0 -> _nspace;
                    quitloop
                endif
            endrepeat;

            unless _zero(_clen) then
                ;;; dump _clen standard printable chars
                0 -> vedscreencharmode;
                vedscr_substring_out(string, _pint(_index _add _1),
                                                            _pint(_clen));
                _index _add _clen -> _index;
                _pint(_clen) fi_+ vedscreencolumn -> vedscreencolumn
            endunless;
            if _index _lt _index_lim then
                if _nonzero(_nspace) then
                    ;;; Some space characters not transmitted. Jump instead
                    _index _add _nspace -> _index;
                    if _no_opt then
                        fast_repeat _pint(_nspace) times
                            vedscreenoutput(`\s`)
                        endrepeat
                    elseif _index _lt _index_lim then
                        vedwindowpoint(vedscreenline fi_- vedscreenoffset,
                                        vedscreencolumn fi_+ _pint(_nspace))
                    endif
                else
                    ;;; output next character singly: it's special
                    ;;; (e.g. graphic or has attributes)
                    vedscreenoutput(
                        fast_subscrdstring(_pint(_index _add _1), string) );
                    _index _add _1 -> _index
                endif
            endif
        endwhile
    enddefine;      /* OUTSTRING */

    _int(_index) _sub _1 -> _index;
    _index _add _int(_clen) -> _index_lim;
    isdstring(string) -> _isdstr;

    ;;; number of spaces cheaper to transmit than repositioning
    ;;; the cursor
    if _no_opt then
        _:MAXSCREENCOL
    elseif vedscreenxy == vedansiscreenxy then
        _6
    else
        _4
    endif -> _ncurpos;

    if isstring16(string) then
        OUTSTRING(s, nonmac _DSTRING16_ATTR_PTR)
    else
        OUTSTRING(b, nonmac _DSTRING_ATTR_PTR)
    endif
enddefine;      /* vedscreensubstringout */


    /*  Refresh part of WINDOW line wline, Starting from WINDOW column wcol,
        using num characters from string, starting from string location
        start. If _flags is true, line is already blank.
        If num == 0 then use all of string.
    */
define vedrefreshpartline(_flags, wline, wcol, index, num, string);
    lvars string, wline, wcol, index, num, diffs, _flags, _len;

    lconstant macro (
        BLANK   = 2:1e0,
        NO_OPT  = 2:1e1,
    );

    dlocal vedonstatus = false; ;;; to prevent vedscreenoutput being clever

    if isinteger(_flags) then
        _int(_flags)
    elseif _flags then
        _:BLANK
    else
        _0
    endif -> _flags;

    returnunless(vedediting);

    vedusedsize(string) -> _len;
    if index == 0 then
        ;;; use current column offset
        vedvscr_set_wline_start(wline, string, _len) -> (index, _, diffs);
        ;;; next line used by var width right scrolling in XVed
        unless num then
            (diffs fi_&& 16:FFFF) fi_+ 1 -> num
        endunless
    endif;
    _len fi_- index fi_+ 1 -> _len; ;;; chars available

    if _flags _bitst _:BLANK or vedscreenblankpartline(wline, wcol, num) then
        vedwindowpoint(wline, wcol)
    endif;

    if num == 0 or num fi_> _len then
        _len -> num     ;;; use them all
    endif;
    if num fi_> 0 then
        vedvscr_refresh_part_line(wline, wcol, string, index, num, _len,
                                                _flags _bitst _:NO_OPT)
    endif
enddefine;

define vedrefreshtail(blank, wline, wcol, scol, string);
    lvars blank, wline, wcol, scol, string;
    dlocal vedonstatus = false;     ;;; n.b. this is needed!
    returnunless(vedediting);
    vedwindowpoint(wline, wcol);
    unless blank then vedscreencleartail() endunless;
    vedrefreshpartline(true, wline, wcol, scol, 0, string);
enddefine;

    /*  Note that here wline is the line in the WINDOW not in the buffer
        status line is line 1.
    */
define vedrefreshline(blank, wline, string, _mark);
    lvars blank, wline, string, _mark;
    dlocal vedonstatus = false;     ;;; n.b. this is needed!
    returnunless(vedediting);
    vedwindowpoint(wline, 1);
    vedscreenoutput(if _mark then _mark else `\s` endif);
    vedrefreshtail(blank, wline, 2, 0, string);
    ;;; invalidate screen marks for any stacked marks
    false -> fast_front(marked_range_stack)
enddefine;

define vars vedrestorescreen();
    ;;; to be used after printout by POP or the system. Assumes that the
    ;;; global variable vedprintingdone is set true
    dlocal  interrupt, cucharout,
            pop_charin_device  = popdevin,
            pop_charout_device = popdevout,
            pop_charerr_device = popdeverr
        ;

    returnif(XWINDOWS or not(vedediting and vedprintingdone));

    popdevin  -> pop_charin_device;
    popdevout -> pop_charout_device;
    popdeverr -> pop_charerr_device;
    charout -> cucharout;
    setpop -> interrupt;

    vedscreenreset();       ;;; ensure cursor at bottom of screen
    printf('\nVED HERE: PRESS RETURN TO CONTINUE: \^@');    ;;; null flushes
    sys_clear_input(poprawdevin);
    rawcharin() -> ;        ;;; NOT vedinascii!!
    false -> vedprintingdone;
    vedscreenraw();
    MAXSCREENCOL ->> vedscreenline -> vedscreencolumn;
    false ->> vedupperfile -> vedlowerfile;
    vedsetonscreen(ved_current_file, false)
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 15 1997
        Changes for XVed variable-width mode.
--- John Gibson, Apr 28 1997
        vedrefreshpartline now puts 16-bit chars out normally.
--- John Gibson, Feb 15 1997
        Made vedrefreshpartline cope with string16s (temporarily puts
        16-bit chars out singly through vedscreenoutput)
--- John Gibson, Jan 22 1996
        Made vedrefreshline assign false to front of marked_range_stack
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- John Gibson, Sep 25 1992
        Made vedrefresh ensure vedscreencursoron is true after refreshing
--- John Gibson, Apr 16 1992
        Sectionised
--- John Gibson, Jan 23 1992
        Replaced vedscreenm*ark with -vedscreenrangemark- for marked range and
        -vedscreenmoremark- for more text
--- John Gibson, Jan  4 1992
        Changed -vedscreensubstringout- to deal with dstrings
--- John Gibson, Dec 20 1991
        Replaced vedscreeng*raphoff() with 0 -> vedscreencharmode
--- Adrian Howard, Jul 24 1991 : Bug fix in -vedrefreshrange-, now does
        -vedediting- check *before* it tries to change the cursor
--- John Gibson, Jul  8 1991
        Bug fix in vedrefreshpartline
--- John Gibson, Jul  8 1991
        Added turning off/on of cursor in vedrefreshrange
--- John Gibson, Jun 28 1991
        vedrefreshpartline changed to allow flag arg to specify no
        optimisation of spaces
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- John Gibson, Apr  9 1991
        Changed to use vedscr_ procedures for output
--- Aaron Sloman, Oct 12 1990
        Changed xved to wved
--- Aaron Sloman, Oct  2 1990
        Altered test for _ncurpos in vedscreensubstringout
        renamed vedrefreshregion as vedrefreshpartline, and fixed bug
        added vedscreenblankpartline
--- Aaron Sloman, Sep 23 1990
        Added XVED test to vedrestorescreen, and other places
--- Aaron Sloman, Aug 10 1990
        Made vedscreenreset vars
--- John Gibson, Jun  6 1990
        Replaced use of internal device vars with public active vars
--- Ian Rogers, Mar 14 1990
        Added -vedrefreshregion- and made -vedrefreshtail- use it. Also, in
        -vedrefreshregion- changed fast_apply of poprawdevout!D_WRITE to a
        call of -rawsubstringout-
--- Rob Duncan, Feb  2 1990
        Made -vedrefresh- do a full -vedscreenraw- to reinitialise the
        terminal properly
 */
