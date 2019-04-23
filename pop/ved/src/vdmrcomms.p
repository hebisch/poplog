/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/ved/src/vdmrcomms.p
 > Purpose:
 > Author:          Various (see revisions)
 */

;;; ------------- COMMANDS FOR DELETING/MOVING MARKED RANGES --------------

#_INCLUDE 'vddeclare.ph'

global constant
        procedure (vedmarkfind, vedrefreshrange)
    ;

global vars
        procedure (ved_d)
    ;


;;; -----------------------------------------------------------------------

section $-Sys$-Ved =>   vveddump,
                        ved_clear, ved_y, ved_copy, ved_t,
                        ved_da, ved_d, ved_m
                    ;

vars
    vveddump    = [],   ;;; Marked range deleted last
    ;


define vars ved_clear();
    lvars used;
    vedusedsize(vedbuffer) -> used;
    if used == 0 then
        vedtopfile();
        0 -> vvedmarkhi; 100000 -> vvedmarklo;  ;;; clear marked range
        vedrefreshrange(1, vedwindowlength fi_- 1, false);  ;;; and on screen
        vedputmessage('\{b}already empty');
    else ;;; mark whole file and delete it
        1 -> vvedmarklo;
        used -> vvedmarkhi;
        ved_d();    ;;; gives 'BUFFER EMPTY' message, keeps old in vveddump
        vedtopfile();
    endif;
enddefine;


;;; --- OPERATIONS ON MARKED RANGES ---------------------------------------


define lconstant Show_range(_afterline, _len);
    lvars _afterline, _len, _bottom;
    if vedediting and not(vedonstatus) then
        Bottom_of_window() -> _bottom;
        if _afterline fi_> _bottom then
            vedalignscreen()
        elseif _afterline == _bottom then
            vedscrollvert(1)
        elseif _afterline fi_<= vedlineoffset then
            vedcheck();
        else
            _afterline fi_+ 1 -> _afterline;
            vedscrollregion(Window_line(_afterline), vedwindowlength,
                            _len, _afterline);
        endif;
    endif;
enddefine;

define lconstant Get_after_line(moving) -> _afterline;
    lvars moving, _afterline;

    define lconstant getlinearg(lowlim);
        lvars line, lowlim;
        if isstring(vedargument) and vedargument /= nullstring then
            unless (strnumber(vedargument) ->> line) and line fi_>= lowlim then
                vederror('\{b}invalid line number in command')
            else line
            endunless;
        else false
        endif;
    enddefine;

    unless getlinearg(0) ->> _afterline then
        vedline -> _afterline;
    endunless;
    if moving and _afterline fi_>= vvedmarklo
    and _afterline fi_<= vvedmarkhi then
        vederror('\{b}cannot move to this position')
    endif;
    unless _afterline == vedline then
        vedjumpto(fi_max(_afterline, 1),vedcolumn)
    endunless;
enddefine;

define Yank_stack(_len, do_copy, codetabs);
    lvars item, item2, do_copy, codetabs, _afterline, _line, _len;
    Get_after_line(false) -> _afterline;
    Shift_buffer_lines(_afterline fi_+ 1, _len);
    if _afterline fi_< 1 then 1 -> vedline; vedsetlinesize(); endif;
    _afterline fi_+ _len -> _line;
    if codetabs then
        fast_repeat _len times
            -> item;
            vedencodetabs(item) -> item2;
            if do_copy and item == item2 then
                Copy_vedstring(item) -> item2
            endif;
            item2 -> Buffer_line(_line);
            _line fi_- 1 -> _line;
        endrepeat;
    else
        repeat _len times
            if do_copy then Copy_vedstring() endif -> Buffer_line(_line);
            _line fi_- 1 -> _line;
        endrepeat;
    endif;
    vedusedsize(vedbuffer) -> vvedbuffersize;
    Show_range(_afterline, _len);
enddefine;

define vars ved_y();
    ;;; copy vveddump into buffer after current line
    lvars _len;
    length(vveddump) -> _len;
    if _len == 0 then vederror('\{b}nothing to yank') endif;
    explode(vveddump);
    Yank_stack(_len, true, true);
enddefine;

define lconstant Append_range(_copy, _normalise);
    ;;; copy marked range and append to vveddump
    lvars string, org_string, list = [], _line, _copy, _normalise;
    if vvedmarkhi == 0 then vedmarkfind() endif; ;;; cause error
    if vedline < 1 then 1 -> vedline; vedsetlinesize() endif;
    vedtrimline();
    vvedmarkhi fi_+ 1 -> _line;
    until _line == vvedmarklo do
        _checkinterrupt();
        _line fi_- 1 -> _line;
        if _normalise then
            Normalised_line(_line, _copy)
        else
            Buffer_line(_line), if _copy then Copy_vedstring() endif
        endif :: list -> list
    enduntil;
    vveddump nc_<> list -> vveddump
enddefine;


define vars ved_copy();
    ;;; copy marked range into vveddump, decoding tabs
    [] -> vveddump;
    Append_range(true, true);
enddefine;

    ;;; Transcribe (copy) marked range after line
define vars ved_t();
    dlocal vveddump = [];
    Append_range(false, false);
    explode(vveddump);
    Yank_stack(length(vveddump), true, false);
enddefine;


define lconstant Num_onscreen();
    lvars _bottom = Bottom_of_window();
    if vvedmarkhi fi_<= vedlineoffset or vvedmarklo fi_> _bottom then
        0
    else
        fi_min(vvedmarkhi, _bottom)
            fi_- fi_max(vvedmarklo, vedlineoffset fi_+ 1) fi_+ 1;
    endif
enddefine;

    ;;; Delete marked range, append to vveddump
define vars ved_da();
    lvars _top, _onscreen;
    Append_range(false, true);
    Num_onscreen() -> _onscreen;
    if _onscreen /== 0 then
        fi_max(Window_line(vvedmarklo), 2) -> _top;
    endif;
    Shift_buffer_lines(vvedmarklo, vvedmarklo fi_- vvedmarkhi fi_- 1);
    if vedline < 1 then 1 -> vedline; vedsetlinesize() endif; ;;; Oct 85 A.S.
    if vedline == vedlineoffset then
        vedlineoffset fi_- 1 -> vedlineoffset;
        if vedediting then
            vedrefreshline(false, 2, Buffer_line(vedline), false);
            3 -> _top;
            _onscreen fi_- 1 -> _onscreen;
        endif;
    endif;
    if vedediting and _onscreen /== 0 then
        vedscrollregion(_top, vedwindowlength, 0 fi_- _onscreen,
                Bottom_of_window() fi_- _onscreen fi_+ 1);
    endif;
    if vvedbuffersize == 0 then vedputmessage('\{b}buffer empty') endif;
enddefine;

define vars ved_d();
    ;;; delete marked range, and store it in vveddump. Lose old contents of vveddump
    if vvedmarkhi == 0 then vedmarkfind() endif; ;;; cause error
    [] -> vveddump;
    ved_da();
enddefine;

    ;;; Move marked range to after current line, or given line
define vars ved_m();
    lvars onscreen, moveon, _afterline, _len, _top, _bottom, _rem;
    if vvedmarkhi == 0 then vedmarkfind() endif; ;;; cause error
    Get_after_line(true) -> _afterline;
    vvedmarkhi fi_- vvedmarklo fi_+ 1 -> _len;
    Num_onscreen() -> onscreen;
    if onscreen /== 0 then
        if vvedmarkhi fi_< _afterline then
            0 fi_- onscreen -> onscreen;
            fi_max(vvedmarklo, vedlineoffset fi_+ 1) -> _top;
            _afterline -> _bottom;
        else
            vvedmarklo -> _bottom;
            _afterline fi_+ 1 -> _top;
        endif;
        Window_line(_top) -> _top;
        Window_line(_bottom) -> _bottom;
    endif;
    ved_winrel_line(_afterline) -> moveon;

    Shift_buffer_lines(_afterline fi_+ 1, _len);
    move_subvector(vvedmarklo, vedbuffer, _afterline fi_+ 1, vedbuffer, _len);
    vedusedsize(vedbuffer) -> vvedbuffersize;
    vvedmarklo -> _rem;
    _afterline fi_+ 1 -> vvedmarklo;
    _afterline fi_+ _len -> vvedmarkhi;
    Shift_buffer_lines(_rem, - _len);
    vvedmarklo fi_- 1 -> _afterline;
    if _afterline fi_< 1 then 1 -> vedline; vedsetlinesize() endif;

    returnunless(vedediting and not(vedonstatus));

    if onscreen == 0 then
        Show_range(_afterline, _len)
    elseunless moveon then
        vedalignscreen()
    else
        vedscrollregion(_top, _bottom, onscreen, vvedmarklo);
        _len fi_- abs(onscreen) -> _rem;
        unless _rem == 0 then
            _len fi_- _rem -> _len;
            if _neg(onscreen) then _bottom fi_+ 1
            else _top fi_+ _len
            endif -> _top;
            unless _top fi_> vedwindowlength then
                vedscrollregion(_top, vedwindowlength, _rem,
                                            vvedmarklo fi_+ _len)
            endunless
        endunless
    endif
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  7 1995
        Changed to use Copy_vedstring instead of copy on buffer strings
--- John Gibson, Dec 21 1992
        Moved text justification commands to vdfill.p
--- John Gibson, Mar 31 1992
        Sectionised
--- John Gibson, Jan  8 1992
        Changes for dstrings
--- John Gibson, Oct 26 1991
        Replaced vedscrollup with vedscrollvert(1)
--- Aaron Sloman, May  9 1989
        Added extra tests to ensure vedline > 0
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
--- John Gibson, Aug 16 1987
        Tidied up
 */
