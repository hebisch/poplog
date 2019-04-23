/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/ved/src/vddelete.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;---------- PROCEDURES FOR DELETING CHARACTERS --------------------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedscreenovermode, veddecodetabs)
    ;

vars
        procedure (vedscreendeletechar, vedvscr_window_column_bounds,
        vedvscr_average_width, vedvscr_substring_width),
        vedhardtabs, vvedpromptchar, vedcharinsert_acont,
        vedvarwidthmode
    ;

section $-Sys$-Ved;

constant
        procedure (Shift_string, Find_tab_start, Realign_tab, Adjust_tab,
        Insert_char, Refresh_screenline_tail, Span_insert, Set_cursor_to,
        Check_active_split
        )
    ;

vars
        screenline_refresh_col, screenline_tail_isclear,
    ;

endsection;


;;; -----------------------------------------------------------------------

section $-Sys$-Ved =>   vvedlinedump, vvedworddump,
                        vedspandelete, vedwordleftdelete,
                        vedwordrightdelete, vedclearhead, vedcleartail,
                        veddotdelete, vedchardelete, vedlinedelete
                    ;

vars
    vvedlinedump    = nullstring,   ;;; last line deleted
    vvedworddump    = nullstring,   ;;; last word or portion of line deleted.
    ;


define Check_not_in_prompt(want_start);
    lvars col, want_start, string;
    Check_not_in_tab(vedcolumn);
    if vvedpromptchar then
        Buffer_line(vedline) -> string;
        if locchar(vvedpromptchar, vedcolumn, string) then
            vederror('\{b}inside prompt')
        endif;
        if want_start then
            locchar_back(vvedpromptchar, vedcolumn, string) -> col;
            if col then col fi_+ 1 else 1 endif;
        endif
    elseif want_start then
        1
    endif;
enddefine;

define Do_span_delete(_scol, _ecol);
    lvars   string = Buffer_line(vedline), vwmode = vedvarwidthmode,
            _scol, _ecol, _col, _size, _right, _oldsize, _coloffs, _wrline,
            _rfcol = false;

    _ecol fi_- _scol -> _size;
    if (vedediting and ved_winrel_line(vedline)) ->> _wrline then
        vedvscr_window_column_bounds(_wrline, true) -> (_coloffs, , _right, );
        fi_max(_coloffs fi_+ 1, _scol) -> _col      ;;; 1st col on screen
    endif;

    if vedstatic and _ecol fi_<= vvedlinesize then
        Set_subvedstring(`\s`, _scol, string, _size);
        if _wrline and not(vedonstatus) then
            if vwmode and _scol fi_< _col then
                ;;; to left of screen -- redo whole line
                0 -> _rfcol
            else
                fi_min(_ecol, _right) -> _ecol;
                while _col fi_< _ecol and Set_cursor_to(vedline, _col) do
                    vedscreenoutput(`\s`);
                    _col fi_+ 1 -> _col
                endwhile;
                if vwmode and vvedlinesize fi_>= _right then
                    _right -> _rfcol
                endif
            endif
        endif
    else
        Shift_string(_scol, 0 fi_- _size, string, false) -> string;
        vvedlinesize -> _oldsize;
        vedsetlinesize();
        if _wrline and _col fi_<= _right and _oldsize fi_> _coloffs then
            _col -> _rfcol;
            if vwmode and _scol fi_< _col then
                ;;; to left of screen -- redo whole line
                0 -> _rfcol
            elseunless _ecol fi_> _oldsize or vvedlinesize fi_<= _coloffs then
                unless vednocharinsert or _ecol fi_> _right
                or (_ecol == _right and _ecol fi_< _oldsize) then
                    if _col fi_< screenline_refresh_col then
                        Set_cursor_to(vedline, _col) ->;
                        repeat _size times vedscreendeletechar() endrepeat;
                        screenline_refresh_col fi_- _size -> screenline_refresh_col;
                    endif;
                    if _oldsize fi_> _right
                    or (vwmode and _oldsize == _right) then
                        _right fi_- _size
                    else
                        false
                    endif -> _rfcol
                endunless
            endif
        endif
    endif;

    if _rfcol then
        fi_min(screenline_refresh_col, _rfcol) -> screenline_refresh_col;
        false -> screenline_tail_isclear
    endif
enddefine;

define Span_delete(_scol, _ecol, save);
    lvars save, _scol, _ecol, _col;
    _scol -> vedcolumn;
    returnif(_scol fi_> vvedlinesize);
    Set_changed();
    fi_min(vvedlinesize fi_+ 1, _ecol) -> _ecol;
    if save then
        Buffer_line(vedline) -> save;
        if (Skipwhite(_scol, save) ->> _col) and _col fi_< _ecol then
            veddecodetabs(subvedstring(_scol, _ecol fi_- _scol, save))
                                        -> vvedworddump
        endif;
    endif;
    Do_span_delete(_scol, _ecol);
    Adjust_tab(_scol, _ecol fi_- _scol, true);
enddefine;

define vedspandelete(_scol, _ecol, save);
    lvars _scol, _ecol, save;
    dlocal vvedworddump;
    unless isinteger(_scol) and isinteger(_ecol)
            and _scol fi_> 0 and _scol fi_< _ecol then
        mishap(_scol, _ecol, 2, 'vedspandelete: INVALID ARGUMENTS');
    endunless;
    Check_not_in_tab(_scol);
    Check_not_in_tab(_ecol);
    Span_delete(_scol, _ecol, save);
    Refresh_screenline_tail();
    if save then vvedworddump endif;
enddefine;

define vedwordleftdelete();
    lvars col;
    Check_not_in_prompt(true) -> col;
    if vedcolumn == col then
        if vedstatic then
            vedscreenbell()
        else
            vedchardelete()
        endif;
    elseif vedcolumn fi_> vvedlinesize fi_+ 1 then
        vvedlinesize fi_+ 1 -> vedcolumn
    else
        vedcolumn -> col;
        vedwordleft();
        Span_delete(vedcolumn, col, true);
        Refresh_screenline_tail();
    endif
enddefine;

define vedwordrightdelete();
    lvars _col;
    if vedcolumn fi_> vvedlinesize then
        vednextline()
    else
        Check_not_in_prompt(false);
        vedcolumn -> _col;
        vedwordright();
        Span_delete(_col, vedcolumn, true);
        Refresh_screenline_tail();
    endif;
enddefine;

define vedclearhead();
    ;;; delete characters to left of cursor.
    ;;; Save in vvedworddump if not all spaces
    lvars col;
    Check_not_in_prompt(true) -> col;
    if vedcolumn == col then
        vedchardelete()
    else
        Span_delete(col, vedcolumn, true);
        Refresh_screenline_tail();
    endif;
enddefine;

define vedcleartail();
    Check_not_in_prompt(false);
    Span_delete(vedcolumn, vvedlinesize fi_+ 1, true);
    Refresh_screenline_tail();
enddefine;

define veddotdelete();
    lvars _col;
    Check_not_in_prompt(false);
    vedcolumn -> _col;
    if _col fi_<= vvedlinesize
    and fast_subscrs(_col, Buffer_line(vedline)) == `\t`
    then
        if vedhardtabs then
            Tab_size_at(_col) fi_+ _col -> _col
        else
            _col fi_+ 1 -> _col;
            Check_not_in_tab(_col)          ;;; i.e. turn tab into spaces
        endif;
    else
        _col fi_+ 1 -> _col
    endif;
    Span_delete(vedcolumn, _col, false);
    Refresh_screenline_tail();
enddefine;

define Delete_char();
    lvars string, c, lastline, width_left, _old, _size, _col, _used, _newcol;
    dlocal vedbreak, vvedlinedump;
    Check_not_in_prompt(true) -> _col;
    if vedediting then vedscreenovermode() endif;
    if vedcolumn /== _col then
        vedcolumn fi_- 1 -> _col;
        Buffer_line(vedline) -> string;
        if _col fi_<= vvedlinesize and fast_subscrs(_col, string) == `\t`
        then
            if vedhardtabs then
                Find_tab_start(_col, string) -> _col
            else
                Check_not_in_tab(_col)      ;;; i.e. turn tab into spaces
            endif
        endif;
        Span_delete(_col, vedcolumn, false);
    elseif vedonstatus or vedstatic then
        vedscreenbell()
    elseif vedline == 1 then
        vederror('\{b}top of file')
    else
        ;;; get previous line and its size
        Buffer_line(vedline fi_- 1) -> lastline;
        vedusedsize(lastline) -> _size;
        if vedbreak then
            vedvscr_average_width(vedlinemax) fi_-
                vedvscr_substring_width(lastline,1,_size) -> width_left;
            if width_left < 0 then
                vederror('\{b}previous line too long')
            endif
        endif;
        Set_changed();
        ;;; join with previous line
        vedline -> _old;
        _size fi_+ 1 -> _newcol;

        ;;; get current line
        vedtrimline();
        Buffer_line(vedline) -> string;
        if _col /== 1 then
            ;;; remove the prompt
            Shift_string(1, 1 fi_- _col, string, false) -> ;
            vedusedsize(string) -> vvedlinesize
        endif;

        if veddelspaces and _size /== 0 and vvedlinesize /== 0 then
            ;;; preceding line not empty, so, before merging lines
            ;;; delete leading spaces but one
            if (Skipwhite(1, string) ->> _col)
            and ispair(fast_subscrvedstring(_col, string) ->> c)
            and fast_back(c) = nullstring
            and (fast_front(c) ->> c) &&/=_0 VEDCMODE_ACTIVE
            and Check_active_split(_newcol, lastline)
            then
                ;;; remove active continuation marker
                c -> fast_subscrvedstring(_col, string);
                c fi_&&~~ #_< VEDCMODE_SP_INSIG_BITS||16:FFFF >_# fi_|| `\s`
            else
                `\s`
            endif -> c;

            if _col then
                if _col /== 1 then
                    Shift_string(1, 1 fi_- _col, string, false) ->
                endif;
                if fast_subscrdstring(_size,lastline) fi_&& 16:FFFF /== `\s`
                then
                    Shift_string(1, 1, string, true) -> string;
                    c -> fast_subscrdstring(1,string);
                    _newcol fi_+ 1 -> _newcol
                endif
            else
                nullstring -> string
            endif
        endif;

        ;;; close up the gap
        vedlinedelete();
        1000 -> screenline_refresh_col; true -> screenline_tail_isclear;

        if vedline == _old then
            ;;; not at end of buffer, so join lines
            vedcharup();
            _size fi_+ 1 -> vedcolumn;
            Realign_tab(_size, string) -> string;
            vedusedsize(string) -> _used;
            if vedbreak and _size /== 0
            and vedvscr_substring_width(string,1,_used) fi_> width_left
            and (locchar(`\s`, 1, string) ->> _col) and _col fi_< _used then
                ;;; if the line is too long and there is a space to break at,
                ;;; join lines, but truncate where necessary
                dlocal vedcharinsert_acont = true;
                veddecodetabs(Realign_tab(0,string)) -> string;
                vedusedsize(string) -> _used;
                1 -> _col;
                until _col fi_> _used do
                    Insert_char(fast_subscrvedstring(_col, string));
                    _col fi_+ 1 -> _col
                enduntil;
                Refresh_screenline_tail();
                while vedline fi_>= _old do vedcharup() endwhile;
            else
                ;;; just insert at the end
                Span_insert(string, _used, 0, vedcolumn)
            endif
        endif;
        _newcol -> vedcolumn
    endif
enddefine;

define vedchardelete();
    Delete_char();
    Refresh_screenline_tail();
enddefine;

define vedlinedelete();
    lvars onscreen;
    returnif(vedline fi_> max(vvedmarkhi,vedusedsize(vedbuffer) fi_+ 1));
    unless vvedlinesize == 0 then
        ;;; save current line in vvedlinedump
        vedtrimline();
        Normalised_line(vedline, false) -> vvedlinedump
    endunless;
    ved_winrel_line(vedline) -> onscreen;
    Shift_buffer_lines(vedline, -1);
    vedline fi_+ 1 -> vedline;
    vedsetlinesize();
    if vedediting and not(vedonstatus) and onscreen then
        vedscrollregion(Window_line(vedline), vedwindowlength, -1,
                                                    Bottom_of_window());
    endif;
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 26 1999
        Made vedbreak mechanism etc work with variable-width mode
--- John Gibson, Sep 30 1997
        Changes for XVed variable-width mode
--- John Gibson, Nov  3 1995
        Changes to cope with active continuation markers
--- John Gibson, Sep  7 1995
        Changed to use new subvedstring, fast_subscrvedstring.
        Fixed bug with tabs in Delete_char.
--- John Gibson, Mar  5 1993
        Made Check_not_in_prompt call Check_not_in_tab as well
--- John Gibson, Apr 18 1992
        Sectionised.
--- John Gibson, Jan  3 1992
        Changes to cope with dstrings
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
--- John Gibson, Aug 16 1987
        Tidied up
 */
