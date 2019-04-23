/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/ved/src/vdtabs.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;---------------- ROUTINES FOR HANDLING TABS -----------------------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure (Sys$-Ved$-Do_span_delete, Sys$-Ved$-Do_span_insert),
        Sys$-Ved$-string_of_tabs
    ;

vars
        vedhardtabs
    ;


;;; ------------------------------------------------------------------------

section $-Sys$-Ved => vedencodetabs, veddecodetabs;


    /*  Return the size of a tab at column _col
    */
define Tab_size_at(_col);
    lvars _col;
    vedindentstep fi_- ((_col fi_- 1) fi_rem vedindentstep)
enddefine;

    /*  Find the start of a tab in string backwards from _col
    */
define Find_tab_start(_col, string);
    lvars string, _col, _lim;
    _col fi_+ Tab_size_at(_col) fi_- vedindentstep -> _lim;
    while _col fi_> _lim  do
        _col fi_- 1 -> _col;
        returnif(fast_subscrs(_col, string) /== `\t`) (_col fi_+ 1)
    endwhile;
    _lim;
enddefine;

    /*  Return true if _col is in the middle of a tab in string
    */
define Col_is_inside_tab(_col, string);
    lvars string, _col;
    not(_col == 1 or _int(_col) _gr string!V_LENGTH
        or fast_subscrs(_col, string) /== `\t`
        or fast_subscrs(_col fi_- 1, string) /== `\t`
        or (_col fi_- 1) rem vedindentstep == 0
       )
enddefine;

    /*  Check if in a tab and either give an error or turn it into spaces
    */
define Check_not_in_tab(_col);
    lvars string = Buffer_line(vedline), _col;
    if isinteger(vedstatic) and vedstatic &&/=_0 1 then
        vederror('\{b}character editing disabled')
    endif;

    returnunless(Col_is_inside_tab(_col, string));

    if vedhardtabs then
        vederror('\{b}in a tab')
    else
        ;;; turn it into spaces
        Find_tab_start(_col, string) -> _col;
        set_subvector(
            (fast_subscrdstring(_col,string) fi_&&~~ 16:FFFF) fi_|| `\s`,
                _col, string, Tab_size_at(_col));
        vedsetlinesize()
    endif
enddefine;

define vedencodetabs(string);
    lvars string, _col, _lim, _dchar, _n, _scol, _has_data = false;
    Check_string(string);
    returnunless(locchar(`\t`, 1, string)) (string);
    1 ->> _col -> _scol;
    datalength(string) -> _lim;
    #|  until _col fi_> _lim do
            fast_subscrvedstring(_col, string) ->> _dchar;  ;;; stack it
            if iscompound(_dchar) then
                fast_front(_dchar) -> _dchar;
                true -> _has_data
            endif;
            if _dchar fi_&& 16:FFFF == `\t` then
                Tab_size_at(_scol) fi_- 1 -> _n;
                fast_repeat _n times _dchar endrepeat;
                _scol fi_+ _n -> _scol
            endif;
            _scol fi_+ 1 -> _scol;
            _col fi_+ 1 -> _col
        enduntil
    |#;
    if _has_data then
        consvedstring()
    else
        fast_apply((), string!KEY!K_CONS_V)
    endif
enddefine;

define veddecodetabs(string);
    lvars string, _scol, _lim, _ecol, _dchar, _has_data = false;
    Check_string(string);
    returnunless(locchar(`\t`, 1, string)) (string);
    1 -> _scol;
    datalength(string) -> _lim;
    #| until _scol fi_> _lim do
            fast_subscrvedstring(_scol, string) ->> _dchar; ;;; stack it
            if iscompound(_dchar) then
                fast_front(_dchar) -> _dchar;
                true -> _has_data
            endif;
            if _dchar fi_&& 16:FFFF == `\t` then
                Tab_size_at(_scol) fi_+ _scol fi_- 1 -> _ecol;
                fi_min(_ecol, _lim) -> _ecol;
                repeat
                    _scol fi_+ 1 -> _scol;
                    quitif(_scol fi_> _ecol
                            or fast_subscrs(_scol,string) /== `\t`);
                endrepeat
            else
                _scol fi_+ 1 -> _scol
            endif
        enduntil
    |#;
    if _has_data then
        consvedstring()
    else
        fast_apply((), string!KEY!K_CONS_V)
    endif
enddefine;

    /*  Adjust next tab before inserting _size chars at _col,
        or after deleting _size chars at _col.
    */
define Adjust_tab(_col, _size, deleting);
    lvars string, deleting, _tabcol, _size, _col;
    returnif(_col fi_> vvedlinesize or vedstatic);
    Buffer_line(vedline) -> string;
    returnunless(locchar(`\t`, _col, string) ->> _tabcol);
    Tab_size_at(_tabcol fi_+ _size) fi_- Tab_size_at(_tabcol) -> _size;
    returnif(_size == 0);
    _tabcol fi_+ 1 -> _col;     ;;; insert/delete at 2nd tab char
    if deleting then 0 fi_- _size -> _size endif;
    if _size fi_< 0 then
        Do_span_delete(_col, _col fi_- _size)
    else
        Do_span_insert(string_of_tabs, _size,
                            fast_subscrdstring(_tabcol,string), _col)
    endif
enddefine;

    /*  Given a column offset and a string, realign the first tab
        (if any) w.r.t. that offset
    */
define Realign_tab(_offset, string) -> string;
    lvars string, _col, _ecol, _offset, _diff, _dchar;
    returnunless(locchar(`\t`, 1, string) ->> _col);
    if skipchar(`\t`, _col, string) ->> _ecol then
        _ecol fi_- 1
    else
        datalength(string)
    endif -> _ecol;
    Tab_size_at(_col fi_+ _offset)
        fi_- (_ecol fi_- _col) rem vedindentstep fi_- 1 -> _diff;
    if _diff /== 0 then
        fast_subscrdstring(_col,string) -> _dchar;
        _col fi_+ 1 -> _col;    ;;; insert/delete at 2nd tab char
        Shift_string(_col, _diff, string, true) -> string;
        unless _diff fi_< 0 then
            set_subvector(_dchar, _col, string, _diff)
        endunless
    endif
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 15 1997
        String16 changes
--- John Gibson, Sep  7 1995
        Changes to cope with chars having associated data
--- John Gibson, Apr 21 1992
        Sectionised.
--- John Gibson, Jan  3 1992
        Changes to cope with dstrings
--- John Gibson, Feb 22 1988
        Check_string into section Sys
--- John Gibson, Dec  6 1987
        Tidied up.
 */
