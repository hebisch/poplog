/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/ved/src/vdinsert.p
 > Purpose:         Procedures for inserting characters in VED
 > Author:          John Gibson & Aaron Sloman (see revisions)
 */

;;;------------- PROCEDURES FOR INSERTING CHARACTERS ----------------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure (vedscreenovermode, vedcurrentchar, ved_winrel_column)
    ;

vars
        procedure (vedscreeninsertchar, vedvscr_window_column_bounds,
        vedvscr_average_width, vedvscr_space_width,
        vedvscr_offsets_of_column, vedvscr_substring_width),
        vedcrinteractive, vvedpromptchar, vedvarwidthmode
    ;

section $-Sys$-Ved;

constant
        procedure (Insert_char, Im$-Input_text, Unmark_cursor_line,
        Check_not_in_prompt, Shift_string, Split_line, Adjust_tab,
        Set_cursor_to, Check_active_split
        )
    ;

endsection;


;;; -----------------------------------------------------------------------

section $-Sys$-Ved =>   vedhardtabs, vedcharinsert_attr, vedcharinsert_acont,
                        vedcharinsert, vedinsertstring,
                        vedlineabove, vedlinebelow,
                        vedchangecase, veddocr, vedsetstatic
                    ;

constant
    string_of_tabs  = consstring(#| repeat 32 times `\t` endrepeat |#),
    string_of_spaces= consstring(#| repeat 32 times `\s` endrepeat |#),
    ;

vars
    vedhardtabs         = true,
    vedcharinsert_attr  = 0,
    vedcharinsert_acont = false,

    screenline_refresh_col  = MAXSCREENCOL,
    screenline_tail_isclear = true,
    ;

lvars
    ;;; specify string expansion amount to Shift_string
    shift_string_expand = 20,
    ;

define Get_left_margin() -> m;
    lvars m;
    if isprocedure(dup(vedleftmargin)) then fast_apply() endif -> m;
    Check_integer(m, 0);
enddefine;

define Do_span_insert(instring, _size, _dattr, _scol);
    lvars   string, instring, org_string, vwmode = vedvarwidthmode,
            _scol, _ecol, _col, _size, _right, _oldsize, _dchar, _dattr,
            _wrline, _coloffs;

    define lconstant New_string(_flags, string) -> new;
        lvars _flags, string, new, dvec;
        subdstring(1, _pint(string!V_LENGTH), string,
                        if _flags _bitst _:M_K_DSTRING then
                            if _flags _bitst _:M_K_STRING16 then
                                dstring16_key
                            else
                                dstring_key
                            endif
                        else
                            string16_key
                        endif) -> new;
        if vedstring_data_prop(string) ->> dvec then
            copy(dvec) -> vedstring_data_prop(new)
        endif
    enddefine;

    _scol fi_+ _size -> _ecol;
    Buffer_line(vedline) ->> string -> org_string;
    vvedlinesize -> _oldsize;
    if not(vedstatic)
    or (_scol fi_> _oldsize and _int(_ecol) _sgr string!V_LENGTH) then
        ;;; need to make space
        Shift_string(_scol, _size, string, shift_string_expand) -> string
    endif;

    if issimple(instring) or ispair(instring) then
        ;;; instring is a single character (and _size is 1)
        instring -> _dchar;
        if iscompound(instring) then fast_front(instring) -> _dchar endif;

        _int(_dchar) -> _right;
        if _right _gr _16:FF then
            _0 -> _col;
            if _right _gr _16:FFFF then _:M_K_DSTRING -> _col endif;
            if _right _bitst _16:FF00 then
                _col _biset _:M_K_STRING16 -> _col
            endif;
            if _nonzero(_col _biclear string!KEY!K_FLAGS) then
                ;;; need to change string
                New_string(_col, string) -> string
            endif
        endif;

        instring -> fast_subscrvedstring(_scol, string);
        if _scol fi_> _oldsize
        and (_right _bimask _16:FFFF /== _:`\s`
             or _right _bitst _:VEDCMODE_SP_SIG_BITS)
        then
            ;;; saves calling vedusedsize
            _scol
        else
            vedusedsize(string)
        endif

    else
        ;;; instring is a (d)string -- add _dattr to each char
        instring!KEY!K_FLAGS -> _col;
        _dattr fi_&&~~ 16:FFFF -> _dattr;
        if _dattr /== 0 then _col _biset _:M_K_DSTRING -> _col endif;
        if _nonzero(_col _biclear string!KEY!K_FLAGS) then
            ;;; need to change string
            New_string(_col, string) -> string
        endif;

        instring -> subvedstring(_scol, _size, string);
        if _dattr /== 0 then
            _scol -> _col;
            fast_repeat _size times
                fast_subscrdstring(_col,string) fi_|| _dattr
                                    -> fast_subscrdstring(_col,string);
                _col fi_+ 1 -> _col
            endrepeat
        endif;
        vedusedsize(string)
    endif -> vvedlinesize;

    if string /== org_string then string -> Buffer_line(vedline) endif;

    returnunless( vedediting and (ved_winrel_line(vedline) ->> _wrline));
    vedvscr_window_column_bounds(_wrline, true) -> (_coloffs, , _right, );
    unless vwmode then
        returnunless( vvedlinesize fi_> _coloffs or _oldsize fi_> _coloffs )
    endunless;

    ;;; may need to change the screen
    fi_max(_coloffs fi_+ 1, _scol) -> _col;
    if _col fi_> _right then
        ;;; 1st col past end of screen
        returnif(vwmode or vvedlinesize fi_<= _right);
        ;;; to ensure 'more' mark
        _right ->> _col -> _ecol
    endif;

    fi_min(vvedlinesize fi_+ 1, _ecol) -> _ecol;

    if _ecol fi_> vvedlinesize
    or (not(vwmode) and (_oldsize fi_<= _coloffs
                        or _ecol fi_> _right
                        or (_ecol == _right and _ecol fi_< vvedlinesize) ))
    then
        ;;; refreshing end of line only
        if _col fi_< screenline_refresh_col then
            _col -> screenline_refresh_col
        endif;
        if _col fi_<= _oldsize then
            false -> screenline_tail_isclear
        endif
    elseif _col fi_< screenline_refresh_col then
        if vednocharinsert and not(vedstatic) then
            _col -> screenline_refresh_col;
            false -> screenline_tail_isclear
        else
            lvars procedure subscrp = if vedonstatus then fast_subscrvedstring
                                      else fast_subscrdstring
                                      endif;
            _ecol fi_- _scol -> _size;
            if vwmode and _scol fi_< _col then
                ;;; to left of screen -- redo whole line
                0 -> screenline_refresh_col
            elseif vedstatic then
                Set_cursor_to(vedline, _col) -> ;
                fast_repeat _size times
                    vedscreenoutput(subscrp(_col,string));
                    _col fi_+ 1 -> _col
                endrepeat;
                if vwmode and vvedlinesize fi_>= _right then
                    fi_min(screenline_refresh_col, _right) -> screenline_refresh_col
                endif
            else
                if vedscreenwrap then
                    ;;; chop _size chars from the end of line to
                    ;;; prevent wrap-around
                    Set_cursor_to(vedline, _right fi_- _size fi_+ 1) -> ;
                    vedscreencleartail()
                endif;
                Set_cursor_to(vedline, _col) -> ;
                screenline_refresh_col fi_+ _size -> screenline_refresh_col;
                fast_repeat _size times
                    vedscreeninsertchar(subscrp(_col,string));
                    _col fi_+ 1 -> _col
                endrepeat;
                vedscreenovermode()
            endif;
            if not(vwmode) and vvedlinesize fi_> _right then
                fi_min(screenline_refresh_col, _right) -> screenline_refresh_col
            endif
        endif
    endif
enddefine;

define Refresh_screenline_tail();
    lvars   string, _wrline, _wrcol, _wline, _wcol,
            _col = screenline_refresh_col;
    if not(vedonstatus) and vedediting
    and (ved_winrel_line(vedline) ->> _wrline)
    and (_col == 0
        or (ved_winrel_column(_wrline, screenline_refresh_col, 2:001)
                                    ->> _wrcol))
    then
        Buffer_line(vedline) -> string;
        _wrline fi_+ vedwlineoffset -> _wline;
        if _col == 0 then
            ;;; only for var width -- refresh whole line
            vedrefreshtail(false, _wline, 2, 0, string)
        else
            _wrcol fi_+ vedwcolumnoffset -> _wcol;
            vedwindowpoint(_wline, _wcol);
            unless screenline_tail_isclear then vedscreencleartail() endunless;
            if screenline_refresh_col == vvedlinesize then
                vedscreenoutput(fast_subscrdstring(screenline_refresh_col, string))
            else
                vedrefreshtail(true, _wline, _wcol, screenline_refresh_col, string);
            endif
        endif
    endif;
    MAXSCREENCOL -> screenline_refresh_col;
    true -> screenline_tail_isclear;
enddefine;

define Span_insert(instring, _size, _dattr, _scol);
    lvars instring, _size, _scol, _col, _dattr, _dchar;
    _scol fi_+ _size -> vedcolumn;
    if _scol fi_<= vvedlinesize then
        Adjust_tab(_scol, _size, false)
    elseif _dattr &&=_0 VEDCMODE_SP_SIG_BITS then
        ;;; do nothing else if only spaces
        if issimple(instring) or ispair(instring) then
            instring -> _dchar;
            if iscompound(instring) then fast_front(instring) -> _dchar endif;
            returnif(_dchar fi_&& 16:FFFF == `\s`
                        and _dchar &&=_0 VEDCMODE_SP_SIG_BITS)
        elseif instring == string_of_spaces then
            return
        elseunless (skipchar(`\s`, 1, instring) ->> _col) and _col fi_<= _size
        then
            1 -> _col;
            repeat
                quitif(fast_subscrdstring(_col,instring)
                                        &&/=_0 VEDCMODE_SP_SIG_BITS);
                returnif((_col fi_+ 1 ->> _col) fi_> _size);
            endrepeat
        endif
    endif;
    Set_changed();
    Do_span_insert(instring, _size, _dattr, _scol);
enddefine;

define lconstant new_screen_line(onscreen);
    lvars onscreen, size;
    returnunless(vedediting and not(vedonstatus));
    1 -> vedcolumn;
    returnunless(onscreen);
    if vedcolumnoffset == 0 then
        ;;; Try to keep context on screen.
        Bottom_of_window() -> size;
        if vedline fi_< size
        or vedline == size and vedline fi_> vvedbuffersize
        then
            vedscreenpushdown(vedline fi_- vedlineoffset fi_+ 1)
        else
            vedscreenpushup(vedline fi_- vedlineoffset);
            vedlineoffset fi_+ 1 -> vedlineoffset
        endif;
        vedrefreshline(true, vedline fi_- vedlineoffset fi_+ 1,
                                Buffer_line(vedline), vedmarked(vedline))
    else
        0 -> vedcolumnoffset;
        vedrefreshwindow(true)
    endif
enddefine;

define lconstant Charinsert_nl();
    lvars string, act_to_left = false, _margin, _wrline, _col;
    if vedstatic then vedscreenbell(); return endif;
    Shift_buffer_lines(vedline fi_+ 1, 1);
    if vvedmarkhi == vedline then
        vedline fi_+ 1 -> vvedmarkhi
    endif;
    unless vedline fi_> vvedbuffersize
            or  (vedline == vvedbuffersize and vedcolumn fi_> vvedlinesize)
        ;;; after end of last text in buffer
    then
        ;;; if veddelspaces is true, remove spaces after break point
        Split_line(vedcolumn, Buffer_line(vedline), veddelspaces)
            -> (act_to_left, Buffer_line(vedline fi_+ 1), Buffer_line(vedline))
    endunless;

    ;;; clear tail of line on screen
    if vedediting then
        if (ved_winrel_line(vedline) ->> _wrline)
        and ved_winrel_column(_wrline, vvedlinesize, 2:011) then
            false -> screenline_tail_isclear;
            fi_min( ved_winrel_column(_wrline, vedcolumn, 2:111),
                            screenline_refresh_col) -> screenline_refresh_col
        endif;
        vedsetlinesize();
        Refresh_screenline_tail()
    endif;

    ;;; go down a line
    vedtrimline();
    Buffer_line(vedline) -> string;
    vvedlinesize -> _col;
    vedline fi_+ 1 -> vedline;
    vedsetlinesize();

    ;;; insert continuation prompt if necessary
    if VDDEV_LOADED and VDDEV_WEAK vedprocswaiting() and vvedpromptchar then
        locchar_back(vvedpromptchar, _col, string) -> _col;
        if _col then
            Buffer_line(vedline) -> string;
            Shift_string(1, _col, string, true) -> string;
            vvedpromptchar -> fast_subscrdstring(_col, string);
            string -> vedthisline();    ;;; sets vvedlinesize
        else
            0 -> _col
        endif;
    else
        0 -> _col
    endif;

    ;;; insert leading spaces if necessary
    if (Get_left_margin() ->> _margin) /== 0 then
        Buffer_line(vedline) -> string;
        fi_min(_margin, (vedvscr_average_width(vedlinemax) fi_-
                                vedvscr_substring_width(string,1,vvedlinesize))
                            fi_div vedvscr_space_width(1)) -> _margin;
        fi_max(0, _margin) -> _margin;
        unless _margin == 0 or vvedlinesize == 0 then
            Shift_string(1, _margin, string, true) -> vedthisline()
        endunless
    endif;

    new_screen_line(_wrline);
    _margin fi_+ _col fi_+ 1 -> vedcolumn;

    if vedcharinsert_acont then
        if vvedlinesize == 0 and act_to_left then
            vedline
        else
            true
        endif -> vedcharinsert_acont
    endif
enddefine;

define lconstant Currentchar_is_white();
    lvars char = vedcurrentchar();
    char == `\s` or char == `\t`
enddefine;

    /*  Inserting character in line which extends too far to right.
        find a good place to break - at a space, or after a dot or comma
        or semicolon, etc.
    */
define lconstant Break_line_to_right() -> ok;
    lvars ok = true;
    dlocal
        veddelspaces = true,    ;;; when line breaks remove spaces
        vedline, vedcolumn;     ;;; restore position afterwards

    vedsetlinesize();
    if Currentchar_is_white() then
        Charinsert_nl();
        vedcheck()
    else
        until Currentchar_is_white() do
            _CHECKINTERRUPT;
            vedcharright()
        enduntil;
        if vedcolumn fi_>= vvedlinesize then
            ;;; no good place to break
            false -> ok
        else
            Charinsert_nl()
        endif
    endif
enddefine;

    /*  Try to find space on left to break line
        find a good place to break - at a space, or after a dot or comma
        or semicolon, etc.
    */
define lconstant Break_line_to_left();
    lvars _leftmargin = Get_left_margin() fi_+ 1, _col = vedcolumn, _diff;
    dlocal veddelspaces;

    vedsetlinesize();
    until vedcolumn == _leftmargin or Currentchar_is_white() do
        _CHECKINTERRUPT;
        vedcharleft()
    enduntil;
    unless vedcolumn == _leftmargin then
        _col fi_- vedcolumn -> _diff;
        while vedcolumn /== _leftmargin and Currentchar_is_white() do
            vedcharleft()
        endwhile;
        unless vedcolumn == _leftmargin then
            vedcharright();
            true -> veddelspaces;   ;;; delete leading tabs & spaces
            Charinsert_nl();
            _diff fi_+ _leftmargin fi_- 1 -> vedcolumn; ;;; allows for margin
            return
        endunless
    endunless;
    ;;; no good place to break. Break at original place
    _col -> vedcolumn;
    Charinsert_nl();
    vedcheck();
    vedcharup();
    vedtextright();
enddefine;

define lconstant Do_vedbreak(vchar, _dchar);
    lvars   nrem, string = Buffer_line(vedline), soffs, loffs, vchar,
            maxsize = vedvscr_average_width(vedlinemax),
            _col = vedcolumn, _char = _dchar fi_&& 16:FFFF, _dchar;
    dlocal  vedbreak;

    vvedlinesize fi_- _col fi_+ 1 -> nrem;
    vedvscr_offsets_of_column(string, _col, _dchar) -> (soffs, loffs);

    unless loffs fi_> maxsize
    or (_char == `\s` and loffs == maxsize and nrem fi_< 1) then
        if nrem fi_>= 1
        ;;; next expr is total width of line (= vvedlinesize in fixed-width)
        and vedvscr_substring_width(string, _col, nrem) fi_+ soffs
                fi_>= maxsize
        then
            ;;; Break line at or to the right of current position, if possible
            if Break_line_to_right() then
                vedsetlinesize()
            else
                Break_line_to_left()
            endif;
            chain(true, vchar, Insert_char)
        else
            ;;; nothing to do
            return(false)
        endif
    endunless;

    ;;; Line too long. Try breaking it
    lvars _leftmargin = Get_left_margin() fi_+ 1, _size = 0;
    if _char == `\s` and nrem fi_>= 1 then
        ;;; next expr is total width of line (= vvedlinesize in fixed-width)
        vedvscr_substring_width(string, _col, nrem) fi_+ soffs -> soffs
    endif;
    unless _char == `\s` and soffs fi_<= maxsize then
        _col fi_- 1 -> _col
    endunless;
    until _col == _leftmargin or _col fi_> vvedlinesize
        ;;; if not beyond line, go left to space or beginning of line
        or fast_subscrs(_col, string) == `\s`
    do
        _CHECKINTERRUPT;
        _col fi_- 1 -> _col;
        _size fi_+ 1 -> _size
    enduntil;
    if _col == _leftmargin and _char /== `\s` then
        ;;; no space at which to break the line. Just insert the character
        false -> vedbreak;
        Insert_char(vchar);
        true -> vedbreak
    elseif _col == _leftmargin then
        ;;; No space to break, inserting a space, so break line here
        Charinsert_nl()
    else
        ;;; found a space not at beginning of line
        _col fi_+ 1 -> vedcolumn;
        Charinsert_nl();
        vedcolumn fi_+ _size -> vedcolumn;
        unless _char == `\s` and _size == 0 then
            Insert_char(vchar)
        endunless
    endif;
    vedsetlinesize();
    true
enddefine;

define Insert_char(vchar);
    lvars vchar, p, _char, _dchar = vchar, _attr, _isact;
    if iscompound(_dchar) then fast_front(_dchar) -> _dchar endif;
    _dchar fi_&& 16:FFFF -> _char;

    if _char == `\n` then
        Charinsert_nl();
        if vedcharinsert_acont then true -> vedcharinsert_acont endif;
        return
    endif;

    ;;; add attributes from vedcharinsert_attr to character
    vedcharinsert_attr fi_&&~~ 16:FFFF -> _attr;
    _dchar fi_|| _attr -> _dchar;

    returnif(vedbreak and not(vedstatic) and Do_vedbreak(vchar, _dchar));

    _int(_dchar) _bitst _:VEDCMODE_ACTIVE -> _isact;
    unless _isact then
        if vedcharinsert_acont then true -> vedcharinsert_acont endif;
    endunless;

    if _char == `\t` then
        (if vednotabs then string_of_spaces else string_of_tabs endif,
            Tab_size_at(vedcolumn), _dchar, vedcolumn)
    else
        if iscompound(vchar) then
            if vedcharinsert_acont then true -> vedcharinsert_acont endif;
            if _attr /== 0 then
                conspair(_dchar, fast_back(vchar)) -> vchar
            endif
        elseif _isact and _char /== `\s` and vedcharinsert_acont == vedline
        then
            ;;; mark active continuation
            conspair(_dchar, nullstring) -> vchar;
            true -> vedcharinsert_acont
        else
            _dchar -> vchar
        endif;
        (vchar, 1, 0, vedcolumn)
    endif;

    Span_insert();

    if _isact and vedcharinsert_acont == true and vedcolumn fi_> vvedlinesize
    and Check_active_split(vedcolumn, Buffer_line(vedline))
    then
        vedline fi_+ 1 -> vedcharinsert_acont
    endif
enddefine;

define lconstant Checkr_vchar();
    if ispair(dup()) then
        Checkr_dchar(fast_front(dup())) ->
    else
        chain((), Checkr_dchar)
    endif
enddefine;

define vedcharinsert(vchar);
    lvars vchar;
    Check_not_in_prompt(false);
    if vchar == termin then
        appdata(termin_printstring, Insert_char)
    else
        Insert_char(Checkr_vchar(vchar))
    endif;
    Refresh_screenline_tail()
enddefine;

define vedinsertstring(item);
    lvars item, i, n, vchar;
    lconstant LARGE_NEG = -1e6;
    dlocal  shift_string_expand,
            vedchanged = LARGE_NEG,     ;;; kludge to stop vedautowrite
            vedcharinsert_acont;

    if isinteger(item) then
        Check_integer(item, 0)
    elseunless isstring(item) or isword(item) then
        mishap(item, 1, 'STRING, WORD OR INTEGER NEEDED')
    endif;
    Check_not_in_prompt(false);
    unless vedcharinsert_acont then true -> vedcharinsert_acont endunless;
    if isinteger(item) then
        ;;; n chars off stack
        item -> n;
        until n == 0 do
            fi_max(20, n) -> shift_string_expand;
            subscr_stack(n) -> vchar;
            if vchar == termin then
                appdata(termin_printstring, Insert_char)
            else
                Insert_char(Checkr_vchar(vchar))
            endif;
            n fi_- 1 -> n
        enduntil;
        erasenum(item)
    else
        ;;; string or word
        if (datalength(item) ->> n) fi_> 20 then
            40 -> shift_string_expand
        endif;
        if isword(item) then
            appdata(item, Insert_char)
        else
            fast_for i to n do
                Insert_char(fast_subscrvedstring(i,item))
            endfor
        endif
    endif;
    Refresh_screenline_tail();

    if vedchanged /== LARGE_NEG then chain(Set_changed) endif
enddefine;

define vedlineabove();
    Shift_buffer_lines(vedline, 1);
    nullstring -> Buffer_line(vedline fi_- 1);
    if vedline == vvedmarklo then
        vvedmarklo fi_- 1 -> vvedmarklo;
    endif;
    vedcharup();
    new_screen_line(ved_winrel_line(vedline));
    Get_left_margin() fi_+ 1 -> vedcolumn;
enddefine;

define vedlinebelow();
    dlocal vedstatic = false;
    vedtextright();
    vedcharinsert(`\n`);
enddefine;

define vedchangecase();
    lvars vchar, dchar, c;
    dlocal vedstatic;
    if vedcolumn fi_> vvedlinesize then
        vednextline()
    else
        fast_subscrvedstring(vedcolumn, vedthisline()) ->> dchar -> vchar;
        if iscompound(dchar) then fast_front(dchar) -> dchar endif;
        if (lowertoupper(dchar) ->> c) == dchar then
            uppertolower(dchar) -> c
        endif;
        if c /== dchar then
            vedstatic or true -> vedstatic;
            if iscompound(vchar) then
                c -> fast_front(vchar), vchar -> c
            endif;
            vedcharinsert(c)
        else
            vedcharright()
        endif
    endif
enddefine;

define vars veddocr();
    if vedonstatus then
        Set_vedcommand(true);
        Unmark_cursor_line();
        veddocommand();
    elseif VDDEV_LOADED and VDDEV_WEAK vedcrinteractive
    and VDDEV_WEAK vedprocswaiting() then
        VDDEV_WEAK Im$-Input_text(false)
    else
        vedcharinsert(`\n`);
    endif
enddefine;

define vedsetstatic();
    if isinteger(vedstatic) and vedstatic &&/=_0 1 then
        vederror('\{b}character editing disabled')
    endif;
    not(vedstatic) -> vedstatic;
    vedputmessage(if vedstatic then '\{b}static mode on'
                  else '\{b}static mode off'
                  endif)
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 26 1999
        Made vedbreak mechanism etc work with variable-width mode
--- John Gibson, Sep 25 1997
        Changes for variable-width mode
--- John Gibson, Feb 15 1997
        String16 changes
--- John Gibson, Dec 10 1996
        Fixed bug in Charinsert_nl (was not reseting vedcharinsert_acont to
        true when it already had a line number value).
--- John Gibson, Mar 27 1996
        Removed test for `\r` in Insert_char -- this character should not
        be treated as a newline, since nothing else in Ved (e.g. reading in a
        file) treats it as such when it's a buffer character.
--- Robert John Duncan, Jan 15 1996
        Fix to Insert_char to propagate attributes from vedcharinsert_attr
--- John Gibson, Nov  3 1995
        Changes to cope with active continuation markers
--- John Gibson, Sep  7 1995
        Changes to cope with chars having associated data
--- John Gibson, Mar  5 1993
        Removed calls to Check_not_in_tab (now done by Check_not_in_prompt)
--- John Gibson, Apr 18 1992
        Sectionised.
--- John Gibson, Apr 18 1992
        Added -vedcharinsert_attr-
--- John Gibson, Jan  3 1992
        Changes to cope with dstrings
--- John Gibson, Aug 30 1991
        Made -vedinsertstring- allow an integer arg to mean N chars off stack
--- John Gibson, Jun 24 1991
        Changed Do_span_insert so that characters inserted to the right of
        the screen cause vedscreenmoremark to be displayed.
--- Aaron Sloman, Apr 27 1990
        Altered Insert_char to prevent line being extended beyond
        vedlinemax. Changed fi_> to fi_>=
--- Rob Duncan, Apr 24 1990
        Tentative change to -Do_span_insert- to fix insertion of spaces
        at the right-hand edge of the screen (it can duplicate the character
        under the cursor instead if -vednocharinsert- is <false>).
 */
