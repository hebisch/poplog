/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/ved/src/vdbuffer.p
 > Purpose:         VED procedures for manipulating the VED buffer
 > Author:          John Gibson & Aaron Sloman (see revisions)
 */

;;;------------------ MANIPULATING THE BUFFER -----------------------------

#_INCLUDE 'vddeclare.ph'

constant
        procedure (Sys$-Ved$-Realign_tab)
    ;

vars
        vvedpromptchar,
    ;


;;; -----------------------------------------------------------------------

section $-Sys$-Ved =>   vedusedsize, vedtrimline, vedbufferextend,
                        vedsetlinesize, vedthisline, vedcurrentvedchar,
                        vedcurrentdchar, vedcurrentchar
                    ;

define vedusedsize(vec);
    lvars vec, _pos, _apos, _lim, _attr, _len, _flags;
    unless isstring(vec) then
        unless isvector(vec) then
            mishap(vec, 1, 'STRING OR VECTOR NEEDED')
        endunless;

        vec@V_WORDS[_0] -> _lim;
        _lim@(w)[vec!V_LENGTH] -> _pos;
        while _pos >@(w) _lim do
            unless _zero((_pos--!(w) -> _pos)!V_LENGTH) then
                return(_pint( ##(w){_pos, _lim} _add _1 ))
            endunless;
        endwhile

    elseif (vec!KEY!K_FLAGS ->> _flags) _bitst _:M_K_DSTRING then
        lconstant _SP_SIG = _int(VEDCMODE_SP_SIG_BITS >> 16);
        vec!V_LENGTH -> _len;
        if _flags _bitst _:M_K_STRING16 then
            vec@V_SHORTS[_0] -> _lim;
            _lim@(s)[_len] -> _pos;
            _DSTRING16_ATTR_PTR(vec, _len) -> _apos;
            while _pos >@(s) _lim do
                _apos--!(b) -> (_attr, _apos);
                if (_pos--!(s) -> _pos) /== _:`\s` or _attr _bitst _SP_SIG then
                    return(_pint( ##(s){_pos, _lim} _add _1 ))
                endif
            endwhile
        else
            vec@V_BYTES[_0] -> _lim;
            _lim@(b)[_len] -> _pos;
            _DSTRING_ATTR_PTR(vec, _len) -> _apos;
            while _pos >@(b) _lim do
                _apos--!(b) -> (_attr, _apos);
                if (_pos--!(b) -> _pos) /== _:`\s` or _attr _bitst _SP_SIG then
                    return(_pint( ##(b){_pos, _lim} _add _1 ))
                endif
            endwhile
        endif
    else
        vec!V_LENGTH -> _len;
        if _flags _bitst _:M_K_STRING16 then
            vec@V_SHORTS[_0] -> _lim;
            _lim@(s)[_len] -> _pos;
            while _pos >@(s) _lim do
                if (_pos--!(s) -> _pos) /== _:`\s` then
                    return(_pint( ##(s){_pos, _lim} _add _1 ))
                endif
            endwhile
        else
            vec@V_BYTES[_0] -> _lim;
            _lim@(b)[_len] -> _pos;
            while _pos >@(b) _lim do
                if (_pos--!(b) -> _pos) /== _:`\s` then
                    return(_pint( ##(b){_pos, _lim} _add _1 ))
                endif
            endwhile
        endif
    endunless;
    0
enddefine;

define lconstant Buffer_extend(vline);
    lvars newbuff, len, size, x, vline;
    datalength(vedbuffer) -> len;
    vedusedsize(vedbuffer) ->> size -> vvedbuffersize;
    fi_max(vline, vvedmarkhi) fi_+ vedwindowlength -> x;
    if len fi_> x
    and (vedonstatus or len fi_< x fi_+ 70 or len fi_< size fi_+ 70
         or (size /== 0 and ((len fi_- size) fi_* 100) div size fi_< 10))
         ;;; i.e. not more than 10% of large buffer is empty
    then
        return
    endif;

    fi_max(x, size fi_+ fi_min(60,size)) -> x;
    Initv(x, nullstring) -> newbuff;
    move_subvector(1, vedbuffer, 1, newbuff, size);
    ;;; decide  whether to save new buffer
    if isvector(vedcurrentfile)
    and fast_subscrv(VF_BUFFER,vedcurrentfile) == vedbuffer
    then
        newbuff -> fast_subscrv(VF_BUFFER,vedcurrentfile)
    endif;
    newbuff -> vedbuffer;
enddefine;

define vedbufferextend();
    Buffer_extend(vedline)
enddefine;

define Shift_buffer_lines(line, _size);
    lvars line, pos, _size, _x, _topline;

    define lconstant Adjust_line(_lo, _diff, _line) -> _line;
        lvars _diff, _lo, _hi, _line;
        ;;; _diff is negative when buffer being expanded
        if _line fi_>= _lo then
            if _line fi_>= _lo fi_+ _diff  then
                _line fi_- _diff
            else
                _lo fi_- 1
            endif -> _line;
        endif;
    enddefine;

    define lconstant Adjust_marks(_line, _size, _lo, _hi);
        lvars oldlo, _line, _size, _lo, _hi;
        _lo -> oldlo;
        Adjust_line(_line, _size, _lo) -> _lo;
        Adjust_line(_line, _size, _hi) -> _hi;
        if oldlo fi_>= _line then
            if _hi fi_< _line then
                0 -> _hi; 1000000 -> _lo
            else
                fi_max(_line, _lo) -> _lo;
            endif;
        endif;
        _hi, fi_max(1, _lo);
    enddefine;

    ;;; if _size is positive then shift all of vedbuffer from line along,
    ;;; so as to make space for _size new lines
    ;;; first make sure buffer is big enough for the new lines
    ;;; if _size is negative get rid if the following _size lines
    if _size == 0 then
        return
    elseif _size fi_>= 0 then
        Buffer_extend(fi_max(line, vvedbuffersize) fi_+ _size);
        ;;; don't bother to shift if all text is above line
        unless line fi_> vvedbuffersize then
            move_subvector(line, vedbuffer, line fi_+ _size, vedbuffer,
                            vvedbuffersize fi_- line fi_+ 1);
            vvedbuffersize fi_+ _size -> vvedbuffersize
        endunless;
        negate(_size) -> _size;
    else
        negate(_size) -> _size;     ;;; notional amount to be removed
        line fi_+ _size -> _topline;
        unless _topline fi_> vvedbuffersize then
            move_subvector(line fi_+ _size, vedbuffer, line, vedbuffer,
                            vvedbuffersize fi_- _topline fi_+ 1);
        endunless;
        if vvedbuffersize fi_>= line then
            ;;; now put empty strings in the cleared part of buffer.
            ;;; (could check that vedbufferextend is not going to contract anyway)
            vvedbuffersize -> _x;   ;;; empty beyond there anyway
            fi_max(line fi_- 1, _x fi_- _size) -> vvedbuffersize;
            until _x == vvedbuffersize do
                nullstring -> fast_subscrv(_x,vedbuffer);
                _x fi_- 1 -> _x;
            enduntil;
            vedbufferextend();      ;;; may adjust vvedbuffersize
        endif;
    endif;
    ;;; sort out saved positions
    Adjust_line(line, _size, vedline) -> vedline;
    if vedline fi_> 0 then vedsetlinesize(); endif;
    for pos in vedpositionstack do
        Adjust_line(line, _size, fast_subscrv(1,pos))
            -> fast_subscrv(1,pos);
    endfor;
    Adjust_marks(line, _size, vvedmarklo, vvedmarkhi)
                    -> vvedmarklo -> vvedmarkhi;
    for pos in fast_back(marked_range_stack) do
        Adjust_marks(line, _size, fast_subscrv(1,pos), fast_subscrv(2,pos))
                    -> fast_subscrv(1,pos) -> fast_subscrv(2,pos);
    endfor;
    vedlineoffset fi_+ 1 -> _topline;
    if _topline fi_>= line and _topline fi_< line fi_+ _size then
        line
    else
        Adjust_line(line, _size, _topline)
    endif fi_- 1 -> vedlineoffset;
    Set_changed();
enddefine;

define vedtrimline();
    lvars string, size, new;
    ;;; ALWAYS call this procedure before changing vedline
    if vedline fi_< 1 then 1 -> vedline endif;
    vedusedsize(vedthisline() ->> string) -> size;
    if _pint(string!V_LENGTH) fi_> size then
        subvedstring(1, size, string) -> subscrv(vedline, vedbuffer)
    endif;
    size -> vvedlinesize;
    if size == 0 and vedline == vvedbuffersize then
        vedusedsize(vedbuffer) -> vvedbuffersize
    endif
enddefine;


define vedsetlinesize();
    if vedline fi_< 1 then 1 -> vedline endif;
    vedusedsize(vedthisline()) -> vvedlinesize;
    if vvedlinesize == 0 and vedline == vvedbuffersize then
        vedusedsize(vedbuffer) -> vvedbuffersize
    endif
enddefine;

define Buffer_line(line);
    lvars line;
    if _int(line) _gr vedbuffer!V_LENGTH then
        nullstring
    else
        fast_subscrv(line,vedbuffer)
    endif;
enddefine;
;;;
define updaterof Buffer_line(string, line);
    lvars string, line;
    if _int(line) _gr vedbuffer!V_LENGTH then Buffer_extend(line) endif;
    string -> fast_subscrv(line,vedbuffer);
    if line == vvedbuffersize then
        if _zero(string!V_LENGTH)  then
            vedusedsize(vedbuffer) -> vvedbuffersize
        endif
    elseif line fi_> vvedbuffersize and _nonzero(string!V_LENGTH) then
        line -> vvedbuffersize
    endif;
enddefine;

define vedthisline();
    Buffer_line(vedline)
enddefine;
;;;
define updaterof vedthisline(string);
    lvars string;
    string -> Buffer_line(vedline);
    vedusedsize(string) -> vvedlinesize
enddefine;

    /*  Get a normalised line from the Ved buffer
    */
define Normalised_line(_line, _copy) -> string;
    lvars string, org_string, _col, _line, _copy;
    Buffer_line(_line) ->> string -> org_string;
    if vvedpromptchar and (locchar(vvedpromptchar, 1, string) ->> _col) then
        Copy_vedstring(string) -> string;
        repeat
            `\s` -> fast_subscrs(_col, string);
            quitunless(locchar(vvedpromptchar, _col, string) ->> _col)
        endrepeat;
    endif;
    veddecodetabs(string) -> string;
    if _copy and string == org_string then
        Copy_vedstring(string) -> string
    endif
enddefine;

define vedcurrentvedchar();
    if vedcolumn == 0 then
        `\n`
    elseif vvedlinesize fi_< vedcolumn or vvedbuffersize fi_< vedline then
        `\s`
    else
        fast_subscrvedstring(vedcolumn, vedthisline())
    endif
enddefine;
;;;
define updaterof vedcurrentvedchar() with_nargs 1;
    dlocal vedstatic = true, vedcolumn;
    vedcharinsert()
enddefine;

define vedcurrentdchar();
    lvars vchar = vedcurrentvedchar();
    if iscompound(vchar) then fast_front(vchar) else vchar endif
enddefine;
;;;
define updaterof vedcurrentdchar(dchar);
    lvars dchar, vchar = vedcurrentvedchar();
    if ispair(dchar) then fast_front(dchar) -> dchar endif;
    if iscompound(vchar) then
        dchar -> fast_front(vchar);
        vchar -> vedcurrentvedchar();
        sys_grbg_destpair(vchar) -> (,)
    else
        dchar -> vedcurrentvedchar()
    endif
enddefine;

define vedcurrentchar();
    vedcurrentdchar() fi_&& 16:FFFF
enddefine;
;;;
define updaterof vedcurrentchar(char);
    lvars char, vchar = vedcurrentvedchar();
    if ispair(char) then fast_front(char) -> char endif;
    char fi_&& 16:FFFF -> char;
    if iscompound(vchar) then
        char fi_|| (fast_front(vchar) fi_&&~~ 16:FFFF) -> fast_front(vchar);
        vchar -> vedcurrentvedchar();
        sys_grbg_destpair(vchar) -> (,)
    else
        char fi_|| (vchar fi_&&~~ 16:FFFF) -> vedcurrentvedchar()
    endif
enddefine;

define Check_active_split(_col, string) /* -> act_to_left */;
    lvars c, string, _col, _lim = 0, _incr = -1, _scol = _col;
    ;;; check contiguous active chars between nonspace chars
    _col fi_- 1 -> _col;
    repeat 2 times
        repeat
            returnif(_col == _lim) (_lim /== 0);
            fast_subscrdstring(_col,string) -> c;
            returnunless(c &&/=_0 VEDCMODE_ACTIVE) (false);
            c fi_&& 16:FFFF -> c;
            quitunless(c == `\s` or c == `\t`);
            _col fi_+ _incr -> _col
        endrepeat;
        if _lim == 0 then
            _scol -> _col;
            vedusedsize(string) fi_+ 1 -> _lim;
            1 -> _incr
        endif
    endrepeat;

    unless ispair(fast_subscrvedstring(_col,string) ->> c) then
        ;;; add active continuation marker
        conspair(c, nullstring) -> fast_subscrvedstring(_col,string)
    endunless;
    false
enddefine;

    /*  Split string into 2 strings, starting the second at _col.
        If _stripwhite is true then remove leading tabs/spaces in the second.
    */
define Split_line(_col, string, _stripwhite)
                            /* -> (act_to_left, string2, string1) */;
    lvars string, _col, _index, _ulim, _stripwhite;

    vedusedsize(string) fi_+ 1 -> _ulim;
    if _col fi_>= _ulim then
        Check_active_split(_ulim, string), nullstring, string
    elseif _col == 1 then
        false, string, nullstring
    else
        Check_active_split(_col, string);
        _col -> _index;
        if _stripwhite and not(Skipwhite(_index, string) ->> _index) then
            nullstring
        else
            Realign_tab(0, subvedstring(_index, _ulim fi_- _index, string))
        endif;
        subvedstring(1, _col fi_- 1, string)
    endif
enddefine;

    /*  Shift the contents of a vedstring from subscript _col down or up
        by _ncols, expanding if necessary
    */
define Shift_string(_col, _ncols, string, expand) -> newstring;
    lvars   newstring, string, expand, tmp, _col, _used, _len, _needed, _ncols,
            _mcols, _flags;

    define lconstant Shift_data_up(_col, _ncols, string);
        lvars string, dvec, data, _ptr, _lim, _i, _col, _ncols, _lcol, _n;
        returnunless(vedstring_data_prop(string) ->> dvec);
        dvec@V_WORDS -> _ptr;
        _ptr@(w)[dvec!V_LENGTH] -> _lim;
        while _ptr <@(w) _lim do
            _ptr!(w) -> _i;
            if _i fi_>= _col then _i fi_+ _ncols -> _ptr!(w) endif;
            _ptr@(w)[_2] -> _ptr
        endwhile
    enddefine;

    _pint(string!V_LENGTH) -> _len;   ;;; datalength
    vedusedsize(string) -> _used;
    string -> newstring;
    if _ncols fi_>= 0 then
        ;;; shifting up
        fi_max(_col, _used) fi_+ _ncols -> _needed;
        if expand and (string == vedspacestring or _needed fi_> _len) then
            ;;; need new string -- add expand or 20 extra
            unless isinteger(expand) then 20 -> expand endunless;
            _needed fi_+ expand -> _len;
            _int(_len);
            string!KEY!K_FLAGS -> _flags;
            if _flags _bitst _:M_K_DSTRING then
                if _flags _bitst _:M_K_STRING16 then
                    Get_dstring16()
                else
                    Get_dstring()
                endif
            else
                if _flags _bitst _:M_K_STRING16 then
                    Get_string16()
                else
                    Get_string()
                endif
            endif -> newstring;
            ;;; fill newstring with spaces
            set_subvector(`\s`, 1, newstring, _len);
            string -> subdstring(1, fi_min(_col fi_- 1, _used), newstring);
            if vedstring_data_prop(string) ->> tmp then
                copy(tmp) -> vedstring_data_prop(newstring)
            endif
        endif;
        if _col fi_<= _used then
            Shift_data_up(_col, _ncols, newstring);
            move_subvector(_col, string, _col fi_+ _ncols, newstring,
                        fi_min(_used, _len fi_- _ncols) fi_- _col fi_+ 1);
            set_subvector(`\s`, _col, newstring, _ncols)
        endif
    else
        ;;; shifting down
        returnunless(_col fi_<= _used);
        0 fi_- _ncols -> _ncols;
        Set_subvedstring(false, _col, newstring, _ncols);
        _col fi_+ _ncols -> _needed;
        _used fi_- _needed fi_+ 1 -> _mcols;
        if _mcols fi_<= 0 then
            _mcols fi_+ _ncols -> _ncols;
            set_subvector(`\s`, _col, newstring, _ncols)
        else
            if _used fi_+ _ncols fi_<= _len then
                move_subvector(_needed, newstring, _col, newstring,
                                                    _mcols fi_+ _ncols)
            else
                move_subvector(_needed, newstring, _col, newstring, _mcols);
                set_subvector(`\s`, _col fi_+ _mcols, newstring, _ncols)
            endif
        endif
    endif
enddefine;

endsection;     /* $-Sys$-Ved */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 15 1997
        String16 changes
--- John Gibson, Jan 22 1996
        marked_range_stack now a pair with the actual stack in its back
--- John Gibson, Nov  7 1995
        Added Shift_string
--- John Gibson, Nov  3 1995
        Added Check_active_split
--- John Gibson, Sep  7 1995
        Changed to use new subvedstring etc, moved Shift_string to vdstring.p.
        Added vedcurrentvedchar
--- John Gibson, Feb  8 1994
        Got rid of Regi*ster_buffer_line
--- Jonathan Meyer, Sep 25 1993
        Removed Adjust_line on end_ved_search variable.
--- John Gibson, Apr 21 1992
        Sectionised.
--- John Gibson, Jan 18 1992
        Added -vedcurrentdchar-
--- John Gibson, Jan  3 1992
        Changes to cope with dstrings
--- John Gibson, Jun 21 1991
        Made -vedtrimline- set vvedlinesize
--- Aaron Sloman, May  9 1989
        Added some checks for vedline < 1.
--- John Gibson, Feb 22 1988
        Get_string into section Sys
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
 */
