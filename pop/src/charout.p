/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/charout.p
 > Purpose:         Write characters to standard output
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *CHARIO
 > Related Files:   C.all/src/charin.p
 */

#_INCLUDE 'declare.ph'
#_INCLUDE 'io.ph'

global vars
        procedure pop_chario_trap
    ;

weak global vars
        vedindentstep
    ;

section $-Sys;

constant
        procedure (Io$-Put_char, Io$-Log_char), termin_printstring
    ;

vars
        charout_dev, charerr_dev
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys => pop_charout_col, pop_charerr_col,
                 poplinewidth, poplinemax, poplineprefix,
                 charout, charerr, cucharout, cucharerr, cuchartrace;

vars
    pop_charout_col     = 0,
    pop_charerr_col     = 0,
    poplinewidth        = 70,
    poplinemax          = 78,
    poplineprefix       = false,
    ;


define lconstant Charout(char, dev, _length_id);
    lvars char, dev, prefix, _length_id, _length, _tab, _char;

    if char == termin then
        termin_printstring -> char
    endif;
    if iscompound(char)
    and ((char!KEY!K_FLAGS ->> _char) _bitst _:M_K_STRING
         or (_char _bitst _:M_K_VECTOR
            and char!KEY!K_FIELD_CODE_V == _:t_BYTE))
    then
        _0 -> _tab;
        while _tab _lt char!V_LENGTH do
            Charout(if _char _bitst _:M_K_STRING16 then
                        _pint(char!V_SHORTS[_tab])
                    else
                        _pint(char!V_BYTES[_tab])
                    endif, dev, _length_id);
            _tab _add _1 -> _tab
        endwhile;
        return
    endif;

    ;;; This procedure is also called by charin with 2nd arg true to
    ;;; mean input rather than output. Initially pop_chario_trap
    ;;; is erasenum(2), but is redefined by Ved etc.
    pop_chario_trap(dev, false);


    define lconstant out_prefix();
        dlocal poplinewidth = false, poplineprefix = false;
        Charout()
    enddefine;

    _int(char) _bimask _16:FFFF -> _char;
    fast_idval(_length_id) -> _length;

    if _length == 0 then
        ;;; start of line
        if ispair(poplineprefix)
        and isstring(fast_front(poplineprefix) ->> prefix) then
            out_prefix(prefix, dev, _length_id);
            fast_idval(_length_id) -> _length
        endif
    elseif _length fi_>= poplinewidth and isinteger(poplinewidth)
    and (_char == _:`\s` or _char == _:`\t`
         or (_length fi_>= poplinemax and _char /== _:`\n`))
    then
        ;;; insert automatic line-break
        Io$-Put_char(`\n`, dev);  Io$-Log_char(`\n`);
        0 -> fast_idval(_length_id);
        if ispair(poplineprefix)
        and isstring(fast_back(poplineprefix) ->> prefix) then
            out_prefix(prefix, dev, _length_id)
        else
            Charout(`\t`, dev, _length_id)
        endif;
        returnif(_char == _:`\s` or _char == _:`\t`);
        fast_idval(_length_id) -> _length
    endif;

    Io$-Put_char(char, dev);  Io$-Log_char(char);

    if _char == _:`\n` then
        0
    elseif _char == _:`\t` then
        if dev!D_FLAGS _bitst _M_D_USER_DEV then
            weakref vedindentstep
        else
            8
        endif -> _tab;
        (_length fi_// _tab) fi_* _tab fi_+ _tab -> _tab -> ;
        _tab
    elseif _char fi_< _:`\s` then
        _length
    else
        _length fi_+ 1
    endif -> fast_idval(_length_id)
enddefine;

define charout(/*char*/) with_nargs 1;
    Charout((), charout_dev, ident pop_charout_col)
enddefine;

define charerr(/*char*/) with_nargs 1;
    Charout((), charerr_dev, ident pop_charerr_col)
enddefine;

vars procedure (
    cucharout       = charout,
    cucharerr       = charerr,
    );

vars
    cuchartrace     = false,
;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 14 1997
        String16 changes
--- John Gibson, Feb 20 1996
        Added poplineprefix
--- John Gibson, May  3 1993
        Moved in cuchartrace from trace.p
--- John Gibson, Nov 16 1990
        Changed call to chario_trap to pop_chario_trap taking 2nd arg false.
--- John Gibson, Oct 26 1990
        Moved -pop_charout/err_device- to popdevout.p
--- John Gibson, Jun  6 1990
        -charout_dev- and -charerr_dev- now contain devices not idents
--- John Gibson, Mar 24 1990
        Changed test in -Charout- for byte vector.
--- John Gibson, Feb 19 1989
        Included io.ph
--- John Williams, Oct  6 1988
        Added -pop_charout-device- and -pop_charerr_device-
--- John Gibson, Mar 16 1988
        Previously in iochar.p
 */
