/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/ratio_float.p
 > Purpose:
 > Author:          John Gibson, Feb  2 1988
 */

;;; -------- BIGINTEGER RATIO TO FLOATING-POINT CONVERSION -------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

constant
        procedure (Sys$-Bigint_//, Sys$-Bigint_->_dfloat)
    ;

;;; -----------------------------------------------------------------------

section $-Sys;

lconstant macro
    DFLOAT_SIG_SLICES   = (DFLOAT_SIG_BITS+SLICE_BITS-1) div SLICE_BITS;


    /*  Float bigintegers x/y into _df_opnd
    */
define Bigint_/_dfloat(_df_opnd, x, y);
    lvars y, rm, x, quot, _rlen, _adjust, _df_opnd;
    lstackmem dfloat _df_work;

    _CLAWBACK_SAVE;
    Bigint_//(x, y) -> quot -> rm;
    if issimple(quot) then
        _pf_dfloat_int(_int(quot), _df_opnd)
    else
        Bigint_->_dfloat(quot, _df_opnd, _0, false)
    endif;
    if rm /== 0 then
        if issimple(rm) then _1 else rm!BGI_LENGTH endif -> _rlen;
        (_:DFLOAT_SIG_SLICES _add _1 _sub _rlen _add y!BGI_LENGTH)
                                            _mult _:SLICE_BITS -> _adjust;
        Bigint_//(rm << _pint(_adjust), y) -> rm -> ;
        if Bigint_->_dfloat(rm, _df_work, _negate(_adjust), true) then
            _pfadd(_df_opnd, _df_work) ->
        endif
    endif;
    Clawback(0) ->          ;;; to reclaim space used
enddefine;

endsection;     /* $-Sys */
