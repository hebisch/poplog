/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/src/initvectorclass.p
 > Purpose:
 > Author:          Aled Morris, Sep  17 1986 (see revisions)
 > Documentation:   REF *DATA
 */

;;; -------------------- BUILDING ANY VECTOR -------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

section $-Sys;

constant
        procedure (Check_vkey, Get_rawvec)
    ;

endsection;

;;; -----------------------------------------------------------------------

section $-Sys => initvectorclass;

define initvectorclass(_siz, initval, key) -> vec;
    lvars _siz, initval, key, vec;
    Check_vkey(key);
    Check_integer(_siz, 0);
    if key!K_FLAGS _bitst _:M_K_FULL_VECTOR then
        Initv(_siz, initval) -> vec;
        key -> vec!KEY
    else
        Get_rawvec(_int(_siz), key, key!K_GC_TYPE == _:GCTYPE_USERVEC) -> vec;
        key!K_FAST_SUBSCR_V!PD_UPDATER -> key;
        until _siz == 0 do
            fast_apply(initval, _siz, vec, key);
            _siz fi_- 1 -> _siz
        enduntil
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  6 1988
        Moved out of vectors.p
 */
