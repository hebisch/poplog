/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/float_random.p
 > Purpose:
 > Author:          John Gibson, Feb  1 1988
 */

;;; ----------------- FLOATING-POINT RANDOM GENERATION ---------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'

constant
        procedure (Sys$-Random_genseed)
    ;

;;; ------------------------------------------------------------------------

section $-Sys;

define Float_random(n, want_0);
    lvars n, want_0, _type, _seed;
    if issimple(n) then
        _pf_dfloat_dec(n, _dfop1), _:ARGTYPE_DECIMAL -> _type
    else
        _pf_dfloat_ddec(n, _dfop1), _:ARGTYPE_DDECIMAL -> _type
    endif;
    if _pfneg(_dfop1) or _pfzero(_dfop1) then
        mishap(n, 1, '(D)DECIMAL > 0 NEEDED')
    endif;

    ;;; float next seed into _dfop2, losing the bottom 3 bits
    _pf_dfloat_int(Random_genseed() _biclear _2:111, _dfop2);
    if _type == _:ARGTYPE_DDECIMAL and _nonzero(Random_genseed() ->> _seed) then
        ;;; another seed for the low order bits
        _pf_dfloat_int(_seed, _dffrac);
        _pf_expof(_dffrac) _sub (_:RANSEED_BITS _sub _3) -> _pf_expof(_dffrac) -> ;
        _pfadd(_dfop2, _dffrac) ->
    endif;
    if _pfzero(_dfop2) then
        unless want_0 then return(n) endunless  ;;; n instead of 0.0
    else
        ;;; make it > 0.0 and < 1.0
        _pf_expof(_dfop2) _sub _:RANSEED_BITS -> _pf_expof(_dfop2) -> ;
        _pfmult(_dfop2, _dfop1) ->          ;;; times n
    endif;
    Consdecimal(_type, _dfop2)
enddefine;

endsection;     /* $-Sys */
