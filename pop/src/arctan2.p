/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/arctan2.p
 > Purpose:
 > Author:          John Gibson 1988 (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;;----------------------- ARCTAN2 -----------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'

constant
        procedure Sys$-Trig_inv_return
    ;

;;; ------------------------------------------------------------------------

section $-Sys => arctan2;

define arctan2(x, y);
    lvars x, y, _X = _dfop1, _Y = _dfop2, _argtype;
    if _neg(Dfloat(x, _X) _biset Dfloat(y, _Y) ->> _argtype) then
        mishap(x, y, 2, 'REAL NUMBER(S) NEEDED')
    else
        unless _pfzero(_X) and _pfzero(_Y) then
            _MATH2(_Y, _X, _f_ATAN2) ->     ;;; note args reversed!
        ;;; else return 0 for arctan2(0,0)
        endunless;
        Trig_inv_return(_argtype, _Y)
    endif
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Replaced _m*ath2 subroutine with _MATH2 macro
 */
