/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/trig.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *NUMBERS
 */

;;;--------------------- TRIG FUNCTIONS --------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'mathlib.ph'

constant
        procedure Sys$-Cxf_trigop
    ;

;;; -----------------------------------------------------------------------

section $-Sys => pi popradians;

global constant
    pi = 3.1415926535897932;

global vars
    popradians  = false;

constant
    _pi_/_180   = _0.017453292519943296;        ;;; pi/180


;;; --- FOR CIRCULAR & HYPERBOLIC TRIG -----------------------------------

define Trig_1arg(x, _lib_routine, opsub);
    lvars x, opsub, _A = _dfop1, _argtype, _lib_routine;
    if _neg(Dfloat(x, _A) ->> _argtype) then
        ;;; arg is complex
        returnif(CXWEAK Cxf_op_cmplx(opsub, x, CXWEAK Cxf_trigop))
    else
        unless popradians then _pfmult(_A, _pi_/_180) -> endunless;
        returnif(_MATH1(_A, _lib_routine)) (Consdecimal(_argtype, _A))
    endif;
    Float_overflow(x, 1, false, 'trig-1arg')
enddefine;


;;; --- FOR INVERSE TRIG ---------------------------------------------------

define Trig_inv_return(_X) with_nargs 2;
    lvars _X;
    unless popradians then _pfdiv(_X, _pi_/_180) -> endunless;
    Consdecimal(_X)     ;;; _argtype on stack
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 25 1995
        Replaced _m*ath1 subroutine with _MATH1 macro
--- John Gibson, Feb  9 1988
        Changes for splitting up files, sectioning, putting in weakrefs, etc.
--- John Gibson, Dec  6 1987
        Lconstant'ed, tidied up.
 */
