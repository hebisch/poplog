/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/trigh.p
 > Purpose:
 > Author:          John Gibson 1988
 > Documentation:   REF *NUMBERS
 */

;;;------------------ HYPERBOLIC TRIG FUNCTIONS -----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'mathlib.ph'

constant
        procedure Sys$-Trig_1arg
    ;

;;; ----------------------------------------------------------------------

define sinh() with_nargs 1;
    Sys$-Trig_1arg(_f_SINH, TRIGOP_sinh)
enddefine;

define cosh() with_nargs 1;
    Sys$-Trig_1arg(_f_COSH, TRIGOP_cosh)
enddefine;

define tanh() with_nargs 1;
    Sys$-Trig_1arg(_f_TANH, TRIGOP_tanh)
enddefine;
