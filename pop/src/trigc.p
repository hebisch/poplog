/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/trigc.p
 > Purpose:
 > Author:          John Gibson 1988
 > Documentation:   REF *NUMBERS
 */

;;;--------------------- CIRCULAR TRIG FUNCTIONS -----------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'mathlib.ph'

constant
        procedure Sys$-Trig_1arg
    ;

;;; ------------------------------------------------------------------------

define sin() with_nargs 1;
    Sys$-Trig_1arg(_f_SIN, TRIGOP_sin)
enddefine;

define cos() with_nargs 1;
    Sys$-Trig_1arg(_f_COS, TRIGOP_cos)
enddefine;

define tan() with_nargs 1;
    Sys$-Trig_1arg(_f_TAN, TRIGOP_tan)
enddefine;
