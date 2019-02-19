/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/src/fi_arithcmp.p
 > Purpose:
 > Author:          John Gibson, Feb  3 1988
 > Documentation:   REF *FASTPROCS
 */

;;; ----------- FAST INTEGER ARITHMETIC COMPARISON ----------------------------

#_INCLUDE 'declare.ph'

constant
        6 (_psgr, _psgreq, _pslt, _pslteq)
    ;


define 6 fi_> with_nargs 2;
     _psgr
enddefine;

define 6 fi_>= with_nargs 2;
     _psgreq
enddefine;

define 6 fi_< with_nargs 2;
     _pslt
enddefine;

define 6 fi_<= with_nargs 2;
     _pslteq
enddefine;

define fi_min(x, y);
    lvars x, y;
    if x fi_< y then x else y endif
enddefine;

define fi_max(x, y);
    lvars x, y;
    if x fi_> y then x else y endif
enddefine;
