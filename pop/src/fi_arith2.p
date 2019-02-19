/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/fi_arith2.p
 > Purpose:
 > Author:          John Gibson, Feb  3 1988 (see revisions)
 > Documentation:   REF *FASTPROCS
 */


;;; ----------------- FAST INTEGER ARITHMETIC ----------------------------

#_INCLUDE 'declare.ph'

;;; ------------------------------------------------------------------------

define 5 fi_+ with_nargs 2;
    () _padd ()
enddefine;

define 5 fi_- with_nargs 2;
    () _psub ()
enddefine;

define 4 fi_* with_nargs 2;
    () _pmult ()
enddefine;

define 4 fi_// with_nargs 2;
    () _pdiv ()
enddefine;

define 2 fi_div -> _q with_nargs 2;
    lvars (, _q) = () _pdiv ();
enddefine;

define 2 fi_rem with_nargs 2;
    () _pdiv () ->
enddefine;

define fi_negate(_x);
    lvars _x;
    0 _psub _x
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 17 1992
        Added fi_negate
--- John Gibson, Mar 14 1988
        Added -fi_div- and -fi_rem-
 */
