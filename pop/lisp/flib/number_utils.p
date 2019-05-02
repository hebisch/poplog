/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/number_utils.p
 > Purpose:         Procedures common to the numeric FORMAT_PRINT directives
 > Author:          John Williams, Jun 30 1992 (see revisions)
 > Documentation:
 > Related Files:   LIB * FORMAT_PRINT, C.all/lisp/flib/f_{DBOX,E,F,dollar}.p
 */

section $-lisp$-fpr;

compile_mode:pop11 +strict;


/* Return sign (as a string) and absolute value of num */

define Sign_and_abs(num, want_plus_sign) -> num -> numsign;
    if num < 0 then
        negate(num) -> num;
        '-'
    else
        if want_plus_sign then
            '+'
        else
            ''
        endif
    endif -> numsign
enddefine;


/* Number of digits needed to print positive integer in given radix */

define Int_length(int, radix);
    dlocal popdprecision = true;

    ;;; LARGEST INT THAT WILL CONVERT TO A FLOAT WITHOUT ERROR
    lconstant max_int = (pop_float_radix**float_precision(1.0d0))-1;

    if int == 0 then
        1;
    else
        lvars guess = intof(log(int<max_int and int or max_int)/log(radix))+1;
        lvars n = radix ** guess;
        guess;
        until int < n do;
            n*radix -> n;
            + 1;
        enduntil
    endif
enddefine;


global vars number_utils = true;        ;;; for -uses-

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- Adrian Howard, Nov  5 1992
        dlocaled popdprecision
--- Adrian Howard, Nov  3 1992
        Fixed Int_length --- old version fouled up with some rounding errors
 */
