/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/float_utils.p
 > Purpose:         Procedures common to the ~F, ~E, ~$, ~G directives
 > Author:          John Williams, Jun 29 1992 (see revisions)
 > Documentation:
 > Related Files:   LIB * FORMAT_PRINT, C.all/lisp/flib/f_{F,E,dollar,G}.p
 */

section $-lisp$-fpr;

compile_mode:pop11 +strict;

/* In the code below

    lvar fsign refers to string holding a sign, i.e. '+', '-', or ''

    lvar f refers to a positive floating point number

*/

constant
    point_size      =   1,          ;;; Size of decimal point
    expchar_size    =   1,          ;;; Size of char signifying exponent
    ;


define Upper_expt(f);
    lvars l;
    /* Calculate exponent where (10 ** (n - 1)) <= f < (10 ** n) */
    if f = 0 then
        0
    else
        log10(f) -> l;
        if fracof(l) = 0
        or l >= 0 then
            intof(l) + 1
        else
            intof(l)
        endif
    endif
enddefine;


define Max_pr_places(f);
    if isddecimal(f) then
        #_< intof(float_digits(1.0d0) * (log(2) / log(10))) >_#
    else
        #_< intof(float_digits(1.0s0) * (log(2) / log(10))) >_#
    endif
enddefine;


define Free_format_float_pr(fsign, f);
    dlocal pop_pr_places = Max_pr_places(f);
    cuch_string(fsign);
    sys_syspr(f)
enddefine;


define Intof_print_length(f);
    intof(f) -> f;
    if f = 0 then
        1
    else
        intof(log10(f)) + 1
    endif
enddefine;


define Fracof_print_length(f, pop_pr_places) -> n;
    dlvars n;
    dlocal pop_pr_places, pop_pr_radix = 10;

    define dlocal cucharout() with_nargs 1;
        ->;
        n fi_+ 1 -> n
    enddefine;

    if pop_pr_places <= 0 then
        0 -> n      ;;; nothing printed after .
    else
        -2 -> n;    ;;; allow for 0.
        sys_syspr(fracof(f))
    endif
enddefine;


define Pr_float(fsign, f, pop_pr_places, leading_0, trailing_0);
    dlocal pop_pr_places;

    if trailing_0
    and pop_pr_places > 0 then
        (`0` << 16) || pop_pr_places -> pop_pr_places;
    endif;

    cuch_string(fsign);         ;;; fsign is '+', '-', or ''

    if leading_0 then
        sys_syspr(f)
    else
        /* Omit leading 0 (first one only) */
        procedure();
            dlvars procedure saved_cucharout = cucharout;

            define dlocal cucharout() with_nargs 1;
                erase();
                saved_cucharout -> cucharout
            enddefine;

            sys_syspr()
        endprocedure(f)
    endif;

    if pop_pr_places <= 0 then
        cucharout(`.`)
    endif
enddefine;


global vars float_utils = true;     ;;; for uses


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
 */
