/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_dollar.p
 > Purpose:         LIB * FORMAT_PRINT ~$ directive
 > Author:          John Williams, Jun 30 1992 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT, C.all/lisp/flib/float_utils.p
 */

uses format_print;

section $-lisp$-fpr;

#_IF not(is_subsystem_loaded("lisp"))
global vars $-lisp$-print_escape;
#_ENDIF

unless isdefined("number_utils") do
    syslibcompile("number_utils", f_liblist) ->
endunless;

unless isdefined("float_utils") do
    syslibcompile("float_utils", f_liblist) ->
endunless;


/* Code for the ~$ directive */

procedure(d, n, w, padchar);
    lvars arg, f, fsign, int_size, total_size;
    dlocal print_escape = nil;
    defaults d 2, n 1, w 0, padchar `\s`;

    next_f_arg() -> arg;

    if isreal(arg) then

        check_positive(d);
        check_positive(n);
        check_positive(w);

        if isrational(arg) then
            /* If arg is a rational number, then it is coerced to
                be a single float ...
            */
            number_coerce(arg, 1.0d0) -> arg
        endif;

        Sign_and_abs(arg, f_at) -> f -> fsign;
        Intof_print_length(f) -> int_size;
        datalength(fsign) + max(int_size, n) + point_size + d
            -> total_size;

        /* First padding and the sign are output ... if the :
            modifier is used, the sign appears before any padding,
            and otherwise after the padding
        */

        if f_colon then
            cuch_string(fsign)
        endif;

        cuch_chars(w - total_size, padchar);

        if not(f_colon) then
            cuch_string(fsign)
        endif;

        /* Then n digits are printed for the integer part of arg,
            with leading 0's if necessary; then a decimal point;
            then d digits of fraction, properly rounded
        */

        cuch_chars(n - int_size, `0`);
        Pr_float('', f, d, true, true)

    else

        /* If arg is a complex number or some non-numeric object,
            then it is printed using the format directive ~wD
        */

        f_arg_index - 1 -> f_arg_index;
        false ->> f_colon -> f_at;
        f_proc(`D`)(w, false, false)

    endif
endprocedure -> f_proc(`$`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 11 1998
        Now coerces to double float if arg is rational.
        Binds print_escape to nil (as specified in CLtL 2).
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 30 1992
        Uses new "number_utils" procedures
--- John Williams, Jun 30 1992
        Completely re-written, fixing BR isl-fr.4310
 */
