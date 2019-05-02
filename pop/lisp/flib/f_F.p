/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_F.p
 > Purpose:         LIB * FORMAT_PRINT ~F directive
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


/* Code for the ~F directive */

procedure(w, d, k, overflowchar, padchar);
    lvars arg, f, fsign, int_size, room, total_size;
    dlocal popdprecision = "ddecimal", print_escape = nil;

    next_f_arg() -> arg;

    if isreal(arg) then

        defaults k 0, padchar `\s`;
        check_integer(k);

        if isrational(arg) then
            /* If arg is a rational number, then it is coerced to
                be a single float ...
            */
            number_coerce(arg, 1.0d0) -> arg
        endif;

        arg * (10 ** k) -> arg;
        Sign_and_abs(arg, f_at) -> f -> fsign;

        if w then

            /* w supplied */

            check_positive(w);
            datalength(fsign) + Intof_print_length(f) + point_size
                -> int_size;

            if d then

                /* w and d both supplied */

                check_positive(d);

                /* Leading 0's are not permitted, except that a single
                    0 is output before the decimal point if the printed
                    value [JW: i.e. absolute value] is less than 1, and
                    this 0 is not printed if w = d + 1
                */

                if (f < 1) and (w = d + 1) then
                    Pr_float(fsign, f, d, false, true)
                else
                    int_size + d -> total_size;
                    if (total_size > w)
                    and overflowchar then
                        cuch_chars(w, overflowchar)
                    else
                        cuch_chars(w - total_size, padchar);
                        Pr_float(fsign, f, d, true, true)
                    endif
                endif

            else

                /* If d is omitted then there is no constraint on the
                    number of digits to appear after the decimal point.
                    A value for d is chosen so that as many digits as
                    possible are printed subject to the width constraint
                    w and the constraint that no trailing 0's may
                    appear in the fraction, unless the fraction is 0,
                    in which case a single 0 is printed after the decimal
                    point of permitted by the width constraint
                */

                /* JW - must know how many significant digits after decimal
                    point in arg to know how many padchar's needed
                */

                w - int_size -> room;
                if room < 0 then
                    if overflowchar then
                        cuch_chars(w, overflowchar)
                    else
                        Free_format_float_pr(fsign, f)
                    endif
                else
                    Fracof_print_length(f, room) -> d;
                    if room > d then
                        cuch_chars(room - d, padchar)
                    endif;
                    Pr_float(fsign, f, d, true, false)
                endif
            endif

        elseif d then

            /* If the w parameter is omitted, ... a value for w is
                chosen in such a way that no leading pad characters
                need to be printed and exactly d characters will
                follow the decimal point
            */

            check_positive(d);
            Pr_float(fsign, f, d, true, true)

        else

            /* If both w and d are omitted, the effect is to
                print the value using ordinary free format output ...
            */

            Free_format_float_pr(fsign, f)

        endif

    else

        /* If arg is a complex number or some non-numeric object,
            then it is printed using the format directive ~wD
        */

        f_arg_index - 1 -> f_arg_index;
        false ->> f_colon -> f_at;
        f_proc(`D`)(w, false, false)

    endif
endprocedure -> f_proc(`F`);


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
