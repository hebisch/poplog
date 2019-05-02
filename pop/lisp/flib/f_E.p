/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_E.p
 > Purpose:         LIB * FORMAT_PRINT ~E directive
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

global vars $-lisp$-default_float_char;


/* Code for the ~E directive */

procedure(w, d, e, k, overflowchar, padchar, expchar);
    dlocal popdprecision = "ddecimal", print_escape = nil;

    lvars arg;
    next_f_arg() -> arg;

    if isreal(arg) then

        if isrational(arg) then
            /* If arg is a rational number, then it is coerced to
                be a single float ...
            */
            number_coerce(arg, 1.0d0) -> arg
        endif;

        lvars f, fsign;
        Sign_and_abs(arg, f_at) -> f -> fsign;

        /* Scale f by expt */

        lvars expt;
        defaults k 1;
        check_integer(k);

        if f = 0 then
            k - 1
        else
            k - Upper_expt(f)
        endif -> expt;

        unless expt == 0 do
            f * (10 ** expt) -> f
        endunless;

        negate(expt) -> expt;

        /* Size and sign of exponent */

        lvars esign, expt_size;
        Sign_and_abs(expt, true) -> expt -> esign;
        Int_length(expt, 10) -> expt_size;
        defaults e expt_size;

        /* If k is 0, then d digits are printed after the decimal
            point, and a single 0 before the decimal point if the field
            width will allow it. If k is positive ... ; k significant
            digits are printed before the decimal point and d - k + 1
            digits are printed after. If k is negative ... ; a single 0
            is printed before the decimal point if the total field width
            will permit it, and after the decimal point are printed first
            -k 0's and then d + k significant digits.
        */

        lvars int_places, frac_places;

        if (k > 0)
        and (f /= 0) then
            k
        else
            1
        endif -> int_places;

        /* If the parameter d is omitted ... a value is chosen for
            d subject to the width constraint w, the scale factor
            k, and the constraint that no trailing 0 digits may appear
            in the fraction, except if the fraction to be printed
            is 0, then a single 0 digit should appear after the decimal
            point if permitted by w.
        */

        /* First calculate size of sign, integer part, and exponent
            i.e. everything but the fractional digits */

        lvars non_frac_size;

        datalength(fsign)
            + int_places
            + point_size
            + expchar_size
            + datalength(esign)
            + e
            -> non_frac_size;

        if d then
            check_positive(d);
            if k > 0 then
                max(d - k + 1, 0)
            else
                d
            endif -> frac_places
        else
            if w then
                Fracof_print_length(f, w - non_frac_size) -> frac_places
            else
                Max_pr_places(f) -> frac_places
            endif
        endif;

        /* Deal with w parameter if supplied */

        lvars leading_0 = true, trailing_0 = false;

        if w then

            /* Calculate total printed size */

            lvars total_size = non_frac_size + frac_places;

            /* Skip leading 0 if necessary */

            if (f < 1)
            and (total_size = w + 1) then
                false -> leading_0;
                total_size - 1 -> total_size
            endif;

            /* Print overflow if w or e too small */

            if ((total_size > w) or (expt_size > e))
            and overflowchar then
                cuch_chars(w, overflowchar);
                return
            endif;

            /* Print padding */

            defaults padchar `\s`;
            cuch_chars(w - total_size, padchar);

            true -> trailing_0;

        endif;

        /* Print fsign and f */

        Pr_float(fsign, f, frac_places, leading_0, trailing_0);

        /* Print exponent */

        defaults expchar
                (if f_subsystem == "lisp" then
                    if default_float_char == `S` then
                        if issimple(arg) then `e` else `d` endif
                    else
                        if issimple(arg) then `f` else `e` endif
                    endif
                 else
                    `e`
                 endif);

        cucharout(expchar);
        cuch_string(esign);
        cuch_chars(e - expt_size, `0`);
        sys_syspr(expt);

    else

        /* If arg is a complex number or some non-numeric object,
            then it is printed using the format directive ~wD
        */

        f_arg_index - 1 -> f_arg_index;
        false ->> f_colon -> f_at;
        f_proc(`D`)(w, false, false)

    endif
endprocedure -> f_proc(`E`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 11 1998
        Now coerces to double float if arg is rational.
        Binds print_escape to nil (as specified in CLtL 2).
--- John Williams, Aug 25 1995
        Fixed BR johnw.1037
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Jul  1 1992
        Changed cucharout(esign) to appdata(esign, cucharout)
--- John Williams, Jun 30 1992
        Uses new "number_utils" procedures
--- John Williams, Jun 30 1992
        Completely re-written, fixing BR isl-fr.4310
 */
