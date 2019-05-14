/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/lib/pop_pr_ratios.p
 > Purpose:         A flag to toggle printing of ratios as floats
 > Author:          John Williams, Dec 17 1985
 > Documentation:   HELP * POP_PR_RATIOS
 > Related Files:
 */

section;

global vars pop_pr_ratios = false;

procedure(num);
    if pop_pr_ratios then
        #_< class_print(ratio_key) >_# (num)
    else
        syspr(number_coerce(num, 0.0d0))
    endif
endprocedure -> class_print(ratio_key);

endsection;
