/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           C.all/lib/auto/prnum.p
 *  Purpose:        Prints number according to a given format
 *  Author:         S Hardy, 1979 (see revisions)
 *  Documentation:  REF *PRINT, HELP *PRNUM
 *  Related Files:  LIB * FORMAT_PRINT
 */

/*
The format consists of two integers - the first specifying the amount of
space allocated for that part of the number preceding the decimal point
(including a minus sign if necessary) and the second specifying the number
of digits printed after the decimal point (including the decimal point
itself).

If the second part of the format is 1 the number is printed as an
integer.

Trailing zeroes are printed to give an indication of the accuracy to which
the number is being printed.

*/

section;

define global constant procedure prnum(num, pre, post);
    lvars num, pre, post;
    dlocal pop_pr_exponent = false, pop_pr_places;
    unless isdecimal(num) then number_coerce(num,0.0d0) -> num endunless;
    if post == 1 then
        0 -> pop_pr_places;
        pr_field(num, pre, `\s`, false);
        cucharout(`.`)
    else
        if post == 0 then
            0
        else
            (`0` fi_<< 16) fi_+ (post-1)
        endif -> pop_pr_places;
        pr_field(num, pre+post, `\s`, false);
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 28 1987
        Rewrote using -pr_field-
 */
