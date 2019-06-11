/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_wmp.p
 > Purpose:         Wiggle matching parenthesis
 > Author:          John Williams, Oct 21 1988
 > Documentation:
 > Related Files:   C.all/lib/ved/ved_mp.p
 */
compile_mode :pop11 +strict;

section;

define vars ved_wmp();
    dlocal vedcolumn, vedline, vvedlinesize;
    ved_mp();
    vedwiggle(vedline, vedcolumn)
enddefine;

endsection;
