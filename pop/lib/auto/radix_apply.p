/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/radix_apply.p
 > Purpose:         Call Print procedure with argument for pop_pr_radix
 > Author:          Aaron Sloman, Sep 16 1988 (see revisions)
 > Documentation:   REF * PRINT
 > Related Files:   LIB * PRNUM, LIB * FORMAT_PRINT
 */
compile_mode :pop11 +strict;

/*
For any printing procedure P

    radix_apply(arg1,..argN, P, <integer>);
E.g.
    vars root2=sqrt(2);
    radix_apply(root2,20,`>`,`<`,pr_field,2);
    >>>>>>1.011011<<<<<<

    radix_apply(1234 && 5678, 5678, 1234,'%p && %p is %p',printf,2);
    10011010010 && 1011000101110 is 10000000010
*/

section;

define radix_apply(p, pop_pr_radix);
    lvars procedure p;
    dlocal pop_pr_radix;
    p()
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 11 1995
        Added +strict etc
--- John Williams, Aug  7 1992
        Declared -radix_apply- as global, in top-level section.
 */
