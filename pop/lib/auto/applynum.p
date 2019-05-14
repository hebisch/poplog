/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/applynum.p
 > Purpose:         Apply the first argument N times
 > Author:          Aaron Sloman, Jan 12 1991 (see revisions)
 >              (Based on a fault report by Steve Knight)
 > Documentation:   REF * applynum, REF * STACK
 > Related Files:   LIB * DUPNUM, LIB * SYSREPEAT
 */
compile_mode:pop11 +strict;

section;

sysunprotect("applynum");

define applynum(p, n);
    lvars p, n;
    if isprocedure(p) then
        fast_repeat fi_check(n, false, false) times
            fast_apply(p)
        endrepeat
    else
        fast_repeat fi_check(n, false, false) times p() endrepeat
    endif
enddefine;

define updaterof applynum(p, n);
    lvars p, n;
    if isprocedure(p) then
        fast_repeat fi_check(n, false, false) times
            -> fast_apply(p)
        endrepeat
    else
        fast_repeat fi_check(n, false, false) times -> p() endrepeat
    endif
enddefine;

sysprotect("applynum");
endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun  9 1991
    Fixed header and put in compile_mode
 */
