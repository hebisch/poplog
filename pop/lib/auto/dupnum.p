/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/dupnum.p
 > Purpose:         Produce N copies of top of stack
 > Author:          Aaron Sloman, Jan 12 1991 (see revisions)
 >              (Based on a fault report by Steve Knight)
 > Documentation:   REF * dupnum , REF STACK
 > Related Files:   LIB * APPLYNUM
 */
compile_mode:pop11 +strict;

section;

sysunprotect("dupnum" );

define global dupnum(x, n);
    lvars x, n;
    fast_repeat fi_check(n, false, false) times
        x
    endrepeat;
enddefine;

sysprotect("dupnum");

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun  9 1991
    put in compile_mode
 */
