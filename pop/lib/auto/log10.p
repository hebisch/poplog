/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/auto/log10.p
 >  Purpose:        Logs to the base 10
 >  Author:         Aaron Sloman, March 1984 (see revisions)
 >  Documentation:  REF * NUMBERS
 */
compile_mode :pop11 +strict;

section;

define log10() with_nargs 1;
    lvars l = log();
    lconstant loge10d = procedure;
                            dlocal popdprecision = true;
                            log(10)
                        endprocedure(),
              loge10s = number_coerce(loge10d, 0.0s0);

    l / if issimple(destcomplex(l) ->) then loge10s else loge10d endif
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jul 19 1995
        Improved to give correct result type
 */
