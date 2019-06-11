/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_ucl.p
 >  Purpose:        CONVERTS NEXT N LINES TO UPPER CASE. (defaults to 1)
 >  Author:         Aaron Sloman, July 1983 (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_ucl
 >  Related Files:  LIB * VED_UCR, * VED_UCW * VED_LCL
 */
compile_mode :pop11 +strict;

section;

define global ved_ucl();
    lvars n = strnumber(vedargument);
    vedconvertline(islowercode, lowertoupper, if n then round(n) else 1 endif)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 15 1992
        Changed to use -vedconvertline-
--- Aaron Sloman, Dec  4 1988
    Altered not to trigger vedautowrite. Also moves to next line at end.
 */
