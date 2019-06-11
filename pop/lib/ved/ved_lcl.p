/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_lcl.p
 >  Purpose:        CONVERTS NEXT N LINES TO LOWER CASE (defaults to 1)
 >  Author:         Aaron Sloman, July 1983 (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_lcl
 >  Related Files: LIB * VED_UCL, * VED_LCW, *VED_LCR
 */
compile_mode :pop11 +strict;

section;

define global ved_lcl();
    lvars n = strnumber(vedargument);
    vedconvertline(isuppercode, uppertolower, if n then round(n) else 1 endif)
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Feb 15 1992
        Changed to use -vedconvertline-
--- Aaron Sloman, Dec  4 1988
    Altered not to trigger vedautowrite.
--- Mark Rubinstein, Oct  4 1985 - sectionised and lvarsed.
 */
