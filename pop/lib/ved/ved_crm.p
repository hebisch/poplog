/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_crm.p
 >  Purpose:        Clear range mark. Could be optimised
 >  Author:         Aaron Sloman, Jul 13 1983 (see revisions)
 >  Documentation:
 >  Related Files:
 */

section;

define global ved_crm();
    dlocal vedline, vedscreenrangemark = `\s`;
    datalength(vedbuffer) -> vedline;
    vedmarklo();
    vedmarkhi();
    0 -> vvedmarkhi;
    9999999 -> vvedmarklo;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 18 1992
        Now uses -vedscreenrangemark-
--- Robert John Duncan, Jul 16 1990
        Corrected value of -vedscreenmark- to character rather than string.
--- Mark Rubinstein, Nov 11 1985 - sectionised.
 */
