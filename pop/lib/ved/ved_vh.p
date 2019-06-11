/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_vh.p
 > Purpose:         Explain VED command
 > Author:          Unknown (see revisions)
 > Documentation:   REF * ved_vh
 > Related Files:
 */

section;

define global ved_vh();
    vedputcommand('?? ved_' <> vedargument);
    vedredocommand()
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Williams, Nov  9 1990
        Now uses -vedputcommand- (to show its connection with -ved_??-).
--- John Gibson, Aug  6 1987
        Changed -ved_whats- to -ved_??-
--- Aled Morris, Aug 12 1986
        Completely rewritten using -ved_?-
--- Mark Rubinstein, Oct  4 1985
        Completely rewritten using VVEDGOTOPLACE and sectionised.
 */
