/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           C.all/lib/auto/printlength.p
 *  Purpose:        return number of chars required to print item
 *  Author:         A Sloman, 1983 (see revisions)
 *  Documentation:  REF* PRINT, HELP * PRINTLENGTH
 *  Related Files:
 */

section;

define global constant procedure printlength(item) -> n;
    lvars item;
    dlvars n = 0;

    define dlocal cucharout();
        -> ;
        n fi_+ 1 -> n
    enddefine;

    pr(item)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 10 1987 corrected spelling of section
--- John Gibson, Jul 27 1987
        Changed to use a dlvar instead of putting chars on stack
 */
