/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/auto/outcharitem.p
 >  Purpose:        takes a character consumer and returns an item consumer
 >  Author:         Aaron Sloman, April 1982 (see revisions)
 >  Documentation:  HELP * OUTCHARITEM
 >  Related Files:  HELP * INCHARITEM
 */
compile_mode :pop11 +strict;

section;

define global outcharitem(x) -> y;
    lvars x y;
    procedure(i, cucharout);
        lvars i;
        dlocal cucharout;
        pr(i)
    endprocedure(% x %) -> y;
    pdprops(x) -> pdprops(y);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun 13 1986 fixed bug 'pr(y)' instead of 'pr(i)'.
*/
