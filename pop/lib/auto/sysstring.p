/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/auto/sysstring.p
 > Purpose:        declare a large constant string for use in libraries
 > Author:         Mark Rubinstein, Mar  6 1986 (see revisions)
 > Documentation:
 > Related Files:
 */

section;

global constant
    sysstringlen    = 512,
    sysstring       = writeable inits(sysstringlen),
    ;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 23 1992
        Made sysstring writeable
--- Aaron Sloman, Nov 21 1986
    Moved to auto
*/
