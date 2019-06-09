/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XawCardinals.ph
 > Purpose:         Set-up the Cardinal macros for Athena Widgets
 > Author:          Andreas Schoter, Jun 20 1991 (see revisions)
 > Documentation:   REF *ATHENA
 */

#_TERMIN_IF DEF XAWCARDINALS_INCLUDED

section;

iconstant macro(
    ZERO = 0,
    ONE = 1,
    TWO = 2,
    THREE = 3,
    FOUR = 4,
    FIVE = 5,
    SIX = 6,
    SEVEN = 7,
    EIGHT = 8,
    NINE = 9,
    TEN = 10
);

iconstant XAWCARDINALS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 17 1993
        Made an include file
--- Andreas Schoter, Jul 15 1991
    Added global constant XawCardinals for compatability with uses.
 */
