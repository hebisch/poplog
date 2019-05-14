/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/INCLUDE_constant.p
 > Purpose:         Macro that produces 'lconstant' or 'global constant'
 >                  depending on whether file is being #_INCLUDEd or not
 > Author:          John Gibson, Jun  5 1989 (see revisions)
 > Documentation:   REF *PROGLIST
 */

section;

global constant macro INCLUDE_constant = "iconstant";

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 17 1991
        "iconstant" now in system, so just produces that.
 */
