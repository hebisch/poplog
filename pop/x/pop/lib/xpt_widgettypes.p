/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_widgettypes.p
 > Purpose:         Typespec of Core and Composite widgets
 > Author:          Adrian Howard, Sep 17 1991 (see revisions)
 > Documentation:   REF *XPT_WIDGETTYPES
 > Related Files:
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

loadinclude xpt_coretypes.ph;
loadinclude xpt_generaltypes.ph;
loadinclude xpt_xtypes.ph;
loadinclude xpt_widgettypes.ph;

global vars $-xpt_widgettypes = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1991
        Moved typespecs to xpt_widgettypes.ph
 */
