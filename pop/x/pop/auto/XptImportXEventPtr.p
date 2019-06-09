/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptImportXEventPtr.p
 > Purpose:         Importing XEventPtr structs
 > Author:          Jonathan Meyer, Nov 28 1990 (see revisions)
 > Documentation:   REF *XPT_COERCE
 > Related Files:
 */

compile_mode:pop11 +strict;

section;

include xpt_constants.ph;   ;;; Load constants in

;;; Import an XEventPtr
define global XptImportXEventPtr
    = XptImportAny(%XDT_XEVENTPTR%);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1991
        Moved XptXEventPtr typespec definition to xpt_xtypes.ph
--- Roger Evans, Feb  3 1991 renamed XptGetDescriptor -> XptImportAny
--- Jonathan Meyer, Dec  5 1990, Made use of XptGetDescriptor
 */
