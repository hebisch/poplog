/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptImportScreenPtr.p
 > Purpose:         Importing ScreenPtr structs
 > Author:          Roger Evans, Oct 20 1990 (see revisions)
 > Documentation:   REF *XPT_COERCE
 > Related Files:
 */

compile_mode:pop11 +strict;

section;

include xpt_constants.ph;   ;;; Load constants in

;;; Import a ScreenPtr - 14/09/90
define global XptImportScreenPtr
        = XptImportAny(%XDT_SCREENPTR%);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1991
        Moved XptScreenPtr typespec definition to xpt_xtypes.ph
--- Roger Evans, Feb  3 1991 changed to use XptImportAny
--- Roger Evans, Nov 19 1990 changd to use consXptDescriptor
--- Roger Evans, Nov 15 1990 added typespec
--- Adrian Howard, Oct 26 1990 : Bug fixed in the updater
--- Adrian Howard, Oct 24 1990 : Used false -XptIsPtrItem- option instead of
        null procedure
 */
