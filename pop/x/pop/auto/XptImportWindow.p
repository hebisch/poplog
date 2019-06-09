/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptImportWindow.p
 > Purpose:         Importing Window structs
 > Author:          Roger Evans, Oct 20 1990 (see revisions)
 > Documentation:   REF *XPT_COERCE
 > Related Files:
 */

compile_mode:pop11 +strict;

section;

include xpt_constants.ph;   ;;; Load constants in

define global XptImportWindow
        = XptImportAny(%XDT_WINDOW%)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1991
        Moved XptWindow typespec definition to xpt_xtypes.ph
--- Roger Evans, Feb  3 1991 renamed XptGetDescriptor -> XptImportAny
--- Jonathan Meyer, Dec  5 1990 changed to use XptGetDescriptor
--- Roger Evans, Nov 19 1990 changed to use consXptDescriptor
--- Roger Evans, Nov 15 1990 added typespec definition
--- Adrian Howard, Oct 26 1990 : Fixed bug in updater
--- Adrian Howard, Oct 25 1990 : Fixed bug which allowed the window to be
        altered after creation.
 */
