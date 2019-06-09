/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/XolManager.p
 > Purpose:         Xol Manager procedures
 > Author:          John Gibson, Apr  6 1993
 > Documentation:   HELP * OPENLOOK
 */
compile_mode :pop11 +strict;

include xpt_coretypes.ph;

section;

XptPopLoadProcedures XolManager [^^XOL_EXLIBS]
    OlCallAcceptFocus(x,y) :short#XptCoerceBoolean,
    OlMoveFocus(x,y,z) :XptWidget,
;

constant XolManager = true;

endsection;
