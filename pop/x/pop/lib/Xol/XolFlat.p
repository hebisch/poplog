/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/XolFlat.p
 > Purpose:         FlatWidget procedures
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */
compile_mode :pop11 +strict;

include xpt_coretypes.ph;

section;

#_IF XOL_VERSION >= 3000

        ;;; NEW IN OLIT 3.0
XptPopLoadProcedures XolFlat [^^XOL_EXLIBS]
    OlFlatCallAcceptFocus(x,y,z) :XptBoolean,
    OlFlatGetFocusItem(x) :ulong,       ;;; returns a Cardinal
    OlFlatGetItemIndex(x,y,z) :ulong,   ;;; returns a Cardinal
    OlFlatGetItemGeometry(u,v,w,x,y,z) :void,
    OlVaFlatGetValues(...) :void,
    OlVaFlatSetValues(...) :void,
;

#_ENDIF

constant XolFlat = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1993
        macros -> include, procedures in
 */
