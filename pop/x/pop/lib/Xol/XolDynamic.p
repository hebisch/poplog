/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/XolDynamic.p
 > Purpose:         Dynamic callback for Xol
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */
compile_mode :pop11 +strict;

section;

XptPopLoadProcedures XolDynamic [^^XOL_EXLIBS]

#_IF XOL_VERSION <= 2000
        ;;; THESE BECAME OBSOLETE IN ALL VERSIONS AFTER 2.0
    LookupOlColors(x,y) :void,
    OlGetApplicationResources(u,v,w,x,y,z) :void,
    OlDetermineMouseAction(x,y) :exptr,
    OlReplayBtnEvent(x,y,z) :void,
#_ENDIF

    LookupOlInputEvent(v,w,x,y,z) :exptr,
    OlGrabDragPointer(x,y,z) :void,
    OlUngrabDragPointer(x) :void,
    OlDragAndDrop(w,x,y,z) :void,
    OlRegisterDynamicCallback(x,y) :void,
    OlUnregisterDynamicCallback(x,y) :int,
    OlCallDynamicCallbacks() :void,
;

constant XolDynamic = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  6 1993
        macros -> XolDynamic.ph. Replaced this file with procedure load
 */
