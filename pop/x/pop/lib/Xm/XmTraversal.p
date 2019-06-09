/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/XmTraversal.p
 > Purpose:
 > Author:          Jonathan Meyer, Feb  9 1991 (see revisions)
 > Documentation:   HELP *MOTIF
 */
compile_mode :pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;

/* This is not of much use except to widget implementors */

XptPopLoadProcedures XmTraversal [^^XM_EXLIBS]
    _XmCreateFocusData() :exptr,
    _XmDestroyFocusData(x) :void,
    _XmSetActiveTabGroup(x,y) :void,
    _XmGetActiveItem(x) :XptWidget,
    _XmNavigInitialize(w,x,y,z) :void,
    _XmChangeNavigationType(x,y) :XptBoolean,
    _XmNavigSetValues(v,w,x,y,z) :XptBoolean,
    _XmNavigDestroy(x) :void,
    _XmCallFocusMoved(x,y,z) :XptBoolean,
    _XmPrimitiveEnter(x,y) :void,
    _XmPrimitiveLeave(x,y) :void,
    _XmPrimitiveUnmap(x,y) :void,
    _XmPrimitiveFocusInInternal(x,y) :void,
    _XmPrimitiveFocusOut(x,y) :void,
    _XmPrimitiveFocusIn(x,y) :void,
    _XmManagerEnter(x,y) :void,
    _XmManagerFocusInInternal(x,y) :void,
    _XmManagerFocusIn(x,y) :void,
    _XmManagerFocusOut(x,y) :void,
    _XmManagerUnmap(x,y) :void,
    _XmMgrTraversal(x,y) :XptBoolean,
    _XmClearKbdFocus(x) :void,
    _XmClearFocusPath(x) :void,
    _XmFindTraversablePrim(x) :XptBoolean,
    _XmTestTraversability(x,y) :XptBoolean,
    _XmFocusIsHere(x) :XptBoolean,
    _XmProcessTraversal(x,y,z) :void,
    _XmFindNextTabGroup(x) :XptWidget,
    _XmFindPrevTabGroup(x) :XptWidget,
    _XmGetFocusPolicy(x) :byte,
    _XmClearTabGroup(x) :void,
    _XmFindTabGroup(x) :XptWidget,
    _XmGetTabGroup(x) :XptWidget,
    _XmFindTopMostShell(x) :XptWidget,
    _XmFocusModelChanged(x,y,z) :void,
    _XmGrabTheFocus(x,y) :XptBoolean,
    _XmGetFocusData(x) :void,
    _XmGetManagedInfo(x) :XptBoolean,
    _XmCreateVisibilityRect(x,y) :XptBoolean,
    _XmSetRect(x,y) :int,
    _XmIntersectRect(x,y,z) :int,
    _XmEmptyRect(x) :int,
    _XmClearRect(x) :int,
;

constant XmTraversal = true;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        typespecs/macros -> XmMisc.ph
--- Andreas Schoter, Sep 10 1991
    Fixed _XmSetRect return type from ont to int
--- Andreas Schoter, Jul 15 1991
    Added global constant XmTraversal for compatibility with uses
 */
