/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/Xolgeneral.p
 > Purpose:         Xol general procedures and startup
 > Author:          John Gibson, Apr  6 1993 (see revisions)
 > Documentation:   HELP * OPENLOOK
 */
compile_mode :pop11 +strict;

include xpt_xtypes.ph;
include xpt_constants.ph;

section;
exload_batch;

XptPopLoadProcedures Xolgeneral [^^XOL_EXLIBS]

#_IF XOL_VERSION >= 3000
    OlActivateWidget(x,y,z) :XptBoolean,
    OlAssociateWidget(x,y,z) :XptBoolean,
    OlUnassociateWidget(x) :void,
    OlClassSearchIEDB(x,y) :void,
    OlClassSearchTextDB(x) :void,
    OlCreateInputEventDB(v,w,x,y,z) :exptr,
    OlLookupInputEvent(w,x,y,z) :exptr,
    OlColormapOfObject(x) :ulong,
    OlDepthOfObject(x) :int,
    OlInternAtom(x,y) :ulong,
    OlVisualOfObject(x) :exptr,
    OlQueryMnemonicDisplay(x) :short,       ;;; returns an OlDefine
    OlQueryAcceleratorDisplay(x) :short,    ;;; returns an OlDefine
    OlSetInputFocus(x,y,z) :XptBoolean,
    OlGetCurrentFocusWidget(x) :XptWidget,
    OlToolkitInitialize(x) :void,
    OlGetApplicationValues(x,y,z) :void,
    OlUpdateDisplay(x) :void,
    OlWMProtocolAction(x,y,z) :void,

        ;;; DRAG AND DROP
    OlDnDRegisterWidgetDropSite(s,t,u,v,w,x,y,z) :exptr,
    OlDnDRegisterWindowDropSite(r,s,t,u,v,w,x,y,z) :exptr,
    OlDnDUpdateDropSiteGeometry(x,y,z) :XptBoolean,
    OlDnDChangeDropSitePreviewHints(x,y) :XptBoolean,
    OlDnDQueryDropSiteInfo(t,u,v,w,x,y,z) :XptBoolean,
    OlDnDSetDropSiteInterest(x,y) :XptBoolean,
    OlDnDSetInterestInWidgetHier(x,y) :void,
    OlDnDDestroyDropSite(x) :void,
    OlDnDInitializeDragState(x) :XptBoolean,
    OlDnDClearDragState(x) :void,
    OlDnDPreviewAndAnimate(t,u,v,w,x,y,z) :XptBoolean,
    OlDnDDeliverPreviewMessage(v,w,x,y,z) :XptBoolean,
    OlDnDDeliverTriggerMessage(t,u,v,w,x,y,z) :XptBoolean,
    OlDnDDragAndDrop(t,u,v,w,x,y,z) :XptBoolean,
    OlDnDAllocTransientAtom(x) :ulong,      ;;; returns an Atom
    OlDnDFreeTransientAtom(x,y) :void,
    OlDnDBeginSelectionTransaction(v,w,x,y,z) :void,
    OlDnDEndSelectionTransaction(v,w,x,y,z) :void,
    OlDnDDragNDropDone(v,w,x,y,z) :void,
    OlDnDErrorDuringSelectionTransaction(v,w,x,y,z) :void,
    OlDnDGetWidgetOfDropSite(x) :XptWidget,
    OlDnDGetWindowOfDropSite(x) :XptWindow,
    OlDnDGetDropSitesOfWidget(x,y) :exptr,
    OlDnDGetDropSitesOfWindow(x,y,z) :exptr,
    OlDnDGetCurrentSelectionsForWidget(x,y,z) :XptBoolean,
    OlDnDOwnSelection(s,t,u,v,w,x,y,z) :XptBoolean,
    OlDnDOwnSelectionIncremental(r,s,t,u,v,w,x,y,z) :XptBoolean,
    OlDnDDisownSelection(x,y,z) :void,
    OlDnDWidgetConfiguredInHier(x) :void,
#_ELSE
    OlInitialize(u,v,w,x,y,z) :XptWidget,
#_ENDIF

    OlCanAcceptFocus(x,y) :XptBoolean,
    OlCreatePackedWidget(x) :XptWidget,
    OlCreatePackedWidgetList(x,y) :XptWidget,
    OlError(x) :void,
    OlFlatGetValues(w,x,y,z) :void,
    OlFlatSetValues(w,x,y,z) :void,
    OlGetResolution(x) :byte,
    OlHasFocus(x) :short#XptCoerceBoolean,
    OlPostInitialize(v,w,x,y,z) :void,
    OlPreInitialize(v,w,x,y,z) :void,
    OlRegisterHelp(v,w,x,y,z) :void,
    OlSetVaDisplayErrorMsgHandler(x,y) :void,
    OlSetVaDisplayWarningMsgHandler(x,y) :void,
    OlSetErrorHandler(x) :void,
    OlSetWarningHandler(x) :void,
    OlVaDisplayErrorMsg(...) :void,
    OlVaDisplayWarningMsg(...) :void,
    OlWarning(x) :void,
;


/**************************************************************************
 * SETUP CODE FOR OPEN LOOK
 **************************************************************************/

lconstant macro XSETUPDONE = pop_runtime == "x";

vars
    XpolTopLevelShell   = false,
    XpolAppContext      = false,
;


#_IF XOL_VERSION < 3000

exload 'XolStartupSymbols' [^^XOL_EXLIBS]
    lvars
        raw_PopXtError <- PopXtError,
        raw_PopXtWarning <- PopXtWarning;
endexload;

;;; Open Look Startup Code (see XptDefaultSetup)

lvars XpolSetupDone = false, XpolInitDone = false;

;;; basic startup code - this does not overwrite any startup variables
;;; set by XptDefaultSetup if it has already been called. This allows us
;;; to perform an Ol startup on top of an XptDefaultSetup.

define lconstant XpolInitialize;
    lvars name, argc, argv;
    returnif(XpolInitDone);
    sys_fname_nam(poparg0) -> name;

    XptSetArgv(popunderx) -> (argc, argv);
    OlInitialize((name ->XptCoerceString()), XT_POPLOG_CLASSNAME,
                         0, 0,
                         argc, argv,
                        ) -> XpolTopLevelShell;
    fast_XptSetUnprocessedArgs(argc, argv);

    ;;; set app context
    fast_XtWidgetToApplicationContext(XpolTopLevelShell) -> XpolAppContext;
    XT_POPLOG_CLASSNAME -> XptDataProps(XpolAppContext);
    XpolAppContext -> XptCurrentAppContext;

    ;;; set Ol error handlers
    OlSetErrorHandler(raw_PopXtError);
    OlSetWarningHandler(raw_PopXtWarning);
    true -> XpolInitDone;
enddefine;

;;; global startup code - this is a parallel to XptDefaultSetup, but
;;; works for OpenLook.

define XpolDefaultSetup;
    lvars name;
    returnif(XpolSetupDone);

    unless isprocedure(OlInitialize) then
        ;;; might be in a load batch
        exload_do_batch_load();
        unless isprocedure(OlInitialize) then
            mishap(0,'CANNOT FIND OPEN LOOK');
        endunless;
    endunless;

    ;;; initialize OpenLook
    unless XptDefaultDisplay then
        ;;; call toolkit initialization
        fast_XptCustomInitialize(XpolInitialize);
    else
        ;;; just call the external procedure directly
        XpolInitialize();
    endunless;

    unless XpolTopLevelShell then
        mishap(0,'OpenLook SETUP FAILED (toolkit already initialised?)');
    endunless;

    ;;; set appcontext
    XpolAppContext -> XptDefaultAppContext;

    ;;; make it async
    true -> XptAsyncAppContext(XptDefaultAppContext);

    ;;; set display
    fast_XtDisplay(XpolTopLevelShell) -> XptDefaultDisplay;

    true -> XpolSetupDone;
enddefine;

sysunprotect("XptDefaultSetup");
XpolDefaultSetup -> XptDefaultSetup;
sysprotect("XptDefaultSetup");

#_IF XSETUPDONE
    ;;; run startup now
    warning(0,'X TOOLKIT SETUP ALREADY DONE (SEE HELP *OPENLOOK)');
    exload_do_batch_load();
    XpolDefaultSetup();
#_ENDIF

#_ELSE /* ie. XOL_VERSION >= 3000, post OLIT 3.0 */

#_IF XSETUPDONE
    exload_do_batch_load();
    OlToolkitInitialize(null_external_ptr);
#_ELSE
    ;;; make this come first in XptToolkitInitialize
    declare_incremental procedure[prec= -100]  XptToolkitInitialize;
    sysunprotect("XptToolkitInitialize");
    OlToolkitInitialize(%null_external_ptr%)
                    <> XptToolkitInitialize -> XptToolkitInitialize;
    sysprotect("XptToolkitInitialize");
#_ENDIF

define XpolDefaultSetup(); XptDefaultSetup() enddefine;

#_ENDIF /* XOL_VERSION < 3000 */


constant macro OPENLOOK = true;


constant Xolgeneral = true;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Apr 27 1993
        Added include xpt_constants for XT_POPLOG_CLASSNAME
 */
