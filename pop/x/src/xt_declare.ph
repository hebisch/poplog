/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/src/xt_declare.ph
 > Purpose:         Xtoolkit interface declarations
 > Author:          Roger Evans, Apr 28 1988 (see revisions)
 > Documentation:   REF *xtoolkit xt_procs
 */

#_INCLUDE '../../src/syspop.ph'

;;; -- X version control ----------------------------------------------

/*  This macro controls the version of the toolkit spec Poplog is built
    against. Cf XtVersion in xt_impl.ph */
#_IF not(DEF _XtSpecificationRelease)
    ;;; default specification release is 4
    lconstant macro _XtSpecificationRelease = 4;
#_ENDIF

#_INCLUDE '../pop/include/xt_constants.ph'
#_INCLUDE 'xt_impl.ph'


;;; -- public variable declarations -----------------------------------

constant procedure (
        XptAsyncAppContext,
        XptCoerceBoolean,
        XptCoerceString,
        XptCoerceTmpString,
        XptCoerceVarargs,
        XptCoerceXtPointer,
        XptDisplayDevice,
        XptImportApplicationContext,
        XptImportDisplayPtr,
        XptImportInputId,
        XptImportIntervalId,
        XptImportProcedure,
        XptImportWidget,
        XptImportWidgetClass,
        XptImportWidgetClassPtr,
        XptInterruptHandler,
        XptRegister,
        fast_XptAppTryEvents,
        fast_XptCreateApplicationContext,
        fast_XptInterruptHandler,
        fast_XptToolkitInitialize,
        fast_XtAddCallback,
        fast_XtAppAddInput,
        fast_XtAppAddTimeOut,
        fast_XtAppCreateShell,
        fast_XtAppNextEvent,
        fast_XtAppPending,
        fast_XtAppSetFallbackResources,
        fast_XtCallCallbacks,
        fast_XtCallbackExclusive,
        fast_XtCallbackNone,
        fast_XtCallbackNonexclusive,
        fast_XtCallbackPopdown,
        fast_XtCloseDisplay,
        fast_XtCreateApplicationContext,
        fast_XtCreateManagedWidget,
        fast_XtCreatePopupShell,
        fast_XtCreateWidget,
        fast_XtDestroyApplicationContext,
        fast_XtDestroyWidget,
        fast_XtDispatchEvent,
        fast_XtDisplay,
        fast_XtDisplayInitialize,
        fast_XtDisplayToApplicationContext,
        fast_XtGetValues,
        fast_XtInitializeWidgetClass,
        fast_XtIsShell,
        fast_XtMapWidget,
        fast_XtOpenDisplay,
        fast_XtPopdown,
        fast_XtPopup,
        fast_XtPopupSpringLoaded,
        fast_XtRealizeWidget,
        fast_XtRemoveActionHook,
        fast_XtRemoveAllCallbacks,
        fast_XtRemoveCallback,
        fast_XtRemoveInput,
        fast_XtRemoveTimeOut,
        fast_XtSetSensitive,
        fast_XtSetValues,
        fast_XtToolkitInitialize,
        fast_XtUnmapWidget,
        fast_XtUnrealizeWidget,
        fast_XtVaAppCreateShell,
        fast_XtVaCreateManagedWidget,
        fast_XtVaCreatePopupShell,
        fast_XtVaCreateWidget,
        fast_XtWidgetToApplicationContext,
        consXptDescriptor,
        freeXptDescriptor,
        isXptDescriptor,
    ),
    XptDescriptor_key,
;


vars
        XptApplyCallback,
        procedure XptCallbackHandler,
        XptCurrentAppContext,
        XptDefaultAppContext,
        XptDefaultDisplay,
        XptGarbageWidget,
        XptTraceCallback,
        XptWMProtocols,
        raw_XtCallbackNone,
        raw_XtCallbackNonexclusive,
        raw_XtCallbackExclusive,
        raw_XtCallbackPopdown,
        procedure sysxsetup,
        procedure XptToolkitInitialize,
        procedure XptGarbageHook,
;

declare_incremental procedure (sysxsetup, XptToolkitInitialize);


;;; -- Private variable declarations ----------------------------------

section $-Sys$-Xt;


constant procedure (
        Appcon_error,
        CloseDisplay,
        Cons_XptDescriptor,
        Cons_Procedure_rec,
        Cons_Procedure_from_closure,
        Cons_Widget,
        Del_item,
        Descriptor,
        DestroyApplicationContext,
        DestroyPopWidget,
        Get_Descriptor,
        Kill_XptDescriptor,
        Init_appcon,
        Init_error,
        Init_popup,
        Init_widget,
        ImportApplicationContextDesc,
        ImportWidget,
        ImportWidgetClassDesc,
        Register,
        X_apply,
        XTC_bool,
        XTC_str,
        XTC_generic,
        XptGarbageHandler,
    ),
    DescriptorProp,
    XptDescriptor_key,
;

endsection;

;;; -- XptDescriptor declarations -------------------------------------

;;; include public library defining XDT_* type constants
#_INCLUDE '../pop/include/xpt_constants.ph'

struct XDESCRIPTOR
    {   full    XP_PROPS,
                KEY;
>->     <word>  XP_PTR;
        full    XD_DEPENDENTS,
                XD_PREFERRED;
    };

;;;  -- descriptor types and dependency formats --------------------

;;; type XDT_WIDGETCLASS
;;; XD_DEPENDENTS is the parent widgetclass descriptor

;;; type XDT_APPCONTEXT
struct XD_AC_DEP
    {   word    V_LENGTH;
        full    KEY;
    >-> full    XD_AC_DISPLAYS,     ;;; list of attached displays
                XD_AC_INPUTS,       ;;; list of attached InputIds
                XD_AC_TIMEOUTS,     ;;; list of attached IntervalIds
                XD_AC_ACTIONS,      ;;; list of attached actions
                XD_AC_ACTIONHOOKS,  ;;; list of attached actionhooks
                XD_AC_ASYNC;        ;;; false or a closure
    };
lconstant XD_AC_DEP_LEN = 6;

;;; type XDT_WIDGET
struct XD_W_DEP
    {   word    V_LENGTH;
        full    KEY;
    >-> full    XD_W_PARENT,        ;;; parent widget
                XD_W_CHILDREN,      ;;; list of attached children
                XD_W_CALLBACKS,     ;;; list of attached callbacks
                XD_W_CLASS,         ;;; widgetclass record
                XD_W_SHELL,         ;;; flag for shell widgets
                XD_W_EVENTHANDLERS; ;;; list of attached event handlers
    };
lconstant XD_W_DEP_LEN = 6;

;;; type XDT_DISPLAY
struct XD_D_DEP
    {   word    V_LENGTH;
        full    KEY;
    >-> full    XD_D_APPCON;        ;;; application context
    };

lconstant XD_D_DEP_LEN = 1;

;;; type XDT_INPUTID
struct XD_I_DEP
    {   word    V_LENGTH;
        full    KEY;
    >-> full    XD_I_DEVICE,        ;;; device used as source arg
                XD_I_COND,          ;;; cond arg
                XD_I_PROC,          ;;; callback procedure
                XD_I_CLIENT,        ;;; client arg to callback
                XD_I_APPCON;        ;;; application context
    };

lconstant XD_I_DEP_LEN = 5;

;;; type XDT_INTERVALID
struct XD_T_DEP
    {   word    V_LENGTH;
        full    KEY;
    >-> full    XD_T_PROC,          ;;; callback procedure
                XD_T_CLIENT,        ;;; client arg to callback
                XD_T_APPCON;        ;;; application context
    };

lconstant XD_T_DEP_LEN = 3;

;;; type XDT_ACTIONHOOK
struct XD_AH_DEP
    {   word    V_LENGTH;
        full    KEY;
    >-> full    XD_AH_PROC,         ;;; callback procedure
                XD_AH_CLIENT,       ;;; client arg to callback
                XD_AH_APPCON;       ;;; application context
    };

lconstant XD_AH_DEP_LEN = 3;

;;; type XDT_PROCEDURE
;;; XD_DEPENDENTS is exfunc_closure record

;;; this constant must be at least the largest vector used
;;; as dependency data (currently APPCONTEXT)
lconstant XD_FREELIST_TABLE_SIZE = XD_AC_DEP_LEN;

define :inline lconstant XTC_VARARGS(other);
    lblock lvars n = (); 0, n fi_+ #_< other+1 >_# endlblock
enddefine;

define :inline lconstant EXTERN_PTR(routine);
    #_< struct EXTERNAL_PTR =>>
                {% false, external_ptr_key, _extern routine %} >_#
enddefine;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Nov 10 1997
        Added declaration for fast_XtIsShell
--- John Gibson, May  4 1993
        Added incremental declaration for XptToolkitInitialize
--- John Gibson, Apr 13 1993
        Removed declarations for (fast_)XptDataType/Props -- these are
        now in src/xt_support.p.
--- John Gibson, Mar 29 1993
        Added EXTERN_PTR macro
--- John Gibson, Dec 11 1992
        Added include for src/syspop.ph at top (and removed includes for
        src/declare.ph from all x/src/ *.p). Removed declarations in
        'common symbols imported from elsewhere' section since all
        such declarations now extracted automatically from src/src.wlb.
--- Adrian Howard, Nov 27 1992
        Added DestroyPopWidget & DescriptorProp
--- John Gibson, Sep  6 1992
        Added XTC_VARARGS
--- Adrian Howard, Jun 12 1992
        Added -Sys_process_destroy_action- declaration
--- Adrian Howard, Oct  2 1991 : Added XD_W_EVENTHANDLERS to widget
        dependancies
--- Jonathan Meyer, Sep  6 1991 Added Xpt_sys_pr_message
--- Jonathan Meyer, Jul 31 1991
        Renamed XptWm -> XptWM
--- Roger Evans, Jun 28 1991
        removed old callback declarations
        added freelist stuff
--- Roger Evans, Jun 25 1991 added Init_widget
--- Roger Evans, Jun 19 1991 Added USER_CALLBACK_DATA
--- Roger Evans, May 29 1991
        added XD_W_SHELL field to widget descriptors and XptWmProtocols
--- Roger Evans, May 26 1991
        Added declarations for IO error handling
--- John Gibson, Mar 13 1991
        Added XD_AC_ASYNC field and some declarations
--- Roger Evans, Jan 30 1991 added RegisterDesc
--- John Gibson, Jan 24 1991
        Added include for xt_constants.ph, removed include for external.ph
--- Roger Evans, Dec  7 1990 added XptDescriptor,
        fast_XtDisplayToApplicationContext. Removed XptDisplayAppContext
--- Roger Evans, Nov 21 1990 added Init_appcon
--- Roger Evans, Nov 19 1990
        Reinstalled version from OCt 20 as master, after master got corrupted.
        Hope its not too far out of date!
--- Roger Evans, Oct 19 1990 added more XptCoerce routines
--- Roger Evans, Oct 11 1990 Much revised
--- Roger Evans, Jul  4 1990 added X_apply
--- Roger Evans, Jun 26 1990 added XptInterruptHandler
--- Roger Evans, Jun 23 1990
    changed external_ptr_to_widgetclass to XptImportWidgetclass
    removed old widgetclass routines
--- Roger Evans, Jun 22 1990 added new widgetclass declarations
--- Roger Evans, Jun 21 1990 added more XptDescriptor declarations
--- Simon Nichols, May 31 1990
        Changed type of XP_PTR in struct XDESCRIPTOR from int to (word), to
        be consistent with declaration in syscomp/symdefs.p.
--- Roger Evans, May 24 1990 added XDESCRIPTOR struct
--- Roger Evans, May  4 1990 added XtVersion
--- Roger Evans, May  3 1990 changed default XT_VERSION to 4 (was 3)
--- Ian Rogers, Mar 25 1990
        (Roger Evans) Removed pointer base in RAWCALLBACK struct
--- Ian Rogers, Jan 12 1990
    Changes for new pointers
 */
