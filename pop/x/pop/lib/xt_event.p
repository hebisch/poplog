/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_event.p
 > Purpose:         XEvent handling routines
 > Author:          Adrian Howard, Aug  9 1990 (see revisions)
 > Documentation:   REF *XT_EVENT
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p
 >                  C.x/x/pop/lib/fast_xt_event.p
 */

compile_mode:pop11 +strict;

section;

uses xpt_typecheck.p;        ;;; Get the type checking routines
uses fast_xt_event.p;       ;;; Get the fast versions of the procedures

include xpt_constants.ph

/*
 * This library contains dummy procedures that are built into the Poplog binary
 * The library should be loaded so users are protected against any movement of
 * the procedures from  the core in future releases.
 *
 * The following identifiers are in the core system:
 *      XptAsyncAppContext
 *      XptSetXtWakeup
 */

;;; Register a new file as an input source for an application - 09/08/90
;;; Input - <AppContext> <Device> <XtPointer> <InputCallbackProc> <XtPointer>
;;; Output - XtInputId
define XtAppAddInput(appcontext, dev, mask, proc, client);
    lvars appcontext, dev, mask, proc, client;
    fast_XtAppAddInput( XptCheckAppContext(appcontext),
                        XptCheckDevice(dev),
                        XptCheckInt(mask),
                        if XptIsValidCallback(proc) then
                            XptExportInput(proc,client, false)
                        else
                            XptCheckProcedure(proc),
                            client
                        endif
                      );
enddefine;


;;; Discontinue a source of input - 09/08/90
;;; Input - <XtInputId>
define XtRemoveInput() with_nargs 1;
    fast_XtRemoveInput(XptTypeCheck(XDT_INPUTID));
enddefine;


;;; Creation of a timeout value - 09/08/90
;;; Input - <AppContext> <ulong> <XtTimerCallbackProc> <XtPointer>
;;; Output - <XtIntervalId>
define XtAppAddTimeOut(appcontext, int, proc, client);
    lvars appcontext, int, proc, client;
    fast_XtAppAddTimeOut(   XptCheckAppContext(appcontext),
                            XptCheckUnsignedIntegral(int),
                            if XptIsValidCallback(proc) then
                                XptExportTimeOut(proc,client,false)
                            else
                                XptCheckProcedure(proc),
                                client;
                            endif
                        );
enddefine;


;;; Clearing of a timeout value - 09/08/90
;;; Input - <XtIntervalId>
define XtRemoveTimeOut() with_nargs 1;
    fast_XtRemoveTimeOut(XptTypeCheck(XDT_INTERVALID));
enddefine;


;;; Determine any events on the input queue - 09/08/90
;;; Input - <AppContext>, Output - <XtInputMask>
define XtAppPending() with_nargs 1;
    fast_XtAppPending(XptCheckAppContext());
enddefine;


;;; Return value of the head of the input Q, without removing the item from
;;; the Q - 09/08/90
;;; Input - <AppContext> <XEventPtr>, Output - <Bool>
define XtAppPeekEvent(appcontext, xeventptr_return);
    lvars appcontext, xeventptr_return;
    fast_XtAppPeekEvent(    XptCheckAppContext(appcontext),
                            XptCheckXEventPtr(xeventptr_return)
                       );
enddefine;


;;; Return the head of an applications X event Q - 09/08/90
;;; Input - <AppContext> <XEventPtr>
define XtAppNextEvent(appcontext, xeventptr_return);
    lvars appcontext, xeventptr_return;
    fast_XtAppNextEvent(    XptCheckAppContext(appcontext),
                            XptCheckXEventPtr(xeventptr_return)
                       );
enddefine;


;;; Process an event - 09/08/90
;;; Input - <AppContext> <XtInputMask>
define XtAppProcessEvent(appcontext, xtinputmask);
    lvars appcontext, xtinputmask;
    fast_XtAppProcessEvent( XptCheckAppContext(appcontext),
                            XptCheckInputMask(xtinputmask)
                          );
enddefine;


;;; Dispach events - 08/08/90
;;; Input - <XEventPtr>, Output - <BOOL>
define XtDispatchEvent() with_nargs 1;
    fast_XtDispatchEvent(XptCheckXEventPtr());
enddefine;


;;; Main loop to process input from a given application - 09/08/90
;;; Input - <AppContext>
define XtAppMainLoop() with_nargs 1;
    fast_XtAppMainLoop(XptCheckAppContext());
enddefine;


;;; Set the sensitivity state of a widget - 09/08/90
;;; Input - <Widget> <Bool>
define XtSetSensitive(widget, bool);
    lvars widget, bool;
    fast_XtSetSensitive(    XptCheckWidget(widget),
                            bool
                       );
enddefine;


;;; Check the sensitivity state of a widget - 09/08/90
;;; Input - <Widget>, Output - <BOOL>
define XtIsSensitive() with_nargs 1;
    fast_XtIsSensitive(XptCheckWidget());
enddefine;

define XptAppTryEvents(appcon);
    lvars appcon, want_result = false;
    if isboolean(appcon) then ((), appcon) -> (appcon, want_result) endif;
    fast_XptAppTryEvents(XptCheckAppContext(appcon), want_result);
enddefine;

;;; EventHandler code

define XtInsertEventHandler(widget,event_mask, nonmaskable, proc,client_data, pos);
    lvars widget event_mask nonmaskable proc client_data pos;
    fast_XtInsertEventHandler(
            XptCheckWidget(widget),
            XptCheckEventMask(event_mask),
            nonmaskable,
            XptExportEventHandlerCached(proc, client_data, false),
            XptCheckListPosition(pos));
enddefine;

define XtAddEventHandler(widget, event_mask, nonmaskable, proc, client_data);
    lvars widget event_mask nonmaskable proc client_data;
    fast_XtAddEventHandler(
            XptCheckWidget(widget),
            XptCheckEventMask(event_mask),
            nonmaskable,
            XptExportEventHandlerCached(proc, client_data, false)
    );
enddefine;

define XtRemoveEventHandler(widget, event_mask, nonmaskable, proc,client_data);
    lvars widget event_mask nonmaskable proc client_data;
    fast_XtRemoveEventHandler(
            XptCheckWidget(widget),
            XptCheckEventMask(event_mask),
            nonmaskable,
            XptExportEventHandlerCached(proc, client_data, false)
    );
enddefine;


define XtInsertRawEventHandler(widget, event_mask, nonmaskable,
            proc, client_data, pos);
    lvars widget event_mask nonmaskable proc client_data pos;
    fast_XtInsertRawEventHandler(
            XptCheckWidget(widget),
            XptCheckEventMask(event_mask),
            nonmaskable,
            XptExportEventHandlerCached(proc, client_data, false),
            XptCheckListPosition(pos));
enddefine;

define XtAddRawEventHandler(widget, event_mask, nonmaskable,
            proc, client_data);
    lvars widget event_mask nonmaskable proc client_data;
    fast_XtAddRawEventHandler(
            XptCheckWidget(widget),
            XptCheckEventMask(event_mask),
            nonmaskable,
            XptExportEventHandlerCached(proc, client_data, false)
    );
enddefine;

define XtRemoveRawEventHandler(widget, event_mask, nonmaskable,
            proc, client_data);
    lvars widget event_mask nonmaskable proc client_data;
    fast_XtRemoveRawEventHandler(
            XptCheckWidget(widget),
            XptCheckEventMask(event_mask),
            nonmaskable,
            XptExportEventHandlerCached(proc, client_data, false)
    );
enddefine;

define XtBuildEventMask() with_nargs 1;
    fast_XtBuildEventMask(XptCheckWidget())
enddefine;

;;; So uses works OK
constant xt_event= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 21 1994
        Added optional boolean arg to XptAppTryEvents to say return
        boolean 'event processed' result
--- Adrian Howard, Oct 15 1991 : Added reference to -XptSetXtWakeup-
--- Adrian Howard, Oct  3 1991
        o Moved calls of -XptCoerceBoolean- to LIB *FAST_XT_EVENT for
          consistancy with all the other libraries
        o Removed usage of -XptCoerceInput-, -XptCoerceTimeOut-, and
          -XptCoerceEventHandler-
        o Altered to use -XptExportInput-, -XptExportTimeOut-, and
          -XptExportEventHandlerCached-
        o Added reference to -XptAsyncAppContext-
        o Fixed type checking in places
--- Jonathan Meyer, Sep  2 1991 Made event handler proce global
--- Jonathan Meyer, Aug  2 1991
        Removed reference to *XEventPtr
--- Adrian Howard, Jul  5 1991
        - Removed -XptCheckBoolean-, changed to -XptCoerceBoolean-
        - XtInsertEventHandler - added missing arg
--- Jonathan Meyer, Jul  4 1991
        Added EventHandler types
--- Roger Evans, Jun 28 1991
        changes for new callback code and removed 'live' check from
        XtRemoveInput/TimeOut
--- Roger Evans, Nov 19 1990
        added XptAppTryEvents, changed input source to be a device
--- Roger Evans, Nov 11 1990 XptCheckProc -> XptCheckProcedure
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
