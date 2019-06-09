/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_event.p
 > Purpose:         non-checking support for Xt event routines
 > Author:          Adrian Howard, Aug  9 1990 (see revisions)
 > Documentation:   REF *XT_EVENT
 > Related Files:   C.x/x/pop/lib/xt_event.p
 */

compile_mode:pop11 +strict;

section;

/*
 * The following identifiers are in the core system:
 *                      fast_XptAppTryEvents
 *                      fast_XtAppAddInput
 *                      fast_XtAppAddTimeOut
 *                      fast_XtAppMainLoop
 *                      fast_XtAppNextEvent
 *                      fast_XtAppPeekEvent
 *                      fast_XtAppPending
 *                      fast_XtAppProcessEvent
 *                      fast_XtDispatchEvent
 *                      fast_XtIsSensitive
 *                      fast_XtRemoveInput
 *                      fast_XtRemoveTimeOut
 *                      fast_XtSetSensitive
 *                      fast_XtInsertEventHandler
 *                      fast_XtAddEventHandler
 *                      fast_XtRemoveEventHandler
 *                      fast_XtInsertRawEventHandler
 *                      fast_XtAddRawEventHandler
 *                      fast_XtRemoveRawEventHandler
 */

include xpt_coretypes.ph;
include xpt_xtypes.ph;


/*  standard wrapper procedure for pop input functions */
define global XptInputWrapper(extdata,proc,client_data);
    lvars extdata proc client_data id stacked_items;

    l_typespec extdata {
                :XptPointer,        ;;; don't use toolkit client data
                :exptr.:int,        ;;; or source data
        id      :exptr.:XptInputId,
    };

    exacc [fast] extdata.id -> id;

    #|
        ;;; stack args for callback
        client_data;                    ;;; use own client data
        XptInputIdToDevice(id);         ;;; fetch device
        id;
        ;;; do call via standard callback handler
        XptCallbackHandler(proc,"input");
    |# -> stacked_items;

    unless stacked_items == 0 do;
        warning(
            proc, stacked_items+1,
            'ITEMS ON STACK AFTER INPUT BEING IGNORED'
        );
    endunless;

enddefine;


/*  standard wrapper procedure for pop timeout functions */
define global XptTimeOutWrapper(extdata,proc,client_data);
    lvars extdata id proc client_data stacked_items;

    l_typespec extdata {
                :XptPointer,        ;;; don't use toolkit client data
        id      :exptr.:XptIntervalId,
    };

    exacc [fast] extdata.id -> id;

    #|
        ;;; stack args for callback
        client_data;                    ;;; don't use toolkit client data
        id;
        ;;; do call via standard callback handler
        XptCallbackHandler(proc,"timeout");
    |# -> stacked_items;

    fast_XtRemoveTimeOut(id);   ;;; So descriptor dependencies are correct

    unless stacked_items == 0 do;
        warning(
            proc, stacked_items+1,
            'ITEMS ON STACK AFTER TIMEOUT BEING IGNORED'
        );
    endunless;

enddefine;


/* standard wrapper procedure for pop event handler functions */
define global XptEventHandlerWrapper(extdata,proc,client_data);
    lvars extdata proc client_data stacked_items;

    l_typespec extdata {
        widget  :XptWidget,
                :XptPointer, ;;; unused
        event   :XptXEventPtr,
        contin  :exptr.:XptBoolean,
    };

    #|
        exacc [fast] extdata.widget;
        client_data;
        exacc [fast] extdata.event;

        ;;; do call via standard callback handler
        XptCallbackHandler(proc,"event")
    |# -> stacked_items;

    if stacked_items == 1 then
        /* STACKED BOOL */ -> exacc [fast] extdata.contin;
    elseif stacked_items > 1 do
        warning(
            proc, stacked_items+1,
            'EXTRA ITEMS ON STACK AFTER EVENT HANDLER BEING IGNORED'
        );
    endif;

enddefine;


;;; Cache, indexing closures of wrappers onto external procedures
lconstant callback_cache =
    newanyproperty( [], 50, false, false,
                    syshash, sys_=, "tmpval",
                    false, false
    );


;;; Instances of the closures used to index -callback_cache-
lconstant XptInputWrapperInstance =
    writeable XptInputWrapper(%false, false%);
lconstant XptTimeOutWrapperInstance =
    writeable XptTimeOutWrapper(%false, false%);
lconstant XptEventHandlerWrapperInstance =
    writeable XptEventHandlerWrapper(%false, false%);


lconstant do_cached_export=
    procedure(proc, client_data, hold, wrapper_instance) -> (efc, exarg);
        lvars proc, client_data, hold, wrapper_instance, efc;
        lconstant exarg = null_external_ptr;    ;;; Never use external arg
        lvars callback_wrapper;
        (proc, client_data) -> explode(wrapper_instance);
        unless (callback_cache(wrapper_instance) ->> efc) do;
            if XptIsValidCallback(proc) then
                copy(wrapper_instance) -> callback_wrapper;
                exfunc_export(callback_wrapper, XptCallbackFlags, hold)
                    ->> callback_cache(callback_wrapper) -> efc;
            else
                mishap(proc, 1, 'PROCEDURE, IDENT, OR WORD NEEDED');
            endif;
        endunless;
    endprocedure;
;;;
define global XptExportInputCached() with_nargs 3;
    do_cached_export(XptInputWrapperInstance);
enddefine;
;;;
define global XptExportTimeOutCached() with_nargs 3;
    do_cached_export(XptTimeOutWrapperInstance);
enddefine;
;;;
define global XptExportEventHandlerCached() with_nargs 3;
    do_cached_export(XptEventHandlerWrapperInstance);
enddefine;


lconstant do_export =
    procedure(proc, data, hold, wrapper); /* -> (EFC, FALSE) */
        lvars proc, data, hold, wrapper;
        if XptIsValidCallback(proc) then
            exfunc_export(wrapper(%proc, data%), XptCallbackFlags, hold);
            null_external_ptr;
        else
            mishap(proc, 1, 'WORD, IDENT, OR PROCEDURE NEEDED');
        endif;
    endprocedure;
;;;
define global XptExportInput() with_nargs 3;
    do_export(XptInputWrapper);
enddefine;
;;;
define global XptExportTimeOut() with_nargs 3;
    do_export(XptTimeOutWrapper);
enddefine;
;;;
define global XptExportEventHandler() with_nargs 3;
    do_export(XptEventHandlerWrapper);
enddefine;


XptLoadProcedures fast_xt_event lvars XtBuildEventMask;

define global fast_XtBuildEventMask(/*widget*/) with_nargs 1;
    exacc [fast] (1):uint raw_XtBuildEventMask()
enddefine;

;;; So uses works OK
constant fast_xt_event= true;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 28 1992
        Added missing "global" on XptExportEventHandlerCached etc.
--- John Gibson, Nov  3 1991
        includes xpt_coretypes.ph
--- Adrian Howard, Oct 18 1991
        o Removed IntervalId *after* call to timeout procedure, so the
          TimeOut gets passed a reasonable value.
        o Altered -XptEventHandlerWrapper- so an EventHandler that does not
          return a result is assumed to allow further event processing
--- Adrian Howard, Oct 15 1991 : Export routines for EventHandlers now return
        a constant external pointer for the client data, so multiple
        registration works properly
--- Adrian Howard, Oct  3 1991
        o Moved "fast" event handlers into C.x/x/src/xt_event.p
        o -XptXEventHandlerWrapper- now passes an -XptXEventPtr-
        o Added calls to -XptCoerceBoolean- in the event handler code
        o Removed -XptCoerceInput-, -XptCoerceTimeOut-, and
          -XptCoerceEventHandler-
        o Added -XptExportInput(Cached)-, -XptExportTimeOut(Cached)-, and
          -XptExportEventHandler(Cached)-
        o Added checks for stacked items after external callbacks
        o Removed reference to -XptAsyncAppContext-
--- Jonathan Meyer, Sep  2 1991 Made event handler type procs global
--- Jonathan Meyer, Jul  4 1991
        Added event handler type
--- Roger Evans, Jun 28 1991 added callback wrapper code
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Adrian Howard, Jun 21 1991 : Added XptAsyncAppContext to list
--- Roger Evans, Nov 18 1990 tidied up
--- Adrian Howard, Sep  7 1990 : Added var so uses works
 */
