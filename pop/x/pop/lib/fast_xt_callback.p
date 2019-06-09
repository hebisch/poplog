/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_callback.p
 > Purpose:         non-checking support for Xt callback functions
 > Author:          Adrian Howard, Aug  9 1990 (see revisions)
 > Documentation:   REF *XT_CALLBACK
 > Related Files:   C.x/x/pop/lib/xt_callback.p
 */

compile_mode:pop11 +strict;

section;

/*
 * The following identifiers are in the core system:
 *                      fast_XtAddCallback,
 *                      fast_XtAddCallbacks,
 *                      fast_XptAddCallbackList,
 *                      fast_XtRemoveCallback,
 *                      fast_XtRemoveCallbacks,
 *                      fast_XptRemoveCallbackList,
 *                      fast_XtRemoveAllCallbacks,
 *                      fast_XtHasCallbacks,
 *                      fast_XtCallCallbacks,
 */

include xpt_coretypes.ph;


/*  standard wrapper procedure for pop callback functions */
define global XptCallbackWrapper(extdata, proc, client_data, conv_p);
    lvars extdata, proc, client_data, conv_p, stacked_items;

    l_typespec extdata {
        widget  :XptWidget,
                :XptPointer,        ;;; don't use toolkit client data
        call    :exval
    };

    #|  ;;; stack args for callback
        exacc [fast] extdata.widget;
        client_data;                    ;;; don't use toolkit client data

        if conv_p then
            ;;; run conversion procedure, passing it a fixed (i.e. constant)
            ;;; pointer
            conv_p(exacc [fast,nc] extdata.call)
        else
            ;;; pass a newly-created exptr
            exacc [fast] extdata.call
        endif;

        ;;; dp call via standard callback handler
        XptCallbackHandler(proc, "callback");
    |# -> stacked_items;
    unless stacked_items == 0 then
        warning(
            proc, stacked_items+1,
            'ITEMS ON STACK AFTER CALLBACK BEING IGNORED'
        );
    endunless;
enddefine;


;;; Cache indexing closures of -XptCallbackWrapper- onto external procedures
lconstant callback_cache =
    newanyproperty( [], 50, false, false,
                    syshash, sys_=, "tmpval",
                    false, false
    );


;;; Instance of a closure of -XptCallbackWrapper- of the type used to index
;;; -callback_cache-
;;; (Because it's a closure, MUST use writeable here for sys_lock_system)
lconstant XptCallbackWrapperInstance
                = writeable XptCallbackWrapper(%false, false, false%);


define global XptExportTypedCallback(proc, arg, conv_p, hold)
                                                -> (ext_proc, ext_val);
    lvars proc, arg, conv_p, hold, ext_proc;
    lconstant ext_val = false;  ;;; Do not use Xt client data

    if XptIsValidCallback(proc) then
        exfunc_export(
            XptCallbackWrapper(%proc, arg, conv_p%),
            XptCallbackFlags,
            hold
        ) -> ext_proc;
    else
        mishap(proc, 1, 'PROCEDURE, IDENT, OR WORD NEEDED');
    endif;
enddefine;

define global XptExportCallback(/*proc,arg*/ hold) with_nargs 3;
    lvars hold;
    XptExportTypedCallback(false, hold);
enddefine;


define global XptExportTypedCallbackCached(proc, arg, conv_p, hold)
                                                -> (ext_proc, ext_val);
    lvars proc, arg, conv_p, hold, ext_proc;
    lconstant ext_val = false;  ;;; Do not use Xt client data
    lvars callback_wrapper;

    (proc, arg, conv_p) -> explode(XptCallbackWrapperInstance);

    unless (callback_cache(XptCallbackWrapperInstance) ->> ext_proc) do;
        if XptIsValidCallback(proc) then
            copy(XptCallbackWrapperInstance) -> callback_wrapper;
            exfunc_export(callback_wrapper, XptCallbackFlags, hold)
                ->> callback_cache(callback_wrapper) -> ext_proc;
        else
            mishap(proc, 1, 'PROCEDURE, IDENT, OR WORD NEEDED');
        endif;
    endunless;

enddefine;

define global XptExportCallbackCached(/*proc,arg*/ hold) with_nargs 3;
    lvars hold;
    XptExportTypedCallbackCached(false, hold);
enddefine;

constant fast_xt_callback = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 23 1994
        Changed XptCallbackWrapper so that when given a conversion procedure
        to extract the call data, the procedure is passed a constant exptr
        instead of creating a new one every time
--- John Gibson, Nov  3 1991
        includes xpt_coretypes.ph
--- Adrian Howard, Aug 30 1991 : The wrapper now checks the user does not
        stack items in the callback procedure.
--- John Gibson, Aug 24 1991
        XptExport(Typed)Callback(Cached) now all take extra boolean -hold-
        arg to pass to exfunc_export.
        Also changed type of callback_cache property to "tmpval" (otherwise
        entries will always be removed at next GC).
--- Adrian Howard, Aug 12 1991
        - Renamed XptExport(Typed)Callback to XptExport(Typed)CallbackCached
        - Added non-caching export procedures -XptExportCallback- &
          -XptExportTypedCallback-
--- John Gibson, Aug  9 1991
        Made XptCallbackWrapperInstance writeable, changed to lconstant
        and changed assignments to frozvals to use -> explode rather
        than frozval 3 times
--- Adrian Howard, Aug  9 1991
        - Changed XptCoerce(Typed)Callback to XptExport(Typed)Callback
        - Added callback caching
--- Roger Evans, Jun 27 1991
        removed numargs args from exfunc_export and XptCallbackHandler
--- Jonathan Meyer, Jun 27 1991
    Corrected order of proc, arg for XptCoerceTypedCallback
--- Roger Evans, Jun 25 1991 removed 'uses xpt_cb_utils'
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Jonathan Meyer, Jun 24 1991
        Added XptCoerceTypedCallback, rewrote XptCoerceCallback to take
        2 args.
--- Roger Evans, Nov 18 1990 tidied up
--- Roger Evans, Oct 11 1990 reinstalled in masters
--- Adrian Howard, Sep 13 1990 : Added reference to fast_XtRemoveAllCallbacks
--- Adrian Howard, Sep  7 1990 : Added vars so uses works ok
 */
