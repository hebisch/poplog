/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_action.p
 > Purpose:         non-checking support for Xt action functions
 > Author:          Adrian Howard, Aug  9 1990 (see revisions)
 > Documentation:   REF *XT_ACTION
 > Related Files:   C.x/x/pop/lib/xt_action.p
 */

compile_mode:pop11 +strict;

section;

/*
 * The following identifiers are in the core system:
 *                      fast_XtAppAddActions
 *                      fast_XptAppAddActionList
 *                      fast_XtAppAddActionHook
 *                      fast_XtRemoveActionHook
 *                      fast_XtCallActionProc
 */


include xpt_coretypes.ph;
include xpt_xtypes.ph;      ;;; For -XptXEventPtr


/*  standard wrapper procedure for pop action functions */
define global XptActionWrapper(extdata,proc);
    lvars extdata proc event params num_params, stacked_items;
    l_typespec extdata {
        widget      :XptWidget,
        event       :XptXEventPtr,
        params      :exptr.:XptString[],
        num_params  :exptr.:XptCardinal,
    };

    #|
        ;;; stack args for callback
        exacc [fast] extdata.widget;
        exacc [fast] extdata.event;
        exacc [fast] extdata.params;
        exacc [fast] extdata.num_params;

        ;;; do call via standard callback handler
        XptCallbackHandler(proc,"action");
    |# -> stacked_items;
    unless stacked_items == 0 do
        warning(
            proc, stacked_items+1,
            'ITEMS ON STACK AFTER ACTION PROCEDURE BEING IGNORED'
        );
    endunless;

enddefine;


/* standard wrapper for pop actionhook procedures */
define global XptActionHookWrapper(extdata,proc,arg);
    lvars extdata proc event params num_params arg, stacked_items;
    l_typespec extargs {
        widget      :XptWidget,
                    :XptPointer,    ;;; don't use client arg
        name        :XptString,
        event       :XptXEventPtr,
        params      :exptr.:XptString[],
        num_params  :exptr.:XptCardinal,
    };

    #|
        ;;; stack args for callback
        exacc [fast] :extargs extdata.widget;
        arg,                           ;;; don't use client arg
        exacc [fast] :extargs extdata.name;
        exacc [fast] :extargs extdata.event;
        exacc [fast] :extargs extdata.params;
        exacc [fast] :extargs extdata.num_params;

        ;;; do call via standard callback handler
        XptCallbackHandler(proc,"actionhook");
    |# -> stacked_items;
    unless stacked_items == 0 do
        warning(
            proc, stacked_items+1,
            'ITEMS ON STACK AFTER ACTIONHOOK PROCEDURE BEING IGNORED'
        );
    endunless;

enddefine;


;;; Cache indexing closures of -XptActionHookWrapper- and -XptActionWrapper-
;;; onto external procedures
lconstant callback_cache =
    newanyproperty( [], 50, false, false,
                    syshash, sys_=, "tmpval",
                    false, false
    );


;;; Instance of the closures used to index -callback_cache-
lconstant XptActionHookWrapperInstance =
    writeable XptActionHookWrapper(%false, false%);
lconstant XptActionWrapperInstance =
    writeable XptActionWrapper(%false%);


;;; Non caching coercion of action procedures
define global XptExportAction(proc, hold); /* -> EFC */
    lvars proc, hold;

    if XptIsValidCallback(proc) then
        exfunc_export(XptActionWrapper(%proc%), XptCallbackFlags, hold);
    else
        mishap(proc, 1, 'PROCEDURE, IDENT, OR WORD NEEDED');
    endif;
enddefine;


;;; Non caching coercion of actionhook procedures
define global XptExportActionHook(proc, arg, hold) -> (efc, ext_arg);
    lvars proc, arg, hold, efc;
    lconstant ext_arg = false;  ;;; Never use toolkit argument

    if XptIsValidCallback(proc) then
        exfunc_export(
            XptActionHookWrapper(%proc, arg%),
            XptCallbackFlags,
            hold
        ) -> efc;
    else
        mishap(proc, 1, 'PROCEDURE, IDENT, OR WORD NEEDED');
    endif;
enddefine;


;;; Cachine coercion of action procedures
define global XptExportActionCached(proc, hold) -> efc;
    lvars proc, hold, efc;
    lvars callback_wrapper;

    proc -> explode(XptActionWrapperInstance);

    unless (callback_cache(XptActionWrapperInstance) ->> efc) do;
        if XptIsValidCallback(proc) then
            copy(XptActionWrapperInstance) -> callback_wrapper;
            exfunc_export(callback_wrapper, XptCallbackFlags, hold)
                ->> callback_cache(callback_wrapper) -> efc;
        else
            mishap(proc, 1, 'PROCEDURE, IDENT, OR WORD NEEDED');
        endif;
    endunless;

enddefine;


;;; Cachine coercion of actionhook procedures
define global XptExportActionHookCached(proc, arg, hold) -> (efc, ext_arg);
    lvars proc, arg, hold, efc;
    lconstant ext_arg = false;  ;;; Never use Xt client data
    lvars callback_wrapper;

    (proc, arg) -> explode(XptActionHookWrapperInstance);

    unless (callback_cache(XptActionHookWrapperInstance) ->> efc) do;
        if XptIsValidCallback(proc) then
            copy(XptActionHookWrapperInstance) -> callback_wrapper;
            exfunc_export(callback_wrapper, XptCallbackFlags, hold)
                ->> callback_cache(callback_wrapper) -> efc;
        else
            mishap(proc, 1, 'PROCEDURE, IDENT, OR WORD NEEDED');
        endif;
    endunless;

enddefine;


global constant fast_xt_action = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Nov  5 1991 : Now passes -XptXEventPtr- structures.
--- John Gibson, Nov  3 1991
        includes xpt_coretypes.ph;
--- Adrian Howard, Sep 30 1991 : Added -XptExportActionCached-
--- Adrian Howard, Aug 30 1991
    - Added proper coercion with -XptExportAction(Hook)(Cached)- procedures
    - Removed redundent -XptCoerceAction(Hook)- procedures.
    - Altered wrappers so they caught the user stacking items during a
      callback
--- Roger Evans, Jun 27 1991
        removed numargs args from exfunc_export and XptCallbackHandler
--- Roger Evans, Jun 25 1991 removed 'uses xpt_cb_utils'
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Jun 24 1991 added callback wrapper code
--- Roger Evans, Nov 15 1990 updated list iof identifiers
--- Adrian Howard, Sep  7 1990 : Added var so -uses- works
 */
