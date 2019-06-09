/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCallbackList.p
 > Purpose:         Construct an Xt callback list
 > Author:          Jonathan Meyer, Jan 29 1991 (see revisions)
 > Documentation:
 > Related Files:   XptArgList.p XptVaArgList.p
 */
compile_mode :pop11 +strict;

section;

/*
XptCallbackList - converts a Pop-11 list into an X Toolkit callback list.
Used to make static callback structures that can be passed in an arglist
at widget creation time.
eg.

    constant foo = XptCallbackList([{^my_cb_proc ^my_cb_data}]);

    XtCreateManagedWidget('example', ExampleClass, example_parent,
            XptArgList([{selectCallback ^foo}])) -> widget;
*/

uses xt_callback;
include xpt_constants;

;;; make nc_ versions of XptCallbackList shadowclass
shadowclass lconstant XptCallbackList [nc, prefix nc_];

;;; only use one empty callback list
lvars null_cb_list = false;

define lconstant stack_item(item);
    lvars item data proc;
    if (#| explode(item) |#) == 2 then
        -> data -> proc;
        if XptIsValidCallback(proc) then
            XptExportCallbackCached(proc, data, false);
        else
            XptCheckProcedure(proc); data
        endif;
    else
        mishap(item,1,'[PROC CLIENT_DATA] PAIR NEEDED');
    endif;
enddefine;

define XptCallbackList(list);
    lvars list, vec, cb, proc, data;
    ;;; put each thing of list on stack, followed by a null-entry
    if isXptCallbackList(list) then
        return(list);
    elseif list == [] then
        null_cb_list or (nc_consXptCallbackList(false, 0, 1) ->> null_cb_list)
    elseif list.islist then
        applist(list, stack_item);
        false, 0, nc_consXptCallbackList(listlength(list)+1)
    else
        mishap(list,1,'LIST or XptCallbackList NEEDED');
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 18 1993
        o Removed creation of null_cb_list at compile-time (no good for POPC)
        o Removed compile-time call to fast_XptToolkitPreInitialize (yuk!)
--- John Gibson, Aug 24 1991
        XptExport(Typed)Callback(Cached) now take extra boolean -hold- arg
--- Adrian Howard, Aug 12 1991 : XptExportCallback --> XptExportCallbackCached
--- Adrian Howard, Aug  9 1991 : XptCoerceCallback --> XptExportCallback
--- Roger Evans, Jun 28 1991 changes for new callback coercion code
--- Jonathan Meyer, Feb 15 1991 Changed shadowclass definition to
        lconstant, removed full typespec on definition.
 */
