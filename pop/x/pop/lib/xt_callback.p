/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_callback.p
 > Purpose:         Calback handling
 > Author:          Adrian Howard, Aug  9 1990 (see revisions)
 > Documentation:   REF *XT_CALLBACK
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p
 >                  C.x/x/pop/lib/fast_xt_callback.p
 */

compile_mode:pop11 +strict;

section;

include xpt_constants.ph;
include xpt_generaltypes.ph;

uses xpt_typecheck;       ;;; Get the type checking routines
uses fast_xt_callback;    ;;; Get the fast versions of the procedures

shadowclass XptCallbackPtr {:XptCallbackRec};
shadowclass XptCallbackList #_< [props ^XDT_CALLBACKLIST] >_#
    :XptCallbackRec[];


;;; Add a single callback procedure to a widget
;;; Pop variant that allows a conversion procedure acc_p to extract the call
;;; data from the exptr passed by XptCallbackWrapper (conv_p may also be false)
define XptAddCallback(widget, name, proc, client, conv_p);
    lvars widget, name, proc, client, conv_p;

    fast_XtAddCallback(
            XptCheckWidget(widget),
            XptCheckString(name),
            if XptIsValidCallback(proc) then
                XptExportTypedCallbackCached(proc, client, conv_p, false)
            else
                XptCheckProcedure(proc),
                client
            endif
         );
enddefine;

;;; Add a single callback procedure to a widget - 09/08/90
;;; Input - <Widget> <String> <CallbackProc> <XtPointer>
define XtAddCallback = XptAddCallback(%false%) enddefine;


;;; Adds a list of callback procedures to a widget - 09/08/90
;;; Input - <Widget> <STRING> <XtCallbackList>
define XtAddCallbacks(widget, name_string, callbacklist);
    lvars widget, name_string, callbacklist;
    fast_XtAddCallbacks(    XptCheckWidget(widget),
                            XptCheckString(name_string),
                            XptCheckNTCallbackList(callbacklist)
                       );
enddefine;

;;; pop list variant of above
define lconstant Do_CallbackList(widget, name_string, callbacklist, add);
    lvars widget, name_string, callbacklist item add proc type;
    unless islist(callbacklist) then mishap(callbacklist,1,'LIST NEEDED') endunless;
    [%  for item in callbacklist do
            if islist(item) and listlength(item) fi_> 1 then
                fast_destpair(item) -> (proc,item);
                if XptIsValidCallback(proc) then
                    proc;
                    fast_destpair(item) -> item; ;;; leave client on stack
                    if item == [] then false else fast_front(item) endif;
                    XptExportTypedCallbackCached(false);
                else
                    XptCheckProcedure(proc);
                    fast_back(item);
                endif;
                conspair();
            else
                mishap(item,1,'[CALLBACK ARG] OR [CALLBACK ARG TYPE] LIST NEEDED');
            endif;
        endfor;
    %] -> callbacklist;

    XptCheckWidget(widget);
    XptCheckString(name_string);
    callbacklist;
    if add then fast_XptAddCallbackList
    else fast_XptRemoveCallbackList
    endif();
enddefine;

define XptAddCallbackList with_nargs 3;
    Do_CallbackList(true);
enddefine;

;;; Remove a single callback procedure from a widget - 09/08/90
;;; Input - <Widget> <STRING> <CallbackProc> <XtPointer>
define XtRemoveCallback(widget, name, proc, client);
    lvars widget, name, proc, client;

    fast_XtRemoveCallback(  XptCheckWidget(widget),
                            XptCheckString(name),
                            if XptIsValidCallback(proc) then
                                XptExportTypedCallbackCached(proc,client,false,false)
                            else
                                XptCheckProcedure(proc),
                                client
                            endif
                          );
enddefine;


;;; Removes a list of callback procedures from a widget - 09/08/90
;;; Input - <Widget> <STRING> <XtCallbackList>
define XtRemoveCallbacks(widget, name_string, callbacklist);
    lvars widget, name_string, callbacklist;
    fast_XtRemoveCallbacks( XptCheckWidget(widget),
                            XptCheckString(name_string),
                            XptCheckNTCallbackList(callbacklist)
                       );
enddefine;


define XptRemoveCallbackList with_nargs 3;
    Do_CallbackList(false);
enddefine;

;;; Execute procedures in a widgets callback list - 09/08/90
;;; Input - <Widget> <STRING> <XtPointer>
define XtCallCallbacks(widget, name_string, call_data);
    lvars widget, name_string, call_data;
    fast_XtCallCallbacks(   XptCheckWidget(widget),
                            XptCheckString(name_string),
                            call_data
                        );
enddefine;


;;; Find the status of a widgets callback list - 09/08/90
;;; Input - <Widget> <STRING>, Output - <XtCallbackStatus>
define XtHasCallbacks(widget, name_string);
    lvars widget, name_string;
    fast_XtHasCallbacks(    XptCheckWidget(widget),
                            XptCheckString(name_string)
                       );
enddefine;


;;; Remove all callbacks from a widget - 13/09/90
;;; Input - <Widget> <STRING>
define XtRemoveAllCallbacks(widget, callback_name);
    lvars widget, callback_name;
    fast_XtRemoveAllCallbacks(  XptCheckWidget(widget),
                                XptCheckString(callback_name)
                             );
enddefine;


;;; So uses works OK
constant xt_callback= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 23 1994
        Added XptAddCallback
--- John Gibson, Nov  3 1991
        Moved typespecs to xpt_generaltypes.ph
--- Adrian Howard, Oct 31 1991 :
    o -XptCallbackRec- now a typespec
    o -XptCallbackRec- shadowclass now -XptCallbackPtr-
--- Adrian Howard, Sep 11 1991 : Added checks for null-terminated callback
        lists
--- John Gibson, Aug 24 1991
        XptExport(Typed)Callback(Cached) now take extra boolean -hold- arg
--- Adrian Howard, Aug 12 1991
        XptExportTypedCallback --> XptExportTypedCallbackCached
--- Adrian Howard, Aug  9 1991
        XptCoerceTypedCallback --> XptExportTypedCallback
--- Roger Evans, Jun 28 1991 added calls to XptIsValidCallback
--- Jonathan Meyer, Jun 24 1991
        Moved XptCoerceCallback into fast_xt_callback.
--- Roger Evans, Jun 24 1991 added new procedure coercion code
--- Roger Evans, Feb 10 1991 changed XptPopObj to XptPointer
--- Roger Evans, Feb  1 1991 fixed completeley erroneous definitions
        for XptAdd/RemoveCallbackList
--- Roger Evans, Jan 26 1991 changed to new shadowclass props format
--- Roger Evans, Nov 18 1990
        added shadowclass def and XptAdd/RemoveCallbackList
--- Roger Evans, Nov 11 1990 XptCheckProc -> XptCheckProcedure
--- Adrian Howard, Sep 13 1990 : Made procs and added
        -XtRemoveAllCallbacks-
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
