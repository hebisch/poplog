/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xpt_clientcallback.p
 > Purpose:         Allows clients to invent their own callback lists
 > Author:          Jonathan Meyer, Sep  1 1991 (see revisions)
 > Documentation:   REF *XPT_CLIENTCALLBACK
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

include xpt_constants;
uses xt_callback;

/*
Client callbacks are stored in a single list for each widget - calling
client callbacks involves iterating over the list looking for callbacks
with the same name. This could be made faster (by having a single list
for each callback name), but at the cost of complexity and storage
requirements.
*/


lconstant
    Callbacks = writeable newanyproperty([], 10,1,10, false, false,
                                        "tmparg", false, false);

lvars Displays = []; ;;; remember all the different displays

/* For each callback, we build a list of the following structure: */
defclass lconstant CBproc {
    cb_name,        ;;; name of the callback
    cb_proc,        ;;; callback procedure
    cb_data,        ;;; client data for procedure
};

define lconstant Descriptor(widget);
    lvars widget;
    XptDescriptor(XptCheckWidget(widget), XDT_WIDGET)
enddefine;

define lconstant Destroy_cb(w, client, call);
    lvars w, client, call;
    ;;; remove entry from property table
    false -> Callbacks(w);
enddefine;

define lconstant Add_callback(widget, cbprocs, cbproc);
    lvars widget, cbprocs, cbproc, dpy;
    unless cbprocs then
        ;;; new callback list
        XtAddCallback(widget, XtN destroyCallback, Destroy_cb, false);
    endunless;
    ;;; construct a new list and grbg old one
    [% if cbprocs then dl(cbprocs); sys_grbg_list(cbprocs) endif, cbproc %]
            -> Callbacks(widget);
    unless fast_lmember(fast_XtDisplay(widget) ->> dpy, Displays) then
        conspair(dpy, Displays) -> Displays; ;;; register a new display
    endunless;
enddefine;

define lconstant Remove_callback(widget, name, proc, data, all);
    lvars widget, name, proc, data, all, one = true, found_list=false,
            cbprocs, cb;

    Callbacks(Descriptor(widget)->>widget) -> cbprocs;

    unless cbprocs then
        warning(widget,1, 'NO CLIENT CALLBACKS'); return;
    endunless;

    /* Only accept wanted callbacks */
    define lconstant Wanted(cb) -> wanted;
        lvars cb, wanted = true;
        if cb.cb_name == name then
            true -> found_list;
            if all or (one and cb.cb_proc == proc and cb.cb_data = data) then
                false -> one;
                false -> wanted;
            endif;
        endif;
    enddefine;

    ;;; build new list and grbg old one
    [%fast_for cb in cbprocs do
        if Wanted(cb) then cb; endif;
    endfast_for;%], sys_grbg_list(cbprocs) -> cbprocs;

    if cbprocs == [] then
        XtRemoveCallback(widget, XtN destroyCallback, Destroy_cb, false);
        false -> cbprocs;
    endif;
    cbprocs -> Callbacks(widget);

    unless found_list then
        warning(widget,name,2,'NO SUCH CLIENT CALLBACK LIST');
    endunless;
enddefine;

define lconstant Has_cbs(cbprocs, name) -> has_some;
    lvars cbprocs, name, cb, has_some = false;
    if cbprocs then
        fast_for cb in cbprocs do
            quitif(cb.cb_name == name and true ->> has_some);
        endfast_for;
    endif;
enddefine;

define lconstant Apply_callback(widget, cbprocs, name, call);
    lvars widget, cbprocs, name, call, cb;
    returnif(is_null_external_ptr(widget));
    XptRegister(widget) -> widget;
    fast_for cb in cbprocs do
        if cb.cb_name == name then
            ;;; does proc(widget, client, call);
            XptCallbackHandler(widget, cb.cb_data, call, cb.cb_proc, name);
        endif;
    endfast_for;
enddefine;

/* Public interface */

define XptAddClientCallback(widget, name, proc, data);
    lvars widget, name, proc, data, cbprocs, cbproc;
    unless (proc.isprocedure and pdnargs(proc) == 3) then
        mishap(proc,pdnargs(proc),2,'PROCEDURE WITH PDNARGS OF 3 NEEDED');
    endunless;
    Callbacks(Descriptor(widget)->>widget) -> cbprocs;
    consCBproc(name, proc, data) -> cbproc;
    Add_callback(widget, cbprocs, cbproc);
enddefine;

define XptRemoveClientCallback
    = Remove_callback(%false%)
enddefine;

define XptRemoveAllClientCallbacks
    = Remove_callback(%undef, undef, true%)
enddefine;

define XptHasClientCallbacks(widget, name);
    lvars widget, name;
    Has_cbs(Callbacks(Descriptor(widget)), name);
enddefine;

define XptCancelClientCallbacks(widget);
    lvars widget;
    false -> Callbacks(widget);
enddefine;

define XptCallClientCallbacks(widget, name, call);
    lvars widget, cbprocs, name, call, defer = false;
    unless XptIsType(widget, "Widget") then
        widget, name, call -> (widget, name, call, defer);
    endunless;
    unless Callbacks(Descriptor(widget)) ->> cbprocs then
        warning(widget,1,'NO REGISTERED CLIENT CALLBACKS'); return;
    endunless;
    if defer then
        external_defer_apply(Apply_callback, widget, cbprocs, name, call, 4);
    else
        fast_chain(widget, cbprocs, name, call, Apply_callback);
    endif;
enddefine;

XptLoadProcedures xpt_clientcallback lvars XFlush;

define XptCallAllClientCallbacks(name, call);
    lvars dpy;
    dlvars name, call;

    fast_appproperty(Callbacks,
        procedure(widget, cbprocs);
            lvars widget, cbprocs;
            Apply_callback(widget, cbprocs, name, call);
        endprocedure);

    ;;; flush the displays to make any changes visible
    fast_for dpy in Displays do
        if not(is_null_external_ptr(dpy)) then
            exacc [fast] (1) raw_XFlush(dpy);
        endif;
    endfast_for;
enddefine;

constant xpt_clientcallback = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 22 1996
        Made XptCallClientCallbacks use new external_defer_apply facility
        for creating closures automatically.
--- John Gibson, Apr 14 1994
        Xpt*DeferApply -> external_defer_apply
--- John Gibson, Mar 16 1993
        Added missing 'uses xt_callback'
--- John Gibson, Nov 29 1991
        Made name and call dlvars in XptCallAllClientCallbacks
--- Jonathan Meyer, Sep  2 1991
        Now calls XptRegister/XptDescriptor
 */
