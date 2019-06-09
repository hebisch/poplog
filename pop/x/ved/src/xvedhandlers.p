/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedhandlers.p
 > Purpose:         Processing xved events
 > Author:          Jonathan Meyer, Mar 30 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xved_declare.ph'
include xved_constants.ph;

section $-xved;

/************************************************************************
 * Xved Event Handling Routines
 ************************************************************************/

constant procedure xved_free_button_info;


define xved_process_event();
    lvars name;
    dlocal xvedwarpmouse = false;   ;;; events don't cause mouse warping

    unless (fast_subscrv(1,xvedrawdevindata) ->> name).isword then
        vederror('unknown event type: ' sys_>< name);
    endunless;
    false -> xvedeventhandled; ;;; this is reset by each event
    xved_dispatch_event(fast_subscrv(2,xvedrawdevindata), name,
                                        fast_subscrv(3,xvedrawdevindata))
enddefine;

define xved_select_window(w);
    lvars w, file = wved_file_of_window(w);
    returnunless(file);
    if ved_current_file /== file then
        ;;; tell the last window we are leaving it
        if wvedwindow.wved_is_live_window then
            xved_dispatch_event(wvedwindow, "unselectWindow", false);
        endif;
        vedsetonscreen(file, nullstring);
        xved_dispatch_event(w, "selectWindow", false);
    endif;
    if wvedwindow.xvedwin_isnew then
        false -> wvedwindow.xvedwin_isnew;
        CHECK_SCROLLBAR;
    endif;
enddefine;

identfn -> xvedeventtable("selectWindow");
identfn -> xvedeventtable("unselectWindow");
identfn -> xvedeventtable("createWindow");
identfn -> xvedeventtable("destroyWindow");



;;; Public Event dispatcher - this procedure is given a window, an event
;;; name and a single piece of data.
;;; This procedure uses the name to determine how to handle the event -
;;; and then calls the appropriate procedure.
;;; Event handlers can use xved_dispatch_event to dispatch events to
;;; another window or handler.

define vars xved_dispatch_event(window, name, data);
    lvars window, name, l, p, data, n, p;
    lconstant tmpl = writeable [0];
    ;;; look for window specific event handler first then general one
    if (window.xvedwin_eventvec->>l) and (l(name) ->> l)
    or (xvedeventtable(name) ->> l)
    then
        unless islist(l) then l -> fast_front(tmpl), tmpl -> l endunless;
        fast_for p in l do
            if xvedblockinput and not(ispair(pdprops(p))) then
                ;;; ignore a handler unless its props are a pair
                vedscreenbell();
                vedscr_flush_output();
                nextloop
            endif;
            #| p(window, name, data) |# -> n;
            if n /== 3 then
                mishap(n, 'STACK CONTENTS WRONG AFTER DISPATCHING '
                                sys_>< name sys_>< ' EVENT')
            else
                -> (window, name, data)
            endif
        endfor

#_IF DEF XVED_DEBUG
    else
        ;;; we cannot find an event handler for this message -
        ;;; print it out if we are debugging
        vedputmessage('Unknown event: ' sys_>< name);
#_ENDIF
    endif;
enddefine;


/* Utilities for adding event handlers */

    /*  So we free buttonPress info (which may not have a handler, but
        gets generated on every press)
    */
procedure(w, name, data) -> (w, name, data) with_props #_<[^false]>_#;
    lvars w, name, data;
    dlocal vedwarpcontext;
    returnif(xvedeventhandled);
    xved_free_button_info(data);
    false -> data;
    true -> xvedeventhandled;
    returnif(xvedblockinput);
    unless wvedalwaysraise then false -> vedwarpcontext endunless;
    xved_select_window(w);
    if xvedhasinputfocus /== wvedwindow then
        /* need to manually set the input focus for the window */
        dlocal xvedpopfocusonly = false;
        wved_set_input_focus(wvedwindow);
        xved_button_release();
    endif
endprocedure -> xvedeventtable("buttonPress");


define xved_add_event_handler(new_proc, window, name, position);
    lvars new_proc proc window, name, position, item, table;
    if name.islist then
        for item in name do
            xved_add_event_handler(new_proc, window, item, position);
        endfor;
        return;
    endif;
    unless name.isword then
        mishap(name,1,'WORD NEEDED');
    endunless;
    xved_value(window, "eventTable") -> table;
    table(name) -> proc;
    if proc and position then
        unless islist(proc) then proc :: [] -> proc endunless;
        if position == "front" then
            new_proc :: proc -> new_proc
        elseif position == "back" then
            proc <> [^new_proc] -> new_proc
        else
            mishap(position,1,'INVALID POSITION: expected "front" or "back"');
        endif
    endif;
    new_proc -> table(name)
enddefine;

define xved_add_button_handler(proc, types, btns, mods, clks, window, place);
    lvars proc, types, btns, mods, clks, window, place;

    define lconstant check_type(type);
        lvars type;
        unless fast_lmember(type, [
                buttonPress buttonRelease
                buttonDrag buttonDragRelease
                buttonHold buttonHoldRelease
            ]) then mishap(type, 1, 'UNKNOWN BUTTON EVENT TYPE');
        endunless;
    enddefine;

    define lconstant button_handler(w, name, data, proc, types,
                                        btns, mods, clks) -> (w, name, data);
        lvars   w, name, data, proc, types, btns, clks, mods,
                ev_mods, ev_btn, ev_clks;

        dlocal 0 %, if dlocal_context==2 then true -> xvedeventhandled endif%,
                vedwarpcontext;

        returnif(xvedeventhandled);
        returnunless(name==types or types.islist and fast_lmember(name,types));

        fast_subscrintvec(XVM_MODIFIERS, data) &&~~ XVM_LOCKMASK -> ev_mods;
        if ev_mods == 0 then XVM_NOMODMASK -> ev_mods endif;
        fast_subscrintvec(XVM_BUTTON, data) fi_- 1  -> ev_btn;
        fast_subscrintvec(XVM_CLICKS, data)         -> ev_clks;
        returnunless((mods == ev_mods or mods &&/=_0 ev_mods)
                     and btns &&/=_0 (1 fi_<< ev_btn)
                     and clks == ev_clks);

        if xvedblockinput and not(ispair(pdprops(proc))) then
            ;;; ignore a handler unless its props are a pair
            vedscreenbell();
            vedscr_flush_output();
            return
        endif;

        unless wvedalwaysraise then false -> vedwarpcontext endunless;
        xved_select_window(w);

        data -> vvedmousedata;
        fast_apply(proc);
        if xvedeventhandled then
            xved_free_button_info(data);
            false -> data
        endif
    enddefine;

    if types.islist then
        applist(types, check_type)
    else
        check_type(types)
    endif;

    if proc then
        unless proc.isprocedure then
            mishap(proc, 1, 'PROCEDURE NEEDED');
        endunless;
        fi_check(clks, 0, false) -> ;
        fi_check(btns, 1, XVM_ANYBTNMASK) -> ;
        fi_check(mods, 0, XVM_ANYMODMASK) -> ;
        button_handler(%proc, types, btns, mods, clks%) -> proc;
        ;;; making props a pair allows it to be executed when xvedblockinput
        ;;; is set
        #_<[^false]>_# -> pdprops(proc)
    endif;

    xved_add_event_handler(proc, window, types, place);
enddefine;

define xved_event_handler(window, name) -> proc;
    lvars window, name proc table;
    xved_value(window, "eventTable") -> table;
    table(name) -> proc;
enddefine;
;;;
define updaterof xved_event_handler(proc, window, name);
    lvars proc window, name table;
    xved_value(window, "eventTable") -> table;
    proc -> table(name);
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 20 1995
        Multiple event handlers for an event are now in the form of a list
        rather than concatenated procedures -- allows xved_dispatch_event
        to process them selectively.
--- John Gibson, Aug 22 1995
        Changed button_handler and buttonPress handler so that vedwarpcontext
        is dlocally set false only if wvedalwaysraise is false (thus if
        wvedalwaysraise is true, window will be raised when a button is
        pressed in it).
--- John Gibson, Dec 14 1993
        Set xvedpopfocusonly locally false inside buttonPress handler
--- John Gibson, Jul  7 1992
        Made testing for button modifiers in -xved_make_b*utton_wrapper-
        ignore XVM_LOCKMASK (i.e. so caps lock makes no difference)
--- John Gibson, Jan  9 1992
        Fixed -xved_select_window- so that it does vedsetonscreen
        before raising the selectWindow event instead of after.
--- John Gibson, Aug 19 1991
        Changes to handling of clicks
--- John Gibson, Aug 12 1991
        Event handlers now take window as first arg
--- John Gibson, Aug 10 1991
        Made events handlers return their arguments, and made
        xved_dispatch_event print a warning if more or less than 2 results
        are returned by handler.
--- John Gibson, Aug  9 1991
        Absorbed xved_event_wrapper into button_handler so it
        can call xved_free_button_info after event handled.
        Also, button_handler now returns (name, data) so that vedmouse__
        procedures don't have to.
--- John Gibson, Aug  3 1991
        Handling of window changes now done with selectWindow handler
--- Jonathan Meyer, Jul 27 1991
        Renamed xved*after_editor_exit xved_before_editor_exit
--- Jonathan Meyer, Jul  5 1991
        Added unselectWindow and dlocal of xvedwarpmouse to Dispatch_event
--- John Gibson, Jun 21 1991
        Uses ved_save_file_globals
--- Jonathan Meyer, Jun  12 1991
        Added event handler wrapper procs
--- Jonathan Meyer, Jun  4 1991
        Added -erasenum- to clear anything left on stack be event procedures.
--- Jonathan Meyer, Jun  4 1991
        Changed to use xved_default_window_value
--- Jonathan Meyer, Jun  3 1991
        Added test for xvedwin_eventvec
--- Jonathan Meyer, May 31 1991 Fixed title generation procedure
 */
