/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedwm.p
 > Purpose:         Window manager manipulation under xved
 > Author:          Jonathan Meyer, Jan 27 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

/* ==== Window Manager and Inter-Client Communication ================== */

/* This file defines Xved window manager interaction. */

#_INCLUDE 'xved_declare.ph'
include xt_constants.ph;
include xpt_xwindow.ph;
include xpt_xevent.ph;
include xpt_xscreen.ph;
include ved_do_text_action.ph;

section $-xved;

lvars had_input_focus = false;

/************************************************************************
 *                  Window Management Actions
 ************************************************************************/


;;; CALLBACK to handle focus changes
;;;     warping and input focus setting only done when this has a value.

define xved_focus_change_callback(w, client, call);
    lvars w, client, call, gotfocus = (call /== 0), file;
    if gotfocus and (wved_file_of_window(w) ->> file)
        and (vedbufferlist == [] or file /== hd(vedbufferlist))
        and (xvedblockinput == false or xvedblockinput == w)
    then
        /* redraw the status line on the new window */
        procedure;
            dlocal vedediting = true, ved_current_file = file;
            vedsetnormalmessage(true)   ;;; true = flush output
        endprocedure();
    endif;

    if gotfocus then
        w -> xvedhasinputfocus
    elseif xvedhasinputfocus == w then
        false -> xvedhasinputfocus
    ;;; following test needed for destroy callback, since xvedwin in
    ;;; xvedhasinputfocus has been zapped already and w is just
    ;;; an anonymous widget
    elseif (xvedhasinputfocus and is_null_external_ptr(xvedhasinputfocus))
    then
        false -> xvedhasinputfocus;
        ;;; use this to record that we just lost focus due to quitting
        ;;; a window (cleared in wved_set_input_focus)
        true -> had_input_focus;
    endif;
    false -> xvedcurrinputwin;
enddefine;


;;; CALLBACK to handle moving in and out of an active segment

define xved_active_change_callback(w, client, call);
    lvars   w, client, call, file, col, row,
            enter = not(is_null_external_ptr(call));

    returnunless((wved_file_of_window(w) ->> file)
                and (xvedblockinput == false or xvedblockinput == w));

    XptVal[fast] w(XtN mouseColumn:int, XtN mouseRow:int) -> (col, row);
    returnunless(row > 0 or enter);
    procedure;
        lvars   sline, scol;
        dlocal  vedmessage, vedediting = true, ved_current_file = file;
        vedscreenline -> sline;
        vedscreencolumn -> scol;
        if enter then
            ved_do_text_action(
                    row fi_+ 1, col fi_+ 1,
                    if xvedblockinput then VDTA_TYPE_CHARS
                    else #_< VDTA_TYPE_ANY &&~~ VDTA_TYPE_ANY_HELP >_#
                    endif,
                    VDTA_MODE_DISPLAY)
                or 0 -> exacc :uint call
        else
            vedsetnormalmessage(true)   ;;; true = flush output
        endif;
        if 1 fi_<= sline and sline fi_<= vedwindowlength
        and 1 fi_<= scol and scol fi_< 1000
        then
            vedwindowpoint(sline, scol);
            vedscr_flush_output()
        endif
    endprocedure()
enddefine;


;;; ACTION to trap WM_DELETE_WINDOW messages
;;;     raises a "deleteWindow" event to kill the window

define lconstant wm_destroy_window_action(w, event, params, num_params);
    lvars w, event, params, num_params;
    unless w == xvedblockinput then
        xved_raise_event(w, "deleteWindow", false);
    endunless;
enddefine;

;;; HANDLER for "deleteWindow" events
;;;     calls ved_q
procedure(w, name, data) -> (w, name, data);
    lvars w, name, data;
    xved_select_window(w);
    ved_q();
endprocedure -> xvedeventtable("deleteWindow");


;;; ACTION to trap XV_DO_DRAG_LOAD messages
;;;     raises a "dragLoad" event to load the specified file

define lconstant do_drag_load_action(w, event, params, num_params);
    lvars w, event, pos, params, num_params;
    l_typespec params :XptString[];
    returnif(num_params == 0);
    exacc params[1] -> params;
    ;;; convert tab-separated filenames into a list of strings
    [%while issubstring('\t', params)->>pos then
        substring(1, pos-1, params);
        substring(pos+1, datalength(params)-pos, params) -> params;
    endwhile, params%] -> params;

    ;;; raise a "dragLoad" action
    xved_raise_event(w, "dragLoad", params);
enddefine;

;;; HANDLER for "dragLoad" events
;;;     reads files (separated by tabs) into ved

define lconstant drag_load_handler(w, name, files) -> (w, name, files);
    lvars w, name, files, file, missing_files;
    returnunless(name == "dragLoad");
    [%for file in files do
        if readable(file) and not(sysisdirectory(file)) then
            file -> vedargument; ved_ved();
        else
            file;
        endif;
    endfor%] -> missing_files;
    if missing_files /== [] then
        mishap('Cannot open the following files:', missing_files);
    endif;
enddefine;

drag_load_handler -> xvedeventtable("dragLoad");

/* === Handler for XpwDoDragMoveAction - pastes selection =========== */

define lconstant do_drag_move_action(w, event, params, num_params);
    lvars w, event, pos, params, num_params;
    l_typespec params :XptString[];
    returnunless(num_params == 0);
    xved_raise_event(w, "dragMove", false);
enddefine;

define lconstant drag_move_handler(w, name, data) -> (w, name, data);
    lvars w, name, data;
    returnunless(name == "dragMove");
    xved_select_window(w);
    vedinsertstring(vvedclipboard);
enddefine;

drag_move_handler -> xvedeventtable("dragMove");

/* ==== Set Widget  Translations and Actions Tables   ================== */

    /*  XVed "event" action
    */
define lconstant event_action_handler(w, event, params, num_params);
    lvars w, event, params, num_params, name, data;
    l_typespec params :XptString[];
    unless num_params == 2 then
        sysprmessage(num_params,1,
            'Invalid number of parameters to event action',
            'XVED WARNING', 2);
        return;
    endunless;
    consword(exacc params[1]) -> name;
    if testdef pop11_compile then
        weakref pop11_compile(stringin(exacc params[2])) -> data;
        xved_raise_event(if w.isxvedwin then w else wvedwindow endif, name, data);
    else
        sysprmessage(name,1,
            'Can\'t process event action - Pop-11 compiler not loaded',
            'XVED WARNING', 2);
    endif
enddefine;


    /* Called from xvedsetup
    */
define xved_init_wm_actions();
    XptAppAddActionList(xvedappcontext, [
        ['XpwDeleteWindow'  ^wm_destroy_window_action]
        ['XpwDoDragLoad'    ^do_drag_load_action]
        ['XpwDoDragMove'    ^do_drag_move_action]
        ['event'            ^event_action_handler]
    ]);
enddefine;


/* ==== Advanced Window Management - mouse warping etc ================= */

    /* Xlib procedures */
XptLoadProcedures 'xvedwm'
lvars
    XRaiseWindow(dpy,win),
    XLowerWindow(dpy,win),
    XMapWindow(dpy,win),
    XIconifyWindow(dpy,win,scrnum),
    XDefaultScreen(dpy) :int,
    XGetInputFocus(dpy, focus, revert_to),
    XSetInputFocus(dpy,win,revert_to,time),
    XQueryPointer(dpy,win,root,child,root_x,root_y,win_x,win_y,keys_buttons)
                                                        :XptLongBoolean,
    XWarpPointer(dpy,src_w,dst_w,src_x,src_y,src_width,src_height,dst_x,dst_y),
    XQueryTree(dpy,win,root,parent,children,nchildren) :XptLongBoolean,
    XGetWindowAttributes(dpy,win,win_attrs),
    XSelectInput(dpy,win,event_mask),
    XSetErrorHandler(handler) :exptr,
    XFree(ptr),
;

lconstant
    int_ptr1    = EXPTRINITSTR(:int),
    int_ptr2    = EXPTRINITSTR(:int),
    int_dummy   = EXPTRINITSTR(:int),
    XID_ptr1    = EXPTRINITSTR(:XptXID),
    XID_dummy   = EXPTRINITSTR(:XptXID),
    exptr_ptr1  = EXPTRINITSTR(:exptr),
;

;;; Sync X output
define lconstant sync_display();
    XptSyncDisplay(xveddisplay);
enddefine;

;;; xved_basewindow_wid() -> WID
;;;     returns an X window ID for the base window if it can find one, or
;;;     false if it can't.
lvars basewindow_wid = false;
;;;
define xved_basewindow_wid() -> wid;
    lvars wid;
    returnunless(systrmdev(popdevin) == true) (false -> wid);
    returnif(basewindow_wid ->> wid);
    if (systranslate('WINDOWID') ->> wid) and (strnumber(wid) ->> wid) then
        ;;; to make sure we get MapNotify events on xvedappcontext
        wid -> basewindow_wid;
        exacc raw_XSelectInput(xveddisplay, wid, StructureNotifyMask);
    else
        false -> wid
    endif
enddefine;

/* turns a window, widget or device into an X Window or false */

define lconstant window_id(win);
    lvars win, file;
    if isdevice(win) then
        if isveddevice(win) then
            ;;; do nothing if file not opened yet
            returnunless( (vedpresent(device_full_name(win)) ->> file)
                            and (wved_window_of_file(file) ->> win)) (false)
        elseif systrmdev(win) then
            "basewindow" -> win
        else
            return(false)
        endif
    endif;
    if win == "basewindow" then
        xved_basewindow_wid()
    elseif isxvedwin(win) then
        ;;; shell's id
        XtWindow(xvedwin_shell(win))
    elseif isinteger(win) or XptDataType(win) == "Window" then
        ;;; leave it alone
        win
    elseif isexternal_ptr_class(win) and XptDataType(win) == "Widget" then
        ;;; widget - get its window
        XtWindow(win)
    else
        false
    endif -> win;
    (isinteger(win) or (isexternal_ptr_class(win)
                        and not(is_null_external_ptr(win)))
    ) and win
enddefine;

;;; Warp X mouse - calls XWarpPointer
;;;     xved_x_warp_pointer(window, false)    ;;; warp to 1,1
;;;     xved_x_warp_pointer(window, true)     ;;; warp to middle
;;;     xved_x_warp_pointer(window, "inside") ;;; warp just within
;;;     xved_x_warp_pointer(window, x, y)   ;;; warp to x, y

define xved_x_warp_pointer(window);
    lvars window, place = false, x = 1, y = 1;
    if isboolean(window) or window == "inside" then
        (), window -> (window, place)
    elseif isinteger(window) then
        (), window -> (window, x, y)
    endif;
    if isexternal_ptr_class(window) and XptDataType(window) == "Widget" then
        if place == "inside" then
            min(20, XptVal[fast] window(XtN width:XptDimension))  -> x;
            3 -> y;
        elseif place then
            XptVal[fast] window(XtN width:XptDimension,
                                XtN height:XptDimension)
                div 2 + 1 -> y, div 2 + 1 -> x;
        endif
    endif;
    returnunless(window_id(window) ->> window);
    exacc raw_XWarpPointer(xveddisplay, 0, window, 0,0,0,0, x,y);
    sync_display();
enddefine;


;;; Set X keyboard input focus:
;;;     xved_x_set_input_focus(window) - calls XSetInputFocus

define xved_x_set_input_focus(window);
    lvars window;
    returnunless(window_id(window) ->> window);
    exacc raw_XSetInputFocus(xveddisplay, window, RevertToParent,
                                                        0 /* CurrentTime */);
    sync_display();
enddefine;

;;; X calls to Iconify and Deiconify an x window
lvars default_screen = false;
define xved_x_set_map_state(win, open);
    lvars win, open;
    ;;; the "iconic" resource for widgets doesn't work properly, so we
    ;;; use the XMapWindow and XIconifyWindow Xlib calls
    if open then
        exacc raw_XMapWindow(xveddisplay, win)
    else
        unless default_screen then
            exacc raw_XDefaultScreen(xveddisplay) -> default_screen;
        endunless;
        exacc raw_XIconifyWindow(xveddisplay, win, default_screen)
    endif;
enddefine;

define xved_x_query_pointer(xwin) -> (child_win, x, y);
    lvars xwin, child_win, x, y, status;
    unless xwin then XtWindow(xveddummyshell) -> xwin endunless;
    sync_display();
    exacc raw_XQueryPointer(xveddisplay, xwin, XID_dummy, XID_ptr1,
                            int_ptr1, int_ptr2,
                            int_dummy, int_dummy, int_dummy) -> status;
    exacc :XptXID XID_ptr1 -> child_win;
    unless status and child_win /== 0 then false -> child_win endunless;
    exacc :int int_ptr1 -> x;
    exacc :int int_ptr2 -> y;
enddefine;

;;; xved_window_has_mouse(window) -> VAL
;;;         true if the mouse is currently within the window
;;;         false if the mouse isn't in the window at all

define xved_window_has_mouse(win);
    lvars win, child_win = false;
    if window_id(win) ->> win then
        xved_x_query_pointer(win) -> (child_win, , )
    endif;
    child_win and true
enddefine;

;;; test a windows map state
lvars win_attrs = false;
;;;
define :XVED_FOR wved_is_open_window(win);
    lvars win, wid;
    returnunless(window_id(win) ->> wid)("undef");
    unless win_attrs then
        EXPTRINITSTR(:XWindowAttributes) -> win_attrs
    endunless;
    exacc raw_XGetWindowAttributes(xveddisplay, wid, win_attrs);
    exacc :XWindowAttributes win_attrs.map_state == IsViewable
enddefine;
;;;
define :XVED_FOR updaterof wved_is_open_window(open, win);
    lvars open, win, wid;
    returnif(wved_is_open_window(win) == open);
    returnunless(window_id(win) ->> wid);
    xved_x_set_map_state(wid, open);
    if open then
        until wved_is_open_window(wid) do
            fast_XtAppProcessEvent(xvedappcontext, XtIMAll)
        enduntil;
    else
        sync_display();
    endif
enddefine;

;;; xved_is_raised_window - true or false if window is obscurred or not
define xved_is_raised_window(win);
    lvars win wid;
    /* this only works on XpwScrollText widgets - since there is no X
       call for getting the visibility state we can't do this for other
       windows, and so must return value of wved_is_open_window
    */
    returnunless(window_id(win) ->> wid)(false);
    if win.isxvedwin then
        wved_is_open_window(wid) and XptVal[fast] win(XtN visible:XptBoolean);
    else
        wved_is_open_window(wid)
    endif;
enddefine;

define updaterof xved_is_raised_window(val, win);
    lvars val win wid;
    returnunless(window_id(win) ->> wid);
    if val then
        exacc raw_XRaiseWindow(xveddisplay, wid)
    else
        exacc raw_XLowerWindow(xveddisplay, wid)
    endif;
    sync_display();
enddefine;

/* DEFINE WVED WINDOW OPERATIONS to do window raising/mouse warping */

;;; deiconify a window
define :XVED_FOR wved_open_window(win);
    lvars win;
    true -> wved_is_open_window(win);
    if wvedalwaysraise then
        true -> xved_is_raised_window(win);
    endif;
enddefine;

;;; iconify a window
define :XVED_FOR wved_close_window(win);
    lvars win;
    if win == "all" then
        ;;; close all windows
        vedappfiles(procedure();
                        if wved_is_live_window(wvedwindow) then
                            false -> wved_is_open_window(wvedwindow)
                        endif
                    endprocedure)
    else
        false -> wved_is_open_window(win)
    endif
enddefine;

;;; bring window to front
define :XVED_FOR wved_raise_window(win);
    lvars win wid;
    ;;; open and raise
    true -> wved_is_open_window(win);
    true -> xved_is_raised_window(win);
enddefine;

    /*  Return false if a non-Poplog window other than the root has
        input focus.
    */
define lconstant can_set_focus();
    lvars   status, win, def_dpy, focus_win, basewin = xved_basewindow_wid();
    lconstant Xerr = exfunc_export(erase, XptCallbackFlags, false);

    ;;; test if win is a window known to Poplog
    define lconstant is_poplog_win(win);
        lvars win;
        win = basewin
        or (def_dpy and fast_XtWindowToWidget(def_dpy, win))
    enddefine;

    define lconstant search_win_tree(win);
        lvars i, win, children, result;
        returnif(is_poplog_win(win)) (true);
        sync_display();

        ;;; search all child windows
        returnunless(exacc raw_XQueryTree(xveddisplay, win,
                        XID_dummy, XID_dummy, exptr_ptr1, int_ptr1)) (false);

        false -> result;
        exacc :exptr exptr_ptr1 -> children;
        fast_for i to exacc :uint int_ptr1 do
            quitif(search_win_tree(exacc :XptXID[] children[i]) ->> result)
        endfor;
        exacc raw_XFree(children);
        result
    enddefine;

    dlocal % exacc raw_XSetErrorHandler(Xerr),
             exacc (1):void raw_XSetErrorHandler() %;

    sync_display();

    XptIsLiveType(XptDefaultDisplay, "DisplayPtr") and XptDefaultDisplay
                            -> def_dpy;

    ;;; get the focus window
    exacc raw_XGetInputFocus(xveddisplay, XID_ptr1, int_dummy);
    exacc :XptXID XID_ptr1 -> focus_win;

    if focus_win == PointerRoot then
        ;;; get the root window ID
        exacc :XScreen (XtScreen(xveddummyshell)).root -> win;
        ;;; search the tree of windows that the pointer is in
        xved_x_query_pointer(win) -> (win, , );
        if win then
            search_win_tree(win)
        else
            true        ;;; focus window is root
        endif
    else
        focus_win == 0 or is_poplog_win(focus_win)
    endif
enddefine;

;;; warp the input focus/mouse pointer to a specific Ved buffer.
;;; if the user is currently doing something with an XVed or Poplog window
;;; then we warp focus/pointer. Otherwise we only raise the window.
define :XVED_FOR wved_set_input_focus(win);
    lvars win, has_focus, do_pointer, do_focus, had_focus;
    sync_display();

    had_input_focus -> had_focus;
    false -> had_input_focus;

    if win == "basewindow" then
        returnunless(xved_basewindow_wid() ->> win)
    else
        returnif(xvedhasinputfocus == win)
    endif;

    xvedhasinputfocus or had_focus or not(xvedpopfocusonly)
        or can_set_focus() -> has_focus;
    has_focus and xvedwarpmouse -> do_pointer;
    has_focus and xvedsetfocus -> do_focus;

    if wvedalwaysraise
    or (do_pointer and not(xved_window_has_mouse(win))) then
        wved_raise_window(win)
    elseif do_focus then
        ;;; to make quite sure window is viewable before trying to
        ;;; set focus
        wved_open_window(win)
    endif;

    ;;; MOUSE WARP
    if do_pointer and not(xved_window_has_mouse(win)) then
        xved_x_warp_pointer(win, "inside")
    endif;

    ;;; KEYBOARD INPUT FOCUS WARP
    if do_focus and xvedhasinputfocus /== win then
        ;;; do it
        xved_x_set_input_focus(win)
    endif;

    sync_display();
enddefine;

define active xvedclicktime;
    XtGetMultiClickTime(xveddisplay);
enddefine;
define updaterof active xvedclicktime(val);
    lvars val;
    XtSetMultiClickTime(xveddisplay, val);
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 19 1995
        Added xved_active_change_callback
--- John Gibson, Apr  1 1995
        Revised types of EXPTRINITSTRs used in X calls etc.
--- John Gibson, Mar 21 1994
        Changed to use new XptSyncDisplay
--- John Gibson, Feb 23 1994
        3rd arg to xved_focus_change_callback now the integer value directly,
        not an exptr
--- John Gibson, Dec 14 1993
        Put back a procedure xved_x_query_pointer so menubar_xm can use it
--- John Gibson, Dec 14 1993
        Added can_set_focus and rewrote wved_set_input_focus to use it when
        xvedpopfocusonly is true.
--- John Gibson, Sep  7 1992
        Changed to use XptVal
--- John Gibson, Aug 26 1992
        Added typespec declarations in the XptLoadProcedures
--- John Gibson, Mar 28 1992
        Moved 'event' action into xved_init_wm_actions from xvedinput.p
--- John Gibson, Oct 29 1991
        Allowed -window_id- to take a device as argument
--- John Gibson, Sep 23 1991
        Added fast_XptAppTryEvents to sync_display
--- John Gibson, Sep 19 1991
        Ensure wved_set_input_focus opens window if focus is to be set
--- Jonathan Meyer, Sep 16 1991 Added xvedshouldopenwindow
--- Jonathan Meyer, Sep 16 1991 Added xvedclicktime
--- Jonathan Meyer, Sep 11 1991
        Changed guard on xved_x_set_input_focus
--- Jonathan Meyer, Aug 30 1991
        xved_basewindow_wid now returns -false- if popdevin is not a terminal.
--- John Gibson, Aug 12 1991
        Event handlers now take window as first arg and must return their args
--- John Gibson, Aug 10 1991
        Made lconstant intvecs etc writeable
--- John Gibson, Aug  8 1991
        Made wved_set_input_focus just return if window already has focus
--- John Gibson, Aug  6 1991
        Focus changes now handled by callback
--- Jonathan Meyer, Aug  1 1991
        Simplified wved_is_open_window and xved_is_raised_window. Made
        them accepts widgets, ved windows, x windows, or "basewindow" via
        window_id. Rewrote wved_open_window and wved_raise_window.

        Made wm_delete_window action check xvedblockinput and do nothing
        if window is grabbed by ved.
--- John Gibson, Aug  1 1991
        Changed wved_set_input_focus to make sure hasfocus is false if window
        is not open
--- Jonathan Meyer, Jul 30 1991
        Removed checks for xvedwin_isnew (now unnecessary).
        Made call to SetInputFocus use XtLastTimestampProcessed since this
        is less likely to cause race conditions with window managers than
        CurrentTime.
        Added updater of wved_is_open_window and also XIconifyWindow to
        close a window. wved_open_window now assigns to wved_is_open_window.
--- John Gibson, Jul 27 1991
        Moved in wved_is_open_window and changed to use XGetWindowAttributes
--- Jonathan Meyer, Jul  5 1991
        Added xved_is_raised_window
--- John Gibson, Jun 21 1991
        ved_current_file now doesn't fiddle with whether file is on
        statusline or not
--- John Gibson, Jun 19 1991
        Made set_focus_action use vedsetnormalmessage
--- Jonathan Meyer, Jun 18 1991
        Made set_focus_action not refresh status if xvedblockinput is
        true - this stops the QUIT FILE... message from appearing on
        all windows at once.
--- Jonathan Meyer, Jun 18 1991
        Made set_focus action refresh status line
--- Jonathan Meyer, Jun  7 1991
        Corrected wved_set_input_focus. Made checks for new windows so that
        they can be opened iconified. Renamed destroyWindow deleteWindow
--- John Gibson, Jun 15 1991
        Removed wvedbase/textwindow
--- Jonathan Meyer, Jun  5 1991
        Removed Map actions - moved them into ClientMessage.c
--- Jonathan Meyer, Jun  4 1991
        changed xvedwin_ismapped to wved_is_open_window
--- Jonathan Meyer, Jun  3 1991
        Added xvedsetinputfocus
--- Jonathan Meyer, Apr  26 1991
        Chopped code to warp to middle of window and greatly simplified.
--- Jonathan Meyer, Apr  21 1991
        Added warping to middle of window before testing if we need to
        Raise it.
--- Jonathan Meyer, Apr  12 1991
        Rewrote mouse warping code - made more X procedures exported.
--- Jonathan Meyer, Apr  8 1991
        Added code to support XV_DO_DRAG_LOAD client messages
--- Jonathan Meyer, Apr  7 1991
        Added xved_notify_set_focus.
        Improved wved_set_input_focus
 */
