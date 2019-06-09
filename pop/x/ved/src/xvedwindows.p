/*  --- Copyright University of Sussex 1997.  All rights reserved. ---------
 >  File:           C.x/x/ved/src/xvedwindows.p
 >  Purpose:        Multi Window VED - window management code.
 >  Author:         Jonathan Meyer, 20 July 1990 (see revisions)
 >  Documentation:  HELP *xved
 >  Related Files:
 */
compile_mode :pop11 +strict;

/* ==== Window Creation And Management ================================== */

uses-now Xpw;

section $-xved;

#_INCLUDE 'xved_declare.ph'
include xt_constants.ph;
include xpt_xtypes.ph;
include xved_constants.ph;
include vedfile_struct.ph;
include XpwScrollText.ph;

weak constant procedure (
        create_parent_xm, create_parent_xol,
        xved_init_menubar, xved_init_scrollbar, xved_init_hscrollbar
    );

XptLoadProcedures xvedwindows [^^XPW_EXLIBS]
lvars
    XpwSetWMProtocols(widget) :void,
    XInternAtom(dpy,atom_name,only_if_exists) :XptAtom,
;

uses xpwCompositeWidget, xpwScrollTextWidget;

vars xvedmaxwindows = 1e6;


;;; Resize callback handler

define lconstant resize_callback(w, client, call);
    lvars w, client, call, file;
    dlocal pop_asts_enabled = false;
    XptRegister(w) -> w;
    returnunless(wved_file_of_window(w) ->> file);
    if xved_size_hints_set(w) then true -> xved_size_hints_set(w) endif;

    procedure(w, file);
        dlocal vedediting = true, ved_current_file = file;
        xved_set_varwidthmode();
        wved_set_size(w);
        vedrefresh();
        vedscr_flush_output();
        CHECK_SCROLLBAR;
    endprocedure(w, file)
enddefine;

define xved_size_hints_set(w);
    lvars w;
    XptVal[fast] (w.xvedwin_shell)(XtN widthInc:int) /== 0
enddefine;
;;;
define updaterof xved_size_hints_set(set, w);
    lvars   set, w, shell, sw, sh, fwidth, fheight, basewidth, baseheight,
            minwidth, minheight, ncols, nrows;

    ;;; Unless window is realized, shell size isn't valid; we mustn't
    ;;; set incorrect size hints because e.g. mwm makes the window conform
    ;;; to the hints when it's created.
    returnunless(XtIsRealized(w.xvedwin_shell ->> shell));

    dlocal pop_asts_enabled = false;

    if set then
        XptVal[fast] w(XtN fontWidth:int, XtN fontHeight:int,
                                XtN numColumns:int, XtN numRows:int)
                -> (fwidth, fheight, ncols, nrows);
        XptWidgetCoords(shell) -> (, , sw, sh);
        sw - ncols*fwidth -> basewidth;
        sh - nrows*fheight -> baseheight;
        basewidth + fwidth*12 -> minwidth;      ;;; allow for status head
        baseheight + fheight*2 -> minheight;    ;;; allow status + 1 line
    else
        ;;; turn off -- set all values zero
        0 ->> fwidth ->> fheight ->> basewidth ->> baseheight
            ->> minwidth -> minheight
    endif;

    (minwidth, minheight, fwidth, fheight, basewidth, baseheight)
    -> XptVal[fast] shell(XtN minWidth:int, XtN minHeight:int,
                          XtN widthInc:int, XtN heightInc:int,
                          XtN baseWidth:int, XtN baseHeight:int)
enddefine;


define lconstant create_text_widget(parent, gui_textargs) -> widget;
    lvars   widget, w, parent, shell, gui_textargs, textargs, icon,
            buffer16;

    define lconstant file_inchar_mode();
        lvars i, b = vedbuffer, endline = datalength(vedbuffer);
        fast_for i to endline do
            returnif(isstring16(fast_subscrv(i,b)))
                                (XpwICMUnicode, true)
        endfor;
        (XpwICMISOLatin1, false)
    enddefine;

    xved_set_args(gui_textargs, true);
    [%  xved_make_create_args(true),
        XtN defaultColorMask,   2:1010101010101010,
;;;     XtN drawShadowMask,     2:1111110000000000,
        XtN noGrayScale,        1,
        XtN autoFlush,          0,
        XtN statusStyle,        if xved_value("application","StatusAtTop") then
                                    1
                                else
                                    2
                                endif,
        XtN numFixedColumns,    1,
        XtN numStatusFixedColumns,  vedstatusheaderlen+1,
        XtN inputCharMode,      (file_inchar_mode() -> buffer16),
    %] -> textargs;
    xved_set_args(textargs, false);
    sys_grbg_list(textargs);


    fast_XtVaCreateManagedWidget(xvedisincreate(2),
                    xpwScrollTextWidget, parent, xved_va_arg_list()) -> w;

    defexacc lconstant ptrint ^int;

    XptAddCallback(w, XtN xpwCallback, resize_callback, false, identfn);
    XptAddCallback(w, XtN keyboardEvent, xved_key_callback, false, ptrint);
    XptAddCallback(w, XtN buttonEvent, xved_button_callback, "button", ptrint);
    XptAddCallback(w, XtN motionEvent, xved_button_callback, "motion", ptrint);
    XptAddCallback(w, XtN focusChange, xved_focus_change_callback, false, ptrint);
    XptAddCallback(w, XtN activeChange, xved_active_change_callback, false, identfn);

    XptShellOfObject(parent) -> shell;
    XptVal shell(XtN iconWindow:XptWindow) -> icon;

    consxvedwin(
        "Widget",               ;;; datatype
        w,                      ;;; widget_ptr
        w,                      ;;; widget
        parent,                 ;;; parent
        shell,                  ;;; top level shell
        icon,                   ;;; icon window
        false,                  ;;; configure
        weakref[xvedmenubar] xved_init_menubar,
                                ;;; menubar
        weakref[xvedscrollbar] xved_init_scrollbar,
                                ;;; scrollbar
        weakref[xvedscrollbar] xved_init_hscrollbar,
                                ;;; hscrollbar
        undef,                  ;;; props
        undef,                  ;;; globals
        xvednexteventvec,       ;;; eventvec
        xvednextkeyseqvec,      ;;; keyseqvec
        true,                   ;;; isnew
        false,                  ;;; positionstack
        xved_init_screendatavec(buffer16) ;;; screendatavec
    ) ->> XptRegister(w) -> widget;
    false ->> xvednexteventvec -> xvednextkeyseqvec;
    wved_destroy_window -> sys_destroy_action(widget);
enddefine;

define lconstant create_window();
    lvars   shellargs, shell;
    dlocal  pop_asts_enabled = false;

    /*  For vanilla */
    define lconstant create_parent(shell, class, create_text) -> text;
        lvars shell, class, procedure create_text, text, parent;
        fast_XtVaCreateManagedWidget(class, xpwCompositeWidget, shell, 0)
                                                -> parent;
        ;;; create text widget
        create_text(parent, []) ->> text
                                -> XptVal[fast] parent(XtN workArea:XptWidget);

        ;;; Make the bg colour of the Form same as Text widget
        XptVal[fast] text(XtN background:XptPixel)
                            -> XptVal[fast] parent(XtN background:XptPixel);

        false   ->> text.xvedwin_menubar
                ->> text.xvedwin_scrollbar -> text.xvedwin_hscrollbar
    enddefine;


    ;;; create shell

    [%  xved_make_create_args(false),
        XtN allowShellResize,   true,
        XtN geometry,           false,      ;;; set before shell is realized

        ;;; grisly hack for HP-VUE
        if exacc raw_XInternAtom(xveddisplay,'_VUE_WORKSPACE_PRESENCE',true)
                                    /== 0 then
            XtN maxWidth,       16:7FFF,
            XtN maxHeight,      16:7FFF,
        endif
    %] -> shellargs;
    xved_set_args(shellargs, true);

    fast_XtAppCreateShell(false, XV_CLASS_NAME, xtApplicationShellWidget,
                            xveddisplay, xved_arg_list()) -> shell;


    ;;; create parent composite widget

    if xvedvanilla then
        create_parent
    elseif testdef create_parent_xm then
        weakref create_parent_xm
    elseif testdef create_parent_xol then
        weakref create_parent_xol
    else
        create_parent
    endif(shell, xvedisincreate(1), create_text_widget)
                    ->> wvedwindow -> wved_window_of_file(ved_current_file);
    wved_set_globals(ved_current_file);

    ;;; N.B. Prior to the previous call to create the widget,
    ;;; shellargs was the only thing holding onto the iconwindow structure
    ;;; (if there was one). So it's essential we don't garbage it until now.
    sys_grbg_list(shellargs);

    ;;; create subparts as appropriate
    xved_get_subpart(wvedwindow, xvedwin_menubar,    XtN menubarOn) -> ;
    xved_get_subpart(wvedwindow, xvedwin_scrollbar,  XtN scrollbarOn) -> ;
    xved_get_subpart(wvedwindow, xvedwin_hscrollbar, XtN hscrollbarOn) -> ;
enddefine;

    /*  Called by vedsetonscreen for ved_current_file
    */
define :XVED_FOR wved_create_window();
    lvars   parent, shell, dest_window, x, y, pos, return_value, nfiles;

    define lconstant exit_action(context);
        lvars context;
        if context == 2 then
            ;;; abnormal exit - reset wvedwindow and destroy new window
            if shell then XtDestroyWidget(shell) endif;
            false ->> wvedwindow -> wved_window_of_file(ved_current_file)
        endif;
        if dest_window then wved_destroy_window(dest_window); endif;
    enddefine;

    dlocal pop_charout_device, pop_charerr_device;
    dlocal 0 % false->>shell->dest_window, exit_action(dlocal_context) %;
    dlocal XptWMProtocols = false;

    if isveddevice(pop_charout_device) then
        popdevout -> pop_charout_device
    endif;
    if isveddevice(pop_charerr_device) then
        popdeverr -> pop_charerr_device
    endif;

    ;;; This list is used by xved_default_window_value as the first
    ;;; arg to xved_value
    dlocal xvedisincreate = [% xved_default_window_value([class type]) %];

    define lconstant find_free_win();
        lvars file, maxwin = xvedmaxwindows, lastwinfile;
        fast_for file in tl(vedbufferlist) do
            if subscrv(VF_WINDOW, file) then
                maxwin fi_- 1 -> maxwin;
                file -> lastwinfile
            endif
        endfor;
        if maxwin fi_<= 0 then
            dlocal ved_current_file = lastwinfile;
            wvedwindow -> wvedfreewindow;
            false -> wvedwindow
        endif
    enddefine;

    define lconstant trans_free_res(win);
        lvars win;
        lconstant res_list = [  numColumns numRows x y varWidthMode
                                fontSet boldFontSet altFontSet boldAltFontSet];
        lvars   l, nxt,
                winres = [% xved_value(win, res_list) %],
                nxtres = [% xved_value("nextWindow", res_list) %];
        fast_for l on winres do
            sys_grbg_destpair(nxtres) -> (nxt, nxtres);
            ;;; don't override an existing "nextWindow" value
            if nxt /== "undef" then nxt -> hd(l) endif
        endfor;
        dl(winres) -> xved_value("nextWindow", res_list);
        sys_grbg_list(winres)
    enddefine;


    if (listlength(vedbufferlist) ->> nfiles) == 1 then
        ;;; going from 0 to 1 files -- run editor entry actions
        xved_after_editor_entry()
    elseif not(wvedfreewindow)
    and xvedmaxwindows fi_> 0 and nfiles fi_> xvedmaxwindows then
        find_free_win()
    endif;

    if wvedfreewindow then
        ;;; We never reuse a window. Instead we recreate a window
        ;;; where the last one was
        wvedfreewindow, false -> (dest_window, wvedfreewindow);
        trans_free_res(dest_window)
    endif;

    ;;; remember if there were next window size(s)
    lvars (next_ncols, next_nrows)
                = xved_value("nextWindow", [numColumns numRows]);

    ;;; create window
    create_window();

    wvedwindow.xvedwin_shell -> shell;
    wvedwindow.xvedwin_parent -> parent;

    ;;; do autosizing of window
    lvars ageom = XptVal wvedwindow(XtN autoGeometry:XptString);
    if ageom then
        [% sys_parse_string(ageom, `/`) %] -> ageom;
        lvars (, , maxcols, maxrows) = XptParseGeometry(dest(ageom)->ageom),
              (, , mincols, minrows) =  if ageom /== [] then
                                            XptParseGeometry(hd(ageom))
                                        else
                                            dupnum(false, 4)
                                        endif;
        maxcols and not(isinteger(next_ncols))
        and max( min(xved_max_linewidth(false)+2,maxcols),
                                                max(mincols or 0, 12)),
        maxrows and not(isinteger(next_nrows))
        and max( min(vvedbuffersize+2,maxrows), max(minrows or 0, 4)),
        -> XptVal wvedwindow(XtN numColumns:int <OPT>, XtN numRows:int <OPT>)
    endif;

    ;;; set the X and Y after the window is created and we know its size
    lconstant xy = [x y];

    ;;; Make xvedautoplace defer to a "nextWindow" setting, but override
    ;;; any "defaultWindow" setting
    if xvedautoplace and not(xved_value("nextWindow", xy) -> (x, y),
                                isinteger(x) and isinteger(y)) then
        xved_make_window_position() -> xved_value("nextWindow", xy)
    endif;

    xved_default_window_value(xy) -> (x, y);    ;;; next or default
    unless isinteger(x) and isinteger(y) then
        ;;; see if text widget has a geometry resource specifying a position
        lvars (gx, gy, , ) =
                XptParseGeometry(XptVal wvedwindow(XtN geometry:XptString));
        if gx and gy then (gx, gy) -> (x, y) endif
    endunless;

    if isinteger(x) and isinteger(y) then
        (x, y, false, false) -> XptWMShellCoords(shell) ;;; sets XtN geometry
    endif;

    ;;; don't map when we realize, because we want the shell to stay
    ;;; in the withdrawn state until we have set the WM_PROTOCOLS field.

    fast_XtSetMappedWhenManaged(shell, false);

    fast_XtRealizeWidget(shell);

    ;;; set size hints on -- can't do this until window is realized
    true -> xved_size_hints_set(wvedwindow);

    ;;; setup the cursor plane of the shell
    true ->> XptGarbageCursorFeedback(shell) -> XptBusyCursorFeedback(shell);

    ;;; set WM_PROTOCOLS to WM_TAKE_FOCUS and WM_DELETE_WINDOW
    ;;; add event handlers for above, and also for XV_DO_DRAG_LOAD
    exacc raw_XpwSetWMProtocols(wvedwindow);

    xved_dispatch_event(wvedwindow, "createWindow", ved_current_file);

    ;;; now we can map
    fast_XtSetMappedWhenManaged(shell, true);
    fast_XtMapWidget(shell);

    ;;; when the shell has input focus, make the toolkit redirect keyboard
    ;;; events from the shell to the widget (and generate synthetic
    ;;; FocusIn and FocusOut events for the widget).
    fast_XtSetKeyboardFocus(shell, wvedwindow);

    xved_set_varwidthmode();
    wved_set_size(wvedwindow);
    CHECK_BUFF_MENU;

    ;;; return true if input focus to be warped to window
    not(XptVal shell(XtN iconic:XptBoolean))
enddefine;

define :XVED_FOR wved_destroy_window(w);
    lvars w, shell, iconw, dest_proc;

    if w == wvedfreewindow then false -> wvedfreewindow endif;
    if w == wvedwindow then false -> wvedwindow endif;
    if w == xvedselectionon then
        xved_dispatch_event(w, "clearSelection", wved_destroy_window)
    endif;

    ;;; kill widget if alive
    w.xvedwin_shell -> shell;
    returnunless (exacc [fast] ^int shell /== 0);

    ;;; notify everyone of windows demise
    xved_dispatch_event(w, "destroyWindow",w);

    XptVal shell(XtN iconWindow:XptWindow) -> iconw;
    if sys_process_destroy_action(iconw) ->> dest_proc then
        dest_proc(iconw)
    endif;

    if sys_process_destroy_action(shell) ->> dest_proc then
        dest_proc(shell)
    endif;
enddefine;

define :XVED_FOR wved_window_size(w);
    lvars w;
    XptVal w(XtN numColumns:int, XtN numRows:int)
enddefine;

define :XVED_FOR wved_window_label(w);
    lvars w;
    XptVal (w.xvedwin_shell)(XtN title:XptString);
enddefine;
;;;
define :XVED_FOR updaterof wved_window_label(w);
    lvars w;
    -> XptVal (w.xvedwin_shell)(XtN title:XptString);
enddefine;
;;;
;;; used by xvedresources.p
constant procedure xved_window_label = XVED_FOR_wved_window_label;

define :XVED_FOR wved_is_live_window(w);
    lvars w;
    ;;; check that w is an external widget pointer with a valid widget
    (w.isxvedwin and exacc [fast] ^int w /== 0);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  3 1997
        Made create_window call wved_set_globals immediately after assigning
        new window to wvedwindow; changed previous call of wved_set_globals at
        end of wved_create_window to be just wved_set_size.
--- John Gibson, Aug 18 1997
        Changes for variable-width mode
--- John Gibson, May  2 1997
        Made create_text_widget set XtN inputCharMode straight to
        XpwICMUnicode if file contains any 16-bit strings.
--- John Gibson, Apr 30 1997
        Uses fontSet resource instead of font, etc.
--- John Gibson, Apr 29 1997
        Made create_text_widget set new XtN inputCharMode to XpwICMISOLatin1.
--- John Gibson, Nov 12 1996
        wved_create_window now sets up xvedisincreate as a list [class type]
        enabling xved_default_window_value to select resources for the
        appropriate class and type of the window.
--- John Gibson, Apr 16 1996
        Added autosizing of windows from autoGeometry resource.
--- John Gibson, Jan 17 1996
        Moved silly procedure wved_g*et_one_input to $popvedlib
--- John Gibson, Oct 19 1995
        Added callback for XtNactiveChange
--- John Gibson, May  3 1994
        sys_s*ignals_enabled -> pop_asts_enabled
--- John Gibson, Feb 23 1994
        Uses new XptAddCallback
--- John Gibson, Feb 11 1994
        Added horiz scrollbar support
--- John Gibson, Feb  8 1994
        Made wved_create_window respect existing "nextWindow" values when
        transferring wvedfreewindow's values to "nextWindow"
--- Robert John Duncan, Feb  2 1994
        Added missing '_' to _VUE_WORKSPACE_PRESENCE
--- John Gibson, Jan 21 1994
        Removed wved_is_ch*anged_window and isch*anged field in xvedwin
        -- not needed.
--- John Gibson, Jan 10 1994
        Added setting of statusStyle text resource from StatusAtTop
        application resource
--- John Gibson, Jan  8 1994
        For HP-VUE, put back maxWidth/maxHeight conditional on the presence
        of X atom VUE_WORKSPACE_PRESENCE
--- John Gibson, Jan  6 1994
        Removed maxWidth & maxHeight from shell args
--- John Gibson, Nov 29 1993
        Added xvedmaxwindows and code to use it.
--- John Gibson, Jun  3 1993
        Stuff from xved*shell.p moved into this file
--- John Gibson, Dec 18 1992
        Combined wved_w*indow_width & height into wved_window_size
--- John Gibson, Sep  7 1992
        Made pop_charout_device be dealt with same as pop_charerr_device
        in wved_create_window
--- John Gibson, Sep  6 1992
        Changed to use XptVal
--- John Gibson, Aug 17 1992
        Renamed xved_set_size_hints as xved_size_hints_set with updater so
        that hints can be turned off dlocally
--- John Gibson, Jul 27 1992
        Changed initialisation of xvedwin scrollbar field to false
--- John Gibson, Jun 24 1992
        Added XtNnoGrayScale = true to text create args
--- Adrian Howard, Jun 12 1992
        -wved_destroy_window- now only destroys widgets if we are in the same
        process XVed was invoked in
--- John Gibson, Jun  2 1992
        Made xved_set_size_hints set shell minWidth and minHeight
--- John Gibson, Jan 13 1992
        Added altFont etc to things carried across from wvedfreewindow
--- John Gibson, Dec 28 1991
        Added another field to -consxvedwin-
--- John Gibson, Dec 18 1991
        Made wved_create_window copy boldFont from wvedfreewindow
--- John Gibson, Nov 16 1991
        Added extra fields to -consxvedwin-
--- John Gibson, Nov  4 1991
        Text widget now has its own XtNgeometry resource -- changes
        to deal with this.
--- John Gibson, Oct 11 1991
        Moved call of fast_XtSetKeyboardFocus to after shell is realized
        (otherwise, doesn't work with vanilla xved using Motif widget set)
--- John Gibson, Oct  3 1991
        Reorganised where text args get setup, and changed setting of
        window position
--- John Gibson, Sep 17 1991
        Added local setting of pop_charerr_device in wved_create_window
--- Jonathan Meyer, Aug 30 1991 Set XptCursorPlane
--- John Gibson, Aug 29 1991
        Made font be inherited from wvedfreewindow
--- Jonathan Meyer, Aug 17 1991
        XptGetWMShellCoords -> XptWMShellCoords (now has updater)
--- John Gibson, Aug  9 1991
        Removed buttonClickEvent callback -- now done by buttonEvent
--- John Gibson, Aug  6 1991
        Added focus change callback
--- Jonathan Meyer, Jul 31 1991 Changed Wm -> WM
--- Jonathan Meyer, Jul 31 1991
        Added call to xved_before_editor_entry
--- Jonathan Meyer, Jul 30 1991
        Added test for wved_is_open_window in wved_create_window so that
        you can correctly create iconic windows
--- John Gibson, Jul 27 1991
        Expose callback is now just resize callback; got rid of
        xvedwaitingforexpose.
--- John Gibson, Jul 27 1991
        Moved wved_is_open_window to xvedwm.p
--- Jonathan Meyer, Jul  5 1991
        Moved the x/y resource setting to after the create and before the
        map.
--- Jonathan Meyer, Jun 24 1991
        Added call to XptCoerceCallback
--- Jonathan Meyer, Jun 24 1991
        Added xved_set_size_hints to make windows size appear in font
        units.
--- John Gibson, Jun 22 1991
        Changed wved_create_window to take no args at all -- file is always
        ved_current_file. Also calls wved_set_size on window at end
--- Jonathan Meyer, Jun 18 1991
        Made better exit action on wved_create_window (I hope).
--- Jonathan Meyer, Jun 18 1991
        Changed wved_create_window to take only a file arg.
--- Jonathan Meyer, Jun 17 1991
        Added createWindow/destroyWindow messages.
--- John Gibson, Jun 15 1991
        Removed wvedbase/textwindow
--- Jonathan Meyer, Jun  5 1991
        Removed Shell translations altogether
--- Jonathan Meyer, Jun  4 1991
        Removed xvedwin_ismapped
--- Jonathan Meyer, Jun  3 1991
        Made use of xved_value in xvedresources.p
--- John Gibson, Jun  1 1991
        Put shellargs in a list in wved_create_window
--- Jonathan Meyer, Apr  7 1991
        Moved xvedsetup to xvedsetup.p and renamed it xved_x_setup
--- Jonathan Meyer, Apr  7 1991
        Removed wved_open_window (see xvedwm.p)
        Tidied - moved some declarations to xveddeclare.p
--- Jonathan Meyer, Apr  4 1991
        Changed destroy_window to work properly for v14.0.
        Set XptAsync* in xvedsetup.
--- Jonathan Meyer, Apr  1 1991
        Added fix for destroy_window because of
        bug in early version of XtDestroyWidget
 */
