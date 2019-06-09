/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/ved/src/xvedvedtraps.p
 > Purpose:         Defines redefinition of several ved and wved hooks
 > Author:          Jonathan Meyer, Apr  1 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xved_declare.ph'
include xt_constants.ph;
include vedfile_struct.ph;

section $-xved;

/************************************************************************
 * XVed Ved/Wved Procedure Definitions
 ************************************************************************/

define :XVED_FOR vedrestorewindows;
    ;;; crude but simple:
    if vedbufferlist /== [] then
        dlocal vedediting = true;
        vedappfiles(vedrefresh);
    endif;
enddefine;


;;; called by ved_name
define :XVED_FOR wved_ved_name_hook;
    lvars w = wvedwindow, class, type, new_class, new_type;
    lconstant ctnames = [class type], resnames = [title iconName iconPixmap];
    returnunless(isxvedwin(w));

    xved_value(w, ctnames) -> (class, type);
    xved_default_window_value(ctnames) -> (new_class, new_type);

    if new_class = class and new_type = type then
        xved_default_window_value(resnames) -> xved_value(w, resnames);
        CHECK_BUFF_MENU;
    else
        ;;; window class/type changed -- need a new window
        w -> wvedfreewindow;
        false -> wvedwindow;
        ved_save_file_globals();
        vedsetonscreen(ved_current_file, false)
    endif
enddefine;


;;; interrupt - called in veddointerrupt. Unblocks xved's input stream so
;;; that users can start typing again.
define :XVED_FOR wved_do_interrupt_trap();
    if wved_is_live_window(xvedblockinput) then
        true -> XptBusyCursorFeedback(xvedblockinput.xvedwin_shell);
    endif;
    false ->> XptBusyCursorOn -> xvedblockinput
enddefine;

lvars in_apply_action = false;
;;;
define :XVED_FOR ved_apply_action(action) with_props false;
    lvars action, save_busy;
    dlocal
        in_apply_action = true,
        0 % if dlocal_context == 1 then XptBusyCursorOn -> save_busy endif,
            if dlocal_context fi_<= 2 then save_busy -> XptBusyCursorOn endif
          %
        ;

    ;;; setup for changing the busy cursor on
    define lconstant is_busy();
        lconstant set_busy = updater(nonactive XptBusyCursorOn)(%true%);
        if in_apply_action then external_defer_apply(set_busy) endif
    enddefine;

    ;;; Clear the selection if appropriate
    if wved_is_live_window(wvedwindow) then
        if xvedselectionon
        and (xvedselectionon == wvedwindow or xvedselectiononstatus)
        and not(fast_lmember(action, #_< [
            ;;; the following procedures take care of selections themselves
                        ^vedstatusswitch
                        ^xved_process_event
                        ^vedselection_cut
                        ^vedselection_copy
                        ^vedselection_paste
                        ^vedselection_compile] >_#))
        and xvedselectiononstatus ==
            (action==vedenter or action==vedredocommand or ved_on_status)
        then
            xved_dispatch_event(xvedselectionon, "clearSelection",
                                                xvedselectionautocut and action);
        endif;
        false -> xvedselectionautocut;
    endif;

    2e5 -> sys_timer(is_busy);
    fast_apply(action);
    false -> sys_timer(is_busy);

    ;;; set label for current file if it needs changing
    if  ved_current_file and (
            vedwriteable /== ved_current_file(VF_WRITEABLE) or
            (vedchanged and true) /== (ved_current_file(VF_CHANGED) and true)
    ) then
        ved_save_file_globals();
    endif;
enddefine;

;;; Called at the end of every vedprocess loop
define :XVED_FOR vedsetnormalmessage(flush);
    lvars flush, seln_on, seln_on_status;
    xvedselectionon and xvedselectionon == wvedwindow -> seln_on;
    seln_on and xvedselectiononstatus -> seln_on_status;

    vedsetstatus(
        if ved_on_status then
            nullstring, false
        elseif datalength(vedmessage) == 0 then
            ;;; no message to print
            ;;; ensure status line redrawn if line has changed
            if seln_on_status or not(xvedshowfilename) then
                nullstring, false
            else
                vednamestring, true
            endif
        else
            if seln_on_status then
                xved_dispatch_event(xvedselectionon, "clearSelection", false);
                false -> seln_on
            endif;
            vedmessage, true
        endif,
        "undef");   ;;; undef = redraw but don't flush output

    ;;; checks selection display is updated after each vedprocess
    if seln_on and (not(ved_on_status) or xvedselectiononstatus) then
        procedure;
            dlocal ved_on_status = xvedselectiononstatus;
            vedcheck();
            vedselection_adjust(false, false, false, false, false);
        endprocedure()
    endif;

    vedsetcursor();
    if flush then
        0 -> vedscreencharmode;
        vedscr_flush_output()
    endif;
    CHECK_SCROLLBAR;
enddefine;


;;; wved_file_of_window - uses a property to perform the lookup.

lvars fileprop = newproperty([], 20, false, "tmpboth");
;;;
define :XVED_FOR wved_file_of_window(win);
    lvars win;
    fileprop(win)
enddefine;

define :XVED_FOR wved_window_of_file(file);
    lvars file;
    subscrv(VF_WINDOW, file);
enddefine;
;;;
;;; sets window of file in fileprop as well
define :XVED_FOR updaterof wved_window_of_file(win, file);
    lvars win, file, oldwin;
    if win and not(win.isxvedwin) then
        mishap(win,1,'Ved WINDOW NEEDED');
    endif;
    subscrv(VF_WINDOW, file) -> oldwin;
    if oldwin and win then
        mishap(win, file, 2, 'FILE ALREADY HAS A WINDOW')
    endif;
    win -> subscrv(VF_WINDOW, file);
    if win then
        file -> fileprop(win)
    else
        false -> fileprop(oldwin)
    endif
enddefine;

;;; switches between window and full-sized window
define :XVED_FOR wved_ved_set_window();
    lvars num, had_mouse;
    XptSyncDisplay(xveddisplay);
    true -> xved_is_raised_window(wvedwindow);
    fast_XtAppProcessEvent(xvedappcontext, XtIMAll);
    XptSyncDisplay(xveddisplay);

    xvedwarpmouse and xved_window_has_mouse(wvedwindow) -> had_mouse;
    unless (xved_value("defaultWindow", "numRows") ->> num).isinteger then
        24 -> num;
    endunless;
    if vedscreenlength fi_> num then
        num,
    else
        num fi_* 2,
    endif -> xved_value("currentWindow", "numRows");
    ;;; wait for size change to happen
    XptSyncDisplay(xveddisplay);

    ;;; make most of it visible
    XptMaxWidgetVisibility(wvedwindow);
    if had_mouse and not(xved_window_has_mouse(wvedwindow)) then
        xved_x_warp_pointer(wvedwindow, "inside");
    endif
enddefine;

define :XVED_FOR wved_ved_quit_file(editing_new_file);
    lvars editing_new_file;
    unless editing_new_file then
        if vedbufferlist == [] then
            ;;; tidy up before quitting last ved file
            xved_before_editor_exit();
        endif;
        ;;; may need to update the buffer list menu
        CHECK_BUFF_MENU;
    endunless
enddefine;


;;; --- SAVE AND SET GLOBALS ------------------------------------------

define :XVED_FOR wved_save_globals(file);
    lvars file, flags, w = wvedwindow, sdvec;
    unless wved_is_live_window(w) then
        if subscrv(VF_WINDOW, file) then
            ;;; had a window, now gone
            false -> wved_window_of_file(file)
        endif;
        return
    endunless;

    xvedwin_screendatavec(w) -> sdvec;
    vedscreenline -> fast_subscrv(SDVEC_SCREEN_LINE,sdvec);
    vedscreencolumn -> fast_subscrv(SDVEC_SCREEN_COL,sdvec);

    if vedwriteable /== file(VF_WRITEABLE)
    or (vedchanged and true) /== (file(VF_CHANGED) and true)
    then
        xved_default_window_value("title") -> wved_window_label(w);
    endif;
enddefine;

define :XVED_FOR wved_set_globals(file);
    lvars file, n, w = wvedwindow, sdvec;
    returnunless(wved_is_live_window(w));

    0 -> vedscreenoffset;

    xvedwin_screendatavec(w) -> sdvec;
    fast_subscrv(SDVEC_SCREEN_LINE,sdvec) -> vedscreenline;
    fast_subscrv(SDVEC_SCREEN_COL, sdvec) -> vedscreencolumn;

    file -> vedupperfile;
    if fast_XtIsRealized(w) then
        wved_set_size(w)        ;;; makes sure vedstatusline is OK
    endif
enddefine;


#_IF DEF POPC_COMPILING
    /* Otherwise there are no perm idents initialised in the file */
constant xvedvedtraps = true;
#_ENDIF

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  3 1997
        Changes for variable-width mode
--- John Gibson, May  2 1997
        Moved ST_ defs to xved_declare.ph
--- John Gibson, Apr 29 1997
        Added xvedb*uffer16 to screenout*state in wved_{save,set}_globals.
--- John Gibson, Sep 12 1996
        Changed sense of argument to wved_ved_quit_file.
--- John Gibson, Oct  5 1995
        Changed is_busy in ved_apply_action so it uses external_defer_apply
        to set the busy cursor on -- otherwise the updater of XptBusyCursorOn
        is liable to get called inside an async X callback (leading to looping
        problems in the toolkit, etc)
--- John Gibson, Jan 24 1994
        Made wved_ved_name_hook create a new window if window class or type
        has changed.
--- John Gibson, Jan 16 1994
        Renamed w*ved_apply_action as ved_apply_action
--- John Gibson, Nov 16 1993
        Made wved_save_globals assign false to wved_window_of_file for file
        if wvedwindow has become false.
--- John Gibson, Dec 19 1992
        Removed wved_window_r*esized and moved code into resize_callback
        in xvedwindows.p
--- John Gibson, Oct  9 1992
        Made fileprop "tmpboth" instead of "tmparg" so that prop entry is
        not retained if file is garbage. Simplified wved_file_of_window
--- Adrian Howard, Sep  8 1992
        wved_ved_name_hook now does nothing if wvedwindow is not initialised.
--- Adrian Howard, Sep  3 1992
        w*ved_apply_action now calls its given procedure under all conditions.
--- John Gibson, Aug 13 1992
        Added call of fast_XptAppTryEvents after xved_*x_sync in
        wved_ved_set_window
--- John Gibson, Jun  5 1992
        vednamestring is not displayed if xvedshowfilename is false
        (application resource ShowFileName)
--- Adrian Howard, Mar 12 1992 : Stopped -vednamestring- being set to null when
    -vedfileprops- was -false-. Not all people are running window managers with
    a title bar (!)
--- John Gibson, Dec 21 1991
        Changed wved_set/save_globals to save and restore xvedscreench*armode
--- John Gibson, Dec 12 1991
        Fixes to wved_do_interrupt_trap and w*ved_apply_action to correct
        handling of XptBusyCursorOn
--- John Gibson, Nov 19 1991
        Added test for xvedwarpmouse in wved_ved_set_window
--- John Gibson, Sep 23 1991
        Added xved_*x_sync to wved_ved_set_window
--- Jonathan Meyer, Sep  5 1991
        Added in_apply_action.
--- John Gibson, Aug 26 1991
        Made selections work on status line
--- John Gibson, Aug 13 1991
        Fixed wved_ved_set_window so that it keeps the pointer in the window
--- Jonathan Meyer, Aug  2 1991 added vedrestorewindows
--- Jonathan Meyer, Jul 31 1991
        Added wved_window_resized, moved vedscreenraw and vedscreencooked
        into xvedscreen.p
--- Jonathan Meyer, Jul 29 1991
        Added call to xved_before_editor_exit
--- John Gibson, Jul  9 1991
        VF_PW*MWINDOW -> VF_WINDOW
--- Jonathan Meyer, Jul  8 1991
    Added wved_ved_quit_file
--- Jonathan Meyer, Jul  6 1991
    Added XptMaxWidgetVisibility test
--- Jonathan Meyer, Jul  4 1991
        MOVED ERROR HANDLERS to xvederrors.p
        Made XIO_sys_error_handler much more sophisticated.
--- John Gibson, Jul  4 1991
        Fixed XIO_sys_error_handler
--- John Gibson, Jul  2 1991
        Made w*ved_apply_action not send clearSelection unless selection
        is in current window
--- John Gibson, Jun 27 1991
        Removed unnecessary redefinition of -ved_get_reply-
--- Jonathan Meyer, Jun 25 1991
        Changed set_window to move currentwindow up and down a pixel
--- John Gibson, Jun 24 1991
        Improved -wved_mishap_reset-
--- John Gibson, Jun 21 1991
        Uses ved_save_file_globals
--- Jonathan Meyer, Jun 18 1991
        Added wved_ved_set_window
--- Jonathan Meyer, Jun 18 1991
        Added vedscreencooked. Redefined vedscreenraw.
--- John Gibson, Jun 15 1991
        Removed wvedbase/textwindow
--- John Gibson, Jun 13 1991
        Added -wved_set_globals-. Removed definition of -wved_set_size- (not
        called now except by -wved_set_globals- and -vedwinresized-)
--- John Gibson, Jun 13 1991
        Removed wved_check_window_size
--- John Gibson, Jun 13 1991
        Changed wved_save_globals to take ved file as argument rather than
        window
--- Jonathan Meyer, Jun  6 1991
        Added w*ved_apply_action to make label appear correctly
--- Jonathan Meyer, Jun  5 1991
        Added wved_window_of_file - uses a property so that windows
        can exist for buffers without being in vedbufferlist.
--- Jonathan Meyer, Jun  4 1991
        Removed code in wved_ved_process_trap
--- Jonathan Meyer, Jun  3 1991
        Changed to use xved_default_window_value
 */
