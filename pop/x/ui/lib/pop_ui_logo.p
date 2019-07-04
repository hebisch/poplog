/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/ui/lib/pop_ui_logo.p
 > Purpose:         Poplog UI logo window
 > Author:          Jonathan Meyer, Sep  2 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

uses-now popxlib, Xpw;

section $-poplog_ui => pop_ui_logo;
exload_batch;

include pop_uiP.ph;
include xdefs.ph;
include xpt_xevent.ph;

uses
    xt_init,
    xt_widget,
    xt_callback,
    xt_display,
    xt_widgetinfo,
    xt_resource,
    xt_event,
    xt_popup,
    XpwCore,
    $-poplog_ui$-guiShells,
;


XptLoadProcedures pop_ui_logo
lvars
    XCopyArea
    XTextWidth
    XDrawImageString
    XFlush
    XRaiseWindow
;

lvars   shell = false,
        popped_down = true,     ;;; boolean controls whether to re-center
                                ;;; the window
        graphic;

lvars procedure logo_info;

lconstant
    POPUIBITMAPS = '$usepop/pop/x/ui/bitmaps/',
;


/* returns a list of strings for display in the info field */
define vars default_poplog_strings;
    lvars len = listlength(vedbufferlist);
    [%
    POPUIBITMAPS  dir_>< 'poplog_64.xbm', ;;; filename of xbm

    sprintf(pop_internal_version / 10000.0, 'Sussex Poplog Version %p'),
    '   Copyright \(169) 1982-1996, The University of Sussex   ',
    'All Rights Reserved.',
    '',
    popmemused div 256 sys_>< ' Kilobytes used, ' sys_><
    (popmemlim - popmemused) div 256 sys_>< ' Kilobytes free',
    '    (editing ' sys_>< len sys_>< ' file',
    if len ==  1 then sys_>< ')    ' else sys_>< 's)    ' endif,
    '',
    'Marketed under licence by:', 'Integral Solutions Ltd.',
    'Berk House, Basing View',
    'Basingstoke, Hants.',
    'RG21 4RG, UK',
    '',
    'Tel: +44 (0)1256 55899',
    'Fax: +44 (0)1256 63467',
    '',
    'Email: isl@isl.co.uk',
    'URL: http://www.isl.co.uk',
    ''
    %]
enddefine;

define lconstant refresh_info_box();
    lvars info i,y = 130,
        dpy = XtDisplay(graphic),
        win = XtWindow(graphic),
        (gc, font) = XptVal graphic(XtN usersGC, XtN font),
        width = 0, filename,
        strings = logo_info();

    dest(strings) -> strings -> filename;

    define lconstant center_string(y, string);
        lvars string y, x, len = datalength(string);
        datalength(string) -> len;
        exacc (3):int raw_XTextWidth(font, string, len) -> x;
        width fi_- (x fi_>> 1) -> x;
        exacc (7) raw_XDrawImageString(dpy, win, gc, x, y, string, len)
    enddefine;

    fast_for i in strings do
        fi_max(width,
            exacc (3):int raw_XTextWidth(font, i, datalength(i))) -> width;
    endfast_for;

    false, false,width + 8 ->> width, y-20+length(strings)*15,
            -> XptWidgetCoords(shell);
    width fi_>> 1 -> width;

    center_string(22, hd(strings));
    exacc (10) raw_XCopyArea(dpy,
            XpwGetPixmap(graphic, filename,
                        XptVal graphic(XtN foreground,
                                       XtN background,
                                       XtN depth)),
            win, gc, 0,0,64,64, width - 32,38);

    fast_for i in tl(strings) do
        center_string(y, i);
        y fi_+ 15 -> y;
    endfast_for;
    exacc (1) raw_XFlush(dpy);
enddefine;

/* redraws the info box */
define lconstant refresh_cb(w, client, call);
    lvars w, client, call;
    refresh_info_box();
enddefine;

define lconstant button_cb(w, client, event);
    lvars w, client, event;
    XtPopdown(shell);
    true -> popped_down;
    false; ;;; continue_to_dispatch
enddefine;

/* creates the info box */
lvars hold_event_handler;

;;; pop_ui_logo takes a single argument which can be a list of strings
;;; to be displayed in the logo window, a procedure which returns a list
;;; of strings or -false- in which case the default Poplog strings are
;;; used. Note that the first string is expected to be a pathname to a
;;; bitmap for the icon. The second string in the list will appear
;;; at the top of the window, followed by the icon, followed by any other
;;; strings in the list.
define pop_ui_logo(info);
    lvars info;

    if info then
        if islist(info) then
            identfn(%info%) -> logo_info;
        elseif isprocedure(info) then
            info -> logo_info;
        else
            mishap(info, 1, 'List or procedure needed');
        endif;
    else
        default_poplog_strings -> logo_info;
    endif;

    unless shell and is_valid_external_ptr(shell) then

        ;;; Never removed, so stick it on the fixed hold list
        XptExportEventHandler(button_cb, false,true) -> -> hold_event_handler;
        XtVaCreatePopupShell('poplogInfo',
                            xtTransientShellWidget, pop_ui_app_shell,
                            #| XtN mappedWhenManaged, false |#) -> shell;

        'Poplog: System Information' -> XptVal shell(XtN title:XptString);

        XtVaCreateManagedWidget('Information',
                        ;;; make it same size as parent.
                        xpwCoreWidget, shell, 0) -> graphic;

        XtVaSetValues(graphic, XptVaArgList([{^XtN font 'variable'}]));
        XtAddCallback(graphic, XtN xpwCallback, refresh_cb, false);
        fast_XtAddEventHandler(shell, ButtonPressMask || ButtonReleaseMask,
                false, hold_event_handler, false);
        guiRealizeWidget(shell, Xm_UNMAP);
    endunless;
    refresh_info_box();
    if popped_down then
        XptCenterWidgetOn(shell, "screen");
    endif;
    XtPopup(shell, 0);
    false -> popped_down;
    exacc (2) raw_XRaiseWindow(XtDisplay(shell), XtWindow(shell));
enddefine;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jul 17 1996
        Added ISL's home page URL
--- Robert John Duncan, Jul 20 1995
        Updated ISL address
--- Integral Solutions Ltd, May 12 1995 (Julian Clinton)
        Updated copyright dates and phone numbers.
--- Integral Solutions Ltd (Julian Clinton), Sep 22 1993
        Changed copyright date and ISL email address.
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr  6 1993
        Uses xt widgetclass libraries instead of XptW*idgetSet
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- Integral Solutions Ltd, Aug 10 1992 (Julian Clinton)
        Changed copyright date.
--- Julian Clinton, 10/12/91
    Set XptWMProtocols -false- and modified deleteResponse when using Motif.
--- John Gibson, Nov 21 1991
        Changed name of bitmap file from poplog.64.xbm to poplog_64.xbm
--- Julian Clinton, 14/10/91
    Corrected ISL email address.
    Removed explicit string null terminator.
--- Adrian Howard, Oct  3 1991 : Changed -XptCoerceEventHandler- to
        -XptExportEventHandler-.
--- Integral Solutions Ltd, Sep 18 1991 (Julian Clinton)
    Now only centers widget on screen if not already popped up.
    Added 'All Rights Reserved.' string and modified layout slightly.
    Added extra spaces either side of the "editing" brackets.
    Version now uses -pop_internal_version-.
--- Jonathan Meyer, Sep 16 1991 Added info about number of files being edited
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Allowed pop_ui_logo to take either a list or a procedure so pop_ui_logo
    can be used for other applications and products.
    Added various "uses" so the file is loadable on its own.
    Removed call of sys_grbg_list (was causing corruption of lists).
--- Jonathan Meyer, Sep 11 1991
        Held event handler record so it isn't garbaged.
--- Jonathan Meyer, Sep  3 1991 Made toplevel widget a Popup
--- Jonathan Meyer, Sep  3 1991
 */
