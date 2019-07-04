/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-librarytool_xol.p
 > Purpose:         Open Look library tool
 > Author:          Julian Clinton, May 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now Xol;

section $-poplog_ui;

include pop_uiP.ph;
include xpt_coretypes.ph;
include XolConstants.ph;

uses
    xt_init,
    xt_widget,
    xt_callback,
    xt_event,
    xt_popup,
    xt_grab,
    xt_widgetinfo,
    xt_resource,

    XpolListItems,

    xolTextFieldWidget,
    xolStaticTextWidget,
    xolCaptionWidget,
    xolScrollingListWidget,
    xolExclusivesWidget,
    xolRectButtonWidget,
    xolOblongButtonWidget,
    xolPopupWindowShellWidget,
;


/* ---------------------------------------------------------------
    Local Variables and Constants
   --------------------------------------------------------------- */

lconstant librarytool_switch_vec= INIT_librarytool_switch_vec;
constant librarytool_xol        = librarytool_switch_vec;

;;; widgets
lvars
    librarytool_widget = false,
    libname_widget
    liblist_widget
    pop11_button
    lisp_button
    prolog_button
    ml_button
    help_widget
    compile_widget
    show_widget
    dismiss_widget
    footer_widget
;

lvars
    last_lib_select_time = 0,
    current_lib_subsystem = "pop11",
    current_lib_string = false,
    current_lib_token = false,
    ss_widget,          ;;; will be a property of subsystem name to widget
;

lconstant
    list_char_width = 45,
;

/* ---------------------------------------------------------------
    Displaying Info
   --------------------------------------------------------------- */

define :macexpr p_SET_LIBTOOL_FOOTER(msg);
    lvars msg;
    msg -> XptVal footer_widget(XtN string:XptString);
enddefine;

define lconstant clear_footer =
    p_SET_LIBTOOL_FOOTER(%nullstring%)
enddefine;


/* ---------------------------------------------------------------
    Callbacks
   --------------------------------------------------------------- */

/*
 * Bug in pre-3.0 OW means pushpin resource ignored so we need this hack
 */
#_IF XOL_VERSION < 3000
define lconstant verify_cb(widget, client_data, call_data);
    lvars widget, client_data, call_data;
    ;;; WE POPDOWN IF PUSHPIN NOT IN AND OTHER VERIFY CBs HAVE
    ;;; NOT CHANGED THE -call_data- STATE
    exacc :XptBoolean call_data and
        not(XptVal widget(XtN pushpin:short)==OL_IN)
        -> exacc :XptBoolean call_data;
enddefine;
#_ENDIF

;;; Called when the 'Dismiss' button is pressed
define lconstant libtool_dismiss_cb(w,client,call);
    lvars w, client, call;

    clear_footer();
    XtPopdown(librarytool_widget);
enddefine;


;;; callback for user changing which subsystem libraries to look at
define lconstant libtool_subsystem_cb(widget, clientdata, calldata);
lvars widget clientdata calldata;
    clear_footer();
    clientdata -> current_lib_subsystem;
    pop_ui_libdescriptors(current_lib_subsystem) ->
                                XpolListItems(liblist_widget);
    false -> current_lib_string;
    nullstring -> XptVal libname_widget(XtN string:XptString);

    compile_widget, if is_subsystem_loaded(current_lib_subsystem) then
                        true
                    else
                        false
                    endif.XtSetSensitive;
enddefine;



;;; callback when Help button or RETURN is pressed
define lconstant libtool_library_cb(widget, clientdata, calldata);
lvars widget clientdata calldata libraries;
dlocal vederror = libtool_error;
    dlocal XptBusyCursorOn = true;
    clear_footer();
    XptVal libname_widget(XtN string:XptString) -> libraries;
    if libraries /= nullstring then
        libtool_string_to_list(libraries) -> libraries;
        if clientdata == "help" then
            ss_libhelp(current_lib_subsystem, libraries);
        elseif clientdata == "show" then
            ss_libshow(current_lib_subsystem, libraries);
        elseif clientdata == "compile" then
            ss_libcompile(current_lib_subsystem, libraries);
        endif;
    endif;
enddefine;

;;; callback for user selecting a library from the scrolling list
define lconstant libtool_select_cb(widget, clientdata, calldata);
lvars widget clientdata calldata librs libr;
    clear_footer();
    calldata -> current_lib_token;
    calldata -> XpolCurrentListItem(widget);
    XpolListTokenToItem(calldata) -> libr;
    if (is_doubleclick(XptDefaultDisplay, last_lib_select_time)
         -> last_lib_select_time)
      and libr = current_lib_string then
        true -> XptVal help_widget(XtN busy:XptBoolean);
        XFlush(XptDefaultDisplay);
        libtool_library_cb(false,"help",false);
        false -> current_lib_string;
        false -> XptVal help_widget(XtN busy:XptBoolean);
        XFlush(XptDefaultDisplay);
    else
        libr -> current_lib_string;
        lib_of_descriptor(false, current_lib_string) -> librs;
        if islist(librs) then
            hd(librs)
        else
            librs
        endif.libtool_list_to_string
            -> XptVal libname_widget(XtN string:XptString);
    endif;
enddefine;

/* ---------------------------------------------------------------
    Creation Routines
   --------------------------------------------------------------- */

define lconstant add_librarytool_callbacks();
#_IF XOL_VERSION < 3000
    XtAddCallback(librarytool_widget, XtN verify, verify_cb, false);
#_ENDIF
    XtAddCallback(libname_widget, XtN verification,
                                        libtool_library_cb, "help");
    XtAddCallback(help_widget, XtN select, libtool_library_cb, "help");
    XtAddCallback(show_widget, XtN select, libtool_library_cb, "show");
    XtAddCallback(compile_widget, XtN select, libtool_library_cb, "compile");
    XtAddCallback(dismiss_widget, XtN select, libtool_dismiss_cb, 1);
    XtAddCallback(pop11_button, XtN select, libtool_subsystem_cb, "pop11");
    XtAddCallback(prolog_button, XtN select, libtool_subsystem_cb, "prolog");
    XtAddCallback(lisp_button, XtN select, libtool_subsystem_cb, "lisp");
    XtAddCallback(ml_button, XtN select, libtool_subsystem_cb, "ml");
    XtAddCallback(liblist_widget, XtN userMakeCurrent, libtool_select_cb, 1);
enddefine;


define :macexpr p_LIBRARYTOOL(library, subsys, ref_widget) -> widget;
lvars library active_options ref_widget widget;
lvars uppercontrol_widget libname_caption lowercontrol_widget
    subsys liboptions_widget libcaption_widget liblistcaption_widget
    footer;

    unless XptIsLiveType(librarytool_widget, "Widget") then

        unless XptIsLiveType(ref_widget, "Widget") then
            pop_ui_app_shell -> ref_widget;
        endunless;

        XtVaCreatePopupShell('libraryTool', xolPopupWindowShellWidget,
            ref_widget, (#|
                XtN title,      'Poplog: Library Tool',
                XtN iconName,   'Poplog Library',
                XtN pushpin,    OL_IN,
            |#)) -> librarytool_widget;

        XptVal librarytool_widget(XtN upperControlArea:XptWidget)
                        -> uppercontrol_widget;

        10, 10, OL_ALL, true
            -> XptVal uppercontrol_widget(XtN hPad, XtN vPad, XtN sameSize,
                                            XtN alignCaptions:XptBoolean);

        XtVaCreateManagedWidget('Libraries:', xolCaptionWidget,
            uppercontrol_widget,
            XptVaArgList([{space 4}{position ^OL_LEFT}{font 'variable'}
                            ])) -> libname_caption;

        XtVaCreateManagedWidget('library_widget', xolTextFieldWidget,
            libname_caption,
            XptVaArgList([
                {width 350}
        ])) -> libname_widget;

        nullstring -> XptVal libname_widget(XtN string:XptString);

        XtVaCreateManagedWidget('Language:', xolCaptionWidget,
            uppercontrol_widget,
            XptVaArgList([{position ^OL_LEFT} {space 4}{font 'variable'}
              ])) -> libcaption_widget;

        XtVaCreateManagedWidget('options', xolExclusivesWidget,
            libcaption_widget,
            XptVaArgList([
              {layoutType ^OL_FIXEDROWS} {center ^false}
              {hPad 30} {hSpace 30}
              ])) -> liboptions_widget;

        XtVaCreateManagedWidget(gen_ss_label("pop11"), xolRectButtonWidget,
            liboptions_widget,
            XptVaArgList([{set ^true}])) -> pop11_button;

        XtVaCreateManagedWidget(gen_ss_label("prolog"), xolRectButtonWidget,
            liboptions_widget,
            XptVaArgList([{set ^false}])) -> prolog_button;

        XtVaCreateManagedWidget(gen_ss_label("lisp"), xolRectButtonWidget,
            liboptions_widget,
            XptVaArgList([{set ^false}])) -> lisp_button;

        XtVaCreateManagedWidget(gen_ss_label("ml"), xolRectButtonWidget,
            liboptions_widget,
            XptVaArgList([{set ^false}])) -> ml_button;

        newassoc([  [pop11 ^pop11_button] [prolog ^prolog_button]
                    [top ^prolog_button] [lisp ^lisp_button]
                    [ml ^ml_button]]) -> ss_widget;

        XtCreateManagedWidget('descriptions', xolStaticTextWidget,
            uppercontrol_widget,
            XptArgList([{string 'Descriptions:'}])) -> liblistcaption_widget;

        XtCreateManagedWidget('List', xolScrollingListWidget,
            uppercontrol_widget,
            XptArgList([{viewHeight 8}
                        {recomputeWidth ^true}
                        {selectable ^false}
                ])) -> liblist_widget;

        XptVal librarytool_widget(XtN lowerControlArea:XptWidget)
                        -> lowercontrol_widget;

        30, true -> XptVal lowercontrol_widget(XtN hSpace,
                                                XtN center:XptBoolean);

        XtVaCreateManagedWidget('Help', xolOblongButtonWidget,
            lowercontrol_widget,
            XptVaArgList([{default ^true}])) -> help_widget;

        XtVaCreateManagedWidget('Show', xolOblongButtonWidget,
            lowercontrol_widget,
            XptVaArgList([{default ^false}])) -> show_widget;

        XtVaCreateManagedWidget('Compile', xolOblongButtonWidget,
            lowercontrol_widget,
            XptVaArgList([{default ^false}])) -> compile_widget;

        XtVaCreateManagedWidget('Dismiss', xolOblongButtonWidget,
            lowercontrol_widget,
            XptVaArgList([])) -> dismiss_widget;

        XptVal librarytool_widget(XtN footerPanel:XptWidget) -> footer;

        XtVaCreateManagedWidget('footer_widget', xolStaticTextWidget,
            footer,
            XptVaArgList([])) -> footer_widget;

        add_librarytool_callbacks();
        [% consstring(repeat list_char_width times `_` endrepeat,
              list_char_width) %] -> XpolListItems(liblist_widget);
        false -> XptVal liblist_widget(XtN recomputeWidth:XptBoolean);

        sys_compiler_subsystem(`T`) or "pop11" -> current_lib_subsystem;
        XtRealizeWidget(librarytool_widget);
        XptCenterWidgetOn(librarytool_widget, "screen");
        true ->> XptBusyCursorFeedback(librarytool_widget)
            -> XptGarbageCursorFeedback(librarytool_widget);
    endunless;

    clear_footer();
    XtSetValues(librarytool_widget, XptArgList([{pushpin ^OL_IN}]));

    if isword(subsys) then
        subsys -> current_lib_subsystem;
    endif;

    if isstring(library) then
        library -> XptVal libname_widget(XtN string:XptString);
    endif;

    true -> XptVal (ss_widget(current_lib_subsystem))(XtN set:XptBoolean);
    XtPopup(librarytool_widget, 0);
    /* give user something to look at */
    pop_ui_libdescriptors(current_lib_subsystem)
                            -> XpolListItems(liblist_widget);
    compile_widget, if is_subsystem_loaded(current_lib_subsystem) then
                        true
                    else
                        false
                    endif.XtSetSensitive;
    XRaiseWindow( XtDisplayOfObject(librarytool_widget),
                                XtWindow(librarytool_widget));
    librarytool_widget -> widget;
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr  9 1993
        Uses Xol/xol*Widget instead of XptW*idgetSet
--- John Gibson, Jan 18 1993
        Replaced c*urrent_ss_name with sys_compiler_subsystem(`T`) etc
--- Adrian Howard, Sep 10 1992
        Fixed problem in pre-3.0 OW which caused widgets to pop down even
        if the pushpin resource was in.
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- John Gibson, Jul 31 1992
        Replaced c*urrent_subsystem_name with c*urrent_ss_name
--- Julian Clinton, 21/10/91
    Tool now centers itself on screen.
--- Julian Clinton, 14/10/91
    Changed item_chartype of `.` in -libtool_string_to_list- so libraries with
    subsystem suffixes are parsed correctly.
--- Integral Solutions Ltd, Sep 18 1991 (Julian Clinton)
    Renamed -set_footer- to -set_libtool_footer- and made global inside
    the poplog_ui section.
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Removed true assignment to -pop_record_writeable-.
--- Poplog System, Sep  3 1991 (Julian Clinton)
    Set list non-selectable and updates XpolCurrentListItem.
--- Jonathan Meyer, Sep  2 1991 Added gc/busy cursor stuff
--- Jonathan Meyer, Aug 29 1991 XpolDefaultSetup -> XptDefaultSetup
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Changed so current_lib_subsystem is not reset each time the library tool
    is displayed.
    Subsystem button labels now created using gen_ss_label.
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 19/8/91
    Double click on list now brings up help on the library.
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 13/8/91
    Set upper control area padding to 10.
Julian Clinton, 2/8/91
    Added 'Dismiss' button.
Julian Clinton, 16/7/91
    Made caption font variable.
Julian Clinton, 10/7/91
    Changed name of subsystem_libraries to pop_ui_libdescriptors.
Julian Clinton, 8/7/91
    Increased popmemlim.
Julian Clinton, 18/6/91
    Now makes 'Compile' button insensitive if subsystem not loaded.
Julian Clinton, 17/6/91
    Changed to use guiXlibs.
Julian Clinton, 13/6/91
    Added ref. widget arg to xollibrarytool.
 */
