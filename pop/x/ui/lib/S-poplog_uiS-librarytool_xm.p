/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-librarytool_xm.p
 > Purpose:         Motif library tool
 > Author:          Julian Clinton, June 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now Xm;

section $-poplog_ui;

include pop_uiP.ph;
include xpt_coretypes.ph;
include XmConstants.ph;

uses
    xt_init,
    xt_widget,
    xt_widgetclass,
    xt_widgetinfo,
    xt_composite,
    xt_popup,
    xt_event,
    xt_callback,
    xt_resource,

    xmTextWidget,
    xmLabelWidget,
    xmListWidget,
    xmFrameWidget,
    xmFormWidget,
    xmRowColumnWidget,
    xmPushButtonWidget,
    xmToggleButtonWidget,
;


/* ---------------------------------------------------------------
    Local Variables and Constants
   --------------------------------------------------------------- */

lconstant librarytool_switch_vec= INIT_librarytool_switch_vec;
constant librarytool_xm         = librarytool_switch_vec;

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
    xmlist_cache = false,       ;;; will be property table
    current_lib_subsystem = "pop11",
    current_lib_offset = false,
    ss_widget = false,  ;;; will be a property of subsystem name to widget
;

lconstant
    list_char_width = 45,
;

/* ---------------------------------------------------------------
    Displaying Info
   --------------------------------------------------------------- */

define :macexpr p_SET_LIBTOOL_FOOTER(msg);
    lvars msg;
    msg -> XptVal footer_widget(XmN labelString:XpmCopiedString);
enddefine;

;;; have to use space rather than nullstring to clear garbage from window
define lconstant clear_footer =
    p_SET_LIBTOOL_FOOTER(%' '%)
enddefine;


/* ---------------------------------------------------------------
    Utilities
   --------------------------------------------------------------- */

define xmsubsystem_libraries(ss) -> xmlibs;
lvars ss len i item libs xmlibs;
    unless xmlist_cache then
        newproperty([  [pop11 ^false] [prolog ^false] [top ^false]
                       [lisp ^false] [ml ^false]],
                    6,false,"perm") -> xmlist_cache;
    endunless;

    pop_ui_libdescriptors(ss) -> libs;
    xmlist_cache(ss) -> xmlibs;

    unless xmlibs and shadow_length(xmlibs) == listlength(libs) then
        listlength(libs) -> len;
        initXpmStringTable(len) -> xmlibs;
        1 -> i;
        fast_for item in libs do
            XmStringCreate(item, XmSTRING_DEFAULT_CHARSET) -> xmlibs(i);
            i fi_+ 1 -> i;
        endfast_for;
        xmlibs -> xmlist_cache(ss);
    else
        xmlist_cache(ss) -> xmlibs;
    endunless;

enddefine;


/* ---------------------------------------------------------------
    Callbacks
   --------------------------------------------------------------- */

define lconstant destroy_cb();
    erasenum(3);
    false -> librarytool_widget;
enddefine;

;;; Called when the 'Dismiss' button is pressed
define lconstant libtool_dismiss_cb();
    erasenum(3);
    clear_footer();
    XtPopdown(librarytool_widget);
enddefine;


;;; callback for user changing which subsystem libraries to look at
define lconstant libtool_subsystem_cb(widget, clientdata, calldata);
    lvars widget clientdata calldata items;
    l_typespec calldata :XmToggleButtonCallbackStruct;

    clear_footer();
    unless exacc calldata.set == 0 then
        clientdata -> current_lib_subsystem;

        xmsubsystem_libraries(current_lib_subsystem) -> items;

        XtVaSetValues(liblist_widget, XmN items, items,
                        XmN itemCount, shadow_length(items), 4);

        false -> current_lib_offset;

        compile_widget, if is_subsystem_loaded(current_lib_subsystem) then
                            true
                        else
                            false
                        endif.XtSetSensitive;
    endunless;
enddefine;


;;; callback when Help button or RETURN is pressed
define lconstant libtool_library_cb(widget, clientdata, calldata);
lvars widget clientdata calldata libraries;
dlocal vederror = libtool_error;
    dlocal XptBusyCursorOn = true;
    clear_footer();
    XmTextGetString(libname_widget) -> libraries;
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
    lvars widget clientdata calldata librs libr str;
    l_typespec calldata :XmListCallbackStruct;

    clear_footer();
    exacc calldata.item_position -> libr;
    if (is_doubleclick(XptDefaultDisplay, last_lib_select_time)
         -> last_lib_select_time)
      and libr == current_lib_offset then
        libtool_library_cb(false,"help",false);
        false -> current_lib_offset;
    else
        libr -> current_lib_offset;
        lib_of_descriptor(current_lib_subsystem, current_lib_offset) -> librs;
        if islist(librs) then
            hd(librs)
        else
            librs
        endif.libtool_list_to_string -> str;
        XmTextSetString(libname_widget, str);
    endif;
enddefine;


/* ---------------------------------------------------------------
    Creation Routines
   --------------------------------------------------------------- */

define lconstant add_librarytool_callbacks();
    XtAddCallback(libname_widget, XmN activateCallback,
                                        libtool_library_cb, "help");
    XtAddCallback(help_widget, XmN activateCallback, libtool_library_cb, "help");
    XtAddCallback(show_widget, XmN activateCallback, libtool_library_cb, "show");
    XtAddCallback(compile_widget, XmN activateCallback, libtool_library_cb, "compile");
    XtAddCallback(dismiss_widget, XmN activateCallback, libtool_dismiss_cb, 1);
    XtAddCallback(pop11_button, XmN valueChangedCallback, libtool_subsystem_cb, "pop11");
    XtAddCallback(prolog_button, XmN valueChangedCallback, libtool_subsystem_cb, "prolog");
    XtAddCallback(lisp_button, XmN valueChangedCallback, libtool_subsystem_cb, "lisp");
    XtAddCallback(ml_button, XmN valueChangedCallback, libtool_subsystem_cb, "ml");
    XtAddCallback(liblist_widget, XmN singleSelectionCallback, libtool_select_cb, 1);
    XtAddCallback(liblist_widget, XmN defaultActionCallback, libtool_select_cb, 1);
    XtAddCallback(librarytool_widget, XmN destroyCallback, destroy_cb, 1);
enddefine;


define :macexpr p_LIBRARYTOOL(library, subsys, ref_widget) -> widget;
lvars library active_options ref_widget widget;
lvars uppercontrol_widget lowercontrol_widget masterform_widget
    subsys liboptions_widget libcaption_widget
    liblabel_widget liblistcaption_widget items footer;
    dlocal XptWMProtocols = false;

    unless XptIsLiveType(librarytool_widget, "Widget") then

        unless XptIsLiveType(ref_widget, "Widget") then
            pop_ui_app_shell -> ref_widget;
        endunless;

        XtVaCreatePopupShell('libraryTool', xtTopLevelShellWidget,
            ref_widget, (#|
                XmN title,              'Poplog: Library Tool',
                XmN iconName,           'Poplog Library',
                XmN deleteResponse,     XmUNMAP,
            |#)) -> librarytool_widget;


        ;;; The Motif library tool contains a main Form widget supplying
        ;;; margins and shadows. Within that are two sub-forms for upper
        ;;; and lower control areas: the lower area contains the buttons
        ;;; and message string; everything else is in the upper area.
        ;;; Vertical resizing is absorbed by the upper area, and within
        ;;; that by the scrolling list.

        ;;; create the main Form widget
        ;;;
        XtVaCreateWidget('', xmFormWidget, librarytool_widget, (#|
                XmN marginHeight,       10,
                XmN marginWidth,        10,
            |#)) -> masterform_widget;

        ;;; create upper and lower control areas
        ;;;
        XtVaCreateWidget('', xmFormWidget, masterform_widget, (#|
                XmN bottomAttachment,   XmATTACH_FORM,
                XmN leftAttachment,     XmATTACH_FORM,
                XmN rightAttachment,    XmATTACH_FORM,
            |#)) -> lowercontrol_widget;
        XtVaCreateWidget('', xmFormWidget, masterform_widget, (#|
                XmN topAttachment,      XmATTACH_FORM,
                XmN bottomAttachment,   XmATTACH_WIDGET,
                XmN bottomWidget,       lowercontrol_widget,
                XmN bottomOffset,       15,
                XmN leftAttachment,     XmATTACH_FORM,
                XmN rightAttachment,    XmATTACH_FORM,
            |#)) -> uppercontrol_widget;

        ;;; UPPER CONTROLS:
        ;;; text field and label
        ;;;
        XtVaCreateManagedWidget('Libraries', xmLabelWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_FORM,
                XmN leftAttachment,     XmATTACH_FORM,
            |#)) -> liblabel_widget;
        XtVaCreateManagedWidget('text', xmTextWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          liblabel_widget,
                XmN leftAttachment,     XmATTACH_FORM,
                XmN rightAttachment,    XmATTACH_FORM,
            |#)) -> libname_widget;
        XmTextSetString(libname_widget, '');

        ;;; subsystem selector buttons and label
        ;;;
        XtVaCreateManagedWidget('Language', xmLabelWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          libname_widget,
                XmN topOffset,          10,
                XmN leftAttachment,     XmATTACH_FORM,
            |#)) -> libcaption_widget;

        lvars frame = XtVaCreateManagedWidget('frame', xmFrameWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          libcaption_widget,
                XmN leftAttachment,     XmATTACH_FORM,
                XmN rightAttachment,    XmATTACH_FORM,
            |#));
        XmCreateRadioBox(frame, 'options',
            XptArgList([{orientation ^XmHORIZONTAL}
                        {packing ^XmPACK_TIGHT}
                        {radioAlwaysOne ^true}
              ])) -> liboptions_widget;
        XtManageChild(liboptions_widget);

        XtCreateManagedWidget(gen_ss_label("pop11"), xmToggleButtonWidget,
            liboptions_widget,
            XptArgList([{set ^false}
                        {indicatorType ^XmONE_OF_MANY}
                ])) -> pop11_button;

        XtCreateManagedWidget(gen_ss_label("prolog"), xmToggleButtonWidget,
            liboptions_widget,
            XptArgList([{set ^false}
                        {indicatorType ^XmONE_OF_MANY}
                ])) -> prolog_button;

        XtCreateManagedWidget(gen_ss_label("lisp"), xmToggleButtonWidget,
            liboptions_widget,
            XptArgList([{set ^false}
                        {indicatorType ^XmONE_OF_MANY}
                ])) -> lisp_button;

        XtCreateManagedWidget(gen_ss_label("ml"), xmToggleButtonWidget,
            liboptions_widget,
            XptArgList([{set ^false}
                        {indicatorType ^XmONE_OF_MANY}
                ])) -> ml_button;

        newassoc([  [pop11 ^pop11_button] [prolog ^prolog_button]
                    [top ^prolog_button] [lisp ^lisp_button]
                    [ml ^ml_button]]) -> ss_widget;

        ;;; the List title
        ;;;
        XtVaCreateManagedWidget('Descriptions', xmLabelWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          frame,
                XmN topOffset,          10,
                XmN leftAttachment,     XmATTACH_FORM,
            |#)) -> liblistcaption_widget;

        ;;; and now the scrolling list
        ;;;
        XmCreateScrolledList(uppercontrol_widget, 'List',
            XptArgList([{visibleItemCount 12}
                        {selectionPolicy ^XmSINGLE_SELECT}
                        {listSizePolicy ^XmCONSTANT}
                        {scrollBarDisplayPolicy ^XmSTATIC}
                        {leftAttachment ^XmATTACH_FORM}
                        {rightAttachment ^XmATTACH_FORM}
                        {topAttachment ^XmATTACH_WIDGET}
                        {topWidget ^liblistcaption_widget}
                        {bottomAttachment ^XmATTACH_FORM}
                        {resizable ^false}
                ])) -> liblist_widget;
        XtManageChild(liblist_widget);

        XtManageChild(uppercontrol_widget);

        ;;; LOWER CONTROLS:
        ;;; the message string
        ;;;
        XtCreateManagedWidget('', xmLabelWidget,
            lowercontrol_widget,
            XptArgList([
                        {bottomAttachment ^XmATTACH_FORM}
                        {leftAttachment ^XmATTACH_FORM}
                        {rightAttachment ^XmATTACH_FORM}
                        ])) -> footer_widget;

        clear_footer();

        ;;; ... and the action buttons
        ;;;
        XtCreateManagedWidget('Help', xmPushButtonWidget,
            lowercontrol_widget,
            XptArgList([{showAsDefault ^true}
                        {leftAttachment ^XmATTACH_POSITION}
                        {leftPosition 5}
                        {rightAttachment ^XmATTACH_POSITION}
                        {rightPosition 20}
                        {topAttachment ^XmATTACH_FORM}
                        {bottomAttachment ^XmATTACH_WIDGET}
                        {bottomWidget ^footer_widget}
                        ])) -> help_widget;

        XtCreateManagedWidget('Show', xmPushButtonWidget,
            lowercontrol_widget,
            XptArgList([{showAsDefault ^false}
                        {leftAttachment ^XmATTACH_POSITION}
                        {leftPosition 30}
                        {rightAttachment ^XmATTACH_POSITION}
                        {rightPosition 45}
                        {topAttachment ^XmATTACH_FORM}
                        {bottomAttachment ^XmATTACH_WIDGET}
                        {bottomWidget ^footer_widget}
                        ])) -> show_widget;

        XtCreateManagedWidget('Compile', xmPushButtonWidget,
            lowercontrol_widget,
            XptArgList([{showAsDefault ^false}
                        {leftAttachment ^XmATTACH_POSITION}
                        {leftPosition 55}
                        {rightAttachment ^XmATTACH_POSITION}
                        {rightPosition 70}
                        {topAttachment ^XmATTACH_FORM}
                        {bottomAttachment ^XmATTACH_WIDGET}
                        {bottomWidget ^footer_widget}
                        ])) -> compile_widget;

        XtCreateManagedWidget('Dismiss', xmPushButtonWidget,
            lowercontrol_widget,
            XptArgList([{showAsDefault ^false}
                        {topAttachment ^XmATTACH_FORM}
                        {leftAttachment ^XmATTACH_POSITION}
                        {leftPosition 80}
                        {rightAttachment ^XmATTACH_POSITION}
                        {rightPosition 95}
                        {bottomAttachment ^XmATTACH_WIDGET}
                        {bottomWidget ^footer_widget}
                        ])) -> dismiss_widget;

        XtManageChild(lowercontrol_widget);

        XtManageChild(masterform_widget);

        ;;; put it on screen, position it and enable the Poplog cursors
        XtRealizeWidget(librarytool_widget);
        XptCenterWidgetOn(librarytool_widget, "screen");
        true ->> XptBusyCursorFeedback(librarytool_widget)
            -> XptGarbageCursorFeedback(librarytool_widget);

        add_librarytool_callbacks();
        sys_compiler_subsystem(`T`) or "pop11" -> current_lib_subsystem;
    endunless;

    clear_footer();

    if isword(subsys) then
        subsys -> current_lib_subsystem;
    endif;

    true -> XptVal (ss_widget(current_lib_subsystem))(XmN set:XptBoolean);

    if isstring(library) then
        XmTextSetString(libname_widget, library);
    endif;

    xmsubsystem_libraries(current_lib_subsystem) -> items;

    XtVaSetValues(liblist_widget, XmN items, items,
                                  XmN itemCount, shadow_length(items), 4);

    false -> XptVal librarytool_widget(XmN allowShellResize:XptBoolean);

    XmProcessTraversal(libname_widget, XmTRAVERSE_CURRENT)->;

    XtPopup(librarytool_widget, 0);
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
--- Robert John Duncan, Jun 23 1995
        Fixed to use XpmCopiedString
--- Robert John Duncan, Jun 15 1995
        Changed widget layout to look more like a Motif dialog and to give
        better resizing behaviour (needs more work to reimplement using a
        proper dialog template)
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr 16 1993
        Uses Xm/xm*Widget etc
--- John Gibson, Jan 18 1993
        Replaced c*urrent_ss_name with sys_compiler_subsystem(`T`) etc
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- John Gibson, Jul 31 1992
        Replaced c*urrent_subsystem_name with c*urrent_ss_name
--- Integral Solutions Ltd, Jun  2 1992 (Julian Clinton)
    Allowed window resize to resize the scrolling list.
Julian Clinton, 10/12/91
    Set XptWMProtocols -false- and modified deleteResponse.
Julian Clinton, 21/10/91
    Tool now centers itself on screen.
Julian Clinton, 15/10/91
    Now set text field to have input focus.
Julian Clinton, 14/10/91
    Made name string consistent with the OPEN LOOK library tool.
    Removed explicit string null terminators.
Julian Clinton, 11/10/91
    Modified to prevent help files being brought up on the first click rather
    than the first double click after the tool is re-displayed.
--- Integral Solutions Ltd, Sep 18 1991 (Julian Clinton)
    Renamed -set_footer- to -set_libtool_footer- and made global inside
    the poplog_ui section.
    Changed item_chartype of `.` in -libtool_string_to_list- so libraries with
    subsystem suffixes are parsed correctly.
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Removed true assignment to -pop_record_writeable-.
--- Jonathan Meyer, Sep  2 1991 Added gc/busy cursor stuff
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Changed so current_lib_subsystem is not reset each time the library tool
    is displayed.
    Subsystem button labels now created using gen_ss_label.
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 19/8/91
    Double click on list item now brings up help on the library.
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 2/8/91
    Added 'Dismiss' button.
Julian Clinton, 10/7/91
    changed name of subsystem_libraries to pop_ui_libdescriptors.
Julian Clinton, 8/7/91
    Increased popmemlim.
 */
