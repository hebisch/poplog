/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-helptool_xm.p
 > Purpose:         Motif help tool
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

lconstant
    SEARCH = 'Search',
    VIEW = 'View Document',
;



/* ---------------------------------------------------------------
    Local Variables and Constants
   --------------------------------------------------------------- */

lconstant helptool_switch_vec   = INIT_helptool_switch_vec;
constant helptool_xm            = helptool_switch_vec;

;;; widgets
lvars
    helptool_widget = false,
    helptool_display = false,
    subject_widget,
    help_widget,
    ref_widget,
    teach_widget,
    doc_widget,
    pop11_widget,
    prolog_widget,
    lisp_widget,
    ml_widget,
    helprefs_widget,
    okay_widget,
    default_widget,
    dismiss_widget,
    footer_widget,
    indexes_widget,
;

lvars
    active_filetype_options = writeable [help ref doc teach],
    active_subsystem_options = writeable [pop11 prolog lisp ml],
    current_help_item = false,
    subsystem_option_widgets,   ;;; will be an association table relating
                                ;;; subsystem button with a widget
    filetype_option_widgets,    ;;; will be an association table relating
                                ;;; file type with a widget
    last_search_string = false,
    last_help_select_time = 0,
    last_subject = SEARCH,
;


lconstant
    ;;; poss_filetype_options must be kept up to date with any changes made
    ;;; to active_filetype_options
    poss_filetype_options = newassoc([[help 1] [ref 2] [doc 3] [teach 4]]),

    ;;; poss_subsystem_options must be kept up to date with any changes made
    ;;; to active_subsystem_options
    poss_subsystem_options = newassoc([[pop11 1] [prolog 2] [lisp 3] [ml 4]]),
    list_char_width = 45,
;


/* ---------------------------------------------------------------
    Utilities for setting footer messages, subsystem and file options
   --------------------------------------------------------------- */

define lconstant set_footer(msg);
    lvars msg;
    msg -> XptVal footer_widget(XmN labelString:XpmCopiedString);
enddefine;

define lconstant clear_footer = set_footer(%' '%) enddefine;

define lconstant xmupdate_button_label(subject, force);
    lvars subject force;
    if force or subject /== last_subject then
        subject -> XptVal okay_widget(XmN labelString:XpmCopiedString);
        subject -> last_subject;
    endif;
enddefine;



define lconstant set_subsystem_options(options);
lvars options;

    define lconstant setopt(item, widget);
    lvars item offset widget;
        unless (poss_subsystem_options(item) ->> offset) then
            mishap(item, 1, 'Unknown subsystem option');
        endunless;

        if islist(options) then
            if lmember(item, options) then
                item, true
            else
                false, false
            endif -> XptVal widget(XmN set:XptBoolean)
                -> subscrl(offset, active_subsystem_options);
        endif;

    enddefine;

    appproperty(subsystem_option_widgets, setopt);
enddefine;


define lconstant set_filetype_options(options);
lvars options;

    define lconstant setopt(item, widget);
    lvars item offset widget;
        unless (poss_filetype_options(item) ->> offset) then
            mishap(item, 1, 'Unknown file type option');
        endunless;

        if lmember(item, options) then
            item, true
        else
            false, false
        endif -> XptVal widget(XmN set:XptBoolean)
              -> subscrl(offset, active_filetype_options);
    enddefine;

    if islist(options) then
        appproperty(filetype_option_widgets, setopt);
    endif;
enddefine;

define lconstant set_list_items(items);
    lvars item, items;
    consXpmStringTable(#|
        for item in items do
            XmStringCreateLtoR(item, XmSTRING_DEFAULT_CHARSET)
        endfor;
    |#) -> items;
    XtVaSetValues(helprefs_widget, (#|
        XmN items,      items,
        XmN itemCount,  shadow_length(items),
    |#));
    repeat destXpmStringTable(items) times
        XmStringFree();
    endrepeat;
enddefine;

define lconstant helptool_veddo(subject);
lvars subject n m l;

    define dlocal vederror(msg);
        lvars msg;
        msg -> XptVal footer_widget(XmN labelString:XpmCopiedString);
        guiInterrupt();
    enddefine;

    ;;; if we have something like 'ref sysio/sysopen' then
    ;;; convert this to 'ref sysopen'.
    if (issubstring('/',2, subject) ->> n) and
       n fi_< (datalength(subject) ->> l) and
       (locchar_back(` `, n, subject) ->> m) then
        substring(1, m, subject) sys_>< substring(n+1, l - n, subject)
            -> subject;
    endif;
    unless vedsetupdone then vedsetup() endunless;
    veddo(subject, true);
enddefine;

define lconstant redisplay_search_string();
    if last_search_string then
        XmTextSetString(subject_widget, last_search_string);
        xmupdate_button_label(SEARCH, true);
        XFlush(helptool_display);
    endif;
enddefine;


/* ---------------------------------------------------------------
    Callbacks
   --------------------------------------------------------------- */

define lconstant destroy_cb();
    erasenum(3);
    false -> helptool_widget;
enddefine;

;;; Called when the 'Dismiss' button is pressed
define lconstant hlptool_dismiss_cb(w,client,call);
    lvars w, client, call;

    clear_footer();
    XtPopdown(helptool_widget);
enddefine;

;;; Called when the 'Default' button is pressed
define lconstant hlptool_default_cb(w,client,call);
    lvars w, client, call;
    dlocal XptBusyCursorOn = true;
    clear_footer();
    if islist(pop_ui_helptool_defaults) then
        set_list_items(pop_ui_helptool_defaults);
    endif;
    redisplay_search_string();
enddefine;


;;; Called when the 'Subject' field is changed
define lconstant hlptool_subject_changed_cb(w,client,call);
    lvars w, client, call string;

    clear_footer();
    XmTextGetString(subject_widget) -> string;
    if issubstring('help ',string) or
       issubstring('ref ',string) or
       issubstring('doc ',string) or
       issubstring('teach ',string) then
        VIEW
    else
        SEARCH
    endif -> string;

    xmupdate_button_label(string, false);
enddefine;

;;; callback for user changing which subsystem documentation to scan
;;; include in the search
define lconstant hlptool_setsubsystem_cb(widget, clientdata, calldata);
    lvars widget clientdata calldata;
    l_typespec calldata :XmToggleButtonCallbackStruct;

    clear_footer();
    if exacc calldata.set == 0 then
        false -> subscrl(poss_subsystem_options(clientdata),
                              active_subsystem_options);
    else
        clientdata -> subscrl(poss_subsystem_options(clientdata),
                              active_subsystem_options);
    endif;
enddefine;


;;; callback for user changing which types of documentation the user
;;; wants to look for
define lconstant hlptool_setfiletypes_cb(widget, clientdata, calldata);
    lvars widget clientdata calldata;
    l_typespec calldata :XmToggleButtonCallbackStruct;

    clear_footer();
    if exacc calldata.set == 0 then
        false
    else
        clientdata
    endif -> subscrl(poss_filetype_options(clientdata),
                                active_filetype_options);
enddefine;


;;; callback when Okay button is pressed
define lconstant hlptool_searchview_cb(widget, clientdata, calldata);
lvars widget clientdata calldata;
lvars subject, use_indexes, list, n;
dlocal active_filetype_options;
    dlocal XptBusyCursorOn = true;
    clear_footer();
    XmTextGetString(subject_widget) -> subject;
    if subject /= nullstring then
        if issubstring('help ', subject) or
                issubstring('ref ', subject) or
                issubstring('teach ', subject) or
                issubstring('doc ', subject) then
            call_ved(helptool_veddo(%subject%));
            XmListDeselectAllItems(helprefs_widget);
            redisplay_search_string();
        else
            XptVal indexes_widget(XmN set:XptBoolean) -> use_indexes;
            subject -> last_search_string;
            set_footer('Searching...');
            XFlush(helptool_display);
            searchforhelp(active_subsystem_options, subject,
                          active_filetype_options, use_indexes) -> list;
            listlength(list) -> n;
            set_footer(((n == 0 and 'No') or n) sys_><
                        ((n == 1 and ' match') or ' matches'));
            set_list_items(list);
            sys_grbg_list(list);
            xmupdate_button_label(SEARCH, true);
            XFlush(helptool_display);
        endif;
    endif;
enddefine;


;;; callback when list item is selected
define lconstant hlptool_select_cb(widget, clientdata, calldata);
    lvars widget clientdata calldata subject;
    l_typespec calldata :XmListCallbackStruct;

    clear_footer();

    exacc calldata.item -> subject;
    XpmCoerceString(subject) -> subject;
    ;;; always update the last_help_select_time
    if (is_doubleclick(XptDefaultDisplay, last_help_select_time) -> last_help_select_time)
      and subject = current_help_item then
        hlptool_searchview_cb(false,false,false);
        false -> current_help_item;
    else
        subject -> current_help_item;
        XmTextSetString(subject_widget, subject);
        xmupdate_button_label(VIEW, true);
    endif;
enddefine;


define lconstant hlptool_subject_cb() with_nargs 3;
    hlptool_searchview_cb();
enddefine;


/* ---------------------------------------------------------------
    Creation Routines
   --------------------------------------------------------------- */

define lconstant add_helptool_callbacks();
    XtAddCallback(subject_widget, XmN activateCallback, hlptool_subject_cb, 1);
    XtAddCallback(subject_widget, XmN valueChangedCallback,
                                    hlptool_subject_changed_cb, 1);
    XtAddCallback(okay_widget, XmN activateCallback, hlptool_subject_cb, 1);
    XtAddCallback(default_widget, XmN activateCallback, hlptool_default_cb, 1);
    XtAddCallback(dismiss_widget, XmN activateCallback, hlptool_dismiss_cb, 1);
    XtAddCallback(helprefs_widget, XmN singleSelectionCallback, hlptool_select_cb, 1);
    XtAddCallback(helprefs_widget, XmN defaultActionCallback, hlptool_select_cb, 1);
    XtAddCallback(pop11_widget, XmN valueChangedCallback, hlptool_setsubsystem_cb, "pop11");
    XtAddCallback(prolog_widget, XmN valueChangedCallback, hlptool_setsubsystem_cb, "prolog");
    XtAddCallback(lisp_widget, XmN valueChangedCallback, hlptool_setsubsystem_cb, "lisp");
    XtAddCallback(ml_widget, XmN valueChangedCallback, hlptool_setsubsystem_cb, "ml");
    XtAddCallback(help_widget, XmN valueChangedCallback, hlptool_setfiletypes_cb, "help");
    XtAddCallback(ref_widget, XmN valueChangedCallback, hlptool_setfiletypes_cb, "ref");
    XtAddCallback(teach_widget, XmN valueChangedCallback, hlptool_setfiletypes_cb, "teach");
    XtAddCallback(doc_widget, XmN valueChangedCallback, hlptool_setfiletypes_cb, "doc");
    XtAddCallback(helptool_widget, XmN destroyCallback, destroy_cb, 1);
enddefine;

define :macexpr p_HELPTOOL(subject, subsysoptions, fileoptions, search_index,
                                                    refer_widget) -> widget;
lvars subject subsysoptions fileoptions search_index
    refer_widget widget;
lvars uppercontrol_widget subjectlabel_widget lowercontrol_widget
    masterform_widget listcaption_widget searchcaption_widget
    searchoptions_widget footer subsyscaption_widget subsysoptions_widget
    indexcaption_widget;

dlocal XptWMProtocols = false;

    Check_string(subject, true);

    unless XptDefaultDisplay then
        XptDefaultSetup();
    endunless;

    unless XptIsLiveType(helptool_widget, "Widget") then

        unless XptIsLiveType(refer_widget, "Widget") then
            pop_ui_app_shell -> refer_widget;
        endunless;

        XtVaCreatePopupShell('helpTool', xtTopLevelShellWidget,
            refer_widget, (#|
                XmN title,              'Poplog: Help Tool',
                XmN iconName,           'Poplog Help',
                XmN deleteResponse,     XmUNMAP,
            |#)) -> helptool_widget;

        ;;; The Motif help tool contains a main Form widget supplying
        ;;; margins and shadows. Within that are two sub-forms for upper
        ;;; and lower control areas: the lower area contains the buttons
        ;;; and message string; everything else is in the upper area.
        ;;; Vertical resizing is absorbed by the upper area, and within
        ;;; that by the scrolling list.

        ;;; create the main Form widget
        ;;;
        XtVaCreateWidget('', xmFormWidget, helptool_widget, (#|
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
        XtVaCreateManagedWidget('Subject', xmLabelWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_FORM,
                XmN leftAttachment,     XmATTACH_FORM,
            |#)) -> subjectlabel_widget;
        XtVaCreateManagedWidget('text', xmTextWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          subjectlabel_widget,
                XmN leftAttachment,     XmATTACH_FORM,
                XmN rightAttachment,    XmATTACH_FORM,
            |#)) -> subject_widget;
        XmTextSetString(subject_widget, '');

        ;;; now create the subsystem selector buttons and label
        ;;;
        XtVaCreateManagedWidget('Languages', xmLabelWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          subject_widget,
                XmN topOffset,          10,
                XmN leftAttachment,     XmATTACH_FORM,
            |#)) -> subsyscaption_widget;

        lvars frame = XtVaCreateManagedWidget('frame', xmFrameWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          subsyscaption_widget,
                XmN leftAttachment,     XmATTACH_FORM,
                XmN rightAttachment,    XmATTACH_FORM,
            |#));
        XmCreateRadioBox(frame, 'options',
            XptArgList([{orientation ^XmHORIZONTAL}
                        {packing ^XmPACK_TIGHT}
                        {radioBehavior ^false}
              ])) -> subsysoptions_widget;
        XtManageChild(subsysoptions_widget);

        XtCreateManagedWidget(gen_ss_label("pop11"), xmToggleButtonWidget,
            subsysoptions_widget,
            XptArgList([{set ^true}
                        {indicatorType ^XmN_OF_MANY}
                ])) -> pop11_widget;

        XtCreateManagedWidget(gen_ss_label("prolog"), xmToggleButtonWidget,
            subsysoptions_widget,
            XptArgList([{set ^false}
                        {indicatorType ^XmN_OF_MANY}
                ])) -> prolog_widget;

        XtCreateManagedWidget(gen_ss_label("lisp"), xmToggleButtonWidget,
            subsysoptions_widget,
            XptArgList([{set ^false}
                        {indicatorType ^XmN_OF_MANY}
                ])) -> lisp_widget;

        XtCreateManagedWidget(gen_ss_label("ml"), xmToggleButtonWidget,
            subsysoptions_widget,
            XptArgList([{set ^false}
                        {indicatorType ^XmN_OF_MANY}
                ])) -> ml_widget;

        newassoc([  [pop11 ^pop11_widget] [prolog ^prolog_widget]
                    [lisp ^lisp_widget] [ml ^ml_widget]])
            -> subsystem_option_widgets;

        appproperty(subsystem_option_widgets,
                        procedure(ss, ss_widget);
                        lvars ss ss_widget;
                            if (is_subsystem_loaded(ss) ->>
                                XptVal ss_widget(XmN set:XptBoolean)) then
                                ss
                            else
                                false
                            endif -> subscrl(poss_subsystem_options(ss),
                                            active_subsystem_options);
                        endprocedure);

        ;;; now create the helpfile type selector buttons and label
        ;;;
        XtVaCreateManagedWidget('File Types', xmLabelWidget,
            uppercontrol_widget,  (#|
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          frame,
                XmN topOffset,          10,
                XmN leftAttachment,     XmATTACH_FORM,
            |#)) -> searchcaption_widget;

        lvars frame = XtVaCreateManagedWidget('frame', xmFrameWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          searchcaption_widget,
                XmN leftAttachment,     XmATTACH_FORM,
                XmN rightAttachment,    XmATTACH_FORM,
            |#));
        lvars form = XtVaCreateManagedWidget('form', xmFormWidget, frame, 0);
        XmCreateRadioBox(form, 'options',
            XptArgList([{orientation ^XmHORIZONTAL}
                        {packing ^XmPACK_TIGHT}
                        {radioBehavior ^false}
                        {navigationType ^XmNONE}
                        {topAttachment ^XmATTACH_FORM}
                        {leftAttachment ^XmATTACH_FORM}
                        {rightAttachment ^XmATTACH_FORM}
              ])) -> searchoptions_widget;
        XtManageChild(searchoptions_widget);

        XtCreateManagedWidget('General Help', xmToggleButtonWidget,
            searchoptions_widget,
            XptArgList([{set ^true}
                        {indicatorType ^XmN_OF_MANY}
                ])) -> help_widget;

        XtCreateManagedWidget('Reference', xmToggleButtonWidget,
            searchoptions_widget,
            XptArgList([{set ^true}
                        {indicatorType ^XmN_OF_MANY}
                ])) -> ref_widget;

        XtCreateManagedWidget('Teach', xmToggleButtonWidget,
            searchoptions_widget,
            XptArgList([{set ^true}
                        {indicatorType ^XmN_OF_MANY}
                ])) -> teach_widget;

        XtCreateManagedWidget('Document', xmToggleButtonWidget,
            searchoptions_widget,
            XptArgList([{set ^true}
                        {indicatorType ^XmN_OF_MANY}
                ])) -> doc_widget;

        newassoc([  [help ^help_widget] [ref ^ref_widget]
                    [doc ^doc_widget] [teach ^teach_widget]])
            -> filetype_option_widgets;

        ;;; now the 'Search indexes' toggle
        ;;;
        XtVaCreateManagedWidget('Search Indexes', xmToggleButtonWidget,
            form, (#|
                XmN set,                true,
                XmN indicatorType,      XmONE_OF_MANY,
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          searchoptions_widget,
                XmN bottomAttachment,   XmATTACH_FORM,
                XmN leftAttachment,     XmATTACH_FORM,
            |#)) -> indexes_widget;
        ;;; line it up
        XptVal searchoptions_widget(XmN marginWidth:XptDimension)
            -> XptVal indexes_widget(XmN leftOffset:int);
        XptVal searchoptions_widget(XmN marginHeight:XptDimension)
            -> XptVal indexes_widget(XmN bottomOffset:int);

        ;;; the List title
        ;;;
        XtVaCreateManagedWidget('Search Result', xmLabelWidget,
            uppercontrol_widget, (#|
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          frame,
                XmN topOffset,          10,
                XmN leftAttachment,     XmATTACH_FORM,
            |#)) -> listcaption_widget;

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
                        {topWidget ^listcaption_widget}
                        {bottomAttachment ^XmATTACH_FORM}
                        {resizable ^false}
                ])) -> helprefs_widget;
        XtManageChild(helprefs_widget);

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

        XtCreateManagedWidget(SEARCH, xmPushButtonWidget,
            lowercontrol_widget,
            XptArgList([{showAsDefault ^true}
                        {leftAttachment ^XmATTACH_POSITION}
                        {leftPosition 5}
                        {rightAttachment ^XmATTACH_POSITION}
                        {rightPosition 35}
                        {topAttachment ^XmATTACH_FORM}
                        {bottomAttachment ^XmATTACH_WIDGET}
                        {bottomWidget ^footer_widget}
                        ])) -> okay_widget;

        XtCreateManagedWidget('Default', xmPushButtonWidget,
            lowercontrol_widget,
            XptArgList([{leftAttachment ^XmATTACH_POSITION}
                        {leftPosition 50}
                        {rightAttachment ^XmATTACH_POSITION}
                        {rightPosition 65}
                        {topAttachment ^XmATTACH_FORM}
                        {bottomAttachment ^XmATTACH_WIDGET}
                        {bottomWidget ^footer_widget}
                        ])) -> default_widget;

        XtCreateManagedWidget('Dismiss', xmPushButtonWidget,
            lowercontrol_widget,
            XptArgList([{leftAttachment ^XmATTACH_POSITION}
                        {leftPosition 80}
                        {rightAttachment ^XmATTACH_POSITION}
                        {rightPosition 95}
                        {topAttachment ^XmATTACH_FORM}
                        {bottomAttachment ^XmATTACH_WIDGET}
                        {bottomWidget ^footer_widget}
                        ])) -> dismiss_widget;

        XtManageChild(lowercontrol_widget);

        XtManageChild(masterform_widget);

        hlptool_default_cb(false,false,false);
        add_helptool_callbacks();
        XtRealizeWidget(helptool_widget);
        XptCenterWidgetOn(helptool_widget, "screen");
        true ->> XptBusyCursorFeedback(helptool_widget)
            -> XptGarbageCursorFeedback(helptool_widget);
    endunless;

    clear_footer();

    if isstring(subject) then
        XmTextSetString(subject_widget, subject);
    endif;

    set_subsystem_options(subsysoptions);
    set_filetype_options(fileoptions);

    if isboolean(search_index) then
        search_index -> XptVal indexes_widget(XmN set:XptBoolean);
    endif;

    false -> XptVal helptool_widget(XmN allowShellResize:XptBoolean);

    XmProcessTraversal(subject_widget, XmTRAVERSE_CURRENT)->;
    XtPopup(helptool_widget, 0);
    XtDisplayOfObject(helptool_widget) -> helptool_display;
    XRaiseWindow(helptool_display, XtWindow(helptool_widget));
    helptool_widget -> widget;
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jun 23 1995
        Fixed to use XpmCopiedString and to free strings created for the
        list items
--- Robert John Duncan, Jun 15 1995
        Changed widget layout to look more like a Motif dialog and to give
        better resizing behaviour (needs more work to reimplement using a
        proper dialog template)
--- Robert John Duncan, May  4 1995
        Changed to use new call_ved
--- Robert John Duncan, Jul 20 1994
        Removed size resource settings for the helptool
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr 16 1993
        Uses Xm/xm*Widget etc
--- John Gibson, Jan 21 1993
        Made active_*_options lists writeable
--- Adrian Howard, Sep 11 1992
        Selection now cleared after document viewed
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- Adrian Howard, Sep  4 1992
        Set XmNradioBehavior to false in RadioBoxes so more than one
        language and file-type can be selected at the same time.
--- Integral Solutions Ltd, Jun  2 1992
    Call of -veddo- in -helptool_veddo- now adds command to VED command
    buffer.
Julian Clinton, 20/5/92
    Allowed window resize to resize the scrolling list.
--- Simon Nichols, Jan 27 1992
        In -helptool_veddo-, added call to -vedsetup- if -vedsetupdone- is
        <false> (see bugreport isl-fr.4395).
Julian Clinton, 10/12/91
    Set XptWMProtocols -false- and modified deleteResponse.
Julian Clinton, 21/10/91
    Tool now centers itself on screen.
Julian Clinton, 15/10/91
    Now set text field to have input focus.
Julian Clinton, 14/10/91
    Made name string consistent with the OPEN LOOK help tool.
    Removed explicit string null terminators.
--- Adrian Howard, Sep 13 1991 : Bug fix of Julian's installed
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Removed true assignment to -pop_record_writeable-.
--- Jonathan Meyer, Sep  3 1991 Added  uses guiMouseEvents to fix
        redeclaration of is_doubleclick
--- Jonathan Meyer, Sep  2 1991 Added gc/busy cursor stuff. tidied.
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Prevented options being reset each time the helptool is redisplayed.
    Subsystem button labels now created using gen_ss_label.
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 21/8/91
    Changed helptool_veddo to convert strings like 'ref sysio/sysopen'
    to 'ref sysopen'.
Julian Clinton, 19/8/91
    Double click on the list now brings up the selected help file.
Julian Clinton, 15/8/91
    Allowed <undef> to be passed as search_index arg.
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 2/8/91
    Changed so Index Search is not disabled if Reference is not selected.
    Added 'Dismiss' button.
    Added 'Default' button to get a default help list to appear.
Julian Clinton, 15/7/91
    Prevented subject line being nullified if the helptool is simply
        raised.
Julian Clinton, 8/7/91
    Increased popmemlim.
    Changed so that search index button is disabled if Ref not selected.
 */
