/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-helptool_xol.p
 > Purpose:         Open Look help tool
 > Author:          Julian Clinton, May 1991 (see revisions)
 > Documentation:
 > Related Files:
*/

compile_mode :pop11 +strict;

uses-now Xol;

section $-poplog_ui;

include pop_uiP.ph;
include xpt_xtypes.ph;
include XolConstants.ph;

;;; For -verify_cb-
#_IF XOL_VERSION < 3000
include xpt_coretypes.ph;
#_ENDIF

uses
    xt_init,
    xt_widget,
    xt_callback,
    xt_event,
    xt_popup,
    xt_grab,
    xt_widgetinfo,
    xt_resource,

    xpol_listutils,

    xolTextFieldWidget,
    xolCaptionWidget,
    xolNonexclusivesWidget,
    xolExclusivesWidget,
    xolScrollingListWidget,
    xolRectButtonWidget,
    xolOblongButtonWidget,
    xolStaticTextWidget,
    xolPopupWindowShellWidget,
;

lconstant
    SEARCH = 'Search',
    VIEW = 'View Document',
;


/* ---------------------------------------------------------------
    Local Variables and Constants
   --------------------------------------------------------------- */

lconstant helptool_switch_vec   = INIT_helptool_switch_vec;
constant helptool_xol           = helptool_switch_vec;

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
    indexcaption_widget,
;

lvars
    last_action_label = false,
    last_help_select_time = 0,
    last_search_string = false,
    active_filetype_options = writeable [help ref doc teach],
    active_subsystem_options = writeable [pop11 prolog lisp ml],
    current_help_item,
    subsystem_option_widgets,   ;;; will be an association table relating
                                ;;; subsystem button with a widget
    filetype_option_widgets,    ;;; will be an association table relating
                                ;;; file type with a widget
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
    msg -> XptVal footer_widget(XtN string:XptString);
enddefine;

define lconstant clear_footer = set_footer(%nullstring%) enddefine;



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
            endif -> XptVal widget(XtN set:XptBoolean)
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
        endif -> XptVal widget(XtN set:XptBoolean)
              -> subscrl(offset, active_filetype_options);
    enddefine;

    if islist(options) then
        appproperty(filetype_option_widgets, setopt);
    endif;
enddefine;

define lconstant helptool_veddo(subject);
lvars subject n m l;

    define dlocal vederror(msg);
        lvars msg;
        msg -> XptVal footer_widget(XtN string:XptString);
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

define lconstant update_action_label(label);
lvars label;
    unless last_action_label = label then
        label ->> XptVal okay_widget(XtN label:XptString)
              -> last_action_label;
    endunless;
enddefine;

define lconstant redisplay_search_string();
    if last_search_string then
        last_search_string -> XptVal subject_widget(XtN string:XptString);
        update_action_label(SEARCH);
    endif;
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
define lconstant hlptool_dismiss_cb(w,client,call);
    lvars w, client, call;

    clear_footer();
    XtPopdown(helptool_widget);
enddefine;

;;; Called when the 'Default' button is pressed
define lconstant hlptool_default_cb(w,client,call);
    lvars w, client, call;
    dlocal XptBusyCursorOn = true, pop_pr_quotes=false;
    clear_footer();
    if islist(pop_ui_helptool_defaults) then
        pop_ui_helptool_defaults -> XpolListItems(helprefs_widget);
    endif;
    redisplay_search_string();
enddefine;

;;; Called when the 'Subject' field is changed
define lconstant hlptool_subject_changed_cb(w,client,call);
    lvars w, client, call string;

    clear_footer();
    XptVal subject_widget(XtN string:XptString) -> string;
    if issubstring('help ',string) or
       issubstring('ref ',string) or
       issubstring('doc ',string) or
       issubstring('teach ',string) then
        VIEW
    else
        SEARCH
    endif -> string;
    update_action_label(string);
enddefine;

;;; callback for user changing which subsystem documentation to scan
;;; include in the search
define lconstant hlptool_setsubsystem_cb(widget, clientdata, calldata);
lvars widget clientdata calldata;
    clear_footer();
    clientdata -> subscrl(poss_subsystem_options(clientdata),
                          active_subsystem_options);
enddefine;


define lconstant hlptool_unsetsubsystem_cb(widget, clientdata, calldata);
lvars widget clientdata calldata;
    clear_footer();
    false -> subscrl(poss_subsystem_options(clientdata),
                        active_subsystem_options);
enddefine;


;;; callback for user changing which types of documentation the user
;;; wants to look for
define lconstant hlptool_setfiletypes_cb(widget, clientdata, calldata);
lvars widget clientdata calldata;
    clear_footer();
    clientdata -> subscrl(poss_filetype_options(clientdata), active_filetype_options);
enddefine;


define lconstant hlptool_unsetfiletypes_cb(widget, clientdata, calldata);
lvars widget clientdata calldata;
    clear_footer();
    false -> subscrl(poss_filetype_options(clientdata), active_filetype_options);
enddefine;


;;; callback when Okay button is pressed
define lconstant hlptool_searchview_cb(widget, clientdata, calldata);
lvars widget clientdata calldata;
lvars subject, use_indexes, list n;
dlocal active_filetype_options, pop_pr_quotes=false;
    dlocal XptBusyCursorOn = true;
    clear_footer();
    XptVal subject_widget(XtN string:XptString) -> subject;

    if subject /= nullstring then
        if issubstring('help ', subject) or
                issubstring('ref ', subject) or
                issubstring('teach ', subject) or
                issubstring('doc ', subject) then
            call_ved(helptool_veddo(%subject%));

            ;;; CLEAR SELECTION
            XpolListItems(helprefs_widget) -> XpolListItems(helprefs_widget);

            redisplay_search_string();
        else
            XptVal indexes_widget(XtN set:XptBoolean) -> use_indexes;
            subject -> last_search_string;
            set_footer('Searching...');
            XFlush(helptool_display);
            searchforhelp(active_subsystem_options, subject,
                          active_filetype_options, use_indexes) -> list;
            list.listlength -> n;

#_IF XOL_VERSION < 3000
            if n > 900 then
                allbutlast(n-900, list) -> list;
                set_footer(n sys_>< ' matches (list truncated)');
            else
                set_footer(((n == 0 and 'No') or n) sys_><
                            ((n == 1 and ' match') or ' matches'));
            endif;
            list -> XpolListItems(helprefs_widget);
#_ELSE
            list -> XpolListItems(helprefs_widget);
            set_footer(((n == 0 and 'No') or n) sys_><
                        ((n == 1 and ' match') or ' matches'));
#_ENDIF
            sys_grbg_list(list);
        endif;
    endif;
    XptVal subject_widget(XtN string:XptString) -> subject;
    if issubstring('help ',subject) or
       issubstring('ref ',subject) or
       issubstring('doc ',subject) or
       issubstring('teach ',subject) then
        VIEW
    else
        SEARCH
    endif -> subject;
    update_action_label(subject);
enddefine;


;;; callback when list item is selected
define lconstant hlptool_select_cb(widget, clientdata, calldata);
lvars widget clientdata calldata;
lvars subject;
    clear_footer();
    calldata -> XpolCurrentListItem(widget);
    XpolListTokenToItem(calldata) -> subject;

    ;;; always update the last_help_select_time
    if (is_doubleclick(XptDefaultDisplay, last_help_select_time) -> last_help_select_time)
      and subject = current_help_item then
        hlptool_searchview_cb(false,false,false);
        false -> current_help_item;
    else
        subject ->> current_help_item
            -> XptVal subject_widget(XtN string:XptString);
        update_action_label(VIEW);
    endif;
enddefine;


define lconstant hlptool_subject_cb(widget, clientdata, calldata);
    lvars widget clientdata calldata;

    true -> XptVal okay_widget(XtN busy:XptBoolean);
    XFlush(helptool_display);
    hlptool_searchview_cb(widget, clientdata, calldata);
    false -> XptVal okay_widget(XtN busy:XptBoolean);
    XFlush(helptool_display);
enddefine;


/* ---------------------------------------------------------------
    Creation Routines
   --------------------------------------------------------------- */

define lconstant add_helptool_callbacks();
#_IF XOL_VERSION < 3000
    XtAddCallback(helptool_widget, XtN verify, verify_cb, false);
#_ENDIF
    XtAddCallback(subject_widget, XtN verification, hlptool_subject_cb, 1);
    XtAddCallback(helprefs_widget, XtN userMakeCurrent,
                    hlptool_select_cb, 1);
    XtAddCallback(okay_widget, XtN select, hlptool_searchview_cb, 1);
    XtAddCallback(default_widget, XtN select, hlptool_default_cb, 1);
    XtAddCallback(dismiss_widget, XtN select, hlptool_dismiss_cb, 1);
    XtAddCallback(pop11_widget, XtN select, hlptool_setsubsystem_cb, "pop11");
    XtAddCallback(prolog_widget, XtN select, hlptool_setsubsystem_cb, "prolog");
    XtAddCallback(lisp_widget, XtN select, hlptool_setsubsystem_cb, "lisp");
    XtAddCallback(ml_widget, XtN select, hlptool_setsubsystem_cb, "ml");
    XtAddCallback(pop11_widget, XtN unselect, hlptool_unsetsubsystem_cb, "pop11");
    XtAddCallback(prolog_widget, XtN unselect, hlptool_unsetsubsystem_cb, "prolog");
    XtAddCallback(lisp_widget, XtN unselect, hlptool_unsetsubsystem_cb, "lisp");
    XtAddCallback(ml_widget, XtN unselect, hlptool_unsetsubsystem_cb, "ml");
    XtAddCallback(help_widget, XtN select, hlptool_setfiletypes_cb, "help");
    XtAddCallback(ref_widget, XtN select, hlptool_setfiletypes_cb, "ref");
    XtAddCallback(teach_widget, XtN select, hlptool_setfiletypes_cb, "teach");
    XtAddCallback(doc_widget, XtN select, hlptool_setfiletypes_cb, "doc");
    XtAddCallback(help_widget, XtN unselect, hlptool_unsetfiletypes_cb, "help");
    XtAddCallback(ref_widget, XtN unselect, hlptool_unsetfiletypes_cb, "ref");
    XtAddCallback(teach_widget, XtN unselect, hlptool_unsetfiletypes_cb, "teach");
    XtAddCallback(doc_widget, XtN unselect, hlptool_unsetfiletypes_cb, "doc");
    XtAddCallback(
        XptVal subject_widget(XtN textEditWidget:XptWidget),
        XtN postModifyNotification, hlptool_subject_changed_cb, false);
enddefine;


;;; xolhelptool creates and pops up the Poplog help tool. The arguments
;;; are:
;;;     subject to appear on subject line (string or <false>)
;;;
;;;     which subsystem buttons should be set
;;;     (list of subsystem names e.g. [pop11 prolog lisp ml] or <false>).
;;;     If <false> then the defaults are the currently loaded subsystems.
;;;
;;;     which types of help file should searched for e.g.
;;;     [help ref teach doc] or <false>. If <false>, all filetypes
;;;     will be searched.
;;;
;;;     search index value (boolean)
;;;
;;;     reference widget or <false>. If <false>, reference widget will
;;;     be pop_ui_app_shell.
;;;
;;;     The helptool widget is returned.
;;;
define :macexpr p_HELPTOOL(subject, subsysoptions, fileoptions, search_index,
                                                    refer_widget) -> widget;
lvars subject subsysoptions fileoptions search_index
    refer_widget widget;
lvars uppercontrol_widget subject_caption lowercontrol_widget
    searchcaption_widget searchoptions_widget footer
    subsyscaption_widget subsysoptions_widget;
dlocal pop_pr_quotes=false;

    Check_string(subject, true);

    unless XptDefaultDisplay then
        XptDefaultSetup();
    endunless;

    unless XptIsLiveType(helptool_widget, "Widget") then

        unless XptIsLiveType(refer_widget, "Widget") then
            pop_ui_app_shell -> refer_widget;
        endunless;

        XtVaCreatePopupShell('helpTool', xolPopupWindowShellWidget,
            refer_widget, (#|
                XtN title,              'Poplog: Help Tool',
                XtN iconName,           'Poplog Help',
                XtN pushpin,            OL_IN,
            |#)) -> helptool_widget;

        XptVal helptool_widget(XtN upperControlArea:XptWidget)
                        -> uppercontrol_widget;

        10, 10, OL_ALL, true
            -> XptVal uppercontrol_widget(XtN hPad, XtN vPad, XtN sameSize,
                                            XtN alignCaptions:XptBoolean);

        XtVaCreateManagedWidget('Subject:', xolCaptionWidget,
            uppercontrol_widget, XptVaArgList([
                {font 'variable'}
            ])) -> subject_caption;

        XtVaCreateManagedWidget('subject_widget', xolTextFieldWidget,
            subject_caption,
            XptVaArgList([{width 350}])) -> subject_widget;

        nullstring -> XptVal subject_widget(XtN string:XptString);

        XtVaCreateManagedWidget('Languages:', xolCaptionWidget,
            uppercontrol_widget,
            XptVaArgList([{position ^OL_LEFT}
                          {font 'variable'}
                            ])) -> subsyscaption_widget;

        XtVaCreateManagedWidget('subsysoptions', xolNonexclusivesWidget,
            subsyscaption_widget,
            XptVaArgList([
              {layoutType ^OL_FIXEDROWS} {center ^false}]))
            -> subsysoptions_widget;

        XtVaCreateManagedWidget(gen_ss_label("pop11"), xolRectButtonWidget,
            subsysoptions_widget, XptVaArgList([])) -> pop11_widget;

        XtVaCreateManagedWidget(gen_ss_label("prolog"), xolRectButtonWidget,
            subsysoptions_widget, XptVaArgList([])) -> prolog_widget;

        XtVaCreateManagedWidget(gen_ss_label("lisp"), xolRectButtonWidget,
            subsysoptions_widget, XptVaArgList([])) -> lisp_widget;

        XtVaCreateManagedWidget(gen_ss_label("ml"), xolRectButtonWidget,
            subsysoptions_widget, XptVaArgList([])) -> ml_widget;

        newassoc([  [pop11 ^pop11_widget] [prolog ^prolog_widget]
                    [lisp ^lisp_widget] [ml ^ml_widget]])
            -> subsystem_option_widgets;

        appproperty(subsystem_option_widgets,
                        procedure(ss, ss_widget);
                        lvars ss ss_widget;
                            if (is_subsystem_loaded(ss) ->>
                                XptVal ss_widget(XtN set:XptBoolean)) then
                                ss
                            else
                                false
                            endif -> subscrl(poss_subsystem_options(ss),
                                            active_subsystem_options);
                        endprocedure);

        XtVaCreateManagedWidget('File types:', xolCaptionWidget,
            uppercontrol_widget,
            XptVaArgList([{position ^OL_LEFT}
                          {font 'variable'}
                          ])) -> searchcaption_widget;

        XtVaCreateManagedWidget('fileoptions', xolNonexclusivesWidget,
            searchcaption_widget,
            XptVaArgList([{layoutType ^OL_FIXEDROWS} {center ^false}]))
                -> searchoptions_widget;

        XtVaCreateManagedWidget('General Help', xolRectButtonWidget,
            searchoptions_widget, XptVaArgList([{set ^true}])) -> help_widget;

        XtVaCreateManagedWidget('Reference', xolRectButtonWidget,
            searchoptions_widget, XptVaArgList([{set ^true}])) -> ref_widget;

        XtVaCreateManagedWidget('Teach', xolRectButtonWidget,
            searchoptions_widget, XptVaArgList([{set ^true}])) -> teach_widget;

        XtVaCreateManagedWidget('Document', xolRectButtonWidget,
            searchoptions_widget, XptVaArgList([{set ^true}])) -> doc_widget;

        newassoc([  [help ^help_widget] [ref ^ref_widget]
                    [doc ^doc_widget] [teach ^teach_widget]])
            -> filetype_option_widgets;

        XtVaCreateManagedWidget('Indexes:', xolCaptionWidget,
            uppercontrol_widget,
            XptVaArgList([{font 'variable'}])) -> indexcaption_widget;

        XtVaCreateManagedWidget('exclusives', xolExclusivesWidget,
            indexcaption_widget,
            XptVaArgList([])) -> indexcaption_widget;

        XtVaCreateManagedWidget('On', xolRectButtonWidget,
            indexcaption_widget,
            XptVaArgList([{set ^true}])) -> indexes_widget;

        XtVaCreateManagedWidget('Off', xolRectButtonWidget,
            indexcaption_widget,
            XptVaArgList([{set ^false}])) ->;

        XtCreateManagedWidget('Search', xolStaticTextWidget,
            uppercontrol_widget, XptArgList([{string 'Search Result: '}])) ->;

        XtCreateManagedWidget('List', xolScrollingListWidget,
            uppercontrol_widget,
            XptArgList([{viewHeight 8} {recomputeWidth ^true}
                        {selectable ^false}]))
                -> helprefs_widget;

        XptVal helptool_widget(XtN lowerControlArea:XptWidget)
                        -> lowercontrol_widget;

        true, 50 -> XptVal lowercontrol_widget(XtN center:XptBoolean,
                                                XtN hSpace);

        ;;; a little bit of messing about to make sure the "okay"
        ;;; widget is big enough to accept the longest of its
        ;;; two labels but that it won't resize and shift the
        ;;; other buttons around when the shorter label is used
        XtVaCreateManagedWidget(VIEW, xolOblongButtonWidget,
            lowercontrol_widget,
            XptVaArgList([{default ^true} {labelJustify ^OL_CENTER}
                          {recomputeSize ^true}])) -> okay_widget;

        false -> XptVal okay_widget(XtN recomputeSize:XptBoolean);

        update_action_label(SEARCH);

        XtVaCreateManagedWidget('Default', xolOblongButtonWidget,
            lowercontrol_widget,
            XptVaArgList([])) -> default_widget;

        XtVaCreateManagedWidget('Dismiss', xolOblongButtonWidget,
            lowercontrol_widget,
            XptVaArgList([])) -> dismiss_widget;

        XptVal helptool_widget(XtN footerPanel:XptWidget) -> footer;

        XtVaCreateManagedWidget('footer_widget', xolStaticTextWidget,
            footer, XptVaArgList([])) -> footer_widget;

        add_helptool_callbacks();
        [% consstring(repeat list_char_width times `_` endrepeat,
              list_char_width) %] -> XpolListItems(helprefs_widget);
        false -> XptVal helprefs_widget(XtN recomputeWidth:XptBoolean);
        hlptool_default_cb(false,false,false);
        set_footer('No matches');
        XtRealizeWidget(helptool_widget);
        XptCenterWidgetOn(helptool_widget, "screen");
        true ->> XptBusyCursorFeedback(helptool_widget)
            -> XptGarbageCursorFeedback(helptool_widget);
    endunless;

    clear_footer();
    XtSetValues(helptool_widget, XptArgList([{pushpin ^OL_IN}]));

    if isstring(subject) then
        subject -> XptVal subject_widget(XtN string:XptString);
    endif;

    if isboolean(search_index) then
        search_index -> XptVal indexes_widget(XtN set:XptBoolean);
    endif;

    set_subsystem_options(subsysoptions);
    set_filetype_options(fileoptions);

    XtPopup(helptool_widget, 0);
    XtDisplayOfObject(helptool_widget) -> helptool_display;
    XRaiseWindow(helptool_display, XtWindow(helptool_widget));
    helptool_widget -> widget;
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, May  4 1995
        Changed to use new call_ved
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr  9 1993
        Uses Xol/xol*Widget instead of XptW*idgetSet
--- John Gibson, Jan 21 1993
        Made active_*_options lists writeable
--- John Williams, Oct 13 1992
        Julian's new version installed at Sussex. Not tested.
--- Julian Clinton, Oct 13 1992
        Fixed truncation problem with Open Windows 2.0. (FR 4471/andyh.9).
--- Adrian Howard, Sep 11 1992
        Selected item is cleared after document is viewed
--- Adrian Howard, Sep 10 1992
        Fixed problem in pre-3.0 OW which caused widgets to pop down even
        if the pushpin resource was in.
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- Adrian Howard, Aug 20 1992
        Set -pop_pr_quotes- to -false- in appropriate places to stop quotes
        being placed round items in the list of subjects
--- Integral Solutions Ltd, Jun  2 1992 (Julian Clinton)
    Call of -veddo- in -helptool_veddo- now adds command to VED command
    buffer.
--- Simon Nichols, Jan 27 1992
        In -helptool_veddo-, added call to -vedsetup- if -vedsetupdone- is
        <false> (see bugreport isl-fr.4395).
Julian Clinton, 21/10/91
    Tool now centers itself on screen.
Julian Clinton, 14/11/91
    Removed explicit string null terminators.
--- Adrian Howard, Sep 13 1991 : couple of bug fixes from Julian installed
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Removed true assignment to -pop_record_writeable-.
    ScrollingList widget has selectable false.
--- Jonathan Meyer, Sep  5 1991 Now uses XpolSelectListItems
--- Jonathan Meyer, Sep  2 1991 Added gc/busy cursor stuff.
--- Jonathan Meyer, Aug 29 1991 XpolDefaultSetup -> XptDefaultSetup
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Prevented options being reset each time the helptool is redisplayed.
    Subsystem button labels now created using gen_ss_label.
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 21/8/91
    Changed helptool_veddo to convert strings like 'ref sysio/sysopen'
    to 'ref sysopen'.
Julian Clinton, 19/8/91
    Double click on the list now invokes help.
Julian Clinton, 15/8/91
    Allowed <undef> to be passed as search_index arg.
Julian Clinton, 14/8/91
    Stopped okay_widget from resizing when label is changed.
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 13/8/91
    Set upper control area padding to 10.
Julian Clinton, 2/8/91
    Changed so Index Search is not disabled if Reference is not selected.
    Added 'Dismiss' button.
    Added 'Default' button to get a default help list to appear.
Julian Clinton, 16/7/91
    Made caption font variable.
Julian Clinton, 15/7/91
    Prevented subject line being nullified if the helptool is simply
        raised.
Julian Clinton, 8/7/91
    Increased popmemlim.
    Changed so that search index caption is disabled if Ref not selected.
Julian Clinton, 21/6/91
    Default subsystems now set according to which subsystems are loaded.
Julian Clinton, 19/6/91
    Added subsystem selection. Users can now define which subsystem docs.
    to search through.
    Added extra arg to xolhelptool to allow callers to define which
    subsystems should be set by default.
Julian Clinton, 17/6/91
    Changed to use guiXlibs.
Julian Clinton, 13/6/91
    Added ref. widget arg to xolhelptool.
 */
