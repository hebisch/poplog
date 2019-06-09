/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/pop/lib/S-propsheetS-switch_vec_xol.p
 > Purpose:         OpenLook interface for LIB * PROPSHEET
 > Author:          John Gibson, May 17 1993 (see revisions)
 > Documentation:
 > Related Files:   LIB * PROPSHEET
 */
compile_mode :pop11 +strict;

uses-now popxlib, Xol;

section $-propsheet;

exload_batch;

#_IF DEF POPC_COMPILING
#_INCLUDE 'Xol/popc_declare.ph'
#_ENDIF

include propsheetP.ph;
include xpt_xtypes.ph;
include XolTextField.ph;

uses
    fast_xt_grab,

    xolAbbrevMenuButtonWidget,
    xolArrowWidget,
    xolCheckBoxWidget,
    xolControlAreaWidget,
    xolExclusivesWidget,
    xolNonexclusivesWidget,
    xolPopupWindowShellWidget,
    xolRectButtonWidget,
    xolTextFieldWidget,
    xolScrollingListWidget,
    xolSliderWidget,
    xolStaticTextWidget,
    xolOblongButtonGadget,

    xpol_listutils,
;

lconstant macro (
    Rbackground     = [XtN background:XptPixel],
    RborderColor    = [XtN borderColor:XptPixel],
);


/*  Vector of resource names and procedures whose elements are
 *  assigned in this file -- see propsheetP.ph
 */
lconstant gui_switch_vec = INIT_gui_switch_vec;
constant switch_vec_xol   = gui_switch_vec;


/* ======= Run-time switched resource names ======================== */

XtN sliderMax       -> N_sliderMax;
XtN sliderMin       -> N_sliderMin;
XtN sliderValue     -> N_sliderValue;
XtN verification    -> N_textFieldCallback;


/* ======= Utility Procedures ====================================== */

;;; Stupid OLIT widgets don't ensure the foreground/fontColor/inputFocusColor
;;; contrasts with the background of the widget. We fix this here:

define lconstant Check_foreground(w);
    lvars w, bg, fg, class = fast_XtClass(w), p, fgname = XtN foreground;

    returnif(class == xolExclusivesWidget or class == xolNonexclusivesWidget
                or class == xolControlAreaWidget);

    if class == xolTextFieldWidget then
        ;;; get out the textEdit widget and check this
        fXptVal w(XtN textEditWidget:XptWidget) -> w;
        fXptVal (fast_XtParent(w))(Rbackground) -> fXptVal w(Rbackground);
        XtN fontColor -> fgname;
    endif;

    fXptVal w(%fgname%:XptPixel) -> fg;
    fXptVal (if class == xolSliderWidget or class == xolCheckBoxWidget then
                ;;; use parents background
                fast_XtParent(w)
             else
                ;;; use my background
                w
            endif) (Rbackground) -> bg;

    ;;; check the foreground color
    if fg == bg then
        ;;; try XtDefaultForeground instead
        XtN XtDefaultForeground -> fXptVal w(%fgname%:XptPixel <TYPE=string>);
        fXptVal w(%fgname%:XptPixel) -> fg;
        if fg == bg then
            ;;; this is not very worthy:
            bg == 0 and 1 or 0 ->> fg -> fXptVal w(%fgname%:XptPixel);
        endif;
    endif;
    ;;; we are now confident that fg /== bg.

    lconstant macro COLORS = [ fXptVal w( XtN fontColor:XptPixel <OPT>,
                                          XtN inputFocusColor:XptPixel <OPT>,
                                          RborderColor <OPT>) ];
    lvars (fc, ifc, bc) = COLORS;
    fc == bg and fg,
    ifc == bg and fg,
    class == xolScrollingListWidget and bc == bg and fg
        -> COLORS;
enddefine;

;;; write a wrapper on create which checks the foreground afterwards:
define lconstant Va_create_managed() -> w;
    lvars w;
    fast_XtVaCreateManagedWidget() -> w;
    Check_foreground(w);
    false -> sys_process_destroy_action(w);
enddefine;

/* Creates a label widget */
define :macexpr p_CREATE_LABEL(name, parent, string, alignment) -> w;
    lvars name, parent, string, alignment, gravity, w;
    unless name then Make_name(string) -> name endunless;
    lconstant macro ( /* see <X11/X.h> */
        WestGravity = 4,
        CenterGravity = 5,
        EastGravity = 6,
    );
    if alignment == "left" then OL_LEFT, WestGravity,
    elseif alignment == "right" then OL_RIGHT, EastGravity,
    else OL_CENTER, CenterGravity
    endif -> (alignment, gravity);
    Va_create_managed(name, xolStaticTextWidget, parent,
        #|  XtN string,     string,
            XtN alignment,  alignment,
            XtN wrap,       false,
            XtN gravity,    gravity,
        |#) -> w;
enddefine;

define :macexpr p_LABEL_STRING(w);
    lvars w, wc = fast_XtClass(w);
    fXptVal w(% if wc == xolStaticTextWidget or wc == xolTextFieldWidget then
                    XtN string
                else
                    XtN label
                endif
              % :XptString)
enddefine;
;;;
define :macexpr updaterof p_LABEL_STRING(w);
    lvars w, wc = fast_XtClass(w);
    () ->
    fXptVal w(% if wc == xolStaticTextWidget or wc == xolTextFieldWidget then
                    XtN string
                else
                    XtN label
                endif
              % :XptString)
enddefine;

define :macexpr p_TEXTFIELD_VALUE(/*field, w*/);
    lvars str, exptr;
    ;;; XtGetValues copies the internal buffer, so we free the copy
    fXptVal ()(XtN string:exptr) -> exptr;
    Call_formatin(exval_to_string(exptr));
    fast_XtFree(exptr);
enddefine;
;;;
define :macexpr updaterof p_TEXTFIELD_VALUE(string, field, w);
    lvars string, field, w;
    Call_formatout(field, string) -> fXptVal w(XtN string:XptString);
enddefine;

;;; sets the menuof preview to the label of the specified button.
;;; also allows the special item -false-, which will set the
;;; preview item to a blank
define :macexpr p_UPDATE_PREVIEW_ITEM(field, button, button_w);
    lvars field, button, preview, button_w;
    SUBPART(PREVIEW_PART, field) -> preview;
    if not(button) then
        XtUnmanageChild(preview);
    else
        XtManageChild(preview);
        fXptVal button_w(XtN label:exptr) -> fXptVal preview(XtN string:exptr);
    endif;
enddefine;


;;; cached function mapping from a widgets font resource to its fontstruct
define :macexpr p_FONTSTRUCT(w) -> font;
    lvars w, font, arg;
    lconstant cache = newproperty([], 5, false, "perm");
    fXptVal w(XtN font:pint) -> arg;
    returnif(cache(arg) ->> font);
    fXptVal w(XtN font:exptr) -> font;
    font -> cache(arg);
enddefine;

define :macexpr p_CREATE_COMMAND(parent, field, value, activate_cb) -> w;
    lvars parent, field, value, activate_cb, w;
    Va_create_managed('button', xolOblongButtonGadget, parent,
                            #| XtN label, value |#) -> w;
    ADDCB(w, XtN select, activate_cb, field);
enddefine;

define :macexpr p_CREATE_BOOLEAN(parent, field, bool_cb) -> w;
    lvars parent, field, bool_cb, w;
    Va_create_managed('toggle', xolCheckBoxWidget, parent,
                    #|  XtN position,   OL_RIGHT,
                        XtN label,      nullstring,
                    |#) -> w;
    ADDCB(w, XtN select, bool_cb, field);
    ADDCB(w, XtN unselect, bool_cb, field);
enddefine;

define lconstant Initialise_textfield(text_w, string_cb, changed_cb, field);
    lvars text_w, field, string_cb, changed_cb;

    define lconstant String_cb(w, client, call);
        lvars w, client, call;
        string_cb(w, client,
            ;;; true for lose focus
            is_null_external_ptr(call)
            or exacc :OlTextFieldVerify call.reason /== OlTextFieldReturn)
    enddefine;

    ADDCB(text_w, XtN verification, String_cb, field);
    fXptVal text_w(XtN textEditWidget:XptWidget) -> text_w;
    ADDCB(text_w, XtN postModifyNotification, changed_cb, field);
enddefine;

define :macexpr p_CREATE_NUMERIC_RANGE(parent, field, value, min_value,
                                        max_value, units, slider_update,
                                        slider_textfield_cb, changed_cb)
                                            -> (text, lab1, w, lab2);

    lvars   parent, field, value, min_value, max_value, units, slider_update,
            slider_textfield_cb, changed_cb, text, lab1, w, lab2;

    define lconstant Slider_moved_cb(w, client, call);
        lvars w, client, call;
        define lconstant Sync_slider(data, id);
            lvars data, id, client;
            destpair(data) -> (w,client);
            slider_update(w, client, null_external_ptr);
        enddefine;
        XtAppAddTimeOut(fast_XtWidgetToApplicationContext(w),
                        1, Sync_slider, conspair(w,client))->;
    enddefine;

    Va_create_managed('text_field', xolTextFieldWidget, parent,
                    #|  XtN string,      Call_formatout(field, value),
                        XtN leftMargin,  0,
                        XtN rightMargin, 0,
                    |#) -> text;
    p_CREATE_LABEL('label_1', parent, Call_formatout(field, min_value),
                        "center") -> lab1;
    Va_create_managed('slider', xolSliderWidget, parent,
                    #|  XtN sliderMin,   min_value,
                        XtN sliderMax,   max_value,
                        XtN sliderValue, value,
                        XtN orientation, OL_HORIZONTAL,
#_IF XOL_VERSION >= 3001
                        XtN width, 1,       ;;; From jonm 6 Jan 94
#_ENDIF
                    |#) -> w;
    p_CREATE_LABEL('label_2', parent, units, "center") -> lab2;

    ;;; add callbacks to link textfield and slider
    ADDCB(w, XtN sliderMoved, Slider_moved_cb, field);
    Initialise_textfield(text, slider_textfield_cb, changed_cb, field);
enddefine;

define :macexpr p_CREATE_STR_OR_NUM(parent, field, string, type, arrow_cb,
                                     string_cb, changed_cb)
                                    -> (tf, arrow1, arrow2);

    lvars   parent, field, string, type, arrow_cb, string_cb, changed_cb,
            tf, (arrow1, arrow2) = (false, false);

    define lconstant Arrow_cb(w, client, call);
        lvars w, client, call;
        arrow_cb(w, client, fXptVal w(XtN direction:short) == OL_TOP)
    enddefine;

    Va_create_managed('text_field', xolTextFieldWidget, parent,
                    #|  XtN string,      string,
                        XtN leftMargin,  0,
                        XtN rightMargin, 0,
                    |#) -> tf;
    if type == NUMBER then
        Va_create_managed('increment', xolArrowWidget, parent, 0) -> arrow1;
        ADDCB(arrow1, XtN btnUp, Arrow_cb, field);
        Va_create_managed('decrement', xolArrowWidget, parent,
                            #| XtN direction, OL_BOTTOM |#) -> arrow2;
        ADDCB(arrow2, XtN btnUp, Arrow_cb, field);
    endif;
    Initialise_textfield(tf, string_cb, changed_cb, field);
enddefine;

define :macexpr p_REBUILD_MENUOF(field, y, measure, options, abbmenu_cb)
                                                            -> widgets;
    lvars   field, y, measure, options, abbmenu_cb, widgets,
            menu, mp, w, item, text;
    SUBPART(MENU_BUTTON_PART, field) -> menu;
    fXptVal menu(XtN menuPane:XptWidget) -> mp;
    if y > 1 then
        measure, OL_FIXEDROWS -> fXptVal mp(XtN measure:int,
                                            XtN layoutType:OlDefine);
    endif;
    {%  fast_for item in_vector options do
            item sys_>< nullstring -> text;
            Va_create_managed(Make_name(text), xolOblongButtonGadget, mp,
                                #| XtN label, text |#) ->> w;
            ADDCB(w, XtN select, abbmenu_cb, conspair(field, item));
         endfor
    %} -> widgets;
    if widgets.datalength /== 0 then
        true -> fXptVal (widgets(1))(XtN default:XptBoolean);
    endif;
enddefine;

define :macexpr p_CREATE_MENUOF(parent) -> (menu, preview, reset_size);
    lvars parent, menu, preview;
    lconstant reset_size = false;
    Va_create_managed('menu_button', xolAbbrevMenuButtonWidget, parent, 0) -> menu;
    Va_create_managed('preview', xolStaticTextWidget, parent, 0) -> preview;
    preview -> fXptVal menu(XtN previewWidget:XptWidget);
enddefine;


define :macexpr p_CREATE_LISTOF(parent, field, change_cb) -> (w, rowcol);
    lvars parent, field, change_cb, w;
    lconstant rowcol = false;

    define lconstant Change_current_cb(w, client, call);
        lvars w, client, call;
        ;;; call is a pointer to an OlListItem.
        change_cb(w, client, XpolListTokenToItem(call));
    enddefine;

    Va_create_managed('list', xolScrollingListWidget, parent,
                    #|  XtN selectable,     false,
                        XtN recomputeWidth, false,
                    |#) -> w;
    XtAddCallback(w, XtN userMakeCurrent, Change_current_cb, field);
#_IF XOL_VERSION <= 1000
    false -> fXptVal (CHILD(2,w))(XtN recomputeWidth:XptBoolean);
#_ENDIF
enddefine;

define :macexpr p_CREATE_ONEOF_OR_SOMEOF(parent, field, options, row_size, y,
                                        type, allownone, list_update,
                                        get_options) -> (list_widget, widgets);

    lvars   parent, field, options, row_size, y, type, allownone,
            list_update, get_options, list_widget, widgets,
            ListClass, ButtonClass, item, text, w;

    define lconstant List_button_cb(w, client, call);
        lvars w, client, call, button, field, options, buttons;
        ;;;     NEED TO DELAY THIS ACTION - USE external_defer_apply instead of timers
        external_defer_apply(list_update, w, client, null_external_ptr, 3);
    enddefine;

    define lconstant List_zero_cb(w, client, call);
        lvars w, client, call, button, field, options, buttons;
        client -> field;
        get_options(field,false) -> options;
        if options == false then
            ;;;         NEED TO DELAY THIS ACTION - USE external_defer_apply instead of timers
            external_defer_apply(list_update, w, client, null_external_ptr, 3);
        endif;
    enddefine;

    if type == ONEOF then
        xolExclusivesWidget -> ListClass; xolRectButtonWidget -> ButtonClass;
    else
        xolNonexclusivesWidget -> ListClass; xolRectButtonWidget -> ButtonClass;
    endif;

    Va_create_managed('list', ListClass, parent,
                    #|  if allownone then XtN noneSet, true endif,
                        if y > 1 then
                            XtN measure,    row_size,
                            XtN layoutType, OL_FIXEDCOLS,
                        endif
                    |#) -> list_widget;

    {%  fast_for item in_vector options do
            item sys_>< nullstring -> text;
            Va_create_managed(Make_name(text), ButtonClass, list_widget,
                                #| XtN label, text |#) ->> w;
            ADDCB(w, XtN select, List_button_cb, field);
            unless type == ONEOF then
                ADDCB(w, XtN unselect, List_button_cb, field);
            elseif allownone then
                ADDCB(w, XtN unselect, List_zero_cb, field);
            endunless
        endfor;
    %} -> widgets;
enddefine;

;;; create a button for a property box
define :macexpr p_CREATE_BOX_BUTTON(parent, box, name, i, n, box_button_cb)
                                                            -> button;
    lvars parent, box, name, i, n, box_button_cb, button, label;

    name sys_>< nullstring -> label;
    Va_create_managed(Make_name(label), xolOblongButtonGadget, parent,
                        #| XtN label, label |#) -> button;
    ADDCB(button, XtN select, box_button_cb, conspair(box,name));
    if i == 0 then
        true -> fXptVal button(XtN default:XptBoolean);
    endif;
enddefine;

define :macexpr p_PROPSHEET_NEW_BOX(parent, title, new) -> (popup, lca, uca);
    lvars parent, title, new, popup, lca, uca;

    fast_XtVaCreatePopupShell('Propsheet', xolPopupWindowShellWidget, parent,
        #|  XtN title,           title or 'Properties',
            XtN pushpin,         OL_IN,
            XtN allowShellResize,true,
            ;;; don't want resize handles
            XtN resizeCorners,   false,
        |#) -> popup;
#_IF XOL_VERSION < 3000
    /*
     * Bug in pre-3.0 OW means pushpin resource ignored so we need this hack
     */
    define lconstant verify_cb(widget, client_data, call_data);
        lvars widget, client_data, call_data;
        ;;; WE POPDOWN IF PUSHPIN NOT IN AND OTHER VERIFY CBs HAVE
        ;;; NOT CHANGED THE -call_data- STATE
        exacc :XptBoolean call_data and
            not(XptVal widget(XtN pushpin:short)==OL_IN)
            -> exacc :XptBoolean call_data;
    enddefine;
    ADDCB(popup, XtN verify, verify_cb, false);
#_ENDIF

    define lconstant Popdown_cb(w, client, call);
        lvars w, client, call;
        client.Pb_allowpopdown
            and exacc :XptBoolean call
            -> exacc :XptBoolean call;
    enddefine;

    ADDCB(popup, XtN verify, Popdown_cb, new);
    fXptVal popup(XtN lowerControlArea:XptWidget,
                  XtN upperControlArea:XptWidget) -> (lca, uca);
    true -> fXptVal uca(XtN traversalManager:XptBoolean);
enddefine;


define :macexpr p_PROPSHEET_NEW(parent, title, new) -> (frame, ca);
    lvars parent, title, new, frame, ca;
    ;;; OLIT does not provide a frame widget
    new -> frame;
    fast_XtVaCreateWidget(title and Make_name(title) or 'propsheet',
                xolControlAreaWidget, parent,
            #|  XtN layoutType,  OL_FIXEDCOLS,
                XtN borderWidth, 2,
                XtN center,      true,
            |#) -> ca;

    ;;; check the border is visible
    lvars (bg, bord) = fXptVal ca(Rbackground, RborderColor);
    if bord == bg then
        XtN XtDefaultForeground -> fXptVal ca(RborderColor <TYPE=string>);
        if fXptVal ca(RborderColor) = bg then
            ;;; this is not very worthy:
            bg == 0 and 1 or 0 -> fXptVal ca(RborderColor);
        endif;
    endif;
    false -> sys_process_destroy_action(ca);
enddefine;

define :macexpr p_CREATE_ROW_CONTROL(name, props, inspos);
    lvars name, props, inspos;
    Va_create_managed(Make_name(name), xolControlAreaWidget, props,
            #|  XtN layoutType,     OL_FIXEDROWS,
                XtN hSpace,         0,
                XtN hPad,           0,
                XtN insertPosition, inspos,
            |#)
enddefine;

define :macexpr p_SET_FOCUS_ON(item);
    lvars item;
    fast_XtCallAcceptFocus(item,
        fast_XtLastTimestampProcessed(fast_XtDisplay(item)))->;
enddefine;


/* ====== Scrolling Lists ================================================ */

XpolListItems       -> p_LIST_ITEMS;
XpolCurrentListItem -> p_CURRENT_LIST_ITEM;

define :macexpr p_LIST_ITEMS_SET_NUM_VIS(w, numvis, allownone);
    lvars w, numvis, allownone;
    numvis -> fXptVal w(XtN viewHeight:int)
enddefine;


endexload_batch;
endsection;     /* $-propsheet_utils */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 14 1994
        Xpt*DeferApply -> external_defer_apply, and removed all following
        Xpt*SetXtWakeups (no longer necessary)
--- John Gibson, Jan  6 1994
        Added setting of slider width to 1 in p_CREATE_NUMERIC_RANGE
 */
