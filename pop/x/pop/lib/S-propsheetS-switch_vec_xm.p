/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/lib/S-propsheetS-switch_vec_xm.p
 > Purpose:         Motif interface for LIB * PROPSHEET
 > Author:          John Gibson, May 17 1993 (see revisions)
 > Documentation:
 > Related Files:   LIB * PROPSHEET
 */
compile_mode :pop11 +strict;

uses-now popxlib, Xm;

section $-propsheet;

exload_batch;

#_IF DEF POPC_COMPILING
#_INCLUDE 'Xm/popc_declare.ph'
#_ENDIF

include sysdefs.ph; ;;; for HPUX define

include propsheetP.ph;
include XmConstants.ph;
include XmMwmUtil.ph;

uses
    xpt_general,

    xmDialogShellWidget,
    xmFormWidget,
    xmFrameWidget,
    xmListWidget,
    xmRowColumnWidget,
    xmScaleWidget,
    xmTextFieldWidget,
    xmArrowButtonGadget,
    xmLabelGadget,
    xmPushButtonGadget,
    xmToggleButtonGadget,

    xpm_listutils,
;

lconstant macro (
    ;;; specifies spacing on form for box buttons
    BOX_SPACING         = 20,
);


/*  Vector of resource names and procedures whose elements are
 *  assigned in this file -- see propsheetP.ph
 */
lconstant gui_switch_vec = INIT_gui_switch_vec;
constant switch_vec_xm   = gui_switch_vec;


/* ======= Run-time switched resource names ======================== */

XmN maximum             -> N_sliderMax;
XmN minimum             -> N_sliderMin;
XmN value               -> N_sliderValue;
XmN activateCallback    -> N_textFieldCallback;


/* ======= Utility Procedures ====================================== */

define lconstant ConsArgList(len);
    lvars len = len fi_>> 1;
    nc_consXptArgList((), len), len
enddefine;

define lconstant Va_create_managed() -> w;
    lvars w;
    fast_XtVaCreateManagedWidget() -> w;
    false -> sys_process_destroy_action(w);
enddefine;

/* wrapper on CREATE that creates a label widget */
define :macexpr p_CREATE_LABEL(name, parent, string, alignment) -> w;
    lvars name, parent, string, alignment, gravity, w;
    unless name then Make_name(string) -> name endunless;
    if alignment == "left" then XmALIGNMENT_BEGINNING
    elseif alignment == "right" then XmALIGNMENT_END
    else XmALIGNMENT_CENTER
    endif -> alignment;
    ;;; we don't use CopiedString for labels - the Motif toolkit does
    ;;; not copy strings passed into XtCreateManagedWidget... hows that
    ;;; for consistency!
    Va_create_managed(name, xmLabelGadget, parent,
                    #|  XmN traversalOn,    false,
                        XmN labelString,    ->XpmCoerceCopiedString(string),
                        XmN alignment,      alignment,
                    |#) -> w;
enddefine;

define :macexpr p_LABEL_STRING(w);
    lvars w;
    fXptVal w(XmN labelString:XpmCopiedString)
enddefine;
;;;
define :macexpr updaterof p_LABEL_STRING(w);
    lvars w;
    () -> fXptVal w(XmN labelString:XpmCopiedString)
enddefine;

define :macexpr p_LABEL_WIDTH(w) -> width;
    lvars (fl, lab) = fXptVal w(XmN fontList:exptr, XmN labelString:exptr);
    lvars width = XmStringWidth(fl, lab);
    XmStringFree(lab);
enddefine;

define :macexpr p_TEXTFIELD_VALUE(/*field, w*/);
    lvars exptr;
    ;;; XtGetValues copies the internal buffer, so we free the copy

    ;;; -------------------------------------------------------------------
    ;;; NB. THE MOTIF DOCS STATE THAT XtGetValues RETURNS A POINTER TO THE
    ;;; INTERNAL BUFFER - however, testing proves that this is not the case
    ;;; -------------------------------------------------------------------
    fXptVal ()(XtN value:exptr) -> exptr;
    Call_formatin(exval_to_string(exptr));
    fast_XtFree(exptr);
enddefine;
;;;
define :macexpr updaterof p_TEXTFIELD_VALUE(string, field, w);
    lvars string, field, w;
    lvars insert_pos = XmTextFieldGetInsertionPosition(w);
    Call_formatout(field, string) -> fXptVal w(XtN value:XptString);
    XmTextFieldSetInsertionPosition(w, insert_pos)
enddefine;


;;; sets the menuof preview to the label of the specified button.
;;; also allows the special item -false-, which will set the
;;; preview item to a blank
define :macexpr p_UPDATE_PREVIEW_ITEM(field, button, button_w);
    lvars field, button, preview, button_w;
    XmOptionButtonGadget(SUBPART(MENU_BUTTON_PART, field)) -> preview;
    if not(button) then
        ;;; set the label type to pixmap - effectively hides the string
        ;;; label. We would like to minimize the size of the button as
        ;;; well, but motif doesn't let us do that.
        XmPIXMAP -> fXptVal preview(XmN labelType:byte);
    else
        XmSTRING -> fXptVal preview(XmN labelType:byte);
        SUBPART(MENU_BUTTON_PART, field) -> field;
        button_w -> fXptVal field(XmN menuHistory:XptWidget);
    endif;
enddefine;

;;; cached function mapping from a widgets fontList resource to its
;;; default fontstruct or fontset plus a flag to indicate which
define :macexpr p_FONTSTRUCT(w);

    lconstant context_p = EXPTRINITSTR(:exptr);
    lconstant type_p = EXPTRINITSTR(:int);

    define cache =
        newproperty([], 5, false, "perm");
    enddefine;

    lvars arg = fXptVal w(XmN fontList:pint), font = cache(arg);
    unless font then

        ;;; get the default entry from the fontList
        lvars fontList = fXptVal w(XmN fontList:exptr);

        unless XmFontListInitFontContext(context_p, fontList) then
            mishap(fontList, 1, 'XmFontList NEEDED');
        endunless;
        lvars context = exacc :exptr context_p;

        ;;; Emulate the toolkit's search strategy for a default font(set).
        ;;; This differs between Text(Field) and other widgets:
        ;;; for text, take (a) the first font set with the default tag,
        ;;; or (b) the first font set, or (c) the first font;
        ;;; for others, take (a) the first entry with the default tag,
        ;;; or (b) the first with the default charset, or (c) the first
        ;;; entry.
        lvars entry, first_entry, best_entry = false;
        lvars is_text = XmIsTextField(w);
        until is_null_external_ptr(XmFontListNextEntry(context) ->> entry) do
            unless best_entry then
                ;;; first entry is always the fallback
                entry ->> first_entry -> best_entry;
            endunless;
            XmFontListEntryGetFont(entry, type_p) -> ;
            nextif(is_text and exacc :int type_p == XmFONT_IS_FONT);
            lvars ptr = XmFontListEntryGetTag(entry);
            lvars tag = exacc_ntstring(ptr);
            fast_XtFree(ptr);
            if tag = XmFONTLIST_DEFAULT_TAG
            or is_text and best_entry == first_entry
            or not(is_text) and tag = XmSTRING_OS_CHARSET
            then
                entry -> best_entry;
                quitif(tag = XmFONTLIST_DEFAULT_TAG);
            endif;
        enduntil;
        XmFontListFreeFontContext(context);
        unless best_entry then
            mishap(fontList, 1, 'INVALID XmFontList');
        endunless;

        best_entry ->> font -> cache(arg);
    endunless;

    ;;; return font/fontSet plus type indicator
    XmFontListEntryGetFont(font, type_p),
    ;;; NB: size of type may be int, char or short; this comparison works
    ;;; because XmFONT_IS_FONT is 0
    exacc :int type_p == XmFONT_IS_FONT;
enddefine;

define :macexpr p_CREATE_COMMAND(parent, field, value, activate_cb) -> w;
    lvars parent, field, value, activate_cb, w;
    Va_create_managed('button', xmPushButtonGadget, parent,
            #|  XmN labelString, ->XpmCoerceCopiedString(value),
                XmN multiClick,  XmMULTICLICK_KEEP,
            |#) -> w;
    ADDCB(w, XmN activateCallback, activate_cb, field);
enddefine;

define :macexpr p_CREATE_BOOLEAN(parent, field, bool_cb) -> w;
    lvars parent, field, bool_cb, w;
    Va_create_managed('toggle', xmToggleButtonGadget, parent,
            #|  XmN alignment,   XmALIGNMENT_END,
                XmN marginWidth, 0,
                ;;; XmN marginLeft,  0,
                XmN multiClick,  XmMULTICLICK_KEEP,
            |#) -> w;
    ADDCB(w, XmN valueChangedCallback, bool_cb, field);
    ;;; indicator size is computed from size of label, probably based
    ;;; on the font height; with a null label and a font set, that can
    ;;; get very small, so let's fix it at something reasonable...
    'X' -> p_LABEL_STRING(w);
    lvars size = fXptVal w(XmN indicatorSize:ushort);
    nullstring -> p_LABEL_STRING(w);
    size -> fXptVal w(XmN indicatorSize:ushort);
enddefine;

define lconstant Initialise_textfield(text_w, string_cb, changed_cb, field);
    lvars text_w, field, string_cb, changed_cb;

    define lconstant String_cb(w, client, call);
        lvars w, client, call;
        string_cb(w, client,
            ;;; true for lose focus
            is_null_external_ptr(call)
            or exacc :XpmAnyCallbackStruct call.XpmACSReason
                    == XmCR_LOSING_FOCUS)
    enddefine;

    ADDCB(text_w, XmN activateCallback, String_cb, field);
    ADDCB(text_w, XmN valueChangedCallback, changed_cb, field);
enddefine;

define :macexpr p_CREATE_NUMERIC_RANGE(parent, field, value, min_value,
                                        max_value, units, slider_update,
                                        slider_textfield_cb, changed_cb)
                                            -> (text, lab1, w, lab2);

    lvars   parent, field, value, min_value, max_value, units, slider_update,
            slider_textfield_cb, changed_cb, text, lab1, w, lab2;

    Va_create_managed('text_field', xmTextFieldWidget, parent, (#|
            XmN value,  ->exval_to_string(Call_formatout(field, value)),
        |#)) -> text;
    p_CREATE_LABEL('label_1', parent, Call_formatout(field, min_value),
                    "center") -> lab1;
    Va_create_managed('slider', xmScaleWidget, parent,
                #|  XmN minimum,     min_value,
                    XmN maximum,     max_value,
                    XmN value,       value,
                    XmN orientation, XmHORIZONTAL,
                |#) -> w;
    if fXptVal w(XmN scaleMultiple:int) == 0 then
        ;;; according to the manual, this should never happen
        1 -> fXptVal w(XmN scaleMultiple:int);
    endif;
    p_CREATE_LABEL('label_2', parent, units, "center") -> lab2;
    ;;; add callbacks to link textfield and slider
    ADDCB(w, XmN dragCallback, slider_update, field);
    ADDCB(w, XmN valueChangedCallback, slider_update, field);
    Initialise_textfield(text, slider_textfield_cb, changed_cb, field);
enddefine;

define :macexpr p_CREATE_STR_OR_NUM(parent, field, string, type, arrow_cb,
                                     string_cb, changed_cb)
                                    -> (tf, arrow1, arrow2);

    lvars   parent, field, string, type, arrow_cb, string_cb, changed_cb,
            tf, (arrow1, arrow2) = (false, false);

    define lconstant Arrow_cb(w, client, call);
        lvars w, client, call;
        arrow_cb(w, client, fXptVal w(XmN arrowDirection:byte) == XmARROW_UP)
    enddefine;

    Va_create_managed('text_field', xmTextFieldWidget, parent, (#|
            XmN value,  ->exval_to_string(string),
        |#)) -> tf;
    if type == NUMBER then
        ;;; we put arrows in their own tab groups
        Va_create_managed('increment', xmArrowButtonGadget, parent,
                        #| XmN navigationType, XmTAB_GROUP |#) -> arrow1;
        ADDCB(arrow1, XmN activateCallback, Arrow_cb, field);
        Va_create_managed('decrement', xmArrowButtonGadget, parent,
                        #|  XmN arrowDirection, XmARROW_DOWN,
                            XmN navigationType, XmTAB_GROUP,
                        |#) -> arrow2;
        ADDCB(arrow2, XmN activateCallback, Arrow_cb, field);
    endif;
    Initialise_textfield(tf, string_cb, changed_cb, field);
enddefine;

define :macexpr p_REBUILD_MENUOF(field, y, measure, options, abbmenu_cb)
                                                            -> widgets;
    lvars   field, y, measure, options, abbmenu_cb, widgets,
            menu, mp, item, w, text;
    SUBPART(MENU_BUTTON_PART, field) -> menu;
    fXptVal menu(XmN subMenuId:XptWidget) -> mp;
    y -> fXptVal mp(XmN numColumns:int);
    {%  fast_for item in_vector options do
            item sys_>< nullstring -> text;
            Va_create_managed(Make_name(text), xmPushButtonGadget, mp,
                    #|  XmN labelString, ->XpmCoerceCopiedString(text),
                        XmN multiClick,  XmMULTICLICK_KEEP,
                    |#) ->> w;
            ADDCB(w, XmN activateCallback, abbmenu_cb, conspair(field, item));
        endfor
    %} -> widgets;
    XtManageChild(menu);
enddefine;

define :macexpr p_CREATE_MENUOF(parent) -> (menu, preview, reset_size);
    lvars parent, menu, mp;
    ;;; we don't have a preview widget
    lconstant preview = false, reset_size = true;
    XmCreatePulldownMenu(parent, 'menu_pane', ConsArgList(#|
            XmN adjustMargin, 0,
            XmN orientation,  XmVERTICAL,
            XmN packing,      XmPACK_COLUMN,
        |#)) -> mp;
    XmCreateOptionMenu(parent, 'menu_button', ConsArgList(#|
            XmN entryAlignment, XmALIGNMENT_END,
            XmN subMenuId,      mp,
            XmN marginWidth,    0,
            XmN spacing,        0,
        |#)) -> menu;
    ;;; set the alignment of menuof types to the right
    XmALIGNMENT_BEGINNING -> fXptVal (XmOptionButtonGadget(menu))(XtN alignment);

#_IF DEFV HPUX >= 9.0
    /* get rid of option label under Motif 1.2 on HPUX */
    nullstring -> p_LABEL_STRING(XmOptionLabelGadget(menu));
#_ENDIF
enddefine;

define :macexpr p_CREATE_LISTOF(parent, field, change_cb) -> (w, rowcol);
    lvars parent, field, change_cb, w, rowcol;

    define lconstant Change_current_cb(w, client, call);
        lvars w, client, call;
        change_cb(w, client, p_CURRENT_LIST_ITEM(w))
    enddefine;

    ;;; the only way to control the size of a list widget is to stick it
    ;;; in a RowColumn and manage the size of that.
    Va_create_managed('rowcol', xmRowColumnWidget, parent,
        #|  XmN resizePolicy, XmRESIZE_NONE,
            XmN resizeWidth,  false,
            XmN packing,      XmPACK_COLUMN,
            XmN adjustMargin, false,
        |#) -> rowcol;
    XmCreateScrolledList(rowcol, 'list', ConsArgList(#|
            XmN listSizePolicy,         XmCONSTANT,
            XmN scrollBarDisplayPolicy, XmSTATIC,
        |#)) -> w;
    XtAddCallback(w, XmN browseSelectionCallback, Change_current_cb, field);
    XtAddCallback(w, XmN singleSelectionCallback, Change_current_cb, field);
    XtAddCallback(w, XmN defaultActionCallback, Change_current_cb, field);
enddefine;

define :macexpr p_CREATE_ONEOF_OR_SOMEOF(parent, field, options, row_size, y,
                                        type, allownone, list_update,
                                        get_options) -> (list_widget, widgets);

    lvars   parent, field, options, row_size, y, type, allownone,
            list_update, get_options, list_widget, widgets,
            item, text, w, indicator_type;

    define lconstant Value_changed_cb(w, client, call);
        lvars w, client, call, field = client, type = field.Pf_type, event;
        l_typespec call :XmToggleButtonCallbackStruct;
        ;;; don't register callbacks for exclusive lists when a button comes up
        returnif((type == ONEOF and not(field.Pf_allownone)
                    and exacc call.set == 0)
            or (type /== SOMEOF and not(exacc call.event)));
        list_update(w, client, call);
    enddefine;

    Va_create_managed('list', xmRowColumnWidget, parent,
        #|  XmN orientation,  XmHORIZONTAL,
            XmN spacing,      0,
            XmN marginWidth,  0,
            XmN marginHeight, 1,
            XmN marginLeft,   0,
            if y /== 1 then
                ;;; more than one row - make all buttons same size
                XmN packing,        XmPACK_COLUMN,
                XmN numColumns,     y,
            else
                ;;; make buttons small as possible
                XmN packing,        XmPACK_TIGHT,
            endif,
            if allownone then
                XmN radioAlwaysOne, false,
                XmN radioBehavior,  true,
            elseif type == ONEOF then
                XmN radioBehavior,  true,
            else
                XmN radioBehavior,  false,
            endif
        |#) -> list_widget;
    XtManageChild(list_widget);
    type == SOMEOF and XmN_OF_MANY or XmONE_OF_MANY -> indicator_type;

    {%  fast_for item in_vector options do
            item sys_>< nullstring -> text;
            Va_create_managed(Make_name(text), xmToggleButtonGadget,
                list_widget,
                #|  XmN indicatorType, indicator_type,
                    XmN labelString,   ->XpmCoerceCopiedString(text),
                |#) ->> w;
            ADDCB(w, XmN valueChangedCallback, Value_changed_cb, field);
        endfor
    %} -> widgets;
enddefine;


;;; create a button for a property box
define :macexpr p_CREATE_BOX_BUTTON(parent, box, name, i, n, box_button_cb)
                                                            -> button;
    lvars parent, box, name, i, n, box_button_cb, button, label;

    if i == 0 then
        (BOX_SPACING fi_* (n fi_+ 1)) - 1 -> fXptVal parent(XmN fractionBase)
    endif;

    ;;; create a button for a property box
    name sys_>< nullstring -> label;
    Va_create_managed(Make_name(label), xmPushButtonGadget, parent,
            #|  XmN labelString, ->XpmCoerceCopiedString(label),
                XmN multiClick,  XmMULTICLICK_KEEP,
            |#) -> button;
    ADDCB(button, XmN activateCallback, box_button_cb, conspair(box,name));

    if i /== 0 then XmATTACH_POSITION else XmATTACH_FORM endif,
    BOX_SPACING fi_* i,
    XmATTACH_FORM,
    XmATTACH_FORM,
    if i /== n then XmATTACH_POSITION else XmATTACH_FORM endif,
    BOX_SPACING fi_* i fi_+ (BOX_SPACING fi_-1),
    i == 0,
    1
        -> fXptVal button(
                XmN leftAttachment,
                XmN leftPosition,
                XmN topAttachment,
                XmN bottomAttachment,
                XmN rightAttachment,
                XmN rightPosition,
                XmN showAsDefault:XptBoolean,
                XmN defaultButtonShadowThickness);
    if i == 0 then
        button -> fXptVal parent(XmN defaultButton:XptWidget);
    endif;
enddefine;

define :macexpr p_PROPSHEET_NEW_BOX(parent, title, new) -> (popup, lca, uca);
    lvars parent, title, new, popup, lca, uca, w;

    fast_XtVaCreatePopupShell('Propsheet', xmDialogShellWidget, parent,
        #|  XmN title,              ->exval_to_string(title or 'properties'),
            XmN allowShellResize,   true,
            XmN mappedWhenManaged,  false,
            ;;; don't want resize handles
            XmN mwmDecorations,     MWM_DECOR_TITLE||MWM_DECOR_MENU
                                            ||MWM_DECOR_BORDER,
        |#) -> popup;
    fast_XtVaCreateWidget('pane', xmRowColumnWidget, popup,
                        #| XmN resizePolicy, XmRESIZE_ANY |#) -> w;
    false -> sys_process_destroy_action(w);
    Va_create_managed('upper_controls', xmRowColumnWidget, w, 0) -> uca;
    fast_XtVaCreateWidget('lower_controls', xmFormWidget, w,
                        #| XmN allowOverlap, true |#) -> lca;
    false -> sys_process_destroy_action(lca);
enddefine;

define :macexpr p_PROPSHEET_NEW(parent, title, new) -> (frame, ca);
    lvars parent, title, new, frame, ca;

    fast_XtVaCreateWidget('frame', xmFrameWidget, parent, 0) -> frame;
    false -> sys_process_destroy_action(frame);
    Va_create_managed(title and Make_name(title) or 'propsheet',
            xmRowColumnWidget, frame,
            #|  XmN adjustMargin,   false,
                XmN packing,        XmPACK_TIGHT,
                XmN spacing,        0,
                XmN marginWidth,    0,
                XmN marginHeight,   0,
                XmN entryAlignment, XmALIGNMENT_CENTER,
            |#) -> ca;
enddefine;

define :macexpr p_CREATE_ROW_CONTROL(name, props, inspos);
    lvars name, props, inspos;
    Va_create_managed(Make_name(name), xmRowColumnWidget, props,
                #|  XmN isAligned,      true,
                    XmN adjustMargin,   false,
                    XmN adjustLast,     false,
                    XmN entryAlignment, XmALIGNMENT_END,
                    XmN orientation,    XmHORIZONTAL,
                    XmN spacing,        0,
                    XmN marginHeight,   1,
                    XmN insertPosition, inspos,
                |#)
enddefine;

define :macexpr p_SET_FOCUS_ON(item);
    lvars item;
    /* documentation advises that we do this twice for good measure ! */
    XmProcessTraversal(item, XmTRAVERSE_CURRENT)->;
    XmProcessTraversal(item, XmTRAVERSE_CURRENT)->;
enddefine;


/* ====== Scrolling Lists ================================================ */

XpmCurrentListItem  -> p_CURRENT_LIST_ITEM;
XpmListItems        -> p_LIST_ITEMS;

define :macexpr p_LIST_ITEMS_SET_NUM_VIS(w, numvis, allownone);
    lvars w, numvis, allownone;
    numvis -> fXptVal w(XmN visibleItemCount:int);
    XtManageChild(w);
    ;;; find out the preferred size of the scrollingwindow parent of the list
    ;;; Query_geom(w) -> (,,,height);
    ;;; FOR SOME UNKNOWN REASON, FORM WIDGETS ONLY SEEM TO ALLOW GROW
    ;;; SO THIS DOESN'T WORK IF THE SIZE HAS GONE DOWN
    ;;; height -> fXptVal (SUBPART(FORM_PART, field))(XmN height:XptDimension);
    allownone and XmSINGLE_SELECT or XmBROWSE_SELECT
        -> fXptVal w(XmN selectionPolicy:byte);
enddefine;


endexload_batch;
endsection;     /* $-propsheet_utils */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Aug 22 1997
        Fix for ToggleButton indicatorSize when using font sets
--- Robert Duncan, Jun 26 1997
        Changed p_FONTSTRUCT to allow for font sets
--- Robert Duncan, Jun 25 1997
        Added p_LABEL_WIDTH
--- Robert Duncan, Apr  4 1997
        Changes for string16
--- Robert John Duncan, Apr 27 1995
        Changed arrow buttons in numeric field to use activate rather than
        disarm callback
--- Robert John Duncan, Apr  6 1995
        Changed p_CREATE_NUMERIC_RANGE to check that the scaleMultiple of
        the slider widget -- i.e. the amount it moves in response to BSelect
        Press -- is non-zero.
--- Jonathan Meyer, Sep 29 1993
        Added HPUX mod to make the 'OptionLabel' disappear on menuof
        under HPUX/Motif 1.2.
 */
