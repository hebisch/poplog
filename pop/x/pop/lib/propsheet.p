/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/lib/propsheet.p
 > Purpose:         High level property sheets library (OpenLook+Motif)
 > Author:          Jonathan Meyer, May 10 1990 (see revisions)
 > Documentation:   REF *PROPSHEET
 */
compile_mode :pop11 +strict;

    ;;; avoid reloading if already loaded globally
#_TERMIN_IF DEF propsheet and valof("propsheet")==true

section $-propsheet =>
        propsheet_init,
        propsheet_new_box,
        propsheet_new,
        propsheet_hide,
        propsheet_show,
        propsheet_visible,
        propsheet_box_can_resize,
        propsheet_box_buttons,
        propsheet_box_callback,
        propsheet_reset_size,
        propsheet_destroy,
        propsheet_sensitive,
        propsheet_label,
        propsheet_set_focus_on,
        propsheet_activate,

        propsheet_save,
        propsheet_reset,
        propsheet_apply,
        propsheet_set_defaults,
        propsheet_refresh,
        propsheet_handle_standard_button,

        propsheet_subpart,
        propsheet_field,
        propsheet_field_name,
        propsheet_field_number,
        propsheet_field_value,
        propsheet_field_default,
        propsheet_field_ident,
        propsheet_field_applier,
        propsheet_field_accepter,
        propsheet_field_acceptor,
        propsheet_field_formatter,
        propsheet_field_converter,
        propsheet_field_width,
        propsheet_field_columns,
        propsheet_ident_class,

        propsheet_field_record,     ;;; FOR COMPATIBILITY ONLY
        propsheet_field_sensitive,  ;;; FOR COMPATIBILITY ONLY
        propsheet_user_data,
        propsheet_import_box,
        propsheet_length,

        propsheet_undef,
        propsheet_acceptreason,
        propsheet_id,
    ),
;

exload_batch;

include propsheetP.ph;
include xt_constants.ph;
include xpt_coretypes.ph;
include xpt_xwindow.ph;
include xpt_xfontstruct.ph;
include ast.ph;

uses
    xt_display,
    xt_widget,
    xt_callback,
    xt_popup,
    xt_widgetinfo,
    xt_composite,
    xt_trans,
    xt_action,
    xt_event,
    fast_xt_util,
    xpt_atomcache,
;

XptLoadProcedures propsheet
lvars
    XtQueryGeometry,        ;;; find a widgets preferred size
    XGetWindowAttributes,   ;;; find out if a window is visible
    XRaiseWindow,           ;;; bring property sheet to the front.
    XTextWidth,             ;;; calculate width of string in pixels
    XExtentsOfFontSet(1) :exptr.{:XFontSetExtents},
                            ;;; get max extents structure for a font set
;

lconstant macro (

    ;;; Resource specs for XptVal
    Rwidth              = [XtN width:XptDimension],
    Rheight             = [XtN height:XptDimension],
    RrecomputeSize      = [XtN recomputeSize:XptBoolean],
    Rset                = [XtN set:XptBoolean],

    ;;; Run-time switched resource name exprs in XptVal
    RsliderMax          = [%"%"% N_sliderMax   %"%"% :int],
    RsliderMin          = [%"%"% N_sliderMin   %"%"% :int],
    RsliderValue        = [%"%"% N_sliderValue %"%"% :int],
);

lvars
    ;;; true for Motif, false for OLIT
    using_motif,

    ;;; set to vector of Motif/OLIT resource names and procedures
    gui_switch_vec,
;

uses-by_name (false, true);


/**********************************************************************
 *                      Property Sheet Declarations
 **********************************************************************/

sysunprotect("propsheet_acceptreason");

constant
    procedure (
        propsheet_init,

        propsheet_new_box,
        propsheet_new,
        propsheet_hide,
        propsheet_show,
        propsheet_visible,
        propsheet_field_ident,
        propsheet_box_buttons,
        propsheet_box_can_resize,
        propsheet_box_callback,
        propsheet_destroy,
        propsheet_set_focus_on,
        propsheet_activate,

        propsheet_save,
        propsheet_reset,
        propsheet_apply,
        propsheet_set_defaults,
        propsheet_refresh,
        propsheet_handle_standard_button,
        propsheet_subpart,
        propsheet_label,
        propsheet_sensitive,

        propsheet_field,
        propsheet_field_name,
        propsheet_field_number,
        propsheet_field_value,
        propsheet_field_default,
        propsheet_field_applier,
        propsheet_field_accepter,
        propsheet_field_formatter,
        propsheet_field_converter,
        propsheet_field_width,
        propsheet_field_columns,
        propsheet_field_record,

        propsheet_ident_class,

        propsheet_user_data,
        propsheet_import_box,
        propsheet_length,
    ),
    macro (
        propsheet_id,
    ),
    propsheet_undef = consundef("value"),
;

vars
    propsheet_acceptreason = "activate",
;

defclass lconstant propbox [external_ptr] {
    pb_datatype,        ;;; "Widget"
>-> pb_ptr:exptr,       ;;; popup shell window widget
    pb_propsheets,      ;;; table of property sheet children
    pb_controlarea,     ;;; control area widget
    pb_buttonarea,      ;;; parent of action button widgets
    pb_buttonwidgets,   ;;; list of button widgets
    pb_buttons,         ;;; list of button labels
    pb_buttoncallback,  ;;; procedure to call when button is clicked
    pb_allowpopdown,    ;;; true if button accepts the click
    pb_userdata,        ;;; user field
};

defclass lconstant propsheet [external_ptr] {
    ps_datatype,        ;;; "Widget"
>-> ps_ptr:exptr,       ;;; control area
    ps_name,            ;;; name of property sheet
    ps_propframe,       ;;; parent of control area
    ps_propbox,         ;;; pointer to parent box
    ps_propfields,      ;;; property containing propsheet fields
    ps_lastfield,       ;;; last created field
    ps_userdata,        ;;; user field
};

l_typespec bool :1#integer_to_boolean;

defclass lconstant propfield /*[external_ptr]*/ {
    pf_propsheet,       ;;; pointer to parent
    pf_name,            ;;; users name
    pf_fieldname,       ;;; lookup name
    pf_number:short,    ;;; index for field
    pf_type:4,          ;;; type of field (a number)
    pf_changed:bool,    ;;; true when field has been modified
    pf_isaligned:bool,  ;;; is aligned
    pf_haslabel:bool,   ;;; whether or not the label is shown
    pf_allownone:bool,  ;;; whether or not the field can have none selected
    pf_old,             ;;; value since last propsheet_save
    pf_current,         ;;; current value
    pf_default,         ;;; default setting
    pf_ident,           ;;; ident associated with this field
    pf_converter,       ;;; inport/export procedure
    pf_accepter,        ;;; field accept procedure
    pf_labelwidget,     ;;; the widget holding the field caption
    pf_buttons,         ;;; list of any button labels
    pf_buttonwidgets,   ;;; list of any button widgets
    pf_subparts,        ;;; widgets that appear on the actual sheet
    pf_controlarea,     ;;; the parent of the field's controls
    pf_backlink,        ;;; previous field on this row
    pf_forwlink,        ;;; next field on this row
    pf_userdata,        ;;; user field
};

    ;;; pf_ and pb_ procedures used in widgtset-specific code
constant procedure (
    Pb_allowpopdown = pb_allowpopdown,
    Pf_type         = pf_type,
    Pf_allownone    = pf_allownone,
    Pf_subparts     = pf_subparts,
);

lconstant

    ;;; Single instance of each of the above classes - new instances
    ;;; are built from copies

    defpropbox = conspropbox
          (
            "Widget",           /*pb_datatype       */
            null_external_ptr,  /*pb_ptr            */
            [],                 /*pb_propsheets     */
            propsheet_undef,    /*pb_controlarea    */
            propsheet_undef,    /*pb_buttonarea     */
            nullvector,         /*pb_buttonwidgets */
            nullvector,         /*pb_buttons        */
            false,              /*pb_buttoncallback*/
            true,               /*pb_allowpopdown  */
            undef               /*pb_userdata      */
          ),

    defpropsheet = conspropsheet
          (
            "Widget",           /*ps_datatype   */
            null_external_ptr,  /*pb_ptr        */
            false,              /*ps_name       */
            false,              /*ps_propframe  */
            false,              /*ps_propbox    */
            false,              /*ps_propfields */
            false,              /*ps_lastfield  */
            undef               /*ps_userdata  */
          ),

    defpropfield = conspropfield
          (
            false,              /*pf_propsheet  */
            false,              /*pf_name       */
            false,              /*pf_fieldname  */
            0,                  /*pf_number     */
            0,                  /*pf_type       */
            false,              /*pf_changed    */
            true,               /*pf_isaligned  */
            true,               /*pf_haslabel   */
            false,              /*pf_allownone  */
            false,              /*pf_old        */
            false,              /*pf_current    */
            false,              /*pf_default    */
            false,              /*pf_ident      */
            false,              /*pf_converter  */
            false,              /*pf_accepter   */
            false,              /*pf_labelwidget */
            false,              /*pf_buttonwidgets / pf_formatter */
            false,              /*pf_buttons / pf_textchanged */
            nullvector,         /*pf_subparts   */
            false,              /*pf_controlarea*/
            false,              /*pf_backlink  */
            false,              /*pf_forwlink */
            undef,              /*pf_userdata  */
        ),

    ident_assoc = newproperty([], 20, false, "tmparg"),
    ident_class = newproperty([], 20, false, "tmparg"),

    macro (
        ;;; the following slots are only used by text-based
        ;;; fields, so we can overload them with slots only used in
        ;;; button based fields
        pf_formatter        = "pf_buttonwidgets",
        pf_textchanged      = "pf_buttons",

        ;;; flags for passing to Get_prop_item
        BOX                 = 2:1e0,
        SHEET               = 2:1e1,
        FIELD               = 2:1e2,
        BUTTON              = 2:1e4,
    ),

    ;;; the following types have text fields in them
    TEXT_TYPES = [^STRING ^NUMBER ^NUMERIC_RANGE],

    ;;; fields which have arrays if buttons in them
    BUTTON_TYPES = [^MENUOF ^SOMEOF ^ONEOF],

    ;;; fields which have multiple items
    LIST_TYPES = [^MENUOF ^SOMEOF ^ONEOF ^LISTOF],

    procedure (
        Refresh_ident ;;; forward declaration
    ),
;

lvars
    ;;; true if we are inside an activate procedure (stops recursion)
    inside_activate = false,

    ;;; true when we are doing several things to a field and only want to
    ;;; align the result once.
    defer_align = false,

    ;;; timer-refresh state variables
    timer_idents = [],
    timer_off = true,
    timer_blocked = false, ;;; true during critical sections of propsheet
    ;;; location where children are inserted into rowcolumn/controlarea widgets
    child_insert_position = false,

    ;;;
    init_done = false,
;

/* ======= Utility Procedures ====================================== */

/* Hook up to the pop ui prompt */
weak global vars active $-pop_ui_promptsource;

define :inline lconstant SET_PROMPTSOURCE(widget);
    if testdef pop_ui_promptsource then
        dlocal weakref pop_ui_promptsource = widget;
    endif;
enddefine;

define :inline lconstant DLOCAL_PROGLIST(value);
    dlocal proglist_state = proglist_new_state(value),  poplastitem;
enddefine;

define lconstant Print_propsheet(p);
    lvars p;
    dlocal pop_pr_quotes = false;
    pr('<'); spr(dataword(p)); pr(XptDescriptor(p, "Widget")); pr('>');
enddefine;

;;; returns first item = to option in options, or false if it is not found.
define lconstant Find_member_vec(item, vec);
    lvars vec, item, anitem, done = false, itemstr, i, n;
    datalength(vec) -> n;
    ;;; first see if the actual item is in the vector
    fast_for i from 1 to n do
        returnif(item == fast_subscrv(i, vec))(item);
    endfast_for;
    ;;; next, see if any item or string ='s the item
    item sys_>< nullstring -> itemstr;
    fast_for i from 1 to n do
        fast_subscrv(i, vec) -> anitem;
        returnif(item = anitem or itemstr = anitem sys_>< nullstring)(anitem);
    endfast_for;
    ;;; finally, if we have an integer, index the items
    item.isinteger and vec(item);
enddefine;

;;; Get_prop_item - main procedure for getting hold of arguments for propshet
;;; procedures. Uses rules to compress things on the stack until there
;;; is either a box, sheet or field (and optionally a button name) left.
;;; The compressions that can take place are:
;;;     <propbox>, <name>   -> <box_button_name>        (BUTTON flag set)
;;;     <propbox>, <name>   -> <propsheet>              (BUTTON flag clear)
;;;     <propsheet>, <name> -> <propfield>
;;;     <propfield>, <name> -> <field_button_name>
;;; -flags- controls which of these parses is valid. If an item is found
;;; but it is not allowed by -flags-, a mishap is generated.

define lconstant Get_prop_item(item, flags);
    lvars item, flags, button = false, names = [], name, num_names = 0, tmp;

    ;;; generates an error message indicating what types are expected
    define lconstant err_msg;
        lvars str;
        '',
        if flags &&/=_0 BOX then sys_>< 'PROPBOX or ' endif;
        if flags &&/=_0 SHEET then sys_>< 'PROPSHEET or ' endif;
        if flags &&/=_0 FIELD then sys_>< 'PROPFIELD or ' endif;
        if flags &&/=_0 BUTTON and flags &&/=_0 BOX then
            sys_>< 'BOX BUTTON or '
        endif;
        if flags &&/=_0 BUTTON and flags &&/=_0 FIELD then
            sys_>< 'FIELD BUTTON or '
        endif;
        -> str;
        allbutlast(3, str) sys_>< 'NEEDED'
    enddefine;
    ;;; take things off the stack until we hit a box, sheet or field
    until item.ispropfield or item.ispropsheet or item.ispropbox do
        unless item.isinteger or item.isword or item.isstring then
            mishap(item, dl(names), num_names fi_+1, err_msg());
        endunless;
        conspair(item, names) -> names;
        num_names fi_+ 1 -> num_names;
        -> item;
    enduntil;

    ;;; parse the item + names -> item [,button]
    if item.ispropbox then
        if num_names == 1 and flags &&/=_0 BUTTON then
            ;;; box, buttonname
            sys_grbg_destpair(names) -> -> name;
            unless Find_member_vec(name, item.pb_buttons) ->> button then
                mishap(item, name, 2, 'NO SUCH (BOX) BUTTON');
            endunless;
        elseif num_names /== 0 then
            sys_grbg_destpair(names) -> names -> name;
            num_names fi_-1 -> num_names;
            ;;; class_apply of boxes takes care of this
            item(name) -> item;
        endif;
    endif;
    if item.ispropsheet then
        if num_names /== 0 then
            sys_grbg_destpair(names) -> names -> name;
            num_names fi_-1 -> num_names;
            if name.isstring then consword(name) -> name; endif;
            unless (item.ps_propfields)(name) ->> tmp then
                mishap(item,name,2,'NO SUCH FIELD');
            endunless;
            tmp -> item;
        endif;
    endif;
    if item.ispropfield then
        if num_names /== 0 then
            if num_names fi_> 1 then
                mishap(item, dl(names), num_names fi_+ 1, err_msg());
            endif;
            sys_grbg_destpair(names) -> -> name;
            if not(fast_lmember(item.pf_type, BUTTON_TYPES)
            and (Find_member_vec(name, item.pf_buttons) ->> button)) then
                mishap(item, name, 2, 'NO SUCH (FIELD) BUTTON');
            endif;
        endif;
    endif;
    ;;; check that we can accept the item
    unless (item.ispropfield and flags &&/=_0 FIELD) or
                (item.ispropsheet and flags &&/=_0 SHEET) or
                (item.ispropbox and flags &&/=_0 BOX) then
        mishap(item, 1, err_msg());
    endunless;
    ;;; if the user specified a button, check that we can accept it
    if button and flags &&=_0 BUTTON then
        mishap(item, button, 2, err_msg());
    endif;
    ;;; stack the things
    item, if flags &&/=_0 BUTTON then button endif;
enddefine;

define lconstant Check_field_type(field, value, type);
    lvars field, value, type, ok = false;
    if type.isinteger then
        field.pf_type == type -> ok;
    elseif type.islist then
        fast_lmember(field.pf_type, type) -> ok;
    elseif type.isprocedure then
        type(value) -> ok;
    endif;
    unless ok then
        mishap(field,value, 2,'INVALID FIELD TYPE');
    endunless;
enddefine;

;;; Field_access is used in closures that want to provide a public
;;; interface to a field slot
define lconstant Field_access(field, access_p,type);
    lvars field, access_p, type;
    Get_prop_item(field, FIELD) -> field;
    field.access_p;
enddefine;

define updaterof Field_access(field, access_p,type);
    lvars value, field, access_p, type;
    Get_prop_item(field, FIELD) -> field -> value;
    if type then Check_field_type(field, value, type) endif;
    value -> field.access_p;
enddefine;

;;; All of the menu/list types can have multiple rows of labels.
;;; This procedure returns the labels in a vector, also saying how many
;;; rows the user has specified
define lconstant Get_list_labels(format) -> options -> x -> y -> format;
    lvars format, options, x = false, y = 0, item;

    ;;;     sticks all elements of nested vectors/lists on stack
    define lconstant Recursive_explode(data);
        lvars data;
        if data.ispair or data.isvector then appdata(data, Recursive_explode);
        elseif data /== [] then data
        endif;
    enddefine;

    if format /== [] and lmember(front(format), [oneof someof menuof listof])
    then
        back(format) -> format;
    endif;

    {%while format /== [] and
                ((front(format) ->> item).islist or item.isvector) do
        ;;; given a list of things
        unless x then length(item) -> x; endunless;
        back(format) -> format;
        y fi_+ 1 -> y;
        Recursive_explode(item);
    endwhile%} -> options;
    if options = nullvector then
        1 ->> x -> y;
    endif;
enddefine;

;;; Takes an option and returns the widget for the option
define lconstant Widget_of_button(field, option);
    lvars field, option, widget, widgets, button, buttons, i, num;
    if field.ispropbox then
        field.pb_buttonwidgets, field.pb_buttons
    else
        field.pf_buttonwidgets, field.pf_buttons
    endif -> (widgets, buttons);
    datalength(widgets) -> num;
    Find_member_vec(option, buttons) -> option;
    fast_for i from 1 to num do
        fast_subscrv(i, widgets) -> widget;
        fast_subscrv(i, buttons) -> button;
        returnif(option == button)(widget);
    endfast_for;
    false;
enddefine;

;;; places all of the fields associated with item on the stack.
;;; if vis_only is -true-, only things that are currently managed are stacked.
define lconstant Explode_propfields(item, vis_only);
    lvars name, field, item, vis_only;
    if item.ispropfield then
        if not(vis_only) or propsheet_visible(item) then item endif;
    elseif item.ispropsheet then
        if not(vis_only) or propsheet_visible(item) then
            if isproperty(item.ps_propfields) then
                fast_appproperty(item.ps_propfields, procedure(name, entry);
                    lvars name, entry;
                    if isword(name) and
                            (not(vis_only) or propsheet_visible(entry)) then
                        entry
                    endif;
                endprocedure);
            endif;
        endif;
    elseif item.islist then
        for field in item do
            Explode_propfields(field, vis_only);
        endfor;
    elseif item.ispropbox then
        Explode_propfields(item.pb_propsheets, vis_only)
    endif;
enddefine;

define Call_formatin(field, value) -> value;
    lvars field, value, proc, type;
    if isprocedure(field.pf_formatter ->> proc) then
        ;;; user specified formatter
        fast_apply(value, proc) -> value;
    endif;
    if field.pf_type /== STRING and not(value.isnumber) then
        strnumber(value) or value -> value;
    endif;
enddefine;

define Call_formatout(field, value) -> value;
    lvars field, value, proc;
    if isprocedure(field.pf_formatter ->> proc) and updater(proc)->>proc then
        ;;; user specified formatter
        fast_apply(value, proc) -> value;
    endif;
    unless value.isstring and fast_subscrs(datalength(value),value) == 0 then
        value sys_>< nullstring -> value;
    endunless;
enddefine;

;;; call accepter for value value and return it if it is ok.
define lconstant Get_new_value(field, value, verify) -> value;
    lvars field, value, procedure verify, proc;
    if isprocedure(field.pf_accepter ->> proc) then
        SET_PROMPTSOURCE(SUBPART(1, field));
        fast_apply(field.pf_propsheet, field.pf_name, value, proc) -> value;
    endif;
    if value /== propsheet_undef then
        if verify(field, value) then
            -> value;
        else
            ->; ;;; get rid of the garbage returned by the verify procedure
            warning(field.pf_propsheet, field, value, 3,
                    'INVALID VALUE FOR FIELD');
            propsheet_undef -> value;
        endif;
    endif;
    if value == propsheet_undef then
        field.pf_current -> value;
    else
        true -> field.pf_changed;
        value -> field.pf_current;
    endif;
enddefine;

define lconstant Update_accept_reason(lose_focus);
    lvars lose_focus;
    if inside_activate or lose_focus then
        "lose_focus"
    else
        "activate"
    endif -> propsheet_acceptreason
enddefine;


;;; turns any string into a legal resource name
define Make_name(str);
    lvars str, i, c, capchar = false, hadfirst = false, isok, wasok = false;
    if str.isword then fast_word_string(str) -> str endif;
    consstring(#|fast_for c in_string str do
        (c fi_>= `A` and c fi_<= `Z`) or (c fi_>= `a` and c fi_<= `z`)
        or (c fi_>= `0` and c fi_<= `9`) or c == `_` or c == `-` -> isok;
        if isok then
            if capchar then lowertoupper(c) ;;; capitalize firt char of word
            elseif wasok then c             ;;; keeps capitalization in words.
            else uppertolower(c)            ;;; otherwise lower case.
            endif;
            false -> capchar; true -> hadfirst;
        elseif c == `\s` or item_chartype(c) == 6 then
            ;;; whitespace - capitalizes next letter
            hadfirst -> capchar;
        endif;
        isok -> wasok;
    endfast_for |#)
enddefine;

define lconstant Checkbox_value(w);
    lvars w;
    fXptVal w(Rset);
enddefine;

define updaterof Checkbox_value(value, item);
    lvars item, value;
    value -> fXptVal item(Rset);
enddefine;

/* The following procedure is set as the insertPosition resource value for
   RowColumn/ControlArea widgets in a property sheet. This is so that we
   can insert widgets in the middle of a RowColumn/ControlArea.
*/
define lconstant Insert_position_proc(exptr);
    lvars w, exptr, n, container;
    if child_insert_position then
        ;;; insert the child where this number specifies
        child_insert_position,
        ;;; make the next insertion happen just after this one
        child_insert_position fi_+1 -> child_insert_position;
    else
        ;;; the default is to always insert the child at the end of the list
        exacc :XptWidget exptr -> w;
        fast_XtParent(w) -> container;
        fXptVal (fast_XtParent(w))(XtN numChildren)
    endif -> exacc [fast] :exval exptr;     ;;; return value
enddefine;

lconstant Insert_position_exfunc = exfunc_export(
            Insert_position_proc,
            XptCallbackFlags,
            false,
        );

/* Takes a widget and tells you where it is in its parents children list.
   Returns -0- if something went wrong
*/
define lconstant Find_child_position(child) -> i;
    lvars child, parent = fast_XtParent(child), i,
        (children, num) = fXptVal parent(XtN children:exptr, XtN numChildren);

    l_typespec children :XptWidget[];
    fast_for i from 1 to num do
        returnif(exacc [fast] children[i] == child);
    endfast_for;
    0 -> i;
enddefine;

define :inline lconstant CHILD(n, w);
    exacc :XptWidget[] (XptVal[fast,nc] (w)(XtN children:exptr)) [n]
enddefine;

;;; Returns the widgets preferred size
define lconstant Query_geom(w);
    lvars w;
    /* see <X11/Intrinsic.h> */
    l_typespec XtWidgetGeometry {
        mask    : uint, /* XtGeometryMask */
        x       : XptPosition,
        y       : XptPosition,
        width   : XptDimension,
        height  : XptDimension,
        sibling : XptWidget,
        stack_mode :int,
      },
        preferred : XtWidgetGeometry,
    ;
    lconstant preferred = EXPTRINITSTR(:XtWidgetGeometry);
    ;;; this should probably do more checking
    exacc [fast] (3) raw_XtQueryGeometry(w, null_external_ptr, preferred);
    exacc [fast] preferred.x;
    exacc [fast] preferred.y;
    exacc [fast] preferred.width;
    exacc [fast] preferred.height;
enddefine;

;;; Cached function that calculated the average width of a font for a widget
define constant Character_width(w) -> aw;
    lconstant cache = newproperty([], 5, false, "perm");
    lvars font, font_is_font = true;
    p_FONTSTRUCT(w) -> font;
    if isboolean(font) then
        ;;; optional font/fontset indicator
        ((), font) -> (font, font_is_font);
    endif;
    lvars arg = exacc [fast] ^pint font;
    returnif(cache(arg) ->> aw);
    ;;; if this isn't a font, return --
    returnif(is_null_external_ptr(font))(false -> aw);

    if font_is_font then
        l_typespec font :XFontStruct;
        lvars dpy = fast_XtDisplay(w);
        ;;; calculate the average character width
        if XptGetFontProperty(font, "QUAD_WIDTH", dpy) ->> aw then
            ;;; use aw
        elseif XptGetFontProperty(font, "AVERAGE_WIDTH", dpy) ->> aw then
            aw / 10 -> aw;
        else
            ;;; (min_bounds.width + max_bounds.width) / 2.3
            ((exacc (exacc font.min_bounds).width
            fi_+ exacc (exacc font.max_bounds).width)) / 2.3 ->aw;
        endif;
    else
        ;;; font set: get the *maximum* width (this is different, but
        ;;; actually agrees better with the TextField XmNcolumns
        ;;; resource)
        exacc[fast] (exacc[nc] (exacc[nc]
            raw_XExtentsOfFontSet(font)).max_logical_extent).width -> aw;
    endif;
    aw -> cache(arg);
enddefine;

define lconstant Label_width(w);
    if using_motif then
        p_LABEL_WIDTH(w)
    else
        lvars string, font;
        p_FONTSTRUCT(w) -> font;
        p_LABEL_STRING(w) -> string;
        exacc [fast] (3):int raw_XTextWidth(font, string, datalength(string))
    endif;
enddefine;

define global propsheet_reset_size(item);
    lvars item, w, width, height, parent;
    Get_prop_item(item, BOX || SHEET) -> item;
    if item.ispropsheet then
        item -> w;
        while XtParent(w) ->> parent do
            Query_geom(w) -> (,, width, height);
            false, false, width, height -> XptWidgetCoords(w);
            parent -> w;
        endwhile;
        propsheet_reset_size(item.ps_propbox);
    else
        if (item.pb_buttonarea ->> w) then
            Query_geom(w) -> (,, width, height);
            false, false, width, height -> XptWidgetCoords(w);
        endif;
    endif;
enddefine;

;;; Aligns the labels of fields in a propsheet/box in columns.
define lconstant Align_fields(prop);
    lvars prop, field, x, maxcolumn = 0, fields, w, width;
    lconstant fieldcolumns = newproperty([],4,[],false);
    ;;; this operation must be done without interruption
    dlocal pop_asts_enabled = false;
    ;;; calculates which column a field is on
    define lconstant Field_column(field) -> col;
        lvars field, col = 1, join;
        while (field.pf_backlink ->> join) do
            col fi_+ 1 -> col;
            join -> field;
        endwhile;
    enddefine;

    ;;; alignment in a property box is done for every visible sheet
    if prop.ispropfield then prop.pf_propsheet.ps_propbox -> prop
    elseif prop.ispropsheet then
        prop.ps_propbox->prop;
    elseunless prop.ispropbox then
        mishap(prop,1,'PROPBOX or SHEET NEEDED');
    endif;

    ;;; things can set defer_align -true- if they want to do more than
    ;;; one operation and then realign the fields
    returnif(defer_align or not(XptIsLiveType(prop, "Widget")));

    ;;; get all fields labels to expand to their desired size
    fast_repeat #| Explode_propfields(prop, false) |# times;
        ->field;
        field.pf_labelwidget -> w;
        if field.pf_isaligned then
            ;;; I need to do further work on this field
            Field_column(field) -> x;
            conspair(w, fieldcolumns(x)) -> fieldcolumns(x);
            fi_max(x, maxcolumn) -> maxcolumn;
        else
            unless field.pf_haslabel then
                ;;; its not aligned and it doesn't have a caption - make
                ;;; it 1 pixels wide whatever it says
                false, 1
            else
                ;;; force the widget to its preferred size
                true -> fXptVal w(RrecomputeSize);
                Query_geom(w) -> (,,width,);
                false, width
            endunless -> fXptVal w(RrecomputeSize, Rwidth);
        endif;
        ;;; need to reset the height of blank fields
        if field.pf_type == BLANK then
            field.pf_current -> fXptVal w(Rheight);
        endif;
    endfast_repeat;

    ;;; going from the left column to the right column,
    ;;; resize labels so that their right hand side is aligned
    lvars x_anchor = 0, max_width, max_widget;
    fast_for x from 1 to maxcolumn do
        fieldcolumns(x) -> fields;
        -1 -> max_width;
        ;;; find the label widget with the largest label text
        fast_for w in fields do
            ;;; don't do this if the field is dead
            nextif(is_null_external_ptr(w));
            Label_width(w) -> width;
            if width fi_> max_width then
                width -> max_width; w -> max_widget;
            endif;
        endfast_for;

        ;;; calculate the preferred size of the widget
        true -> fXptVal max_widget(RrecomputeSize);
        Query_geom(max_widget) -> (x_anchor,,width,);
        false -> fXptVal max_widget(RrecomputeSize);
        x_anchor fi_+ width -> x_anchor;

        ;;; resize all the labels to the size specified by the largest label
        fast_for w in fields do
            nextif(is_null_external_ptr(w));
            (x_anchor fi_- fXptVal w(XtN x:XptPosition)) -> fXptVal w(Rwidth);
        endfast_for;
        sys_grbg_list(fields);
    endfast_for;
    clearproperty(fieldcolumns);
enddefine;

/**********************************************************************
 *                      Property Sheet Field Classes
 **********************************************************************/

/* ===== Message Strings - just displays string. No user input ============= */

define lconstant Verify_message(field, value) -> (value, ok);
    lvars field, value, ok = true;
    value sys_>< nullstring -> value;
enddefine;

define lconstant Create_message(field, parent, type, format) -> format;
    lvars field, parent, type, format, w, value;
    dest(format) -> format -> ; dest(format) -> format -> value;
    value sys_>< nullstring -> value;
    p_CREATE_LABEL('message_label', parent, value, "left") -> w;
    value ->> field.pf_current -> field.pf_default;
    {%w%} -> field.pf_subparts;
enddefine;

define lconstant Set_message(field, value);
    lvars field, value, w = SUBPART(MESSAGE_LABEL_PART, field);
    value -> p_LABEL_STRING(w);
enddefine;

/* ===== Blank - just displays blank space. No user input =============== */

define lconstant Verify_blank(field, value) -> (value, ok);
    lvars field, value, ok;
    value.isinteger and value > 1 and value < 1000 -> ok;
enddefine;

define lconstant Create_blank(field, parent, type, format) -> format;
    lvars field, parent, type, format, value = false;
    if format /== [] and hd(format) == "blank" then
        dest(format) -> format ->
    endif;
    if format /== [] and format.hd /== "(" then
        dest(format) -> format -> value;
    endif;
    unless value.isinteger then
        50 -> value;
    endunless;
    false, value -> fXptVal (field.pf_labelwidget)(RrecomputeSize, Rheight);
    value -> field.pf_current;
    propsheet_undef -> field.pf_default;
enddefine;

define lconstant Set_blank(field, value);
    lvars field, value;
    value -> fXptVal (field.pf_labelwidget)(Rheight);
enddefine;

/* ==== Commands (push buttons) ====================================== */

define lconstant Verify_command(field, value) -> (value, ok);
    lvars field, value, ok = (value.isword or value.isstring);
    value sys_>< nullstring -> value;
enddefine;

define lconstant Create_command(field, parent, type, format) -> format;
    lvars field, parent, type, format, w, value;

    define lconstant Activate_cb(w, client, call);
        lvars w, client, call, value;
        Get_new_value(client, client.pf_current, Verify_command) -> value;
        value -> p_LABEL_STRING(w);
    enddefine;

    dest(format) -> format ->;
    if format /== [] and format.hd /== "(" then
        dest(format) -> format
    else
        field.pf_fieldname
    endif sys_>< nullstring -> value;

    p_CREATE_COMMAND(parent, field, value, Activate_cb) -> w;
    value -> field.pf_current;
    propsheet_undef -> field.pf_default;
    {%w%} -> field.pf_subparts;
enddefine;

define lconstant Set_command(field, value);
    lvars field, value, w = SUBPART(COMMAND_BUTTON_PART, field);
    value -> p_LABEL_STRING(w);
enddefine;

/* ==== Booleans (displayed using a CheckBox) ======================== */

define lconstant Verify_boolean(field, value) -> (value, ok);
    lvars field, value, ok;
    value.isboolean -> ok;
enddefine;

define lconstant Create_boolean(field,parent, type, format) -> format;
    lvars field, parent, type, format, w, value;

    define lconstant Bool_cb(w, client, call);
        lvars w, client, call;
        Get_new_value(client, Checkbox_value(w), Verify_boolean)
            -> Checkbox_value(w);
    enddefine;

    dest(format) -> format -> value;
    if value.isword then valof(value) -> value; endif;

    p_CREATE_BOOLEAN(parent, field, Bool_cb) -> w;
    if value then true else false endif -> value;
    value -> Checkbox_value(w);
    value ->> field.pf_current -> field.pf_default;
    {%w%} -> field.pf_subparts;
enddefine;

define lconstant Set_boolean(field, value);
    lvars field, value;
    value -> fXptVal (SUBPART(TOGGLE_PART, field))(Rset);
enddefine;

/* ======= Numeric Ranges (displayed using a slider) =============== */

define lconstant Verify_numeric_range(field, value) -> (value, ok);
    lvars field, value, ok;
    SUBPART(SLIDER_PART, field) -> field;
    lvars (Min, Max) = fXptVal field(RsliderMin, RsliderMax);
    value.isinteger and value fi_>= Min and value fi_<= Max -> ok;
enddefine;

define lconstant Changed_cb(w, client, call);
    lvars w, client, call;
    true -> client.pf_textchanged;
enddefine;

define lconstant Create_numeric_range(field,parent, type, format) -> format;
    lvars   field, parent, type, format, value, units, min_value,
            max_value, w;

    define lconstant Slider_textfield_cb(w, client, lose_focus);
        lvars w, client, lose_focus, new;
        dlocal propsheet_acceptreason;
        Update_accept_reason(lose_focus);
        Get_new_value(client, p_TEXTFIELD_VALUE(client, w),
                                Verify_numeric_range) -> new;
        new -> fXptVal (SUBPART(SLIDER_PART, client))(RsliderValue);
        new -> p_TEXTFIELD_VALUE(client, w);
        false -> client.pf_textchanged;
    enddefine;

    define lconstant Slider_update(w, client, call);
        lvars w, client, call, new;
        dlocal propsheet_acceptreason = "slider_moved";
        Get_new_value(client, fXptVal w(RsliderValue), Verify_numeric_range)
                                -> new;
        new -> fXptVal w(RsliderValue);
        new -> p_TEXTFIELD_VALUE(client, SUBPART(TEXT_FIELD_PART, client));
        false -> client.pf_textchanged;
    enddefine;

    DLOCAL_PROGLIST(format);
    intof(readitem()) ->> min_value -> value;
    pop11_try_nextreaditem("-")->;
    intof(abs(readitem())) -> max_value;

    if pop11_try_nextreaditem("-") then
        max_value, intof(abs(readitem())) -> (value, max_value);
    endif;

    if (nextreaditem() ->> w).isstring or w.isword and w /== "(" then
        readitem()->;
        Call_formatout(field, max_value) sys_>< ' ' sys_>< w -> units;
    else
        Call_formatout(field, max_value) -> units;
    endif;
    proglist -> format;

    ;;; returns vector (text, lab1, w, lab2)
    {% p_CREATE_NUMERIC_RANGE(parent, field, value, min_value, max_value,
                                units, Slider_update, Slider_textfield_cb,
                                Changed_cb)
    %} -> field.pf_subparts;
    value ->> field.pf_current -> field.pf_default;
enddefine;

define lconstant Set_numeric_range(field, value);
    lvars field, value, subparts;
    value -> p_TEXTFIELD_VALUE(field, SUBPART(TEXT_FIELD_PART, field));
    value -> fXptVal (SUBPART(SLIDER_PART, field))(RsliderValue);
    ;;; we need to do this to stop the accepter from being fired
    false -> field.pf_textchanged;
enddefine;

/* ====== Strings and Numbers (using a TextField) ======================== */

define lconstant Verify_string_or_number(field, value) -> (value, ok);
    lvars field, value, ok;
    (field.pf_type == STRING and value.isstring) or value.isnumber -> ok;
enddefine;

define lconstant Create_string_or_number(field,parent, type, format) -> format;
    lvars field, parent, type, format, value, w, string, units = false,
        tf, arrow1, arrow2;

    define lconstant String_cb(w, client, lose_focus);
        lvars w, client, lose_focus, new;
        dlocal propsheet_acceptreason;
        Update_accept_reason(lose_focus);
        Get_new_value(client,p_TEXTFIELD_VALUE(client, w),
                            Verify_string_or_number) -> new;
        new -> p_TEXTFIELD_VALUE(client, w);
        false -> client.pf_textchanged;
    enddefine;

    define lconstant Arrow_cb(w, client, arrow_up);
        lvars w, client, arrow_up, new, incr;
        dlocal propsheet_acceptreason;
        if arrow_up then
            "increment", 1
        else
            "decrement", -1
        endif -> (propsheet_acceptreason, incr);

        client.pf_current + incr -> new;
        Get_new_value(client, new, Verify_string_or_number) -> new;
        new -> p_TEXTFIELD_VALUE(client, SUBPART(TEXT_FIELD_PART, client));
        false -> client.pf_textchanged;
    enddefine;

    DLOCAL_PROGLIST(format);
    readitem() -> value;
    Call_formatout(field, value) -> string;
    if type == NUMBER and (isstring(nextreaditem() ->> w)
                or (w.isword and w /== "(")) then
        readitem()->;
        w sys_>< nullstring -> units;
    endif;
    proglist -> format;

    p_CREATE_STR_OR_NUM(parent, field, string, type, Arrow_cb, String_cb,
                        Changed_cb) -> (tf, arrow1, arrow2);

    if type== NUMBER then
        if units then
            p_CREATE_LABEL('units', parent, units,"center") -> units
        endif;
        {%tf, arrow1, arrow2, if units then units endif%} -> field.pf_subparts;
    else
        {%tf%} -> field.pf_subparts;
    endif;
    value ->> field.pf_current -> field.pf_default;
enddefine;

define lconstant Set_string_or_number(field, value);
    lvars field, value;
    value -> p_TEXTFIELD_VALUE(field, SUBPART(TEXT_FIELD_PART, field));
    ;;; we need to do this to stop the accepter from being fired
    false -> field.pf_textchanged;
enddefine;

/* ===== Menu List (displayed using an Abbreviated Menu Button) ========= */

define lconstant Verify_menuof(field, value) -> (value, ok);
    lvars field, value, ok;
    if not(value) then
        true -> ok
    else
        Find_member_vec(value, field.pf_buttons) ->> value -> ok;
    endif;
enddefine;

define lconstant Update_preview_item(field, button);
    lvars field, button, w = false;
    if button and not(Widget_of_button(field, button) ->> w) then
        mishap(field, button, 2, 'NO SUCH BUTTON')
    endif;
    p_UPDATE_PREVIEW_ITEM(field, button, w)
enddefine;

define lconstant Rebuild_menuof(field, format) -> format;
    lvars field, format, item, options, measure, y, widgets;

    define lconstant Abbmenu_cb(w, client, call);
        lvars w, client, call, button, field, tmp = false;
        destpair(client) -> button -> field;
        Get_new_value(field, button, Verify_menuof) -> button;
        Update_preview_item(field, button);
    enddefine;

    Get_list_labels(format) -> options -> measure -> y -> format;

    if (field.pf_buttonwidgets ->> widgets).isvector then
        appdata(widgets, DESTROY);
    endif;

    p_REBUILD_MENUOF(field, y, measure, options, Abbmenu_cb)
            -> field.pf_buttonwidgets;
    options -> field.pf_buttons;
    options.datalength /== 0 and options(1)
        ->> field.pf_current-> field.pf_default;
    if options.datalength /== 0 then
        Update_preview_item(field, options(1));
    endif;
enddefine;

define lconstant Create_menuof(field,parent, type, format) -> format;
    lvars field, parent, type, format, preview, menu, reset_size;
    p_CREATE_MENUOF(parent) -> (menu, preview, reset_size);
    {% menu, if preview then preview endif %} -> field.pf_subparts;
    Rebuild_menuof(field, format) -> format;
    if reset_size then
        ;;; MOTIF - for some reason we need to get the widget to set its size
        ;;; manually
        lvars (, , width, height) = Query_geom(menu);
        false, false, width, height -> XptWidgetCoords(menu);
    endif
enddefine;

define lconstant Set_menuof(field, value);
    lvars field, value, orig_value = value;
    returnif(field.pf_buttons = nullvector);
    Update_preview_item(field, value);
enddefine;

/* ====== Scrolling Lists ================================================ */

define lconstant Verify_listof(field, value) -> (value, ok);
    lvars field, value, ok;
    if not(value) and field.pf_allownone then
        true -> ok
    else
        Find_member_vec(value, field.pf_buttons) ->> value -> ok;
    endif;
enddefine;

/* Changes the items on view in a scrolling list */
define lconstant Rebuild_listof(field, format) -> format;
    lvars field, format, options, measure, y, w =SUBPART(LIST_PART, field),
            allownone;
    dest(format) -> format -> /* "listof" */;
    if format.hd.isinteger then
        dest(format) -> format -> measure;
        Get_list_labels(format) -> options -> -> -> format;
    else
        Get_list_labels(format) -> options -> measure -> y -> format;
        if y == 1 then 4 -> measure endif;
    endif;
    ;;; determine if we can have no items set
    format matches #_<[== ( == allowNone == ) ==]>_#
            ->> field.pf_allownone -> allownone;

    ;;; WE SHOULD CHANGE Xp*ListItems to accept vectors...
    sys_grbg_list(options.destvector.conslist.dup -> p_LIST_ITEMS(w));
    if options.datalength /== 0 and not(allownone) then
        options(1) -> p_CURRENT_LIST_ITEM(w);
    endif;
    options -> field.pf_buttons;
    options.datalength /== 0 and not(allownone) and options(1)
            ->> field.pf_current -> field.pf_default;

    ;;; change the number of items on view
    p_LIST_ITEMS_SET_NUM_VIS(w, measure, allownone);
enddefine;

define lconstant Create_listof(field, parent, type, format) -> format;
    lvars field, parent, type, format, w, rowcol;

    define lconstant Change_current_cb(w, field, button);
        lvars w, field, button;
        unless using_motif then
            ;;; OLIT - if the user selects the same item twice, and the field
            ;;; allows none, then the item is deselected
            if field.pf_allownone and button == field.pf_current then
                false ->> button -> p_CURRENT_LIST_ITEM(w);
            endif
        endunless;
        Get_new_value(field, button, Verify_listof) -> button;
        button -> p_CURRENT_LIST_ITEM(w);
    enddefine;

    p_CREATE_LISTOF(parent, field, Change_current_cb) -> (w, rowcol);
    {% w, if rowcol then rowcol endif %} -> field.pf_subparts;
    Rebuild_listof(field, format) -> format;
enddefine;

define lconstant Set_listof(field, value);
    lvars field, value, orig_value = value;
    returnif(field.pf_buttons = nullvector);
    value -> p_CURRENT_LIST_ITEM(SUBPART(LIST_PART, field));
enddefine;

/* ====== Selection Lists (Exclusive/Nonexclusive lists) ================= */

;;; sets a selection list from the field.pf_current value
;;; (this should be validated elsewhere)
define lconstant Update_options(field);
    lvars field, item, options, option, widgets, list_w, type, set,
            i, n, buttons = field.pf_buttons, Rnone, Vnone;
    field.pf_current -> options;
    SUBPART(LIST_PART, field) -> list_w;
    field.pf_buttonwidgets -> widgets;
    datalength(widgets) -> n;
    if options == false or options == [] then
        ;;; clear all things
        fast_for i from 1 to n do
            fast_subscrv(i, widgets) -> item;
            false -> Checkbox_value(item);
        endfast_for;
    elseif field.pf_type /== SOMEOF then
        ;;; set one thing one
        if options.ispair then fast_front(options) -> options endif;
        if using_motif then
            XmN radioAlwaysOne, false
        else
            XtN noneSet, true
        endif -> (Rnone, Vnone);
        procedure;
            dlocal %fXptVal list_w(%Rnone%:XptBoolean)% = Vnone;

            fast_for i from 1 to n do
                fast_subscrv(i, widgets) -> item;
                fast_subscrv(i, buttons) -> option;
                options == option -> set;
                if set /== Checkbox_value(item) then
                    set -> Checkbox_value(item)
                endif;
            endfast_for;
        endprocedure();
    else
        ;;; set several things on
        fast_for i from 1 to n do
            fast_subscrv(i, widgets) -> item;
            fast_subscrv(i, buttons) -> option;
            ispair(fast_lmember(option, options)) -> set;
            if set /== Checkbox_value(item) then
                set -> Checkbox_value(item);
            endif;
        endfast_for;
    endif;
enddefine;

;;; converts users names to internal names.
define lconstant Verify_oneof_or_someof(field, options) -> (options, ok);
    lvars field, options, ok = true, options, set,
        buttons = field.pf_buttons;
    if field.pf_type == SOMEOF then
        if options.islist then
            maplist(options, procedure(name);
                    lvars name, new;
                    if Find_member_vec(name, buttons) ->> new then new
                    else
                        warning(name, 1, 'UNKNOWN FIELD ELEMENT');
                        false -> ok;
                    endif;
                endprocedure) -> options;
        else
            false -> ok;
        endif;
    else
        unless Find_member_vec(options, buttons) ->> set then
            ;;; the only valid non-member is the item -false-
            ;;; in exclusive lists which allow no setting
            unless field.pf_allownone and not(options) then
                false -> ok;
            endunless;
        endunless;
        set -> options;
    endif;
enddefine;

define lconstant Create_oneof_or_someof(field,parent, type, format) -> format;
    lvars field, parent, type, format, list_widget, row_size, item, text, w,
        widgets, options, ListClass, ButtonClass, y, indicator_type, allownone;

    ;;; complicated by the fact that Motif and OLIT do wierd things inside
    ;;; callbacks for radio lists.

    ;;; reads the current widgets to determine what options are set
    define lconstant Get_options(field, curr_w) -> n;
        lvars field, item, n, curr_w, type = field.pf_type;
        if type == SOMEOF then false -> curr_w; endif;
        [%fast_for item,n in_vector field.pf_buttonwidgets, field.pf_buttons do
                if curr_w then
                    ;;; only interested in the value of this one setting
                    if curr_w == item and fXptVal item(Rset)
                    then n; quitloop;
                    endif;
                else
                    ;;; collate all the settings
                    if fXptVal item(Rset) then n endif;
                endif;
            endfast_for%] -> n;
        if type /== SOMEOF then
            ;;; only interested in the first item
            (n /== [] and (sys_grbg_destpair(n) ->)) -> n;
        endif;
    enddefine;

    define lconstant List_update(w, client, call);
        lvars w, client, call, field = client;
        Get_new_value(field, Get_options(field, w), Verify_oneof_or_someof) -> ;
        Update_options(field);
    enddefine;

    Get_list_labels(format) -> options -> row_size -> y -> format;

    ;;; determine if we can have no items set
    type == ONEOF and format matches #_<[== ( == allowNone == ) ==]>_#
            ->> field.pf_allownone -> allownone;

    p_CREATE_ONEOF_OR_SOMEOF(parent, field, options, row_size, y, type,
                            allownone, List_update, Get_options)
                            -> (list_widget, widgets);

    options -> field.pf_buttons;
    widgets -> field.pf_buttonwidgets;
    {%list_widget%} -> field.pf_subparts;
    if type == ONEOF and not(allownone) and widgets.datalength /== 0
    then
        true -> Checkbox_value(widgets(1));
    endif;
    widgets.datalength /== 0 and Get_options(field, false)
        ->> field.pf_current
        -> field.pf_default;
enddefine;

define lconstant Set_oneof_or_someof(field, value);
    lvars field, value, buttons, set;
    returnif(field.pf_buttons = nullvector);
    Update_options(field);
enddefine;

lconstant
    verify_procs = {%
        Verify_boolean,                 /* BOOLEAN */
        Verify_string_or_number,        /* STRING */
        Verify_string_or_number,        /* NUMBER */
        Verify_numeric_range,           /* NUMERIC_RANGE */
        Verify_menuof,                  /* MENUOF */
        Verify_listof,                  /* LISTOF */
        Verify_oneof_or_someof,         /* ONEOF */
        Verify_oneof_or_someof,         /* SOMEOF */
        Verify_message,                 /* MESSAGE*/
        Verify_blank,                   /* BLANK */
        Verify_command,                 /* COMMAND */
    %},

    create_procs = {%
        Create_boolean,                 /* BOOLEAN */
        Create_string_or_number,        /* STRING */
        Create_string_or_number,        /* NUMBER */
        Create_numeric_range,           /* NUMERIC_RANGE */
        Create_menuof,                  /* MENUOF */
        Create_listof,                  /* LISTOF */
        Create_oneof_or_someof,         /* ONEOF */
        Create_oneof_or_someof,         /* SOMEOF */
        Create_message,                 /* MESSAGE */
        Create_blank,                   /* BLANK */
        Create_command,                 /* COMMAND */
    %},

    set_procs = {%
        Set_boolean,                    /* BOOLEAN */
        Set_string_or_number,           /* STRING */
        Set_string_or_number,           /* NUMBER */
        Set_numeric_range,              /* NUMERIC_RANGE */
        Set_menuof,                     /* MENUOF */
        Set_listof,                     /* LISTOF */
        Set_oneof_or_someof,            /* ONEOF */
        Set_oneof_or_someof,            /* SOMEOF */
        Set_message,                    /* MESSAGE */
        Set_blank,                      /* BLANK */
        Set_command,                    /* COMMAND */
    %},
;

/* ========= Propsheet Subparts ====================================== */

define global propsheet_subpart(item, subpart);
    lvars item, subpart, list, w, button = false, n;
    lconstant subpart_map = [
        ;;; name       pos   field types that have this subpart
        command_button  1   [^COMMAND]
        message_label   1   [^MESSAGE]
        text_field      1   [^^TEXT_TYPES]
        label_1         2   [^NUMERIC_RANGE]
        slider          3   [^NUMERIC_RANGE]
        label_2         4   [^NUMERIC_RANGE]
        toggle          1   [^BOOLEAN]
        increment       2   [^NUMBER]
        decrement       3   [^NUMBER]
        units           4   [^NUMBER]
        menu_button     1   [^MENUOF]
        list            1   [^^LIST_TYPES]
        preview         2   [^MENUOF]           ;;; OPENLOOK only
    ];
    Get_prop_item(item, #_< BOX || FIELD >_#) -> item;
    if item.ispropbox then
        if subpart == "upper_controls" then
            item.pb_controlarea
        elseif subpart == "lower_controls" then
            item.pb_buttonarea
        elseif subpart == "buttons" then
            item.pb_buttonwidgets
        elseif Widget_of_button(item, subpart) ->> w then
            w
        else
            mishap(subpart,1,'UNKNOWN (BOX) SUBPART');
        endif;
    else
        lconstant FIELD_ERR = 'UNKNOWN (FIELD) SUBPART';
        if subpart == "buttons" then
            unless fast_lmember(item.pf_type, BUTTON_TYPES) then
                mishap(item, subpart, 2, FIELD_ERR);
            endunless;
            ;;; return the button widgets in a vector
            item.pf_buttonwidgets
        elseif subpart == "control_area" then
            ;;; return the control area that contains the button
            item.pf_controlarea
        elseif subpart == "label" then
            item.pf_labelwidget
        elseif subpart == "all" then
            item.pf_subparts
        else
            if subpart.isword
            and (fast_lmember(subpart, subpart_map) ->> list)
            and not(using_motif and subpart == "preview")
            then
                ;;; specified a subpart that I know about
                unless fast_lmember(item.pf_type, list.tl.tl.hd) then
                    mishap(item, subpart, 2, FIELD_ERR);
                endunless;
                list.tl.hd -> n;
                fi_check(n, 1, datalength(item.pf_subparts))->;
                SUBPART(n, item);
            elseif fast_lmember(item.pf_type, BUTTON_TYPES) and
                        Widget_of_button(item, subpart) ->> w then
                w
            else
                mishap(subpart, 1, FIELD_ERR);
            endif;
        endif;
    endif;
enddefine;

/* ====== Field Attribute Procedures =============================== */

define lconstant Get_default(field) -> value;
    lvars field, value, try_convert = false;
    field.pf_default -> value;
    if value.isident then
        idval(value) -> value;
        true -> try_convert;
    elseif value.isprocedure then
        fast_apply(field.pf_propsheet, field.pf_name, value) -> value;
        true -> try_convert;
    endif;
    if try_convert then
        if field.pf_converter then
            value -> (field.pf_converter)() -> value;
            if verify_procs(field.pf_type)(field, value) then
                -> value;
            else
                ->; ;;; throw away result of verify procedure
                warning(field.pf_propsheet, field, value, 3,
                        'INVALID VALUE FOR FIELD');
                propsheet_undef -> value;
            endif;
        endif;
    endif;
enddefine;

define global propsheet_field_value(field) -> value;
    lvars field, value;
    dlocal timer_blocked = true;
    Get_prop_item(field, FIELD) -> field;
    if field.pf_textchanged
    and fast_lmember(field.pf_type, TEXT_TYPES) then
        ;;; make it appear as if the user had hit return
        fast_XtCallCallbacks(SUBPART(TEXT_FIELD_PART, field),
                                N_textFieldCallback, null_external_ptr);
    endif;
    field.pf_current -> value;
    if field.pf_converter then
        (field.pf_converter)(value) -> value;
    endif;
enddefine;

define updaterof propsheet_field_value(field);
    lvars propsheet, field, value, type, tmp;
    dlocal timer_blocked = true;
    Get_prop_item(field, FIELD) -> field -> value;
    field.pf_type -> type;
    if field.pf_converter then
        value -> (field.pf_converter)() -> value;
    endif;
    if value == propsheet_undef then Get_default(field) -> value
    elseif verify_procs(type)(field, value) then
        -> value;
    else
        mishap(value, 1, 'INVALID VALUE FOR FIELD');
    endif;
    field.pf_current -> field.pf_old;
    true -> field.pf_changed;
    value -> field.pf_current;
    set_procs(field.pf_type)(field, value);
enddefine;

define global propsheet_field_default(field) -> value;
    lvars propsheet, field, value, tmp;
    Get_prop_item(field, FIELD) -> field;
    field.pf_default -> value;
    if not(value.isident or value.isprocedure) and field.pf_converter then
        (field.pf_converter)(value) -> value;
    endif;
enddefine;

define updaterof propsheet_field_default(field);
    lvars propsheet, field, value;
    Get_prop_item(field, FIELD) -> field -> value;
    if not(value.isident or value.isprocedure) and field.pf_converter then
        value -> (field.pf_converter)() -> value;
        unless verify_procs(field.pf_type)(field, value) then
            mishap(value, 1, 'INVALID VALUE FOR FIELD');
        endunless;
        -> value;
    endif;
    value -> field.pf_default;
enddefine;

define lconstant Update_field_width(field, do_overall_width) with_nargs 3;
    lvars field, value, type, w, do_overall_width, cw, height = false;
    ;;; do_overall_width says whether we are specifying the width of all
    ;;; the widgets in the field, or just the text field widget

    Get_prop_item(field, FIELD) -> field -> value;
    field.pf_type -> type;

    ;;; we can (currently) only adjust the width of live fields with
    ;;; textfields or with a scrolling list in them.
    returnunless((type == LISTOF or type == NUMERIC_RANGE or
            type == STRING or type == NUMBER) and
            XptIsLiveType(field.pf_labelwidget, "Widget"));

    ;;; we accept "small", "medium" and "large" as values for adjusting the
    ;;; overall width of a field
    if do_overall_width and value == "medium" then
        40 -> value;
    elseif do_overall_width and value == "large" then
        60 -> value
    elseif do_overall_width and value == "small" then
        20 -> value
    endif;
    fi_check(value, 1, 1024) -> value;

    SUBPART(1, field) -> w;
    Character_width(w) -> cw; ;;; gets the average character width

    unless using_motif then
        ;;; In dumb OLIT, if you update the width of an unrealized textfield widget
        ;;; you get an X Toolkit error unless you also specify the height. However,
        ;;; the initial height of the widget is 0, so we request the desired height
        ;;; of the field and use that.
        if type /== LISTOF then
            unless fast_XtIsRealized(w) then
                fXptVal w(XtN textEditWidget:XptWidget) -> w;
                Query_geom(w) -> (,,, height);
            endunless;
        endif
    endunless;

    if do_overall_width and type == NUMERIC_RANGE then
        ;;; width of numeric range is divided between textfield and slider.
        ;;; The slider is 4/5ths the width. the text is 1/5th the width.
        value / 5 -> value;
        fi_max(1, intof(value * 4 * cw) fi_-
        fXptVal (SUBPART(LABEL_1_PART, field))(Rwidth))
            -> fXptVal (SUBPART(SLIDER_PART,field))(Rwidth);

    elseif do_overall_width and type == NUMBER then
        ;;; the width of a number field includes the two arrows
        value - ((fXptVal (SUBPART(INCREMENT_PART, field))
                    (Rwidth) fi_* 2) / cw) -> value;
    elseif type == LISTOF then
        ;;; may need to add the size of the scrollbar
        lvars sb, num_times;
        if using_motif then
            ;;; scrollbar is a resource of the parent
            fXptVal (XtParent(w))(XmN verticalScrollBar:XptWidget) -> sb;
            do_overall_width and 1 or 2 -> num_times;
            ;;; For scrolling lists we change the size of the rowcolumn parent
            SUBPART(ROWCOL_PART, field) -> w;
        else
            ;;; the scrollbar is the first child
            CHILD(1,w) -> sb;
            ;;; the second child is the acutal list pane that we want to size
            CHILD(2,w) -> w;
            do_overall_width and -1 or 0 -> num_times;
        endif;
        value + ((num_times fi_* fXptVal sb(Rwidth)) / cw) -> value;
    endif;

    if using_motif then
        ;;; for Motif, we specify the size of the field in columns, since
        ;;; textfields ignore their XtNwidth resource.
        if type == LISTOF then
            fi_max(1, intof(value * cw)) -> fXptVal w(XmN width:XptDimension);
        else
            fi_max(1, intof(value)) -> fXptVal w(XmN columns:short);
        endif
    else
        false, false, fi_max(1, intof(value * cw)), height -> XptWidgetCoords(w);
    endif
enddefine;

define updaterof global propsheet_field_width
    = Update_field_width(%true%);
enddefine;

define updaterof global propsheet_field_columns
    = Update_field_width(%false%);
enddefine;

define global propsheet_sensitive(field);
    lvars propsheet, field, button, item, widget;
    Get_prop_item(field, #_< FIELD || BOX || SHEET || BUTTON >_#)
        -> (field, button);
    if button then
        Widget_of_button(field, button)
    elseif field.ispropfield then
        field.pf_labelwidget
    endif -> field;
    fXptVal field(XtN sensitive:XptBoolean)
enddefine;

define updaterof propsheet_sensitive(field) with_nargs 2;
    lvars value, propsheet, field, button, item, widget;

    define lconstant Update_sensitive(w);
        lvars w;
        ;;; just calls XtSetSensitive with the current thing in -value-
        XtSetSensitive(w, value);
    enddefine;

    Get_prop_item(field, #_< FIELD || BOX || SHEET || BUTTON >_#)
        -> (value, field, button);
    if button then
        Update_sensitive(Widget_of_button(field, button));
    elseif field.ispropfield then
        ;;; apply XtSetSensitive to ALL the widgets in the field
        ;;; (including any popup menus)
        appdata(field.pf_subparts, Update_sensitive);
        Update_sensitive(field.pf_labelwidget);
    else
        Update_sensitive(field);
    endif;
enddefine;

define global propsheet_label(field) -> value;
    lvars propsheet, field, button, w, value;
    Get_prop_item(field, #_< FIELD || BOX || BUTTON >_#) -> (field, button);
    if button then
        Widget_of_button(field, button)
    elseif field.ispropfield then
        returnunless(field.pf_haslabel)(false -> value);
        field.pf_labelwidget
    else
        mishap(field, 1, 'PROPFIELD or BOX BUTTON or FIELD BUTTON NEEDED');
    endif -> w;
    p_LABEL_STRING(w) -> value;
    ;;; remove ':' from field labels
    unless button or length(value) == 0 then
        allbutlast(1, value) -> value;
    endunless;
enddefine;

define updaterof propsheet_label(field) with_nargs 2;
    lvars propsheet, field, value, w, button;
    Get_prop_item(field, #_< BOX || FIELD || BUTTON >_#)
        -> (value, field, button);
    if button then
        Widget_of_button(field, button) -> w;
        value sys_>< nullstring -> value;
    elseif field.ispropfield then
        value -> field.pf_haslabel;
        field.pf_labelwidget -> w;
        if value then value sys_>< ':' -> value endif;
    else
        mishap(field, 1, 'PROPFIELD or BOX BUTTON or FIELD BUTTON NEEDED');
    endif;
    value or nullstring -> p_LABEL_STRING(w);
    if field.ispropfield then Align_fields(field) endif;
enddefine;

define lconstant Proc_or_false(x);
    lvars x;
    isprocedure(x) or not(x);
enddefine;

define global propsheet_field_converter
    = Field_access(%pf_converter, Proc_or_false%)
enddefine;
;;;
define global propsheet_field_accepter
    = Field_access(%pf_accepter, Proc_or_false%)
enddefine;
;;;
define global propsheet_field_applier
    = Field_access(%pf_ident, Proc_or_false%)
enddefine;
;;;
define global propsheet_field_formatter
    = Field_access(%pf_formatter, TEXT_TYPES%)
enddefine;
;;;
define global propsheet_field_name() with_nargs 1;
    Field_access(pf_name, false); /* no updater ! */
enddefine;
;;;
define global propsheet_field_number() with_nargs 1;
    Field_access(pf_number, false); /* no updater ! */
enddefine;
;;;
define global propsheet_field_ident() with_nargs 1;
    Field_access(pf_ident, Proc_or_false); /* updater defined below */
enddefine;
;;;
define updaterof propsheet_field_ident(field);
    lvars field, wident, class;
    Get_prop_item(field, FIELD) -> field -> wident;
    if wident.isword then sys_current_ident(wident) -> wident; endif;
    unless wident.isident then mishap(0,'ident needed'); endunless;
    wident -> field.pf_ident;
    unless propsheet_ident_class(wident) ->>class then
        "manual" -> propsheet_ident_class(wident);
    endunless;
    ;;; ADD THE FIELD TO THOSE ASSOCIATED WITH THE IDENT
    lvars ref = ident_assoc(wident);
    field :: fast_cont(ref) -> fast_cont(ref);
enddefine;

/* ===== Boxes and Sheets ================================================ */

;;; converts an arbritrary widget into a propbox
define global propsheet_import_box(widget) -> box;
    lvars widget, box;
    XptRegister(widget) -> widget;
    returnif(widget.ispropbox)(widget->box);
    if XptRegister(widget) /== XptDescriptor(widget, "Widget") then
        mishap(widget,1, 'Widget already has a preferred representation');
    endif;
    copy(defpropbox) -> box;
    widget -> box.pb_ptr;
    box -> box.pb_controlarea;
    box -> XptRegister(widget);
    propsheet_destroy -> sys_process_destroy_action(box);
enddefine;

;;; standard response to a button being pressed
define global propsheet_handle_standard_button(propbox, button);
    lvars propbox, button;

    if button == "Reset" then
        propsheet_reset(propbox, true);
    elseif button == "Apply" then
        propsheet_apply(propbox, true);
        propsheet_save(propbox, true);
    elseif button = #_< 'Set Defaults' >_# then
        propsheet_set_defaults(propbox, true);
    elseif button == "Refresh" then
        propsheet_refresh(propbox, true);
    elseif button == "Dismiss" then
        propsheet_hide(propbox);
    endif;
enddefine;

;;; handles button events for property box buttons
define lconstant Box_button_cb(w, client, call);
    lvars w, client, call, button, propbox;
    dlocal inside_activate;
    dlocal timer_blocked = true;
    true -> inside_activate;
    destpair(client) -> button -> propbox;
    SET_PROMPTSOURCE(w);
    if (propbox.pb_buttoncallback ->>call) then
        call(propbox, button) ->> call -> propbox.pb_allowpopdown;
        if using_motif then
            /* popdown manually if necessary */
            if call then propsheet_hide(propbox) endif
        endif;
    else
        propsheet_handle_standard_button(propbox, button);
    endif;
enddefine;


;;; creates all the buttons for a property box
define lconstant Create_box_button_list(box, parent, list);
    lvars box, parent, list, i, name, label, button,
            num_actions = datalength(list) fi_-1;
    list -> box.pb_buttons;
    {%  fast_for i from 0 to num_actions do
            fast_subscrv(i fi_+ 1, list) -> name;
            unless name.isstring or name.isword then
                mishap(name,1,'STRING OR WORD NEEDED');
            endunless;

            p_CREATE_BOX_BUTTON(parent,box,name,i,num_actions,Box_button_cb)
        endfor
    %} -> box.pb_buttonwidgets;
enddefine;

;;; creates new property boxes
define propsheet_new_box(title, parent, button_callback, buttons)-> new;
    lvars new, popup, lca, i, w, button_callback, buttons, title, parent;

    define lconstant create_shell -> toplevelshell;
        lvars toplevelshell;
        fast_XtVaAppCreateShell('poplog', 'Poplog',
            xtApplicationShellWidget,
            XptDefaultDisplay,
            #|  (XtN width, 1),
                (XtN height, 1),
                (XtN mappedWhenManaged, false),
            |#) -> toplevelshell;
        XtRealizeWidget(toplevelshell);
    enddefine;

    copy(defpropbox) -> new;
    buttons or #_< [Apply Reset 'Set Defaults' Dismiss] >_# -> buttons;
    unless parent then
        XptDefaultSetup();
        create_shell() -> parent;
        false -> sys_process_destroy_action(parent);
    endunless;
    ;;; copy the buttons
    buttons.destlist.consvector ->> buttons -> new.pb_buttons;
    p_PROPSHEET_NEW_BOX(parent, title, new) -> (popup, lca, new.pb_controlarea);
    false -> sys_process_destroy_action(popup);
    popup -> new.pb_ptr;
    lca -> new.pb_buttonarea;
    Create_box_button_list(new, lca, buttons);
    fast_XtManageChild(lca);
    if button_callback then
        button_callback -> new.pb_buttoncallback;
    endif;
    new -> XptRegister(popup);
    propsheet_destroy -> sys_process_destroy_action(new);
enddefine;

;;; used to redefine the buttons shown at the bottom of a box
define global propsheet_box_buttons(box) -> list;
    lvars box, list;
    ;;; turn the internal list into a vector
    box.pb_buttons.destvector.conslist -> list;
enddefine;
;;;
define updaterof propsheet_box_buttons(list, box);
    lvars box, cbproc, list, pane, lca, b, label, buttons, lca;
    if list.isprocedure then
        list -> (list, cbproc);
    else
        box.pb_buttoncallback -> cbproc;
    endif;
    box.pb_buttonarea -> lca;
    box.pb_buttonwidgets -> buttons;
    unless XptIsLiveType(lca, "Widget") then
        mishap(box,1, 'ATTEMPTING TO ADD BUTTONS TO IMPORTED PROPSHEET BOX');
    endunless;
    XtUnmanageChild(lca);
    ;;; destroy current buttons
    if buttons.isvector then
        false -> box.pb_buttonwidgets;
        ;;; we need to unmanage them so that the form does not try to
        ;;; rearrange itself
        appdata(buttons, XtUnmanageChild);
        appdata(buttons, DESTROY);
    endif;
    ;;; (copy list into vector to be safe)
    list.destlist.consvector -> list;
    Create_box_button_list(box, lca, list);
    ;;; install the new button labels and callback
    list -> box.pb_buttons;
    cbproc -> box.pb_buttoncallback;
    XtManageChild(lca);
enddefine;

define global propsheet_box_callback
    = pb_buttoncallback(%%);
enddefine;


;;; simulates pressing a button on a propbox
define global propsheet_activate(box, button);
    lvars box, button, buttons, w;
    returnif(inside_activate);
    if box.ispropsheet then box.ps_propbox -> box endif;
    box.pb_buttonwidgets -> buttons;
    if buttons.isvector and buttons.datalength /== 0 then
        if Widget_of_button(box, button) ->> w then
            Box_button_cb(w, conspair(box, button), null_external_ptr);
        else
            mishap(box,button,2,'NO SUCH BUTTON');
        endif;
    else
        mishap(box,button,2,'BOX DOES NOT HAVE BUTTONS');
    endif;
enddefine;

;;; create a new propsheet
define global propsheet_new(title, box, fields) -> new;
    lvars box, title, ca, w, new, fields;
    unless box.ispropbox then
        mishap(box, 1, 'PROP BOX NEEDED');
    endunless;
    copy(defpropsheet) -> new;
    newproperty([],5, false, "perm") -> new.ps_propfields;

    p_PROPSHEET_NEW(box.pb_controlarea, title, new) -> (new.ps_propframe, ca);
    ca -> new.ps_ptr;

    if title then
        p_CREATE_LABEL(false, ca, title sys_>< nullstring, "center")->;
    endif;
    ;;; add the new property sheet to the end of the pb_propsheets list
    [^^(box.pb_propsheets) ^new] -> box.pb_propsheets;
    box -> new.ps_propbox;
    new -> XptRegister(ca);
    propsheet_destroy -> sys_process_destroy_action(new);
    title -> new.ps_name;
    if fields.islist and fields /== [] then
        propsheet_field(new, fields);
    endif;
enddefine;

/* ====== Field Building Procedures ================================ */

;;; takes a field pattern and interprets its type
define lconstant Decode_pattern(format) -> type;
    lvars format, type, item, list, key;
    returnunless(format.ispair)(false -> type);
    fast_front(format) -> item;
    datakey(item) -> key;
    if item == "message" then
        MESSAGE
    elseif item == "command" then
        COMMAND
    elseif item == "blank" then
        BLANK
    elseif item == "oneof" then
        ONEOF
    elseif item == "someof" or key == vector_key then
        SOMEOF
    elseif item == "menuof" then
        MENUOF
    elseif item == "listof" then
        LISTOF
    elseif key == boolean_key or item == "true" or item == "false" then
        BOOLEAN
    elseif key == string_key or key == string16_key then
        STRING
    elseif key == integer_key then
        if (listlength(format) fi_> 1 and
                isinteger(fast_front(fast_back(format)))) or
                (listlength(format) fi_> 2 and
                    subscrl(3,format).isinteger) then
            NUMERIC_RANGE
        else
            NUMBER
        endif;
    elseif key == pair_key then
        ONEOF
    else
        mishap(format,1,'unknown pattern');
    endif -> type;
enddefine;

;;; takes the ( attr = value ...) part of a field pattern and modifies
;;; the attributes/values.
define lconstant Parse_attributes(new, attriblist) -> found_width;
    lvars new, attriblist, id, item, name, changed = false, found_width=false;
    lvars found_default = false, default_item;
    DLOCAL_PROGLIST(attriblist);
    returnunless(pop11_try_nextreaditem("("));
    lconstant macro READLITERAL = [
        pop11_need_nextreaditem("=")->;
        readitem() -> item;
    ];
    lconstant macro READVAL = [
        READLITERAL;
        if item.isword then valof(item) -> item; endif;
    ];

    until proglist == [] or (readitem() ->>name) == termin or poplastitem = ")" do
        if name == "accepter" or name == "acceptor" then
            READVAL;
            item -> new.pf_accepter;
        elseif name == "converter" then
            READVAL;
            item -> new.pf_converter;
        elseif name == "displayer" then
            READVAL;
            item -> new.pf_formatter;
        elseif name == "applier" then
            READVAL;
            item -> new.pf_ident;
        elseif name == "user_data" then
            READLITERAL;
            item -> new.pf_userdata;
        elseif name == "noinput" then
            false -> propsheet_sensitive(new);
        elseif name == "nodefault" then
            propsheet_undef -> new.pf_default;
        elseif name == "nolabel" then
            false -> propsheet_label(new);
        elseif name == "aligned" then
            READVAL;
            item -> new.pf_isaligned;
        elseif name == "default" then
            READVAL;
            ;;; delay this, because computation of default may depend on
            ;;; other attributes not yet set
            item -> default_item;
            true -> found_default;
        elseif name == "ident" then
            READLITERAL; item -> id;
            if pop11_try_nextreaditem(":") then
                readitem() -> propsheet_ident_class(id);
            endif;
            id -> propsheet_field_ident(new);
        elseif name == "label" then
            READLITERAL;
            item -> propsheet_label(new);
        elseif name == "allowNone" then
            ;;; this attribute is interpreted by the create functions
        elseif name == "width" then
            READLITERAL;
            item -> propsheet_field_width(new);
            true -> found_width;
        elseif name == "columns" then
            READLITERAL;
            item -> propsheet_field_columns(new);
            true -> found_width;
        else
            ;;; warn the user that I don't know whats going on
            warning(name, 1, 'UNRECOGNIZED ATTRIBUTE NAME');
        endif;
        while pop11_try_nextreaditem(",") do endwhile;
    enduntil;
    if found_default then
        default_item -> propsheet_field_default(new);
        Get_default(new) -> new.pf_current;
        set_procs(new.pf_type)(new, new.pf_current);
    endif;
    ;;; ignore the fact that the field may have been changed by an attribute
    false -> new.pf_changed;
    new.pf_current -> new.pf_old;
enddefine;

;;; basic field creation procedure
define lconstant Define_field(props, join, format);
    lvars props, join, format;

    ;;; this operation must not be interrupted
    dlocal pop_asts_enabled = false;
    dlocal child_insert_position = false;

    lvars name, fieldname;
    dest(format) -> format ->> name -> fieldname;
    if fieldname.isstring then
        consword(fieldname) -> fieldname;
    elseunless fieldname.isword then
        mishap(fieldname, 1, 'STRING or WORD NEEDED')
    endif;

    lvars new = (props.ps_propfields)(fieldname);
    lvars type = Decode_pattern(format);
    ;;; if the type is unknown, delete the field
    returnunless(type)(if new then propsheet_destroy(new) endif);

    lvars isnew, controlarea, label_widget, number;
    if new then
        /* RE-USE EXISTING FIELD */
        false -> isnew;

        ;;; A special case - if we are respecifying a field to be the same
        ;;; type, and we have a rebuild procedure for the field, use it
        if new.pf_type == type then
            if type == LISTOF then
                Rebuild_listof(new, format) -> format;
                Parse_attributes(new, format) ->;
                return;
            elseif type == MENUOF then
                Rebuild_menuof(new, format) -> format;
                Parse_attributes(new, format) ->;
                return;
            endif;
        endif;

        ;;; save all of the non-volatile slots of the field
        ;;; (ie. the following things about a field are remembered
        ;;; when a field is reused)
        lvars haslabel, isaligned;
        new.pf_fieldname    -> fieldname;
        new.pf_name         -> name;
        new.pf_number       -> number;
        new.pf_controlarea  -> controlarea;
        new.pf_backlink     -> join;
        new.pf_labelwidget  -> label_widget;
        new.pf_haslabel     -> haslabel;
        new.pf_isaligned    -> isaligned;

        ;;; new widgets created by the procedures below will be inserted
        ;;; after the caption for this widget
        Find_child_position(label_widget) -> child_insert_position;

        ;;; Destroy the widget subparts that make up the field
        appdata(new.pf_subparts, DESTROY);

        ;;; clear all of the volatile slots of the field
        explode(defpropfield) -> explode(new);

        ;;; restore these (rest done below)
        haslabel  -> new.pf_haslabel;
        isaligned -> new.pf_isaligned;
    else
        /* CREATE NEW FIELD */
        true -> isnew;
        copy(defpropfield) -> new;

        ;;; if they want to join and there is something to join to,
        ;;; this will set -join- to that something
        join and props.ps_lastfield -> join;

        if join then
            ;;; joining this field to the end of another controlarea
            join.pf_controlarea
        else
            ;;; first field on a row - make a new controlarea for the row
            p_CREATE_ROW_CONTROL(name, props, Insert_position_exfunc)
        endif -> controlarea;

        ;;; make the caption holder for the field
        p_CREATE_LABEL('caption', controlarea, name sys_>< ':', "right")
            -> label_widget;
        false -> fXptVal label_widget(RrecomputeSize);
        length(props.ps_propfields) fi_div 2 fi_+ 1 -> number;
    endif;

    ;;; fill basic slots in field (always need to do this)
    fieldname       -> new.pf_fieldname;
    name            -> new.pf_name;
    number          -> new.pf_number;
    type            -> new.pf_type;
    controlarea     -> new.pf_controlarea;
    props           -> new.pf_propsheet;
    label_widget    -> new.pf_labelwidget;
    join            -> new.pf_backlink;

    ;;; link from previous field on the line to this field
    if join then new -> join.pf_forwlink endif;

    ;;; create widgets
    lvars procedure create_p = fast_subscrv(type, create_procs);
    create_p(new, controlarea, type, format) -> format;
    if isnew then
        ;;; after creating a new field, link it in with the parent
        new -> (props.ps_propfields)(number);
        new -> (props.ps_propfields)(fieldname); ;;; register with propsheet
        ;;; record that this was the last field created
        new -> props.ps_lastfield;
    endif;

    ;;; set things from the attribute list
    unless Parse_attributes(new, format) then
        ;;; Parse_attributes returns -true- if the width of the field
        ;;; was specified. If it is not, we need to specify it ourselves.
        ;;; We have to do this after creating the field to get its font.
        "medium" -> propsheet_field_width(new);
    endunless;
enddefine;

;;; Calls Define_field to set the pattern for a field
define global propsheet_field(props, format);
    lvars props, format, name, shell, join = false;
    Get_prop_item(props, SHEET) -> props;
    XptShellOfObject(props) -> shell;
    procedure;
    dlocal defer_align = true;
    dlocal timer_blocked = true;
    if format /== [] and ispair(fast_front(format)) or
                fast_front(format) == "+" then /* recurse */
        fast_for name in format do
            if name == "+" then
                true -> join; nextloop;
            endif;
            Define_field(props, join, name);
            false -> join;
        endfast_for;
    else
        Define_field(props, false, format);
    endif;
    endprocedure();
    Align_fields(props);
enddefine;


/**********************************************************************
 *                      Property Sheet Management
 **********************************************************************/


/* ====== Property Sheet Hide, Show and Destroy ======================= */

define global propsheet_hide(item);
    lvars item, button;
    if item.islist then
        applist(item, propsheet_hide);
    else
        Get_prop_item(item, #_<BOX || SHEET || FIELD || BUTTON >_#)
            -> (item, button);
        if button then
            XtUnmanageChild(Widget_of_button(item, button));
            Align_fields(item);
        elseif item.ispropbox then
            unless item.pb_buttonarea==propsheet_undef then
                ;;; NORMAL PROPBOX
                if using_motif then
                    XtUnmanageChild(item.pb_controlarea.XtParent);
                endif;
                XtPopdown(item);
            else
                ;;; IMPORTED PROPBOX
                XtUnmanageChild(item);
            endunless;
        elseif item.ispropsheet then
            XtUnmanageChild(item.ps_propframe);
        elseif item.ispropfield then
            appdata(item.pf_subparts, XtUnmanageChild);
            XtUnmanageChild(item.pf_labelwidget);
            Align_fields(item);
        endif;
    endif;
enddefine;

define global propsheet_show(item);
    lvars item, button;
    if item.islist then
        applist(item, propsheet_show);
    else
        Get_prop_item(item, #_< BOX || SHEET || FIELD || BUTTON >_#)
            -> (item, button);
        if button then
            XtManageChild(Widget_of_button(item, button));
            Align_fields(item);
        elseif item.ispropbox then
            unless item.pb_buttonarea==propsheet_undef then
                ;;; NORMAL PROPBOX
                if using_motif then
                    XtManageChild(item.pb_controlarea.XtParent);
                endif;
                XtPopup(item,0);
                exacc (2) raw_XRaiseWindow(XtDisplay(item), XtWindow(item));

                ;;; wait until the thing is actually visible
                repeat
                    XptSyncDisplay(XtDisplay(item));
                    quitif(propsheet_visible(item))
                endrepeat
            else
                ;;; IMPORTED PROPBOX
                XtManageChild(item);
            endunless;
        elseif item.ispropsheet then
            XtManageChild(item.ps_propframe);
            Align_fields(item);
        elseif item.ispropfield then
            ;;; I need to manage the widgets in the field
            appdata(item.pf_subparts, XtManageChild);
            XtManageChild(item.pf_labelwidget);
            Align_fields(item);
        endif;
    endif;
enddefine;

lvars win_attrs = false;
define global propsheet_visible(item);
    lvars item, button;
    Get_prop_item(item, #_<BOX || SHEET || FIELD || BUTTON>_#)-> (item, button);
    if button then
        fast_XtIsManaged(Widget_of_button(item, button));
    elseif item.ispropbox then
        returnunless(XtIsRealized(item))(false);
        unless win_attrs then
            EXPTRINITSTR(:XWindowAttributes) -> win_attrs
        endunless;
        exacc (3) raw_XGetWindowAttributes(XtDisplay(item), XtWindow(item),
                                win_attrs);
        exacc :XWindowAttributes win_attrs.map_state == IsViewable
    elseif item.ispropfield then
        ;;; its visible if its label is visible
        item.pf_labelwidget -> item;
        fast_XtIsManaged(item);
    elseif item.ispropsheet then
        item.ps_propframe -> item;
        fast_XtIsManaged(item);
    endif;
enddefine;

define updaterof propsheet_visible(item);
    lvars val, item, button;
    Get_prop_item(item, #_< BOX || SHEET || FIELD || BUTTON >_#)
            -> (val, item, button);
    if val then propsheet_show(item, if button then button endif)
    else propsheet_hide(item, if button then button endif)
    endif;
enddefine;

define propsheet_set_focus_on(item);
    lvars item;
    Get_prop_item(item, #_< BOX || SHEET || FIELD >_#)-> item;
    ;;; if item is a field, focus on the first thing in the field
    if item.ispropfield then
        if item.pf_type == BLANK then item.pf_labelwidget
        else SUBPART(1, item)
        endif -> item
    endif;
    p_SET_FOCUS_ON(item)
enddefine;

;;; wrapper on the allowShellResize resource
define global propsheet_box_can_resize(item);
    lvars item;
    Get_prop_item(item, #_< BOX || SHEET || FIELD >_#)-> item;
    if item.ispropfield then item.pf_propsheet -> item endif;
    fXptVal (XptShellOfObject(item))(XtN allowShellResize:XptBoolean);
enddefine;
;;;
define updaterof propsheet_box_can_resize(item) with_nargs 2;
    lvars item, shell, val, s;
    Get_prop_item(item, #_< BOX || SHEET || FIELD >_#)-> item;
    if item.ispropfield then item.pf_propsheet.ps_propbox -> item
    elseif item.ispropsheet then item.ps_propbox -> item
    endif;
    -> val;
    returnif((val and true) == propsheet_box_can_resize(item));
    XptShellOfObject(item) -> shell;
    val -> fXptVal shell(XtN allowShellResize:XptBoolean);
    if val then
        propsheet_reset_size(item);
        Align_fields(item);
        fast_for s in item.pb_propsheets do
            if fast_XtIsManaged(s) then
                propsheet_reset_size(s);
            endif;
        endfast_for;
    endif;
enddefine;

;;; kills fields, sheets or boxes
define global propsheet_destroy(item);
    lvars item, propsheet;
    dlocal timer_blocked = true;

    ;;; this operation must not be interrupted
    dlocal pop_asts_enabled = false;

    define lconstant Remove_ident_links(fieldlist);
        lvars fieldlist;
        returnif(fieldlist = []);
        fast_appproperty(
            ident_assoc,
            procedure(id, fieldref);
                lvars id, fieldref, ident_field_list = fast_cont(fieldref);
                lvars field;
                fast_for field in ident_field_list do;
                    if fast_lmember(field, fieldlist)
                    then
                        fast_ncdelete(field, ident_field_list) -> ident_field_list;
                    endif;
                endfast_for;
                ident_field_list -> fast_cont(fieldref);
            endprocedure
        );
        sys_grbg_list(fieldlist);
    enddefine;

    if item.islist then
        applist(item, propsheet_destroy);
    else
        Get_prop_item(item, #_< BOX || SHEET || FIELD >_#)-> item;
        Remove_ident_links([%Explode_propfields(item, false)%]);
        ;;; just return if we are destroying something that is already dead
        returnunless(XptIsLiveType(item, "Widget") or (item.ispropfield
                    and XptIsLiveType(item.pf_labelwidget, "Widget")));

        ;;; Remove item from ancestors chain
        if item.ispropsheet then
            fast_ncdelete(item, item.ps_propbox.pb_propsheets)
                -> item.ps_propbox.pb_propsheets;
            DESTROY(item.ps_propframe);
        elseif item.ispropbox then
            DESTROY(item);
        elseif item.ispropfield then
            item.pf_propsheet -> propsheet;
            if propsheet.ps_lastfield == item then
                ;;; if we are a field on a multi-field row, then pick up
                ;;; the previous or next field on the row and use that as the
                ;;; last field
                item.pf_backlink or item.pf_forwlink -> propsheet.ps_lastfield;
            endif;

            ;;; unlink this field from the chain of fields on this row
            if item.pf_backlink then
                item.pf_forwlink -> (item.pf_backlink).pf_forwlink;
            endif;
            if item.pf_forwlink then
                item.pf_backlink -> (item.pf_forwlink).pf_backlink;
            endif;

            ;;; remove the entry for the field number:
            ;;; renumber all the fields which were created after this field
            lvars i, n, fields = propsheet.ps_propfields, field;
            length(fields) div 2 -> n;
            fast_for i from item.pf_number fi_+1 to n fi_+1 do
                fields(i) -> field;
                field -> fields(i fi_- 1);
                if field.ispropfield then
                    i fi_-1 -> field.pf_number;
                endif;
            endfast_for;
            ;;; remove the entry for the field name
            false -> (propsheet.ps_propfields)(item.pf_fieldname);

            ;;; kill off ALL the widgets created by this field
            appdata(item.pf_subparts, DESTROY);
            DESTROY(item.pf_labelwidget);

            ;;; see if we can kill the whole row
            if fXptVal (item.pf_controlarea)(XtN numChildren:int) == 0 then
                DESTROY(item.pf_controlarea);
            endif;
            Align_fields(item.pf_propsheet);
        endif;
    endif;
enddefine;

/* ========= Save, Restore, Apply, Refresh, Set Defaults ============== */

;;; propsheet_save - saves current sheet settings

define global propsheet_save(item);
    lvars vis_only = false, item, name, field, ps;
    dlocal timer_blocked = true;
    if isboolean(item) then item -> (item, vis_only) endif;
    if item.islist then
        applist(item, propsheet_save(%vis_only%));
    else
        Get_prop_item(item, #_< BOX || SHEET || FIELD >_#)-> item;
        repeat (#| Explode_propfields(item, vis_only) |#) times
            -> field;
            field.pf_current -> field.pf_old;
            false -> field.pf_changed;
        endrepeat;
    endif;
enddefine;

;;; propsheet_reset - restores settings to the values on the last save

define global propsheet_reset(item);
    lvars vis_only = false, item, name, field, ps;
    dlocal timer_blocked = true;
    if isboolean(item) then item -> (item, vis_only) endif;
    if item.islist then
        applist(item, propsheet_reset(%vis_only%));
    else
        Get_prop_item(item, #_< BOX || SHEET || FIELD >_#)-> item;
        repeat (#| Explode_propfields(item, vis_only) |#) times
            -> field;
            if field.pf_changed and field.pf_default /== propsheet_undef then
                ;;; seems reasonable to assume that the old value does not
                ;;; need verification
                field.pf_old, field.pf_current -> (field.pf_current, field.pf_old);
                set_procs(field.pf_type)(field, field.pf_current);
            endif;
        endrepeat;
    endif;
enddefine;

;;; propsheet apply - applies current settings to any registered idents

define global propsheet_apply(item);
    lvars vis_only = false, item, field, id;
    dlocal timer_blocked = true;
    if isboolean(item) then item -> (item, vis_only) endif;
    if item.islist then
        applist(item, propsheet_apply(%vis_only%));
    else
        Get_prop_item(item, #_< BOX || SHEET || FIELD >_#)-> item;
        repeat (#| Explode_propfields(item, vis_only) |#) times
            -> field;
            if field.pf_ident ->> id then
                lvars value = propsheet_field_value(field);
                if id.isprocedure then
                    id(field.pf_propsheet, field.pf_name, value)
                elseif id.isident then
                    if isconstant(id) or isprotected(id) then
                        warning(field.pf_propsheet, field, id, 3,
                            'CANNOT ASSIGN FIELD VALUE TO PROTECTED/CONSTANT IDENTIFIER');
                    else
                        value -> idval(id);
                    endif;
                endif;
            endif;
        endrepeat;
    endif;
enddefine;

;;; propsheet refresh - sets propsheet according to any associated variables

define global propsheet_refresh(item);
    lvars vis_only = false, item, name, field, ps, value;
    dlocal timer_blocked = true;
    if isboolean(item) then item -> (item, vis_only) endif;
    if item.islist then
        applist(item, propsheet_refresh(%vis_only%));
    elseif item.isident or (item.isword and
                (stacklength() == 0 or not(.dup.ispropsheet))) then
        Refresh_ident(item);
    else
        Get_prop_item(item, #_< BOX || SHEET || FIELD >_#)-> item;
        repeat (#| Explode_propfields(item, vis_only) |#) times
            -> field;
            if field.pf_ident.isident then
                field.pf_ident.idval -> propsheet_field_value(field)
            endif;
        endrepeat;
    endif;
enddefine;

;;; propsheet_set_defaults - sets settings according to their default values

define global propsheet_set_defaults(item);
    lvars vis_only = false, item, name, field, ps, value;
    dlocal timer_blocked = true;
    if isboolean(item) then item -> (item, vis_only) endif;
    if item.islist then
        applist(item, propsheet_set_defaults(%vis_only%));
    else
        Get_prop_item(item, #_< BOX || SHEET || FIELD >_#)-> item;
        repeat (#| Explode_propfields(item, vis_only) |#) times
            -> field;
            Get_default(field) -> value;
            if value /== propsheet_undef then
                field.pf_current -> field.pf_old;
                value -> field.pf_current;
                true -> field.pf_changed;
                set_procs(field.pf_type)(field, value);
            endif;
        endrepeat;
        propsheet_apply(item, vis_only);
    endif;
enddefine;

/* ====== User Data Field ============================================= */

define global propsheet_user_data(p);
    lvars p;
    Get_prop_item(p, #_< BOX || SHEET || FIELD >_#)-> p;
    if p.ispropsheet then
        p.ps_userdata;
    elseif p.ispropbox then
        p.pb_userdata;
    elseif p.ispropfield then
        p.pf_userdata
    endif;
enddefine;

define updaterof propsheet_user_data(p) with_nargs 2;
    lvars val, p;
    Get_prop_item(p, #_< BOX || SHEET || FIELD >_#)-> p;
    if p.ispropsheet then
        -> p.ps_userdata;
    elseif p.ispropbox then
        -> p.pb_userdata;
    elseif p.ispropfield then
        -> p.pf_userdata;
    endif;
enddefine;

/**********************************************************************
 *                      Property Sheet Ident Registration
 **********************************************************************/

/* ========== Automatic Ident Refreshing ============================ */

lconstant
    macro (
        NO_REFRESH     = 0,
        TIMER_REFRESH  = 1,
        POP_REFRESH    = 2,
        ACTIVE_REFRESH = 3,
        MANUAL_REFRESH = 4,
    ),
;

define lconstant Refresh_class(class) -> class;
    lvars class, p;
    lconstant refresh_classes = [^false 0 timer 1 sysPOP 2 active 3 manual 4];
    unless class.isinteger then
        (fast_lmember(class, refresh_classes) ->> p) and hd(tl(p)) -> p;
        unless p then mishap(class,1,'UNKNOWN REFRESH CLASS'); endunless;
        p -> class;
    endunless;
enddefine;

define updaterof Refresh_class(class) -> class;
    lvars class, p;
    lconstant refresh_classes = [^false ^false 1 timer 2 sysPOP 3 active 4 manual];
    (fast_lmember(class, refresh_classes) ->> p) and hd(tl(p)) -> class;
enddefine;

define lconstant Do_refresh(val, field);
    lvars val, field;
    fast_for field in fast_cont(field) do;
        if field and is_valid_external_ptr(field.pf_labelwidget) then
            val -> propsheet_field_value(field);
        endif;
    endfast_for;
enddefine;

;;; MANUAL REFRESH

define lconstant procedure Refresh_ident(wident);
    lvars wident, fieldsref;
    if wident.isword then sys_current_ident(wident) -> wident endif;

    if ident_assoc(wident) ->> fieldsref then
        Do_refresh(idval(wident), fieldsref);
    else
        mishap(wident, 1, 'UNRECOGNIZED IDENT');
    endif;
enddefine;

;;; sysPOP REFRESH

lvars sysPOP_redefined = false;

lconstant macro VM_WEAK = [weakref[sysCOMPILE]];

define lconstant POP_refresh(wordarg) -> wordarg;
    lvars wordarg, id = wordarg, field;
    unless isident(id) then
        sys_use_current_ident(id) -> (id, )
    endunless;
    if ident_class(id) == POP_REFRESH then
        ident_assoc(id) -> field;
        VM_WEAK sysPUSHS(0);
        VM_WEAK sysPUSHQ(field);
        VM_WEAK sysCALLQ(Do_refresh);
    endif;
enddefine;


;;; TIMER REFRESH
lvars Timer_ast_p = false;
define lconstant procedure Timer_cb();
    lvars rec, new, data, id, field;
    returnif(timer_off); ;;; cancels timer
    ;;; true if we are doing something to the field
    unless timer_blocked then
        fast_for rec in timer_idents do
            fast_subscrv(1, rec) -> id;
            fast_subscrv(2, rec) -> data;
            if (idval(id) ->>new) /== data then
                new -> fast_subscrv(2, rec);
                fast_subscrv(3, rec) -> field;
                Do_refresh(new, field);
            endif;
        endfast_for;
    endunless;
    1e6 -> sys_timer(Timer_ast_p);
enddefine;
;;;
;;; block timer refresh events during callback, because they may cause
;;; arbitrary updates to windows, etc.
lvars Timer_ast_p = conspair(Timer_cb, ASTP_BLOCK_IN_EXTERNAL);

define lconstant Active_refresh(update_p, id, field);
    lvars procedure update_p, id, field;
    update_p();
    Do_refresh(idval(id), field);
enddefine;

;;; CHANGING AN IDENTS REFRESH CLASS

define lconstant Manage_ident(i) -> fieldref;
    lvars i, fieldref;
    unless (ident_assoc(i)->>fieldref) then
        writeable consref([]) ->> ident_assoc(i) -> fieldref;
    endunless;
enddefine;

define lconstant Unmanage_ident(wident);
    lvars wident, val;
    if ident_assoc(wident) then
        if ident_class(wident) == TIMER_REFRESH then
            ;;; remove ident from timer chain
            ncdelete(
                wident,
                timer_idents,
                procedure(vec,wident);
                    lvars vec,wident;
                    vec(1) == wident;
                endprocedure
            ) -> timer_idents;
            if timer_idents == [] then true -> timer_off endif;
        elseif  ident_class(wident) == ACTIVE_REFRESH
            and isclosure(updater(nonactive_idval(wident))->>val)
            and pdpart(val) == Active_refresh
        then
            frozval(1, val) -> nonactive_idval(wident);
        endif;
    endif;
enddefine;

define global propsheet_ident_class(wident);
    lvars wident;
    if wident.islist then
        [%applist(wident, propsheet_ident_class)%]
    else
        if wident.isword then sys_current_ident(wident) -> wident; endif;
        ident_class(wident) -> Refresh_class();
    endif;
enddefine;

define updaterof propsheet_ident_class(class, wident);
    lvars class, wident, val, fieldref;
    Refresh_class(class) -> class;
    if wident.islist then
        for val in wident do class -> propsheet_ident_class(val) endfor;
        return;
    endif;
    if wident.isword then sys_current_ident(wident) -> wident; endif;
    if isconstant(wident) then
        mishap(wident,1,'VARIABLE NEEDED');
    endif;
    if class == NO_REFRESH then
        Unmanage_ident(wident);
        false -> ident_class(wident);
        return;
    endif;
    if (ident_class(wident) ->> val) and val /== class then
        Unmanage_ident(wident);
    endif;
    if class == ACTIVE_REFRESH then
        unless isactive(wident) then
            mishap(wident, 1, 'ACTIVE VARIABLE NEEDED');
        endunless;
        unless isclosure(updater(nonactive_idval(wident))->>val) and
                pdpart(val) == Active_refresh then
            Manage_ident(wident) -> fieldref;
            Active_refresh(%val, wident, fieldref%)->
                    updater(nonactive_idval(wident));
            class -> ident_class(wident);
        endunless;
    elseif class == POP_REFRESH then
        unless sysPOP_redefined then
            unless testdef sysCOMPILE then
                mishap(0, 'propsheet_ident_class: CANNOT REDEFINE sysPOP - VM COMPILER NOT LOADED')
            endunless;
            lblock compile_mode :vm -prmprt;
                POP_refresh <> VM_WEAK sysPOP -> VM_WEAK sysPOP;
            endlblock;
            "sysPOP" -> pdprops(VM_WEAK sysPOP);
            true -> sysPOP_redefined;
        endunless;
        class -> ident_class(wident);
        Manage_ident(wident)->;
    elseif class == TIMER_REFRESH then
        class -> ident_class(wident);
        Manage_ident(wident) -> fieldref;
        writeable {%wident, idval(wident), fieldref%}
            :: timer_idents -> timer_idents;
        if timer_off then
            false -> timer_off;
            ;;; if prospheet_init has been called, start the timer
            if init_done then Timer_cb(); endif;
        endif;
    elseif class == MANUAL_REFRESH then
        Manage_ident(wident) -> ident_assoc(wident);
        MANUAL_REFRESH -> ident_class(wident);
    endif;
enddefine;

define global constant macro propsheet_id;
    lvars name, id, class;
    sys_current_ident(readitem()) -> id;
    unless id then
        mishap(poplastitem,1,'no such variable');
    endunless;
    if pop11_try_nextreaditem(":") then
        readitem() -> propsheet_ident_class(id);
    endif;
    id;
enddefine;

/* ======== Property Sheet Initialization Action ===================== */

;;; this does very little now:
define global propsheet_init;
    unless init_done then
        true -> init_done;
        unless timer_off then Timer_cb(); endunless;
    endunless;
enddefine;

/* ======== Misc Functions ============================================ */

define global propsheet_length(item);
    lvars item;
    Get_prop_item(item, #_< BOX || SHEET || FIELD >_#) -> item;
    if item.ispropbox then
        listlength(item.pb_propsheets)
    elseif item.ispropsheet then
        length(item.ps_propfields) div 2
    elseif item.ispropfield then
        if fast_lmember(item.pf_type, LIST_TYPES) then
            datalength(item.pf_buttons)
        endif;
    endif;
enddefine;

procedure(item, box);
    lvars item, box, p;
    if item.isinteger then
        return((box.pb_propsheets)(item));
    else
        item sys_>< nullstring -> item;
        fast_for p in box.pb_propsheets do
            returnif(p.ps_name sys_>< nullstring = item)(p);
        endfast_for;
    endif;
    ;;; did not find the property sheet
    mishap(item,1,'NO SUCH PROPERTY SHEET');
endprocedure            -> class_apply(propbox_key);

Print_propsheet ->> class_print(propsheet_key)
                -> class_print(propbox_key);
procedure(item);
    lvars item;
    dlocal pop_pr_quotes = false;
    pr('<propfield ');
    pr(item.pf_fieldname);
    ;;; print out buttons
    if fast_lmember(item.pf_type, BUTTON_TYPES) then
        pr('\s'); pr(item.pf_buttons)
    endif;
    pr('>');
endprocedure -> class_print(propfield_key);

;;; the class apply of a propsheet reverses the order of the top two
;;; things on the stack and then calls propsheet_field_value
procedure;
    #_< sysSWAP(1,2); >_#
endprocedure <> propsheet_field_value -> class_apply(propsheet_key);

;;; allow english and american spelling
ident propsheet_field_accepter -> identof("propsheet_field_acceptor");

/* THIS PROCEDURE IS PROVIDED ONLY FOR COMPATIBILITY */
ident propsheet_sensitive -> identof("propsheet_field_sensitive");

/* THIS PROCEDURE IS PROVIDED ONLY FOR COMPATIBILITY */
define global propsheet_field_record =
    Get_prop_item(%FIELD%)
enddefine;

sysprotect("propsheet_acceptreason");


/* ======= Set for Motif or OLIT ====================================== */

weak constant
    $-popxlink_motif,    switch_vec_xm,
    $-popxlink_openlook, switch_vec_xol,
;

#_IF not(DEF POPC_COMPILING)
    #_IF DEF popxlink_motif
        uses $-propsheet$-switch_vec_xm;
    #_ELSEIF DEF popxlink_openlook
        uses $-propsheet$-switch_vec_xol;
    #_ENDIF
#_ENDIF

define lconstant set_gui();
    if testdef popxlink_motif then
        true, weakref[popxlink_motif] switch_vec_xm
    elseif testdef popxlink_openlook then
        false, weakref[popxlink_openlook] switch_vec_xol
    else
        mishap(0, 'propsheet: SYSTEM NOT LINKED WITH MOTIF OR OPENLOOK');
    endif -> (using_motif, gui_switch_vec)
enddefine;

#_IF DEF POPC_COMPILING
    sys_runtime_apply(set_gui);
#_ELSE
    set_gui();
#_ENDIF

constant $-propsheet = true;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jun 26 1997
        Changed Character_width to allow for p_FONTSTRUCT returning a font
        set
--- Robert Duncan, Jun 25 1997
        Fixed Label_width -- for Motif only -- to allow for font sets
--- Robert Duncan, Apr  4 1997
        Changes for string16
--- John Gibson, Jan 16 1997
        Made propsheet_show use XptSyncDisplay instead of XtAppProcessEvent
        (the latter can cause things to hang).
--- Robert Duncan, Sep  3 1996
        Fixed propsheet_label to return <false> for fields without labels
--- Robert Duncan, Jun 24 1996
        Fixed propsheet_field to accept a psheetspec as first argument, as
        documented in REF PROPSHEET
--- John Gibson, Dec  5 1995
        Changed Character_width to pass widget's display ptr to
        XptGetFontProperty.
--- Robert John Duncan, Aug 16 1995
        Rewrote Define_field to provide some missing initialisations -- the
        isaligned and haslabel attributes for new fields were getting
        random values -- and to remove redundant code and declarations.
--- John Williams, May 26 1995
        POP_refresh uses sys_use_current_ident instead of sys_current_ident
        in case its argument is an active variable.
--- John Williams, May 26 1995
        POP_refresh does "unless isident(id)" instead of "if isword(id)"
        in case "id" is be a symbol.
--- Robert John Duncan, Apr 26 1995
        Changed propsheet_apply so that the field applier is called on the
        result of propsheet_field_value rather than on the raw (pf_current)
        value: this makes it consistent with the identifier case.
--- Robert John Duncan, Apr 25 1995
        Further change to handling of "default" attribute in Parse_attributes
        so that it copes with a field converter as well
--- Robert John Duncan, Apr 12 1995
        Fixed handling of "default" attribute in Parse_attributes to allow
        for ident or procedure as the default
--- Robert John Duncan, Apr  7 1995
        Fixed bug in Unmanage_ident: it depends on the order of arguments
        given to eq_p by ncdelete, recently changed.
        Set flags to block execution of the ident refresh timer during
        callbacks, since the interactions can be unpredictable.
--- John Gibson, Mar 30 1995
        Corrected assignment for return value from Insert_position_proc
        (must assign value as "exval")
--- Adrian Howard, Jul  7 1993
        Added propsheet_field_number
--- John Gibson, May 17 1993
        o  All #_IFs for Motif or OLIT removed, and the GUI-dependent code
           shifted to S-propsheetS-switch_vec_xm.p and
           S-propsheetS-switch_vec_xol.p. Via macros in include file
           propsheetP.ph, these define vectors of resource names
           and procedures which are referenced via the same macros in this
           file, after assigning the appropriate vector to gui_switch_vec.
        o  Made redefinition of sysPOP be done only when the updater of
           propsheet_ident_class is actually called with a "sysPOP" arg.
        o  No longer uses props*heet_utils.p.
--- John Gibson, Apr 16 1993
        Uses xol*Widget and xm*Widget etc instead of XptW*idgetSet
--- Adrian Howard, Oct 20 1992
        (From JonM) Minor fix for widget names.
--- Adrian Howard, Sep 22 1992
        The insert cursor in Motif TextFields now stays in the same position
        after a field has been accessed.
--- Adrian Howard, Sep 18 1992
        o Cleaned up a little
        o Altered so an single ident can be linked to more than one PROPSHEET
        field
--- John Gibson, Sep 11 1992
        Changed to use XptVal
--- Adrian Howard, Sep 10 1992
        Fixed problem in pre-3.0 OW which caused widgets to pop down even
        if the pushpin resource was in.
--- John Gibson, Sep 10 1992
        Changed calls of XptWidgetCoords to use false not undef (why in
        God's name use undef?!!)
--- Adrian Howard, Aug 20 1992
        Made -propsheet_show- and -propsheet_hide- act correctly for imported
        property boxes
--- Adrian Howard, Aug 19 1992
        Stopped the library loading the -propsheet- was defined
--- Jonathan Meyer, Aug 10 1992
        o  fixes to make propsheet_id not start a timer running unless
           propsheet_init has been called, and to make the datastructure
           build by propsheet_ident_class writeable.
        o  Removed propsheet_field_widget (undocumented, no longer required).
--- Jonathan Meyer, Jul 28 1992
        o  Changed propsheet_destroy so that it (a) always removes ident
           links if they have been made, (b) destroys a propsheet ps_propframe
           rather than the registered widget.
        o  Fixed bug in Align_fields when it is being applied to fields with
           no labels.
--- Jonathan Meyer, June 30 1992
        o  Changed Motif parent of upper_controls and lower_controls back
           into a RowColumn widget rather than a form widget. Form widgets
           work very badly when elements of the form change in size.
--- Jonathan Meyer, June 27 1992
        o  Made Parse_attributes return whether the attributes included a
           size specification. This is used by Define_field to determine
           if it needs to set the fields size itself.
        o  added propsheet_box_callback
        o  align fields now works on the whole box again, rather than on
        o  each sheet separately. This looks much better.
        o  propsheet_show now waits until a box appears before returning.
        o  setting propsheet_box_can_resize to -true- now prioritises the
           sheets above the buttons on the box when it comes to sizing things.
        o  the Motif 'lower_controls' widget is now attached to the right
           side of the 'upper_controls', rather than the right of the form.
        o  set  pf_textchanged to -false- after doing a Set_string_or_number
           so that the accepter is not called.
--- Jonathan Meyer, Jun 16 1992
        o  propsheet_init now does nothing.
        o  fixed bug in Parse_attributes for setting the default value
        o  XmNmultiCkick set to XmMULTICLICK_KEEP so that multiple clicks are
           processed.
        o  propsheet_box_can_resize now automatically calls reset_size after
           you restore its value to -true-. This ensures that a propsheet is
           the size it wants to be.
        o  added propsheet_reset_size. Makes use of propsheet_box_can_resize
           a viable option.
        o  updater of propsheet_box_buttons now unmanages children before
           destroying them. This keeps the Motif form widget happy
        o  fixed bug in propsheet_field_widget (now obsolete, but kept anyway)
        o  propsheet_reset now does nothing on fields which have no
           default value.
        o  The variable -timer_blocked- is now set to -true- when propsheet
           is doing things to a field. This temporarily stops the refresh
           timer from checking if things need to be refreshed.
        o  propsheet_field_value will now call the accepter to
           get a new value of a text field if the field has changed. This
           is no longer done by Box_button_cb.
        o  Motif boxes now use a Form to contain the upper/lower controls.
           This resizes better.
        o  p_TEXTFIELD_VALUE now frees the (copied) external data that it gets.
        o  Changed instances of CoerceXmString to XpmCoerceCopiedString. This
           makes sure that we free external data that is copied.
        o  Renamed the pf_type names to be consistent with the keywords used
           to activate those types. Made the pf_type field specified as :4.
        o  Create_listof now calls Rebuild_listof instead of duplicating code.
        o  Added Rebuild_menuof.
        o  LISTOF types now recognize the allowNone attribute
        o  Removed the ZERO*EXCLUSIVE_LIST type, adding an extra field to
           propfields called pf_allownone, and merging the zero exclusive list
           types with the exclusive list types.
        o  propsheet_refresh no longer installs the default values into
           fields which don't have identifiers.
        o  propsheet_apply now checks for constant/protected idents, and
           issues a warning if they are found.
        o  Made propsheet_field_value use the verify_procs followed by
           the set_procs. Set procs now no longer verify values themselves.
        o  propsheet_field_default and Get_default use the verify_procs
           and generate mishaps/warnings if they are given invalid values.
        o  CREATE now clears the destroy actions for all widgets. The only
           things with destroy actions are property boxes and property sheets.
           Also switched to using sys_process_destroy_action for >14.1 Poplog.
        o  Align_fields now determines the largest label widget using
           Label_width and then calculates only its preferred size by setting
           XtNrecomputeSize true and then false - rather than by setting
           recomputeSize true and then false for every label widget to find
           the largest size (changing recomputeSize in OLIT causes all of the
           widgets to jiggle around, and it looks very unpleasant).
        o  Get*field_width_name now redundant - removed and replaced by
           Get_prop_item.
--- Jonathan Meyer, Jun 15 1992
        o  Get_prop_item extended to allow box, sheet, field and
           box, sheet, field, button naming.
        o  LISTOF now uses a RowColumn widget to hold the scrolling list
           so that I can force the list to be the size I want it to be.
        o  You can now use propsheet_sensitive, propsheet_label,
           propsheet_visible, propsheet_hide and propsheet_show on box
           buttons and field buttons.
--- Jonathan Meyer, Jun 14 1992
        o  PREVIEW_PART only applicable to OpenLook now.
        o  No longer record the menu_pane of a menu list as a subpart.
        o  Changed widget geometry handlers to use Query_geom.
--- Jonathan Meyer, Jun 9 1992
        o  Made GET_NEW_VALUE macro a procedure called Get_new_value.
           Added verify procedures to each field type.
        o  Define_field, propsheet_destroy and Align_fields now
           disable interrupts while they work.
        o  Made descriptor registered classes not hold onto their descriptors.
        o  Removed top*levelshell. Now each propsheet creates its own top
           level shell. This is so that propboxes get their garbage actions
           right. Now, if you don't have a handle on a box or one of the
           sheets in the box, the thing becomes garbaged.
--- Jonathan Meyer, Jun 3 1992
        o  Added propsheet_field_width, propsheet_field_columns.
        o  Fixed propsheet_destroy so that it renumbers remaining fields right.
           Made Define_field force field names to be words or strings.
        o  propsheet_subpart now implemented. This is the `official' way that
           applications will get hold of widgets in the propsheet library.
        o  propsheet_handle_standard_button now notices the "Refresh" button
           and handles that by calling -propsheet_refresh-.
--- Jonathan Meyer, Jun 2 1992
        o  propsheet_handle_standard_button now by default calls the
           propsheet_save, set_defaults, etc. with the second optional arg
           of true - ie. they only work on the currently visible sheet.
           This seems to be the more common case option.
        o  Accepter handlers now generate a warning if an accepter returns
           an invalid value.
        o  Switched from lists to vectors for almost all of the internal
           datastructures (saves space).
        o  OLIT labels are now StaticText widgets rather than button widgets
        o  Made pf_subparts always either a vector of widgets or -false-.
           Destroying a field works by destroying everything in pf_subparts.
           Each subpart is now given a symbolic name, rather than a number.
        o  Fields are now no longer -descriptors- as well. This saves a little
           space (2 words per field), but the main reason to do this was
           because fields can no longer be identified with a single container
           widget (multiple fields on a line share container widgets).
        o  Changed OLIT menuof to use a button rather than a caption for the
           preview item.
        o  Rewrote the code for handling pf_textchanged to use XtCallCallbacks
        o  CREATE for OLIT now always calls Check_foreground. Fixes reverse-
           video problems for OLIT. Groan.
        o  Motif now uses DialogShells, not TopLevelShells. Added code to call
           Xt{Un}ManageChild on the single rowcolumn child of the shell to
           make it appear and disappear. This gets mwm menus on the shell right
        o  Changed OLIT and MOTIF to always use a single controlareas for each
           row, and a label for each field. Rewrote code for aligning fields.
        o  Allowed "acceptor" as well as "accepter".
        o  Parse_attributes warns users if they specify an unknown attribute.
        o  User procedure propsheet_label now provided.
           -nocaption- and -caption- options removed. Replaced by label attrib.
        o  Fixed class_apply of propboxes so that you can pass the name of
           a property sheet.
--- Jonathan Meyer, Apr 16 1992
        o  Made Rebuild_listof also update the current item.
--- Jonathan Meyer, Jan 20 1991
        o  A field can be joined to the last created field, allowing
           multiple columns of fields.
        o  Fields which are aligned but don't have captions (aligned=true,
           label=false) still have a label created to take up the
           place for the caption. This siimplifies the job of aligning them.
        o  Made ident property tables "tmparg" so that the propsheets get
           garbaged correctly.
        o  Made Motif scales update their values on dragCallback as well
           as on valueChangedCallback.
        o  Commented out check that disallowed creation of 0 or 1 element list.
        o  Fixed bug in Create_oneof_or_someof so that you can now have
           multiple row Motif lists with only one item in each row.
        o  Fixed bug in accepter for option lists - it didn't map a users
           set of strings into the strings found inside the pf_buttons.
        o  Added propsheet_box_can_resize, which is used dlocally to
           'freeze' a propsheet box while you are changing it.
        o  Made Motif propbox buttons appear on a Form with decent spacing
           and resize capabilities.
        o  Added "LISTOF" field type (keyword "listof").
        o  Allowed all list field types to have zero options
        o  Added propsheet_box_buttons - allows you to update the list
           of action buttons on a propbox.
        o  propsheet_save, reset, apply, refresh, set_defaults all take
           an optional extra boolean argument saying whether or not to
           apply the change only to things that are currently visible.
        o  Fixed bug in Motif oneof lists so that the accepter is only fired
           once if the same item is reselected.
        o  Made XmNdeleteResponse for Motif propbox do an unmap
--- Jonathan Meyer, Nov 21 1991
        Worked on reducing code, memory, resource and real-estate usage:
        o More code now common between Motif and OpenLook.
        o Now uses gadgets where possible.
        o Added option of "nocaption", and "aligned" option for the caption.
        o Reduced num of widgets used by the OLIT textfield by 1 ControlArea.
        o Reduced num of widgets used by all Motif fields by 1 RowCol.
        o Reduced the spacing and margins (ie. real estate) used in Motif.
        o Added the "BLANK" and the "MESSAGE" field types.
        o Made propsheet_set_focus_on work using XmProcessTraversal in Motif
        o Made widgets use consistent naming conventions for labels
        o Added Check_foreground for improved color setting in OLIT1.2
        o Renamed all lconstant procs to start with a capital letter
        o Fixed Motif oneof/someof lists so that the accepters fire correctly
        o Fixed arguments in call of Field_access in propsheet_field_name
        o Added propsheet_visible
--- Adrian Howard, Nov  1 1991
        o XmAnyCallbackStruct --> XpmAnyCallbackStruct
        o XmAnyCallbackStruct.reason --> XpmAnyCallbackStruct.XpmACSReason
        o Changed to use -XptArgPtr-
--- Jonathan Meyer, Sep 26 1991
        Allowed field default values to be procedures/idents which are used
        to get dynamic defaults.
--- Jonathan Meyer, Sep 25 1991
        Changed how option menu lists are destroyed in Motif.
--- Jonathan Meyer, Sep 16 1991 Added "user_data" attribute
--- Jonathan Meyer, Sep 16 1991
        Bools changes back from using CREATE to using XtVaCreate - need the
        resource converters for the XtDefaultForeground
--- Ian Rogers, Sep  9 1991
        Fixed bug in OLIT -Create_boolean-; Changed -XtVaCreateManagedWidget
        to CREATE
--- Jonathan Meyer, Sept 5 1991
        Tidied greatly. Added format_p and propsheet_field_formatter
--- John Gibson, Aug 23 1991
        1) Rewrote OLIT Center_mouse_on so it works with OblongButton a gadget.
        2) Fixed MOTIF bug where String_flush_changes was using OLIT name
            for textfield value (by introducing -p_TEXTFIELD_VALUE-), and
            commoned up code for set_string, number, numeric_range etc.
        3) Fixed Increment_cb bugs.
        4) Corrected bad spelling of "loose" to "lose".
--- John Gibson, Aug 10 1991
        Made intvecs writeable
--- Jonathan Meyer, Aug  2 1991
        Made OblongButton a gadget
--- Jonathan Meyer, Jul 31 1991
        Protected propsheet_acceptreason.
        Added propsheet_handle_standard_button
--- Jason Handby, Jul 31 1991
        Made the name of the popup shells 'Propsheet'
--- Jonathan Meyer, Jul 29 1991
        Added propsheet_field_name. Made string based accepters only get
        called if their textual value has changed. Made Option lists call
        their accepters on unsets as well as sets.
--- Jonathan Meyer, Jul  8 1991
        Added Update_accept_reason
--- Jonathan Meyer, Jul  8 1991
        Added propsheet_set_focus_on and propsheet_activate
--- Jonathan Meyer, Jul  6 1991
        Changed to use null_external_ptr
--- Jonathan Meyer, Jul  5 1991
        Made all set procs record their current value
--- Jonathan Meyer, Jul  5 1991
        Added correction for numbering of fields
--- Jonathan Meyer, Jul  3 1991
        Changed OLIT colours to use XtDefaultForeground instead of 'black'.
        Made OLIT TextFields assign their colours to their child based on the
        caption parent.
--- Jonathan Meyer, Jul  3 1991
        Corrected pf_current setting for MENUOFS
        Made allowShellResize true
--- Jonathan Meyer, Jul  1 1991
        Added keyboard accelarators for prompts
        Added Textfield_flush_changes
 */
