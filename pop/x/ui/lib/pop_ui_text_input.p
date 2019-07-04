/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_text_input.p
 > Purpose:         Poplog text input dialog
 > Author:          Julian Clinton, May 1995 (see revisions)
 > Documentation:
 > Related Files:
*/

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui =>  pop_ui_text_input;

include pop_uiP;
include xt_constants;

uses
    XptBusyCursorFeedback,
    XptShellDeleteResponse,
    $-poplog_ui$-guiUtils,
    propsheet,
;

lvars
    text_box = false,
    text_sheet = false,
    checking = false,       ;;; set when the input is being checked
    action = false;

define lconstant do_popdown();
    erase();        ;;; remove argument
    true -> action;
enddefine;


define lvars active:1 text_box_busy;
    XptBusyCursorFeedback(text_box);
enddefine;

define updaterof active:1 text_box_busy(val); lvars val;
    val -> XptBusyCursorFeedback(text_box);
enddefine;


define lconstant Check_text(text) -> text; lvars text;
lvars pdr = propsheet_user_data(text_box);
dlocal text_box_busy;

    if isprocedure(pdr) then
        lvars result = pdr(text);

        if isstring(result) then
            ;;; not a valid input so display the returned string
            ;;;
            true -> text_box_busy;
            pop_ui_message(result, false, text_box);
            false -> text_box_busy;
            false -> text;

        elseunless result then
            ;;; no message, just disallow the text
            ;;;
            false -> text;

        endif;
    endif;
enddefine;


define lconstant text_accepter(sheet, field, val) -> val;
dlocal checking;
lvars sheet field val;

    returnif(checking);
    true -> checking;
    unless Check_text(val) then
        propsheet_undef -> val;
    else
        val -> action;
    endunless;
enddefine;


define lconstant text_input_cb(box, button) -> popdown;
lvars box, button, popdown = false;
dlocal checking;

    returnif(checking);
    true -> checking;

    if button == "Cancel" then
        true -> popdown;
        true -> action;

    else
        ;;; Apply button - if the user passed a check procedure,
        ;;; apply that to the field value, otherwise simply return
        ;;;
        lvars string = propsheet_field_value(text_sheet, "text");

        if (Check_text(string) ->> string) then
            string -> action;
            true -> popdown;
        endif;
    endif;
enddefine;


/* ---------------------------------------------------------------
    Top-level Routines
   --------------------------------------------------------------- */

;;; Text input dialog
;;;
define pop_ui_text_input(title, label, default, check_pdr, ref_widget) -> text;
lvars title label default check_pdr ref_widget text;
dlocal XptBusyCursorOn = true;

    returnif(text_box);

    check_string(label);

    unless default and default.isstring then
        nullstring -> default;
    endunless;

    propsheet_new_box((title.isstring and title) or 'Poplog: Text Input',
        ref_widget, text_input_cb, [OK Cancel]) -> text_box;

    propsheet_new(false, text_box,
        [text ^default (aligned = ^false, label = ^label,
            columns = 30, accepter = ^text_accepter
    )]) -> text_sheet;

    check_pdr -> propsheet_user_data(text_box);

    lvars old_busy = XptBusyCursorOn;
    procedure;
        EXIT_ACTION((propsheet_destroy(text_box), false -> text_box,
                            old_busy -> XptBusyCursorOn));

        lvars shell = XptShellOfObject(text_sheet);
        do_popdown -> XptShellDeleteResponse(shell);
        true -> XptBusyCursorOn;
        XptCenterWidgetOn(shell, ref_widget);
        propsheet_show(text_sheet);
        propsheet_show(text_box);
        false -> action;
        until action do
            XtAppProcessEvent(XptDefaultAppContext, XtIMAll);
        enduntil;
    endprocedure();

    (isstring(action) and action) or false -> text;
    false ->> action -> text_sheet;
enddefine;

endsection; /* $-poplog_ui */
