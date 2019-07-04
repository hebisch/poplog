/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_confirm.p
 > Purpose:         Poplog UI confirmation window
 > Author:          Julian Clinton, July 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui => pop_ui_confirm;
exload_batch;

include pop_uiP.ph;

uses
    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiShells,
;

lvars confirm_switch_vec;


;;; pop_ui_confirm takes five arguments:
;;;
;;;     The question (a string).
;;;
;;;     A list of strings which represent the labels of the buttons
;;;
;;;     The index in the list of the default value (an integer)
;;;
;;;     A text justification flag (<true> = centre, <false> = left)
;;;
;;;     reference widget (widget or <false>). If <false> then
;;;     pop_ui_app_shell is used
;;;
;;; Can also take an optional last argument (a string) which replaces
;;; any default window title.
;;;
;;; The result is the index in the list of the option selected
;;;

define pop_ui_confirm(question, options, default, centre_text, ref_widget)
                                                        -> result;
    lvars   title = 'Poplog: Confirm', question, options, default, centre_text,
            ref_widget, result;

    if isstring(ref_widget) then    ;;; change window title
        -> title;
        (title, question, options, default, centre_text, ref_widget) ->
            (question, options, default, centre_text, ref_widget, title);
    endif;

    Check_string(question, false);
    Check_list(options, false);
    Check_integer(default, true);

    p_CONFIRM(question, options, default, centre_text, ref_widget, title)
                                                -> result
enddefine;


SET_GUI(confirm_switch_vec, confirm_xm, confirm_xol, 'pop_ui_confirm');

endexload_batch;
endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
Julian Clinton,  20/8/91
    Changed mishap to a warning if widget set cannot be determined.
Julian Clinton,  15/7/91
    Changed to use XOPENLOOK instead of XOPENWINDOWS.
 */
