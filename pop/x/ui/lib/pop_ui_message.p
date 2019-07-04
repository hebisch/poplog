/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_message.p
 > Purpose:         Poplog UI message window
 > Author:          Julian Clinton, July 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui => pop_ui_message;
exload_batch;

include pop_uiP.ph;

uses
    $-poplog_ui$-guiShells,
    $-poplog_ui$-guiUtils,
;

lvars message_switch_vec;


;;; pop_ui_message takes three arguments:
;;;
;;;     The message (a string).
;;;
;;;     A text justification flag (<true> = centre, <false> = left)
;;;
;;;     reference widget (widget or <false>). If <false> then
;;;     pop_ui_app_shell is used.
;;;
;;; Can also take an optional last argument (a string) which replaces
;;; any default window title.
;;;
define pop_ui_message(message, centre_text, ref_widget);
    lvars message, centre_text, ref_widget, title = 'Poplog: Message';

    if isstring(ref_widget) then    ;;; change window title
        ((), message, centre_text, ref_widget) ->
                (message, centre_text, ref_widget, title);
    endif;
    p_MESSAGE(message, centre_text, ref_widget, title)
enddefine;

SET_GUI(message_switch_vec, message_xm, message_xol, 'pop_ui_message');

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
