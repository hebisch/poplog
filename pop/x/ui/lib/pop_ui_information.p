/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_information.p
 > Purpose:         Poplog UI information board
 > Author:          Julian Clinton, July 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui => pop_ui_information;
exload_batch;

include pop_uiP.ph;

uses
    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiShells,
;

lvars information_switch_vec;

;;; pop_ui_information takes two arguments:
;;;
;;;     The information (a string).
;;;
;;;     A text justification flag (<true> = centre, <false> = left)
;;;
;;;     reference widget (widget or <false>). If <false> then
;;;     pop_ui_app_shell is used.
;;;
;;; Can also take an optional last argument (a string) which replaces
;;; any default window title.
;;;
;;; The result is information widget.
;;;
define pop_ui_information(info, centre_text, ref_widget) -> info_shell;
    lvars   info, centre_text, ref_widget, widget, info_shell,
            title =  'Poplog: Information';

    if isstring(ref_widget) then    ;;; change window title
        ((), info, centre_text, ref_widget) ->
                (info, centre_text, ref_widget, title);
    endif;

    Check_string(info, false);

    p_INFORMATION(info, centre_text, ref_widget, title) -> info_shell
enddefine;

SET_GUI(information_switch_vec, information_xm, information_xol,
                                                    'pop_ui_information');

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
