/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_helptool.p
 > Purpose:         Poplog UI help tool
 > Author:          Julian Clinton, July 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui => pop_ui_helptool, pop_ui_helptool_defaults;
exload_batch;

include pop_uiP.ph;

uses
    $-poplog_ui$-searchforhelp,
    $-poplog_ui$-guiXlibs,
    $-poplog_ui$-guiActions,
    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiSubsystem,
    $-poplog_ui$-guiShells,
    $-poplog_ui$-guiMouseEvents,
;

vars pop_ui_helptool_defaults = []; ;;; default contents of help tool
                                    ;;; scrolling list -
                                    ;;;     user-assignable

lvars helptool_switch_vec;

;;; pop_ui_helptool creates and pops up the Poplog help tool. The arguments
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
define pop_ui_helptool(/*subject, subsysoptions, fileoptions,
                              search_index, refer_widget*/);
    p_HELPTOOL()
enddefine;

SET_GUI(helptool_switch_vec, helptool_xm, helptool_xol, 'pop_ui_helptool');

endexload_batch;
endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
Julian Clinton,  20/8/91
    Changed mishap to a warning if widget set cannot be determined.
Julian Clinton,  2/8/91
    Added global vars pop_ui_helptool_defaults.
Julian Clinton,  15/7/91
    Changed to use XOPENLOOK instead of XOPENWINDOWS.
 */
