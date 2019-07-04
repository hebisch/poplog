/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_filetool.p
 > Purpose:         Poplog UI file selection tool
 > Author:          Julian Clinton, August 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui =>  pop_ui_filetool,
                        pop_ui_edittool,
                        pop_ui_compiletool;
exload_batch;

include sysdefs.ph;

uses
    xpt_cursorplane,
    pop_ui_choose_file,
    $-poplog_ui$-guiActions,
    $-poplog_ui$-guiUtils,
;

    ;;; this preserves historical behaviour: all the File/Edit/Compile
    ;;; tools want to share the same instance of the File Chooser
    ;;; parented on whatever widget was passed as parent on the first
    ;;; (creating) call; pop_ui_choose_file will create a new instance
    ;;; for each distinct parent, so we have to pass the same one every
    ;;; time
lvars filetool_parent = false;
define lconstant Get_parent(parent);
    lvars parent;
    if XptIsLiveType(filetool_parent, "Widget") then
        filetool_parent
    elseif parent then
        XptLiveTypeCheck(parent, "Widget")
    else
        pop_ui_app_shell
    endif ->> filetool_parent;
enddefine;

define pop_ui_filetool(fname, directory, filter, label, allow_new_file,
                        ref_widget);
    lvars fname, directory, filter, label, allow_new_file, ref_widget;
    pop_ui_choose_file(Get_parent(ref_widget), 'Poplog: File Tool', label,
        directory, filter, fname, allow_new_file);
enddefine;

define pop_ui_edittool(name, directory, filter, ref_widget);
    lvars name, directory, filter, ref_widget;
    lvars filename = pop_ui_choose_file(Get_parent(ref_widget),
        'Poplog: Open File', 'Open', directory, filter, name, true);
    if filename then
        ;;; make relative to current directory, if possible
        lvars path = sysfileok(filename);
        if isstartstring(current_directory, path) then
            allbutfirst(length(current_directory), path) -> filename;
        endif;
#_IF DEF VMS
        ;;; under VMS, we need remove the version number, otherwise
        ;;; trying to write the file gives an error
        sys_fname(filename, 1, 5) -> filename;
#_ENDIF
        external_defer_apply(call_ved, 'ved\s' <> filename, 1);
    endif;
enddefine;

define pop_ui_compiletool(name, directory, filter, ref_widget);
    lvars name, directory, filter, ref_widget;
    lvars filename = pop_ui_choose_file(Get_parent(ref_widget),
        'Poplog: Compile File', 'Compile', directory, filter, name, false);
    if filename then
        external_defer_apply(call_ved, 'load\s' <> filename, 1);
    endif;
enddefine;

endexload_batch;
endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 22 1996
        Changed to use new external_defer_apply facility for creating
        closures automatically.
--- Robert John Duncan, Jun 14 1995
        Changed the compile tool to use <ENTER> load
--- Robert John Duncan, Apr 10 1995
        Moved the Edit and Compile tools into this file and changed them all
        to use the new pop_ui_choose_file
--- John Gibson, Jun 28 1993
        Changes for POPC
Julian Clinton,  21/8/91
    Put declarations inside compiler checks.
Julian Clinton,  20/8/91
    Changed mishap to a warning if widget set cannot be determined.
 */
