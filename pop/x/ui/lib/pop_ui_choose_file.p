/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ui/lib/pop_ui_choose_file.p
 > Purpose:         Poplog UI file selection box
 > Author:          Robert John Duncan, Apr 10 1995
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;


uses-now popxlib;

section $-poplog_ui =>  pop_ui_choose_file,
                        pop_ui_file_search_directory,
                        pop_ui_file_search_pattern,
                        pop_ui_file_search_name,
;

exload_batch;

include pop_uiP.ph;

uses
    $-poplog_ui$-guiShells,
    $-poplog_ui$-guiUtils,
;

lvars filetool_switch_vec;

vars
    ;;; these variables allow customisation of the initial search spec:
    ;;; musn't be initialised globally, because users may already have
    ;;; defined them
    pop_ui_file_search_directory,
    pop_ui_file_search_pattern,
    pop_ui_file_search_name,
;

define file_search_defaults(dir, pattern, file) -> (dir, pattern, file);
    lvars dir, pattern, file;
    unless isstring(dir) then
        if isstring(pop_ui_file_search_directory) then
            pop_ui_file_search_directory
        else
            nullstring
        endif -> dir;
    endunless;
    unless isstring(pattern) then
        if isstring(pop_ui_file_search_pattern)
        and pop_ui_file_search_pattern /= nullstring
        then
            pop_ui_file_search_pattern
        else
            '*' <> pop_default_type
        endif -> pattern;
    endunless;
    unless isstring(file) then
        if isstring(pop_ui_file_search_name) then
            pop_ui_file_search_name
        else
            nullstring
        endif -> file;
    endunless;
enddefine;

    ;;; pop up a dialog box for choosing a file and return the full
    ;;; pathname or <false> if the box was cancelled
    ;;;     parent         parent widget
    ;;;     title          dialog title
    ;;;     label          label for the OK button
    ;;;     directory      directory for file match
    ;;;     pattern            wildcard pattern for file match
    ;;;     file           an initial choice
    ;;;     flags          flags for fine-tuning
define pop_ui_choose_file(parent, title, label, directory, pattern, file,
                            flags);
    lvars parent, title, label, directory, pattern, file, flags;
    unless parent then
        pop_ui_app_shell -> parent;
    endunless;
    XptLiveTypeCheck(parent, "Widget") -> parent;
    Check_string(title, true);
    Check_string(label, true);
    Check_string(directory, true);
    Check_string(pattern, true);
    Check_string(file, true);
    ;;; process flags
    unless isinteger(flags) then flags and 1 or 0 -> flags endunless;
    lvars allow_new_file = flags &&/=_0 2:01;
    lvars show_buffer_name = false;
    if flags &&/=_0 2:10 and isstring(vednamestring) then
        lvars i;
        if locchar(`\s`, 1, vednamestring) ->> i then
            allbutfirst(i, vednamestring) -> show_buffer_name;
        endif;
    endif;
    p_FILETOOL(parent, title, label, directory, pattern, file,
        show_buffer_name, allow_new_file);
enddefine;

SET_GUI(filetool_switch_vec, filetool_xm, filetool_xol, 'pop_ui_filetool');

endexload_batch;
endsection; /* $-poplog_ui */
