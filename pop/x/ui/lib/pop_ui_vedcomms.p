/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ui/lib/pop_ui_vedcomms.p
 > Purpose:         Ved commands using UI tools
 > Author:          Robert John Duncan, Apr 21 1995 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode:pop11 +strict;


section $-poplog_ui =>
    ved_openfile,
    ved_compilefile,
    ved_savefileas,
    ved_writefileto,
    ved_writerangeto,
    ved_insertfile,
    ved_helptool,
    ved_librarytool,
    ved_aboutpoplog,
    pop_ui_vedcomms,
;

uses
    pop_ui_logo,
    pop_ui_choose_file,
    pop_ui_edittool,
    pop_ui_compiletool,
    pop_ui_helptool,
    pop_ui_librarytool,
;

include vm_flags.ph;
include subsystem.ph;

    ;;; open a file for editing
define vars ved_openfile;
    pop_ui_edittool(false, false, false, false);
enddefine;

    ;;; compile an existing file
define vars ved_compilefile;
    pop_ui_compiletool(false, false, false, false);
enddefine;

    ;;; rename and save the current file
define vars ved_savefileas;
    lvars name = pop_ui_choose_file(false, 'Poplog: Save File', 'Save',
        false, false, nullstring, 2:11);
    if name then
        veddo('name ' <> name, true);
        veddo('w1', true);
    endif;
enddefine;

    ;;; write the current buffer under a different name
define vars ved_writefileto;
    lvars name = pop_ui_choose_file(false, 'Poplog: Write File', 'Write',
        false, false, nullstring, 2:11);
    if name then
        veddo('w ' <> name, true);
    endif;
enddefine;

    ;;; write the marked range in the current buffer
define vars ved_writerangeto;
    lvars name = pop_ui_choose_file(false, 'Poplog: Write Range', 'Write',
        false, false, nullstring, 2:11);
    if name then
        veddo('wr ' <> name, true);
    endif;
enddefine;

    ;;; read an existing file into the current buffer
define vars ved_insertfile;
    lvars name = pop_ui_choose_file(false, 'Poplog: Insert File', 'Insert',
        false, false, false, 2:11);
    if name then
        veddo('r ' <> name, true);
    endif;
enddefine;

    ;;; pop up the Help Tool
define vars ved_helptool;
    pop_ui_helptool(false, false, false, true, false) ->;
enddefine;

    ;;; pop up the Library Tool
define vars ved_librarytool;
    pop_ui_librarytool(false, false, false) ->;
enddefine;

    ;;; pop up the 'About Poplog' message
define vars ved_aboutpoplog;
    pop_ui_logo(false);
enddefine;

constant pop_ui_vedcomms = true;    ;;; for uses

endsection;     /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 28 1995
        Made pop_ui_vedcomms a constant
--- Robert John Duncan, Jul  7 1995
        Fixed silly bug in ved_savefileas
--- Robert John Duncan, Jun 14 1995
        Got rid of ved_n*ewfile
--- Robert John Duncan, Jun  7 1995
        Exported pop_ui_vedcomms
 */
