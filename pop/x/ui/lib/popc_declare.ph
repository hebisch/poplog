/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/ui/lib/popc_declare.ph
 > Purpose:         Identifier declarations for POPC in this directory
 > Author:          John Gibson, Jun  4 1993 (see revisions)
 */
compile_mode :pop11 +strict;

library_declare_section '$usepop/pop/x/ui/lib/'

section;

weak constant procedure (
        pop_ui_add_property,
        pop_ui_choose_file,
        pop_ui_compiletool,
        pop_ui_confirm,
        pop_ui_edittool,
        pop_ui_filetool,
        pop_ui_helptool,
        pop_ui_information,
        pop_ui_librarytool,
        pop_ui_logo,
        pop_ui_message,
        pop_ui_popcontroltool,
        pop_ui_prompttool,
        pop_ui_propertytool,
        pop_ui_save_properties,
        pop_ui_show_property,
    );

weak constant active (
        pop_ui_property_list,
    );

weak constant
        pop_ui_vedcomms,
    ;

weak vars active (
        pop_ui_promptsource,
    );

weak vars procedure (
        ved_helptool,
        ved_librarytool,
        ved_writerangeto,
    );

weak vars
        pop_property_panel,
        pop_ui_file_search_directory,
        pop_ui_file_search_name,
        pop_ui_file_search_pattern,
        pop_ui_helptool_defaults,
        pop_ui_promptwarpmouse,
    ;

endsection;

section $-poplog_ui;

weak constant procedure (
        Check_integer,
        Check_list,
        Check_string,
        Check_word,
        ConsArgList,
        XFlush
        XRaiseWindow,
        call_action,
        call_ved,
        file_search_defaults,
        gen_ss_label,
        guiInterrupt,
        guiRealizeWidget,
        is_doubleclick,
        isabsolute_pathname,
        isfile,
        lib_of_descriptor,
        libtool_error,
        libtool_list_to_string,
        libtool_string_to_list,
        panel_ss_table,
        pop_ui_close_project,
        pop_ui_kill_project,
        pop_ui_libdescriptors,
        pop_ui_new_project,
        pop_ui_open_project,
        pop_ui_projecttool,
        pop_ui_save_project,
        proptool_create,
        proptool_init,
        proptool_is_created,
        proptool_names,
        proptool_needs_saving,
        proptool_save_pr,
        proptool_save_to_file,
        searchforhelp,
        ss_libcompile,
        ss_libhelp,
        ss_libshow,
        subsystem_datavec,
        sys_dir_name,
        sys_get_files,
    );

weak constant active (
        pop_ui_app_shell,
        pop_ui_current_project,
    );

weak constant
        guiActions,
        guiFileutils,
        guiMouseEvents,
        guiShells,
        guiSubsystem,
        guiUtils,
        guiXlibs,
        panel_ss_ordering,
    ;

endsection;

end_library_declare_section;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 12 1996
        Added missing declarations for new things used by
        pop_ui_popcontroltool.p
--- John Gibson, Jul 28 1995
        Added missing declarations for new things used by xveddialogs.p and
        other ui/lib files
--- Robert John Duncan, May  4 1995
        Various additions and deletions for recent round of changes
 */
