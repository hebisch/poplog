/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/pop_ui_projecttool.p
 > Purpose:         Poplog Project Panel & Preferences Tools
 > Author:          Julian Clinton, April 1995 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

#_IF DEF POPC_COMPILING
    section $-poplog_ui;
        define pop_ui_close_project(); enddefine;
        define pop_ui_kill_project(); enddefine;
        define pop_ui_new_project(); enddefine;
        define pop_ui_open_project(); enddefine;
        define pop_ui_save_project(); enddefine;
        define pop_ui_projecttool(); enddefine;
        define active pop_ui_current_project; false enddefine;
    endsection;
    #_TERMIN_IF true
#_ENDIF


uses-now popxlib;

section $-poplog_ui => ved_project;

include pop_uiP;
include xpt_coretypes;
include sysdefs;
include xdefs;

exload_batch

uses
    propsheet,
    XptBusyCursorFeedback,
    XptGarbageCursorFeedback,
    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiShells,
    $-poplog_ui$-guiFileutils,
    $-poplog_ui$-guiActions,
    $-poplog_ui$-guiXlibs,
    $-poplog_ui$-guiProjectUtils,
    pop_ui_information,
    pop_ui_prompttool,
    pop_ui_filetool,
    pop_ui_text_input,

    xt_util,
;


;;; The Project Tool library consists of 4 main parts:
;;;
;;; 1. the Project Tool itself (a window containing a list of files
;;; associated with the project which allows the user to edit, compile
;;; etc.
;;;
;;; 2. the Project Prefsuration Tool which allows the user to set
;;; information about the project e.g. how dates/times are displayed,
;;; what information to save and restore etc.
;;;
;;; 3. the Target Tool which allows the user to specify and customise
;;; targets for the program
;;;
;;; 4. the File Information tool which allows the user to change
;;; information associated with a particular file

;;; Forward declarations of the main procedures
;;;
constant procedure (
    pop_ui_read_project,
    pop_ui_projecttool,
    pop_ui_projectprefstool,
    pop_ui_projectmkimagetool,
    pop_ui_new_project,
    pop_ui_save_project,
    pop_ui_close_project,

    ;;; should be lconstants but we try to create closures on
    ;;; procedures which refer to them...
    ;;;
    show_project_file_sheet,
    update_project_file_sheet,
    destroy_project_file_sheet,
    close_all_project_file_sheets
);

lconstant procedure (
    BuildImage,
    GenerateScripts,
    update_project_subwindows,
    close_project_subwindows,
    close_project_edit_windows,
    close_project_windows,
);


/* ---------------------------------------------------------------
    Local Constants
   --------------------------------------------------------------- */

lvars
    projecttool_switch_vec;

lvars
    ;;; tables for mapping between project file descriptor strings and
    ;;; the actual file structures
    ;;;
    project_file_to_string = newproperty([], 64, false, "perm"),
    project_string_to_file = newmapping([], 64, false, "perm"),

    ;;; project tool variables
    ;;;
    projecttool_win = false,
;


;;; The active variable pop_ui_current_project does the bits and pieces
;;; such as saving the project descriptor and the workspace. It is
;;; not responsible for closing the windows associated with the project.
;;; The value assigned is either a boolean or a new project. For booleans,
;;; <true> means any current project is saved; if <false> then the existing
;;; project is simply discarded (this should only occur if the user presses
;;; Cancel when configuring a new project). Note that in both cases, <false>
;;; will be assigned to the underlying lvar.
;;;
lvars current_project = false;

define global active pop_ui_current_project;
    current_project;
enddefine;

define updaterof global active pop_ui_current_project(newproj);
    lvars newproj;

    returnif(current_project == newproj);

    ;;; type check
    unless isboolean(newproj) then
        newproj.Proj_desc_data -> ;
    endunless;

    ;;; check if need to save the current project
    ;;;
    if current_project and newproj then
        pop_ui_save_project();
    endif;

    if isboolean(newproj) then
        false
    else
        newproj
    endif -> current_project;
enddefine;


/* ---------------------------------------------------------------
    The Project Files Window
   --------------------------------------------------------------- */

lconstant macro (
    ;;; callbacks in the projecttool window
    PROJ_WIN_DISPLAY        = 1,
    PROJ_WIN_RAISE          = 2,
    PROJ_WIN_CLOSE          = 3,
    PROJ_WIN_SETSHELLPOS    = 4,
    PROJ_WIN_SETTITLE       = 5,
    PROJ_WIN_SETFOOTER      = 6,
    PROJ_WIN_CLEARFOOTER    = 7,
    PROJ_WIN_GETLIST        = 8,
    PROJ_WIN_GETLISTCOUNT   = 9,
    PROJ_WIN_GETSELECTION   = 10,
    PROJ_WIN_GETSELECTCOUNT = 11,
    PROJ_WIN_SETLIST        = 12,
    PROJ_WIN_SETSELECTION   = 13,
    PROJ_WIN_SELECTALL      = 14,
    PROJ_WIN_DESELECTALL    = 15,
    PROJ_WIN_REPLACEITEM    = 16,
    PROJ_WIN_ADDLISTITEMS   = 17,
    PROJ_WIN_REMOVELISTITEMS= 18,
    PROJ_WIN_GETMENUBAR     = 18,
    PROJ_WIN_GETMENUS       = 20,
    PROJ_WIN_GETSHELL       = 21,
    PROJ_WIN_GETPARENT      = 22,
    PROJ_WIN_GETCANCELSTATE = 23,
    PROJ_WIN_SETCANCELSTATE = 24,

    ;;; list events
    PROJ_EV_SELECT          = 1,
    PROJ_EV_ACTION          = 2,


    ;;; project manipulation menu
    PRJPROJ_CB_NEW          = 1,
    PRJPROJ_CB_OPEN         = 2,
    PRJPROJ_CB_SAVE         = 3,
    PRJPROJ_CB_SAVEAS       = 4,
    PRJPROJ_CB_PREFS       = 5,
    PRJPROJ_CB_RENAME       = 6,
    PRJPROJ_CB_CLOSE        = 7,

    ;;; project select menu
    PRJSELN_CB_OPEN         = 1,
    PRJSELN_CB_VIEW         = 2,
    PRJSELN_CB_SAVE         = 3,
    PRJSELN_CB_CLOSE        = 4,
    PRJSELN_CB_COMPILE      = 5,
    PRJSELN_CB_RUN          = 6,
    PRJSELN_CB_FILEINFO     = 7,

    ;;; project file edit menu
    PRJEDIT_CB_ADD          = 1,
    PRJEDIT_CB_ADDAFTER     = 2,
    PRJEDIT_CB_ADDBEFORE    = 3,
    PRJEDIT_CB_RENAME       = 4,
    PRJEDIT_CB_REMFILE      = 5,
    PRJEDIT_CB_DELALL       = 6,
    PRJEDIT_CB_REFRESH      = 7,
    PRJEDIT_CB_SELECTALL    = 8,
    PRJEDIT_CB_DESELECTALL  = 9,

    ;;; project compilation and execution menu
    PRJBUILD_CB_COMPILE     = 1,
    PRJBUILD_CB_COMPILECHANGED      = 2,
    PRJBUILD_CB_COMPILEALL  = 3,
    PRJBUILD_CB_MKIMAGE     = 4,
    PRJBUILD_CB_GENSCRIPTS  = 5,
    PRJBUILD_CB_IMAGETOOL   = 6,

    ;;; project help menu
    PRJHELP_CB_PROJECT      = 1,
    PRJHELP_CB_TOOL         = 2,
    PRJHELP_CB_POPLOG       = 3,
    PRJHELP_CB_HELPTOOL     = 4,
);


lconstant
    PRJ_MESSAGETITLE = 'Project Message',
    PRJ_ERRORTITLE = 'Project Error',
;

lconstant
    TOOL_OPENING            = 'Opening %S ...',
    TOOL_CLOSING            = 'Closing %S ...',
    TOOL_ADDING             = 'Adding %S ...',
    TOOL_BUILDING           = 'Building %S ...',
    TOOL_REMOVING           = 'Removing %S ...',
    TOOL_REMOVINGALL        = 'Removing all files ...',
    TOOL_RENAMING           = 'Renaming %S ...',
    TOOL_COMPILING          = 'Compiling %S ...',
    TOOL_SAVING             = 'Saving %S ...',
    TOOL_RUNNING            = 'Running %S ...',
    TOOL_STARTING           = 'Starting %S ...',
    TOOL_GENERATING         = 'Generating %S ...',
    TOOL_OPENPROJ           = 'Opening project ...',
    TOOL_RENAMEPROJ         = 'Renaming project ...',
    TOOL_SAVEASPROJ         = 'Save project as ...',
    TOOL_UPDATEPROJ         = 'Updating project ...',
    TOOL_AUTOSAVEPROJ       = 'Auto-saving project ...',
    TOOL_OKAY               = 'OK',
    TOOL_FAILED             = 'Failed',
    TOOL_CANCELLED          = 'Cancelled',
    TOOL_INTERRUPTED        = 'Interrupted',
;


define lconstant set_file_message(fname, msg); lvars fname, msg;

    if projecttool_win then
        p_PROJECTTOOL_OP(if fname then sprintf(fname, msg) else msg endif,
            projecttool_win, PROJ_WIN_SETFOOTER);
#_IF DEFV pop_internal_version < 145200
        XptAppTryEvents(XptDefaultAppContext);
#_ELSE
        while XptAppTryEvents(XptDefaultAppContext, true) do
        endwhile;
#_ENDIF
    endif;
enddefine;

lconstant procedure (
    OpeningMsg      = set_file_message(%TOOL_OPENING%),
    ClosingMsg      = set_file_message(%TOOL_CLOSING%),
    AddingMsg       = set_file_message(%TOOL_ADDING%),
    BuildingMsg     = set_file_message(%TOOL_BUILDING%),
    RemovingMsg     = set_file_message(%TOOL_REMOVING%),
    RemovingAllMsg  = set_file_message(%false, TOOL_REMOVINGALL%),
    RenamingMsg     = set_file_message(%TOOL_RENAMING%),
    CompilingMsg    = set_file_message(%TOOL_COMPILING%),
    SavingMsg       = set_file_message(%TOOL_SAVING%),
    RunningMsg      = set_file_message(%TOOL_RUNNING%),
    StartingMsg     = set_file_message(%TOOL_STARTING%),
    GeneratingMsg   = set_file_message(%TOOL_GENERATING%),
    OpenProjMsg     = set_file_message(%false, TOOL_OPENPROJ%),
    RenameProjMsg   = set_file_message(%false, TOOL_RENAMEPROJ%),
    SaveAsProjMsg   = set_file_message(%false, TOOL_SAVEASPROJ%),
    UpdateProjMsg   = set_file_message(%false, TOOL_UPDATEPROJ%),
    AutosaveProjMsg = set_file_message(%false, TOOL_AUTOSAVEPROJ%),
    OkayMsg         = set_file_message(%false, TOOL_OKAY%),
    FailedMsg       = set_file_message(%false, TOOL_FAILED%),
    CancelledMsg    = set_file_message(%false, TOOL_CANCELLED%),
    InterruptedMsg  = set_file_message(%false, TOOL_INTERRUPTED%),
);


/* ---------------------------------------------------------------
    Utility Procs For Accepting & Verifying Values
   --------------------------------------------------------------- */

;;; issimplename checks that a name:
;;;    (a) is a string of at least one character
;;;    (b) starts with an alphabetic character
;;;    (c) only contains alphnumeric characters after that
;;;
define lconstant issimplename(name);
lvars name i c;

    check_string(name);
    returnif(name = nullstring)(false);

    returnunless(isalphacode(subscrs(1, name) ->> c))(false);
    fast_for i from 2 to length(name) do
        returnunless(isalphacode(subscrs(i, name) ->> c) or isnumbercode(c))(false);
    endfast_for;
    name;
enddefine;


define check_project_name(name);
lconstant err = 'A project name must start with an alphabetic\
character and only contain alphanumeric characters';
lvars name;

    unless issimplename(name) then
        err
    else
        true
    endunless;
enddefine;

define check_project_path(path) /* -> okay */ ;
lvars path, dir = sys_fname_path(path);

    returnif(sys_dir_name(dir))(true);

    pop_ui_message(sprintf(path,
        'Can\'t locate file - invalid directory:\n\n    %S'), false,
        p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL),
        PRJ_ERRORTITLE);
    FailedMsg();
    false;
enddefine;


/* ---------------------------------------------------------------
    Environment Utilities
   --------------------------------------------------------------- */

define lconstant get_edit_save_prefs(project)
        -> (save_all, save_any, save_edpos, save_toolpos);
lvars project, save_all, save_any, save_edpos, save_toolpos;

    switchon project.Proj_save_prefs.ProjSV_ed_savewhat
        case = PRJ_ALL_EDFILES_LABEL then
            true, true
        case = PRJ_PROJ_EDFILES_LABEL then
            false, true
        else
            false, false
    endswitchon -> (save_all, save_any);

    if project.Proj_save_prefs.ProjSV_ed_saveprefs then
        true
    else
        false
    endif -> save_edpos;

    if project.Proj_save_prefs.ProjSV_tool_saveprefs then
        true
    else
        false
    endif -> save_toolpos;
enddefine;


;;; currently the workspace is simply a list of lists. The first
;;; contains information about the state of the project tool and
;;; the second list contains names of open Ved file names.
;;;

;;; For each Ved file, the list contains:
;;;
;;; subsystem
;;; vedfileprops
;;; file name or abbreviated part (for ref, help, include etc.)
;;; if to be saved then positional information
;;; vedwriteable
;;;
define create_project_workspace() /* -> workspace */ ;
    dlocal current_directory = get_project_basedir(current_project);

    lvars files = current_project.Proj_files_list;
    lvars save_all, save_any, save_edpos, save_toolpos;

    get_edit_save_prefs(current_project) ->
        (save_all, save_any, save_edpos, save_toolpos);

    [%
        pop_ui_projecttool_version,

        [%  if save_toolpos then
                XptWidgetCoords(p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL))
            endif %],

        [% if save_any and vedsetupdone then
            vedappfiles(
                procedure; lvars p;

                    ;;; if save_all is true, don't need to check whether
                    ;;; we're saving a project file
                    ;;;
                    if save_all
                    or member(gen_project_relpath(current_project,vedcurrent),
                        files)
                    then
                      [%
                        subsystem;

                        vedfileprops ->> p;

                        ;;; for things such as showlib etc. abbreviate the name
                        ;;;
                        if p then
                            sys_fname_nam(vedcurrent);
                        else
                            vedcurrent;
                        endif;

                        vedwriteable;

                        if save_edpos then
                            [% get_ved_window_params(ved_current_file) %];
                            [% get_ved_file_params(ved_current_file) %];
                        endif;

                      %];
                    endif;
                endprocedure);
        endif %]
    %];
enddefine;


;;; once a workspace has been read from disk and the main project
;;; tool has been open, restore the project workspace as far as possible
;;;
define lconstant restore_project_workspace(workspace);
lvars workspace;

    tl(workspace) -> workspace;     ;;; ignore version for now

    ;;; first is the shell position for the project
    ;;;
    lvars toolpos;
    fast_destpair(workspace) -> workspace -> toolpos;

    lvars save_all, save_any, save_edpos, save_toolpos;

    get_edit_save_prefs(current_project)
            -> (save_all, save_any, save_edpos, save_toolpos);

    if save_toolpos and toolpos /== [] then
        dl(toolpos) -> XptWidgetCoords(p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL));
    endif;

    if save_any then
        lvars basedir = get_project_basedir(current_project);
        lvars file, files = hd(workspace);
        for file in files do
            open_ved_window(basedir, dl(file));
        endfor;
        /*
        unless files == [] then
            vedinput(vedappfiles(%vedrefresh%));
        endunless;
        */
    endif;
enddefine;


/* ---------------------------------------------------------------
    Menu Buttons
   --------------------------------------------------------------- */

lconstant
    projMenu = 'projectMenu',
    selectMenu = 'selectMenu',
    editMenu = 'editMenu',
    buildMenu = 'buildMenu',
    runMenu = 'runMenu',
    helpMenu = 'helpMenu',

    menuSeparator = {^nullstring ^nullstring ^false ^false},

    ;;; create button labels for menu buttons that may have their
    ;;; sensitivity changed (currently only on the File, Edit or Build menus)
    ;;;
    selnOpenName            = 'openButton',
    selnViewName            = 'viewButton',
    selnSaveName            = 'saveButton',
    selnCloseName           = 'closeButton',
    selnCompileName         = 'compileButton',
    selnRunName             = 'runButton',
    selnFileInfoName        = 'fileInfoButton',
    editAddName             = 'addButton',
    editAddAfterName        = 'addAfterButton',
    editAddBeforeName       = 'addBeforeButton',
    editRenameName          = 'renameButton',
    editRemoveName          = 'deleteButton',
    editRefreshName         = 'refreshButton',
    editSelectAllName       = 'selectAllButton',
    editDeselectAllName     = 'deselectAllButton',
    buildCompileChangedName = 'compileChangedButton',
    buildCompileAllName     = 'compileAllButton',
    buildSavedImageName     = 'savedImageButton',
    buildImageScriptsName   = 'imageScriptsButton',
    buildImageToolName      = 'imageToolButton',
;


/* ---------------------------------------------------------------
    Setting The Sensitivity Of Menu Options
   --------------------------------------------------------------- */


;;; check_selection looks through the selected files and
;;; applies the test to each file in the selection. As soon
;;; as a test returns true, the selection returns true.
;;;
;;;
define lconstant check_selection(seln, test_pdr) /* -> one_applies */ ;
lvars seln, test_pdr;
dlocal current_directory = get_project_basedir(current_project);

    lvars file;

    repeat forever
        quitif(null(seln));
        fast_destpair(seln) -> seln -> file;
        if (project_string_to_file(file) ->> file) and test_pdr(file) then
            return(true);
        endif;
    endrepeat;

    false;
enddefine;


;;; test whether a file is being edited
;;;
define lconstant projfile_vedpresent(fname); lvars fname;
lvars fullname;

    define lconstant check_present;
        if vedcurrent = fname or vedpathname = fullname then
            ved_current_file;
            exitfrom(projfile_vedpresent);
        endif;
    enddefine;

    if vedsetupdone then
        gen_project_abspath(current_project, fname) -> fullname;
        vedappfiles(check_present);
    endif;
    false;
enddefine;


define lconstant can_compile_file(file); lvars file, ss;
    is_filetype_compilable(file) and (is_filetype_poplog(file) ->> ss)
        and is_subsystem_loaded(ss);
enddefine;


define lconstant can_run_file(file); lvars file;
#_IF DEF VMS
    file.File_contents == DCL_LABEL;
#_ELSE
    file.File_contents == SH_LABEL or file.File_contents == CSH_LABEL;
#_ENDIF
enddefine;


define lconstant can_close_file(file); lvars file;
dlocal ved_current_file;
    if projfile_vedpresent(file.File_name) then
        true
    else
        false
    endif;
enddefine;


define lconstant can_save_file(file); lvars file, buff;
dlocal ved_current_file;
    if (projfile_vedpresent(file.File_name)->>buff) then
        buff -> ved_current_file;
        vedwriteable and vedchanged
    else
        false
    endif;
enddefine;


define lconstant compilable_selection = check_selection(%can_compile_file%);
enddefine;

define lconstant closeable_selection = check_selection(%can_close_file%);
enddefine;

define lconstant saveable_selection = check_selection(%can_save_file%);
enddefine;

define lconstant runnable_selection = check_selection(%can_run_file%);
enddefine;



;;; Modify whether certain menu buttons are active depending on the
;;; selection of items in the files list
;;;
define lconstant is_sensitive(widget, name) -> flag;
    lvars widget, name, flag = false;
    if XtNameToWidget(widget, name) ->> widget then
        XtIsSensitive(widget) -> flag;
    endif;
enddefine;
;;;
define updaterof is_sensitive(flag, widget, name);
    lvars flag, widget, name;

    if XtNameToWidget(widget, name) ->> widget then
        XtSetSensitive(widget, flag and true);
    endif;
enddefine;


;;; set_sensitive alters the sensitivity of various menu options
;;; depending on the selection from the file list and the number of
;;; files in the list
;;;
define set_sensitive(n_seln, n_files);
lvars n_seln, n_files;

    lvars pmenu, smenu, emenu, bmenu, hmenu;

    p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETMENUS) ->
        (pmenu, smenu, emenu, bmenu, hmenu);

    if n_seln == 0 then

        ;;; no selection
        ;;;
        false ->> is_sensitive(smenu, selnOpenName)
            ->> is_sensitive(smenu, selnViewName)
            ->> is_sensitive(smenu, selnSaveName)
            ->> is_sensitive(smenu, selnCloseName)
            ->> is_sensitive(smenu, selnCompileName)
            ->> is_sensitive(smenu, selnRunName)
            ->> is_sensitive(smenu, selnFileInfoName)
            ->> is_sensitive(emenu, editAddAfterName)
            ->> is_sensitive(emenu, editAddBeforeName)
            ->> is_sensitive(emenu, editRenameName)
            ->> is_sensitive(emenu, editRemoveName)
            -> is_sensitive(emenu, editDeselectAllName);

        ;;; some operations only make sense if there are files
        ;;;
        (n_files > 0)
            ->> is_sensitive(emenu, editRefreshName)
            ->> is_sensitive(emenu, editSelectAllName)
            ->> is_sensitive(bmenu, buildCompileChangedName)
            -> is_sensitive(bmenu, buildCompileAllName);

    else

        ;;; items selected
        ;;;
        true ->> is_sensitive(smenu, selnOpenName)
            ->> is_sensitive(smenu, selnViewName)
            ->> is_sensitive(smenu, selnFileInfoName)
            ->> is_sensitive(emenu, editRemoveName)
            ->> is_sensitive(emenu, editRefreshName)
            ->> is_sensitive(emenu, editDeselectAllName)
            ->> is_sensitive(bmenu, buildCompileChangedName)
            -> is_sensitive(bmenu, buildCompileAllName);

        lvars seln = p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSELECTION);

        saveable_selection(seln) -> is_sensitive(smenu, selnSaveName);
        closeable_selection(seln) -> is_sensitive(smenu, selnCloseName);
        compilable_selection(seln) -> is_sensitive(smenu, selnCompileName);
        runnable_selection(seln) -> is_sensitive(smenu, selnRunName);

        sys_grbg_list(seln);

        ;;; if all files selected, disable select all
        not(n_files == n_seln)
            -> is_sensitive(emenu, editSelectAllName);

        ;;; can only add before or after, or rename if there's
        ;;; only a single selection
        ;;;
        n_seln == 1
            ->> is_sensitive(emenu, editAddAfterName)
            ->> is_sensitive(emenu, editAddBeforeName)
            -> is_sensitive(emenu, editRenameName);
    endif;
enddefine;


;;; used to set the sensitivity of various menu options
;;;
define lconstant SetMenuSensitivity();
    set_sensitive(
        p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSELECTCOUNT),
        current_project.Proj_files_count);
enddefine;


/* ---------------------------------------------------------------
    Enable/Disable The Tool's "Cancel" Button
   --------------------------------------------------------------- */

lvars cancel_enabled = false;
define lvars active CancelEnabled;
    cancel_enabled;
enddefine;

define updaterof active CancelEnabled(newval);
lvars newval;

    p_PROJECTTOOL_OP(newval, projecttool_win, PROJ_WIN_SETCANCELSTATE);
    newval -> cancel_enabled;
enddefine;


define lconstant CancelOp;
    interrupt();
enddefine;

define lconstant UserCancel_cb;
    erasenum(3);
    XptDeferApply(CancelOp);
    XptSetXtWakeup();
enddefine;


/* ---------------------------------------------------------------
    File List Manipulation
   --------------------------------------------------------------- */

;;; takes a project and generates a list
;;;
define lconstant gen_proj_desc_list(project, f_to_s);
lvars project, f_to_s;
lvars ftable = project.Proj_files_table;

    [%
        lvars f;
        for f in project.Proj_files_list do
            f_to_s(ftable(f))
        endfor;
    %]
enddefine;


;;; adds associations between a generated file description string and
;;; a file descriptor structure so we can map both ways between them
;;;
define lconstant add_to_projtool_tables(s, f, f_to_s, s_to_f);
lvars s, f, f_to_s, s_to_f;
    s -> f_to_s(f);
    f -> s_to_f(s);
enddefine;


;;; removes association between a generated file description string and
;;; a file descriptor. Returns the old description string
;;;
define lconstant remove_from_projtool_tables(f, f_to_s, s_to_f) -> s;
lvars s, f, f_to_s, s_to_f;

    if (f_to_s(f) ->> s) then
        false ->> s_to_f(s) -> f_to_s(s);
    endif;
enddefine;


;;; fills in property tables which map from the generated description
;;; string to the file structure and back
;;;
define lconstant gen_projtool_tables(project, f_to_s, s_to_f);
lvars project, f_to_s, s_to_f,
    vprefs = project.Proj_view_prefs,
    fileprefs = vprefs.ProjVP_fileprefs,
    timeformat = vprefs.ProjVP_timeformat,
    dateformat = vprefs.ProjVP_dateformat,
    filetab = project.Proj_files_table;

    lvars fn, f, s;
    for fn in project.Proj_files_list do
        filetab(fn) -> f;           ;;; lookup the file structure
        gen_file_description(project, f, timeformat, dateformat, fileprefs)
            -> s;
        add_to_projtool_tables(s, f, f_to_s, s_to_f);
    endfor;
enddefine;

    ;;; prompt for the name of a file to be added to the project
define lconstant ChooseFile(parent, title, label, file, flags) -> choice;
    lvars parent, title, label, file, flags, choice;
    if current_project and not(isstring(pop_ui_file_search_directory)) then
        ;;; suggest project directory as place to start searching
        dlocal pop_ui_file_search_directory =
            get_project_basedir(current_project);
    endif;
    lvars choice = pop_ui_choose_file(parent, title, label, false, false,
        file, flags);
#_IF DEF VMS
    if choice then
        ;;; remove the version number
        sys_fname(choice, 1, 5) -> choice;
    endif;
#_ENDIF
enddefine;

;;; DoAddFile adds a file to the project tool. the "where" argument is:
;;;     -1      - before current
;;;      1      - after current
;;;      0      - at end
;;;
define lconstant DoAddFile(fname, where, category, contents, description);
lvars fname, where, category, contents, description;

    gen_project_relpath(current_project, fname) -> fname;

    ;;; if this file has already been added, don't add it again
    ;;; and print a warning
    ;;;
    lvars
        proj_files = current_project.Proj_files_list,
        ftable = current_project.Proj_files_table;

    returnif(ftable(fname));

    AddingMsg(fname);

    ;;; create a file structure and add it to the project table
    ;;;
    lvars fstruct = initFileDesc();
    initialise_projfiledesc(fname, fstruct, current_project);

    if category then
        category -> fstruct.File_category;
    endif;

    if contents then
        contents -> fstruct.File_contents;
    endif;

    if description then
        description -> fstruct.File_description;
    endif;

    fstruct -> ftable(fname);

    ;;; generate a description which can be added to the project tool
    ;;; file list...
    ;;;
    lvars
        vprefs = current_project.Proj_view_prefs,
        fstring = gen_file_description(current_project, fstruct,
                    vprefs.ProjVP_timeformat,
                    vprefs.ProjVP_dateformat,
                    vprefs.ProjVP_fileprefs);

    ;;; ...and add it to the tables that map between the visible
    ;;; description string and the file description structure
    ;;;
    add_to_projtool_tables(fstring, fstruct, project_file_to_string,
                        project_string_to_file);


    ;;; Now need to add the file name to the list of file names in the
    ;;; project at the location requested by the user. Also need to
    ;;; add the description string into the displayed list inside the
    ;;; project tool.
    ;;;
    lvars
        visible_files = p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETLIST);

    if where == 0 then
        ;;; add at start
        ;;;
        visible_files nc_<> [^fstring] -> visible_files;
        proj_files nc_<> [^fname] -> proj_files;
    else
        ;;; add above/below selected file
        ;;;
        lvars selected_file = p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSELECTION),
            newfstring, newfname, point, index, nth;

        member_before(hd(selected_file), visible_files, nonop =)
            -> point -> index;

        if where == -1 then
            ;;; add before the current item i.e. after point
            ;;;
            if index == 0 then
                ;;; add at front
                ;;;
                fstring :: visible_files -> visible_files;
                fname :: proj_files -> proj_files;
            else
                ;;; add before point
                nthpair(index, visible_files) -> nth;
                conspair(fstring, tl(nth)) -> tl(nth);
                nthpair(index, proj_files) -> nth;
                conspair(fname, tl(nth)) -> tl(nth);
            endif;

        else
            ;;; add after the current item
            ;;;
            if index == 0 then
                nthpair(index, visible_files) -> nth;
                conspair(fstring, tl(nth)) -> tl(nth);
                nthpair(index, proj_files) -> nth;
                conspair(fname, tl(nth)) -> tl(nth);
            else
                nthpair(index, visible_files) -> nth;
                conspair(fstring, tl(tl(nth))) -> tl(tl(nth));
                nthpair(index, proj_files) -> nth;
                conspair(fname, tl(tl(nth))) -> tl(tl(nth));
            endif;
        endif;
    endif;

    ;;; update the project structure and the project tool
    ;;;
    proj_files -> current_project.Proj_files_list;
    listlength(proj_files) -> current_project.Proj_files_count;

    p_PROJECTTOOL_OP(visible_files, projecttool_win, PROJ_WIN_SETLIST);
    OkayMsg();
enddefine;


define lconstant AddFile(where);
    lvars where;

    lvars fname = ChooseFile(false, 'Poplog: Add To Project', 'Add', false, true);
    returnunless(fname);

    gen_project_relpath(current_project, fname) -> fname;

    ;;; if this file has already been added, don't add it again
    ;;; and print a warning
    ;;;
    lvars ftable = current_project.Proj_files_table;

    if ftable(fname) then
        lvars shell = p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL);
        pop_ui_message(
            sprintf(fname, 'The file:\n\n    %S\n\nis already listed in the project'),
            false, shell, PRJ_MESSAGETITLE);
        FailedMsg();
        return;
    endif;

    DoAddFile(fname, where, false, false, false);
enddefine;


define lconstant AddFileAtEnd = AddFile(%0%);
enddefine;

define lconstant AddFileBefore = AddFile(%-1%);
enddefine;

define lconstant AddFileAfter = AddFile(%1%);
enddefine;


;;; UpdateProjTool generates the description strings and updates the
;;; file display list
;;;
define lconstant UpdateProjTool();

    UpdateProjMsg();

    ;;; clear the current_project tool tables
    ;;;
    clearproperty(project_file_to_string);
    clearproperty(project_string_to_file);

    ;;; generate the description strings and then set the tool file list
    ;;;
    gen_projtool_tables(current_project, project_file_to_string, project_string_to_file);

    lvars l;

    p_PROJECTTOOL_OP(
        gen_proj_desc_list(current_project, project_file_to_string) ->> l,
        projecttool_win, PROJ_WIN_SETLIST);

    ;;; set the sensitivity of various menu options
    ;;;
    listlength(l) -> current_project.Proj_files_count;
    sys_grbg_list(l);

    OkayMsg();
enddefine;


;;; UpdateProjFile takes the current name of file, a file descriptor
;;; (which may have been modified) and the project tool widget structure.
;;;
;;;
define lconstant UpdateProjFile(oldname, newname, f);
lvars oldname, newname, f;

    lvars fstring = project_file_to_string(f);
    lvars flist = current_project.Proj_files_list;
    lvars ftable = current_project.Proj_files_table;

    lvars point, index;

    member_before(oldname or f.File_name, flist, nonop =) -> point -> index;

    ;;; just need to know the index of the item since we're replacing
    ;;; rather than adding
    ;;;
    index + 1 -> index;

    remove_from_projtool_tables(f,
        project_file_to_string, project_string_to_file) ->;

    ;;; if the name has the name changed, replace the old name in the
    ;;; project file list, the file record and the file table
    ;;;
    if oldname and oldname /= newname then
        false -> ftable(oldname);
        newname ->> subscrl(index, flist) -> f.File_name;
        f -> ftable(newname);
    endif;

    lvars vprefs = current_project.Proj_view_prefs;

    gen_file_description(current_project, f,
            vprefs.ProjVP_timeformat,
            vprefs.ProjVP_dateformat,
            vprefs.ProjVP_fileprefs) -> fstring;

    add_to_projtool_tables(fstring, f,
        project_file_to_string, project_string_to_file);

    ;;; Now update the file list the file window (if present)
    ;;;
    p_PROJECTTOOL_OP(fstring, index, projecttool_win, PROJ_WIN_REPLACEITEM);
    update_project_file_sheet(f);
    OkayMsg();
enddefine;


/* ---------------------------------------------------------------
    Preferences Tool Actions On Existing Projects
   --------------------------------------------------------------- */

;;; used to ensure that any changes to the project preferences
;;; update the displayed files e.g. in terms of date/time formats,
;;; file display preferences etc. The last two flags passed in indicate
;;; whether anything about the current sheet was changed and the default
;;; value of the popdown flag to be returned by the procedure is
;;;
define lconstant do_props_update(project, sheet, changed, popdown) -> popdown;
lvars project sheet changed popdown;

    ;;; only update if the user has changed something
    ;;;
    if changed then
        switchon sheet
            case = PRJ_DESC_LABEL then
                p_PROJECTTOOL_OP(
                    project.Proj_desc_data.ProjD_name,
                    projecttool_win, PROJ_WIN_SETTITLE);
            case = PRJ_AUTO_LABEL then
                UpdateProjTool();
            case = PRJ_VIEW_LABEL then
                UpdateProjTool();
        endswitchon;
    endif;
enddefine;


define lconstant do_gen_close(popdown) -> popdown;
lvars popdown;
    erasenum(3);
enddefine;


define lconstant close_ved_file(fbuff, prompt) -> continue;
lvars fbuff, prompt, continue = true;

dlocal ved_current_file;
dlocal pop_ui_promptsource;

    fbuff -> ved_current_file;
    if vedwriteable and vedchanged then
        if prompt then
            lvars ans;
            vededit(fbuff);
            wvedwindow -> pop_ui_promptsource;
            pop_ui_prompttool('Project:Warning', "question",
                'File changed.\n Do you wish to save your edits',
                true, [Yes No Cancel], 1) -> (ans,);
            if ans == "Yes" then
                ved_wq();
            elseif ans == "Cancel" then
                false -> continue;
            else
                ved_rrq();
            endif;
        else
            ved_wq();
        endif;
    else
        ved_q();
    endif;
enddefine;


define lconstant close_project_edit_windows(check) -> continue ;
lvars check, continue = true, files = current_project.Proj_files_list;
lvars save_all = (current_project.Proj_save_prefs.ProjSV_ed_savewhat =
                    PRJ_ALL_EDFILES_LABEL);

    if vedsetupdone then
        vedappfiles(
            procedure;
                if save_all
                or member(gen_project_relpath(current_project, vedcurrent),
                    files)
                then
                    ClosingMsg(vedcurrent);
                    if check then
                        unless close_ved_file(ved_current_file, check) then
                            false -> continue;
                            exitto(close_project_edit_windows);
                        endunless;
                    else
                        close_ved_file(ved_current_file, check) ->;
                    endif;
                endif;
            endprocedure);
    endif;

    if continue then
        OkayMsg();
    else
        CancelledMsg();
    endif;
enddefine;


/* ---------------------------------------------------------------
    Various Menu Actions
   --------------------------------------------------------------- */

define lconstant OpenFile(f) -> continue; lvars f, continue = false;
    if (project_string_to_file(f) ->> f) then
        if is_filetype_editable(f) then
            OpeningMsg(f.File_name);

            lvars fullname = gen_project_abspath(current_project, f.File_name);

            returnunless(check_project_path(fullname));
            open_ved_window(get_project_basedir(current_project),
                is_filetype_poplog(f), false, f.File_name, true);
            OkayMsg();
            true -> continue;
        else
            pop_ui_message(
                sprintf(f.File_name, 'Sorry, don\'t know how to edit\n\n    %S\n'),
                false, p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL),
                PRJ_MESSAGETITLE);
            FailedMsg();
        endif;
    endif;
enddefine;

define lconstant ViewFile(f) -> continue; lvars f, continue = false;
    if (project_string_to_file(f) ->> f) then
        if is_filetype_viewable(f) then
            OpeningMsg(f.File_name);

            lvars fullname = gen_project_abspath(current_project, f.File_name);

            returnunless(check_project_path(fullname));
            open_ved_window(get_project_basedir(current_project),
                is_filetype_poplog(f), false, f.File_name, false);
            OkayMsg();
            true -> continue;
        else
            pop_ui_message(
                sprintf(f.File_name, 'Sorry, don\'t know how to view\n\n    %S\n'),
                false, p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL),
                PRJ_MESSAGETITLE);
            FailedMsg();
        endif;
    endif;
enddefine;


define lconstant SaveFile(f) -> continue; lvars f, continue = true, buff;

    define lconstant save_ved_file(fbuff); lvars fbuff;
    dlocal ved_current_file;
        fbuff -> ved_current_file;
        if vedwriteable and vedchanged then
            SavingMsg(f.File_name);
            ;;; ved_w1();
            vedinput(ved_w1);
            vedprocess_try_input();
            OkayMsg();
        endif;
    enddefine;

    if (project_string_to_file(f) ->> f) then
        if vedsetupdone and (projfile_vedpresent(f.File_name) ->> buff) then
            save_ved_file(buff);
        endif;
    endif;
enddefine;


define lconstant CloseFile(f) -> continue; lvars f, continue = true, buff;

    if (project_string_to_file(f) ->> f) then
        if vedsetupdone and (projfile_vedpresent(f.File_name) ->> buff) then
            ClosingMsg(f.File_name);
            close_ved_file(buff, true) -> continue;
            if continue then
                OkayMsg();
            else
                CancelledMsg();
            endif;
        endif;
    endif;
enddefine;


define lconstant RenameFile(f) -> continue; lvars f, continue = false;

    define lconstant rename_ved_file(fbuff, fname); lvars fbuff, fname;
    dlocal current_directory = get_project_basedir(current_project);
    dlocal ved_current_file, vedargument;
        fbuff -> ved_current_file;
        fname -> vedargument;
        ved_name();
    enddefine;

    if (project_string_to_file(f) ->> f) then

        lvars fname = ChooseFile(false, 'Poplog: Rename File', 'Rename',
                        nullstring, true);
        returnunless(fname);

        lvars shell = p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL);
        lvars relname = gen_project_relpath(current_project, fname);

        if member(relname, current_project.Proj_files_list) then
            pop_ui_message(
                sprintf(relname,
'Can\'t rename: %S is in the project already.\n\
Please remove it from the project or\nchoose an alternative name'),
                false, shell, PRJ_MESSAGETITLE);
            FailedMsg();
        else
            returnunless(check_project_path(fname));
            if sys_file_exists(fname) then

                if pop_ui_confirm(
                    sprintf(fname, 'The file:\n    %S\nalready exists.\n\
OK to replace?'),
                        [Replace Cancel], 1, false, shell) /== 1 then
                    CancelledMsg();
                    return;
                endif;
            endif;
            RenamingMsg(f.File_name);

            lvars fbuff,
                oldname = f.File_name,
                fulloldname = gen_project_abspath(current_project, f.File_name);

            ;;; make a copy of the old file to the new file...
            if sys_file_exists(fulloldname) then
                sys_file_copy(fulloldname, fname);
            else
                pop_ui_message(sprintf(fulloldname,
                    'Can\'t copy existing file\n\n    %S'), false,
                    shell, PRJ_MESSAGETITLE);
            endif;

            if vedsetupdone and (projfile_vedpresent(oldname) ->> fbuff) then

                ;;; If a Ved buffer exists for the file, rename it
                ;;; provided the new name is not already in Ved
                ;;;
                if projfile_vedpresent(relname) then
                    pop_ui_message(sprintf(relname,
                        'Can\'t rename: %S is in Ved already'), false,
                        shell, PRJ_ERRORTITLE);
                    InterruptedMsg();
                    return;
                else
                    rename_ved_file(fbuff, relname);
                endif;
            endif;

            UpdateProjFile(oldname, relname, f);
            update_project_file_sheet(f);
            OkayMsg();
            true -> continue;
        endif;
    endif;
enddefine;


;;; checks to see whether the file is currently being edited in which
;;; case it compiles that, otherwise compile from disk
;;;
define lconstant ss_compile_ved_or_file(file, ss) -> continue;
lvars file ss continue = false, buff;

    define lconstant ss_compile;
        subsystem_compile(file, ss);
    enddefine;

    define lconstant compile_file;
        returnunless(check_project_path(file));
        ProjectProtect(ss_compile, InterruptedMsg);
        OkayMsg();
        true -> continue;
    enddefine;

    define lconstant compile_ved_buffer(fbuff); lvars fbuff, rep;
        buff -> ved_current_file;
        vedrangerepeater(1, vvedbuffersize) -> rep;
        file -> pdprops(rep);
        rep -> file;
        ProjectProtect(ss_compile, InterruptedMsg);
        OkayMsg();
        true -> continue;
    enddefine;

    if vedsetupdone and (projfile_vedpresent(file) ->> buff) then
        compile_ved_buffer(buff);
    else
        gen_project_abspath(current_project, file) -> file;
        compile_file();
    endif;
enddefine;


define lconstant CompileFile(f) -> continue;
lvars f, check = false, continue = false;

    if isboolean(f) then
        ;;; been passed the "check" flag
        f -> (f, check);
    endif;

lvars oldst = stacklength();

    if (project_string_to_file(f) ->> f) and
            is_filetype_compilable(f) then

        lvars ss = is_filetype_poplog(f);

        if ss then
            if is_subsystem_loaded(ss) then
                if check and f.File_category == UTILITY_LABEL then
                    true -> continue
                else
                    CompilingMsg(f.File_name);
                    ss_compile_ved_or_file(f.File_name, ss) -> continue;
                endif;
            endif;
        /*
        ;;; at some point, will allow users to specifiy how to compile
        ;;; a file e.g. a sysobey command such as 'cc' etc. For the time
        ;;; being, do nothing
        ;;;
        else
            pop_ui_message(
                sprintf(f.File_name, 'Sorry, don\'t know how to compile\n\n    %S\n'),
                false, p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL),
                PRJ_MESSAGETITLE);

            FailedMsg();
            false -> continue;
        */
        endif;


        ;;; check whether the stack length has changed
        ;;;
        lvars newst = stacklength();

        if newst < oldst then
            mishap(f.File_name, 1, 'ITEMS REMOVED FROM USERSTACK DURING COMPILATION');
        else
            ;;; print and remove any items left behind
            ;;;
            repeat newst - oldst times
                pr(); nl(1);
            endrepeat;
        endif;
    else
        true -> continue;
    endif;
enddefine;


define lconstant RunFile(f) -> continue; lvars f, continue = false;
lvars ftype = false;

    define lconstant pr_cant_exec;
        pop_ui_message(
            sprintf(f.File_name, 'Sorry, can\'t run file:\n\n    %S'),
            false, p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL),
            PRJ_MESSAGETITLE);
    enddefine;


    if (project_string_to_file(f) ->> f) and
            is_filetype_executable(f) then

        lvars fname = gen_project_abspath(current_project, f.File_name);

        returnunless(check_project_path(fname));

#_IF DEF VMS
        switchon f.File_contents
            case = SH_LABEL then
                pr_cant_exec();
            case = CSH_LABEL then
                pr_cant_exec();
            case = DCL_LABEL then
                '@' -> ftype;
            else
                'r ' -> ftype;
        endswitchon;
        if ftype then
            RunningMsg(f.File_name);
            sysobey(sprintf(fname, ftype, '%S%S'));
            OkayMsg();
            true -> continue;
        endif;
#_ELSE
        switchon f.File_contents
            case = SH_LABEL then
                `$` -> ftype;
            case = CSH_LABEL then
                `%` -> ftype;
            case = DCL_LABEL then
                pr_cant_exec();
            else
                `!` -> ftype;
        endswitchon;
        if ftype then
            RunningMsg(f.File_name);
            sysobey(sprintf(fname, '%S'), ftype);
            OkayMsg();
            true -> continue;
        endif;
#_ENDIF
    else
        ;;; not runnable so just ignore
        true -> continue;
    endif;
enddefine;


define lconstant ShowFileInfo(f, c) -> continue; lvars c, f, continue = true;
    if (project_string_to_file(f) ->> f) then
        show_project_file_sheet(f, p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL), c);
    endif;
enddefine;



;;; generic procedure for applying procedures to single file
;;;
define lconstant DoSingleFile(fseln, op, allow_cancel, cdroot);
lvars fseln, op, allow_cancel, cdroot;
dlocal XptBusyCursorOn = true;
dlocal CancelEnabled = allow_cancel;

    define lconstant do_single;
    dlocal current_directory;

        if cdroot then
            get_project_basedir(current_project) -> current_directory;
        endif;

        op(fseln)->;
        OkayMsg();
    enddefine;

    ProjectProtect(do_single, InterruptedMsg);
enddefine;


;;; generic procedure for applying procedures to selected files
;;;
define lconstant DoSelectedFiles(op, pass_count, allow_cancel, cdroot);
lvars op, pass_count, allow_cancel, cdroot;
dlocal XptBusyCursorOn = true;
dlocal CancelEnabled = allow_cancel;

lvars
    selected_files = p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSELECTION);

    define lconstant do_list; lvars file count = 1;
    dlocal current_directory;

        if cdroot then
            get_project_basedir(current_project) -> current_directory;
        endif;

        for file in selected_files do
            returnunless(op(file,
                if pass_count then
                    count
                endif));
            count + 1 -> count;
        endfor;
        OkayMsg();
    enddefine;

    ProjectProtect(do_list, InterruptedMsg);
    sys_grbg_list(selected_files);
enddefine;

define lconstant OpenSelectedFiles
    = DoSelectedFiles(%OpenFile, false, false, true%);
enddefine;

define lconstant ViewSelectedFiles
    = DoSelectedFiles(%ViewFile, false, false, true%);
enddefine;

define lconstant SaveSelectedFiles
    = DoSelectedFiles(%SaveFile, false, true, true%);
enddefine;

define lconstant CloseSelectedFiles
    = DoSelectedFiles(%CloseFile, false, false, true%);
enddefine;

define lconstant CompileSelectedFiles
    = DoSelectedFiles(%CompileFile, false, true, false%);
enddefine;

define lconstant RunSelectedFiles
    = DoSelectedFiles(%RunFile, false, false, false%);
enddefine;

define lconstant FileInfoSelectedFiles
    = DoSelectedFiles(%ShowFileInfo, true, true, false%);
enddefine;

define lconstant RenameSelectedFiles
    = DoSelectedFiles(%RenameFile, false, false, true%);
enddefine;


;;; generic procedure for applying procedures to all files
;;;
define lconstant DoAllFiles(op, allow_cancel, cdroot);
dlocal XptBusyCursorOn = true;
lvars op, allow_cancel, cdroot;
dlocal CancelEnabled = allow_cancel;

lvars all_files = p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETLIST);

    define lconstant do_list; lvars file;
    dlocal current_directory;

        if cdroot then
            get_project_basedir(current_project) -> current_directory;
        endif;

        for file in all_files do
            returnunless(op(file));
        endfor;
        OkayMsg();
    enddefine;

    ProjectProtect(do_list, InterruptedMsg);
    sys_grbg_list(all_files);
enddefine;


define lconstant OpenAllFiles = DoAllFiles(%OpenFile, true, true%);
enddefine;

define lconstant ViewAllFiles = DoAllFiles(%ViewFile, true, true%);
enddefine;


define lconstant CompileAllFiles();
dlocal XptBusyCursorOn = true;
dlocal CancelEnabled = true;

lvars all_files = p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETLIST);

    define lconstant do_list; lvars file;
    dlocal current_directory;

        for file in all_files do
            returnunless(CompileFile(file, true));
        endfor;

        OkayMsg();
    enddefine;

    ProjectProtect(do_list, InterruptedMsg);
    sys_grbg_list(all_files);
enddefine;


;;; don't need to change to project root directory for saving and
;;; closing editor files
;;;
define lconstant SaveAllFiles = DoAllFiles(%SaveFile, false, false%);
enddefine;

define lconstant CloseAllFiles = DoAllFiles(%CloseFile, false, false%);
enddefine;

lconstant macro (
    REM_OPT = 1,
    CLOSEREM_OPT = 2,
    CANCEL_OPT = 3,
);

define lconstant RemoveFiles(op);
lconstant Rem = 'Remove', SaveRem = 'Close & Remove', Canc = 'Cancel',
    EditingOpts = [^Rem ^SaveRem ^Canc], DefaultOpts = [^Rem ^Canc];

lconstant
    no_open_files = 'Okay to remove file(s) from the project?\n\
Press "Remove" to remove files from the project.\
Press "Cancel" if you don\'t want to remove the files.',

    has_open_files = 'Okay to remove file(s) from the project?\n\
Press "Remove" to remove files from the project.\
Press "Close & Remove" to also save and close files being edited.\
Press "Cancel" if you don\'t want to remove the files.';

lvars op, res, Opts, cancel_op;

    if vedbufferlist /== [] then
        lvars files = p_PROJECTTOOL_OP(projecttool_win, op);
        if closeable_selection(files) then
            sys_grbg_list(files);
            EditingOpts, 3
        else
            sys_grbg_list(files);
            DefaultOpts, 2
        endif
    else
        DefaultOpts, 2
    endif -> (Opts, cancel_op);

    if (pop_ui_confirm(
            if cancel_op == 2 then
                no_open_files
            else
                has_open_files
            endif,
            Opts, 1, false,
            p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL)) ->> res)
                /== cancel_op then

        lvars rfiles = p_PROJECTTOOL_OP(projecttool_win, op);

        lvars file;
        if res == CLOSEREM_OPT then
            for file in rfiles do
                unless CloseFile(file) then
                    return;
                endunless;
            endfor;
        endif;

        if op == PROJ_WIN_GETSELECTION then
            lvars file, flabel, fstruct,
                pfiles = current_project.Proj_files_list,
                ptable = current_project.Proj_files_table;

            [% for file in pfiles do

                ;;; get the displayed description for this file
                ;;;
                project_file_to_string(ptable(file) ->> fstruct) -> flabel;

                if member(flabel, rfiles) then
                    RemovingMsg(file);
                    ;;; delete it and any associated windows
                    destroy_project_file_sheet(fstruct);
                    false -> ptable(file);
                else
                    file
                endif;
            endfor %] -> current_project.Proj_files_list;

            listlength(current_project.Proj_files_list)
                -> current_project.Proj_files_count;

            sys_grbg_list(pfiles);

        else
            ;;; remove all files
            RemovingAllMsg();
            clearproperty(current_project.Proj_files_table);
            sys_grbg_list(current_project.Proj_files_list);
            close_all_project_file_sheets();

            [] -> current_project.Proj_files_list;
            0 -> current_project.Proj_files_count;
        endif;
        OkayMsg();
    else
        CancelledMsg();
    endif;
    UpdateProjTool();
enddefine;


define lconstant RemoveSelectedFiles = RemoveFiles(%PROJ_WIN_GETSELECTION%);
enddefine;

define lconstant RemoveAllFiles = RemoveFiles(%PROJ_WIN_GETLIST%);
enddefine;


define lconstant ShowHelpTool();
dlocal XptBusyCursorOn = true;
    StartingMsg('Help Tool');
    pop_ui_helptool(false, false, false, undef,
        p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL)) ->;
    OkayMsg();
enddefine;

define lconstant ShowPrefsTool();
dlocal XptBusyCursorOn = true;
    StartingMsg('Preferences Tool');
    pop_ui_projectprefstool(false, do_props_update, false,
                do_props_update, false, false);
    OkayMsg();
enddefine;


define lconstant ShowMkimageTool();
dlocal XptBusyCursorOn = true;
    StartingMsg('Image Tool');
    pop_ui_projectmkimagetool(false, false, false,
                false, do_gen_close, false);
    OkayMsg();
enddefine;


define lconstant AboutProject();
dlocal XptBusyCursorOn = true;

    lvars
        n_files = current_project.Proj_files_count,
        text = consstring(#|
            explode(current_project.Proj_desc_data.ProjD_desc);
            explode('\n\nCreated by ');
            explode(current_project.Proj_create_data.ProjC_creator);
            explode('\nat ');
            gen_date_chars(
                    current_project.Proj_create_data.ProjC_createtime,
                    current_project.Proj_view_prefs.ProjVP_timeformat,
                    current_project.Proj_view_prefs.ProjVP_dateformat,
                    ', ');
            explode('\n\nBase directory ');
            explode(get_project_basedir(current_project));
            explode('\n\nThe project contains ');
            explode(current_project.Proj_files_count sys_>< nullstring);
            explode(' file');
            if n_files /== 1 then `s` endif |#);

    pop_ui_information(text, false,
        p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL),
        'About ' sys_>< current_project.Proj_desc_data.ProjD_name)->;
    OkayMsg();
enddefine;


define lconstant AboutProjectTool();
    call_ved('help pop_ui_projecttool');
enddefine;


    ;;; prompt for the name of a project file: we want to distinguish
    ;;; this from the standard file-chooser, so we create a dummy shell
    ;;; as its parent to get a new selection box
lvars chooser_shell = false;
define lconstant ChooseProject(title, label, file, flags);
    lvars title, label, file, flags;
    dlocal pop_ui_file_search_pattern = '*.prj';
    unless XptIsLiveType(chooser_shell, "Widget") then
        XtVaCreatePopupShell(nullstring, xtTopLevelShellWidget,
            pop_ui_app_shell, (#|
                XtN width,              1,
                XtN height,             1,
                XtN mappedWhenManaged,  false,
            |#)) -> chooser_shell;
        XtRealizeWidget(chooser_shell);
        ;;; centering the shell should center the dialog too, at least
        ;;; with Motif
        XptCenterWidgetOn(chooser_shell, "screen");
    endunless;
    ChooseFile(chooser_shell, title, label, file, flags);
enddefine;


define DoOpenProject(proj); lvars proj;
    if current_project then
        pop_ui_save_project();
        if close_project_edit_windows(true) then
            close_project_subwindows();
        else
            return;
        endif;
    endif;
    proj -> pop_ui_current_project;
    pop_ui_projecttool();
enddefine;


define lconstant OpenProject();
    OpenProjMsg();
    lvars name = ChooseProject('Poplog: Open Project', 'Open', false, false);
    if name then
        lvars proj = pop_ui_read_project(name);

        if proj then
            DoOpenProject(proj);
        else
            lvars shell = projecttool_win and
                p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL) or
                pop_ui_app_shell;
            pop_ui_message(sprintf(name,
                        'Can\'t read project file\n\n    %S'), false,
                        shell, PRJ_ERRORTITLE);
            FailedMsg();
        endif;
    else
        CancelledMsg();
    endif;
enddefine;


define lconstant RenameProject();
    RenameProjMsg();
    lvars shell,
        name = pop_ui_text_input('Poplog: Rename Project',
        'New name', false, check_project_name,
        p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL) ->> shell);

    if name then
        lvars file = gen_project_abspath(current_project,
                        sys_fname_nam(name) sys_>< '.prj');

        if sys_file_exists(file) then
            if pop_ui_confirm(
                sprintf(get_project_basedir(current_project),
                    name, 'The project "%S" already exists in the\
directory %S\n\
OK to replace ?'),
                [Replace Cancel], 1, false, shell) /== 1 then
                CancelledMsg();
                return;
            endif;
        endif;

        name -> current_project.Proj_desc_data.ProjD_name;
        p_PROJECTTOOL_OP(current_project.Proj_desc_data.ProjD_name,
                projecttool_win, PROJ_WIN_SETTITLE);
        OkayMsg();
    else
        CancelledMsg();
    endif;
enddefine;


/*
define lconstant SaveAsProject();
    SaveAsProjMsg();
    lvars
        shell = p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL),
        file = ChooseProject('Poplog: Save Project', 'Save', nullstring, true);
    if file then
        lvars
            name = sys_fname_nam(file),
            dir = sys_dir_name(sys_fname_path(file)) or current_directory,
            extn = sys_fname_extn(file),
            ;

        if extn == nullstring then
            sprintf(file, '%S.prj') -> file;
        elseif extn /= '.prj' then
            ;;; user has given a dodgy extension so replace it with .prj
            sprintf(sys_fname(file, 1, 4), '%S.prj') -> file;
        endif;

        if sys_file_exists(file) then
            if pop_ui_confirm(
                sprintf(name, 'The project "%S" already exists in the selected directory.\n\
OK to overwrite ?'),
                [Overwrite Cancel], 1, false, shell) /== 1 then
                CancelledMsg();
                return;
            endif;
        endif;

        name -> current_project.Proj_desc_data.ProjD_name;
        dir -> current_project.Proj_desc_data.ProjD_dir;

        p_PROJECTTOOL_OP(name, projecttool_win, PROJ_WIN_SETTITLE);
        update_project_subwindows();
        OkayMsg();
    else
        CancelledMsg();
    endif;
enddefine;
*/


define lconstant SelectAllFiles(); lvars l;
    p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_SELECTALL);
enddefine;


define lconstant DeselectAllFiles();
    p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_DESELECTALL);
enddefine;


/* ---------------------------------------------------------------
    Building Saved Images For A Project
   --------------------------------------------------------------- */

define lconstant BuildImage;
#_IF DEF VMS
    lconstant
        script_type = DCL_LABEL,
        OKAY_STATUS = 1;
#_ELSE
    lconstant
        script_type = SH_LABEL,
        OKAY_STATUS = 0;
#_ENDIF

dlocal XptBusyCursorOn = true;

    lvars command, loaderfile;

    gen_build_image_command(current_project, script_type)
        -> (loaderfile, command);

    BuildingMsg(current_project.Proj_image_prefs.ProjIM_imagename);

    sysobey(command);

    unless pop_status == OKAY_STATUS then
        pop_ui_message('The image build has failed', false,
                p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL),
                PRJ_ERRORTITLE);
        FailedMsg();
    else
        OkayMsg();
    endunless;

#_IF DEF UNIX
    sysunlink(loaderfile)->;
#_ELSE
    sysdelete(loaderfile)->;
#_ENDIF

    UpdateProjTool();
enddefine;


;;; generates command scripts for the user
;;;
define lconstant GenerateScripts;
lvars
    sprefs = current_project.Proj_script_prefs;

#_IF DEF VMS
    lconstant script_type = DCL_LABEL;
#_ELSE
    lconstant script_type = SH_LABEL;
#_ENDIF

dlocal XptBusyCursorOn = true;

    define lconstant check_add(do_action, file, category, type, desc);
    lvars do_action, file, category, type, desc;

        if do_action then
            ;;; add the file to the project
            DoAddFile(file, 0, category, type, desc);
        endif;
    enddefine;

    define lconstant do_generate;
        ;;; Check the loaderfile and the compile script
        ;;;
        if sprefs.ProjGS_genloaderfile then
            GeneratingMsg(gen_project_abspath(current_project,
                                sprefs.ProjGS_loaderfile));
            gen_loader_file(current_project, script_type,
                gen_project_abspath(current_project, sprefs.ProjGS_loaderfile));

            check_add(sprefs.ProjGS_addloaderfile, sprefs.ProjGS_loaderfile,
                    UTILITY_LABEL, POP11SRC_LABEL,
                    'loads project files and sets runtime params');
        endif;

        lvars mkimagecom = gen_project_abspath(current_project,
                            sprefs.ProjGS_mkscript);

        if sprefs.ProjGS_genmkscript then
            GeneratingMsg(mkimagecom);
            gen_build_image_script(current_project, script_type,
                gen_project_abspath(current_project, sprefs.ProjGS_mkscript));

            check_add(sprefs.ProjGS_addmkscript, sprefs.ProjGS_mkscript,
                    SCRIPT_LABEL, script_type, 'builds the saved image');
        endif;

        UpdateProjTool();
    enddefine;

    ProjectProtect(do_generate, InterruptedMsg);
enddefine;


/* ---------------------------------------------------------------
    Menu Callbacks
   --------------------------------------------------------------- */

define lconstant ProjDefer(p); lvars p;
    XptDeferApply(p);
    XptSetXtWakeup();
enddefine;


define lconstant projmenu_cb(w, client, call);
    lvars w, client, call;

    go_on client to NEW OPEN SAVE SAVEAS PREFS RENAME CLOSE;

    NEW:
        ProjDefer(pop_ui_new_project);
        return;

    OPEN:
        ProjDefer(OpenProject);
        return;

    SAVE:
        ProjDefer(pop_ui_save_project);
        return;

    SAVEAS:
        ;;; ProjDefer(SaveAsProject);
        return;

    PREFS:
        ProjDefer(ShowPrefsTool);
        return;

    RENAME:
        ProjDefer(RenameProject);
        return;

    CLOSE:
        ProjDefer(pop_ui_close_project);
        return;
enddefine;


define lconstant selectmenu_cb(w, client, call);
    lvars w, client, call;

    go_on client to OPEN VIEW SAVE CLOSE COMPILE RUN FILEINFO;

    OPEN:
        ProjDefer(OpenSelectedFiles);
        return;

    VIEW:
        ProjDefer(ViewSelectedFiles);
        return;

    SAVE:
        ProjDefer(SaveSelectedFiles);
        return;

    CLOSE:
        ProjDefer(CloseSelectedFiles);
        return;

    COMPILE:
        ProjDefer(CompileSelectedFiles);
        return;

    RUN:
        ProjDefer(RunSelectedFiles);
        return;

    FILEINFO:
        ProjDefer(FileInfoSelectedFiles);
        return;

enddefine;


define lconstant editmenu_cb(w, client, call);
    lvars w, client, call;

    go_on client to ADD ADDAFTER ADDBEFORE RENAME DELFILE DELALL REFRESH
        SELECTALL DESELECTALL;

    ADD:
        ProjDefer(AddFileAtEnd);
        return;

    ADDAFTER:
        ProjDefer(AddFileAfter);
        return;

    ADDBEFORE:
        ProjDefer(AddFileBefore);
        return;

    RENAME:
        ProjDefer(RenameSelectedFiles);
        return;

    DELFILE:
        ProjDefer(RemoveSelectedFiles);
        return;

    DELALL:
        ProjDefer(RemoveAllFiles);
        return;

    REFRESH:
        ProjDefer(UpdateProjTool);
        return;

    SELECTALL:
        ProjDefer(SelectAllFiles);
        return;

    DESELECTALL:
        ProjDefer(DeselectAllFiles);
        return;
enddefine;


define lconstant buildmenu_cb(w, client, call);
    lvars w, client, call;

    go_on client to COMPILE COMPILECHANGED COMPILEALL MKIMAGE GENSCRIPTS IMAGETOOL;

    COMPILE:
        return;

    COMPILECHANGED:
        return;

    COMPILEALL:
        ProjDefer(CompileAllFiles);
        return;

    MKIMAGE:
        ProjDefer(BuildImage);
        return;

    GENSCRIPTS:
        ProjDefer(GenerateScripts);
        return;

    IMAGETOOL:
        ProjDefer(ShowMkimageTool);
        return;
enddefine;


define lconstant helpmenu_cb(w, client, call);
    lvars w, client, call;

    go_on client to PROJECT TOOL POPLOG HELPTOOL;

    PROJECT:
        ProjDefer(AboutProject);
        return;

    TOOL:
        ProjDefer(AboutProjectTool);
        return;

    POPLOG:
        pop_ui_logo(false);
        return;

    HELPTOOL:
        ProjDefer(ShowHelpTool);
        return;
enddefine;



/* ---------------------------------------------------------------
    Menu Definitions
   --------------------------------------------------------------- */

lconstant
    ;;; button name         label           default     callback num

    project_menu_buttons = {
        'Project'
        ^projMenu
        ^projmenu_cb
        {^(projMenu <> 'newButton') 'New...'        ^true       ^PRJPROJ_CB_NEW}
        {^(projMenu <> 'openButton') 'Open...'      ^false      ^PRJPROJ_CB_OPEN}
        {^(projMenu <> 'saveButton') 'Save'         ^false      ^PRJPROJ_CB_SAVE}
        ;;; {^(projMenu <> 'saveAsButton') 'Save As...'         ^false      ^PRJPROJ_CB_SAVEAS}
        ^menuSeparator
        {^(projMenu <> 'preferencesButton') 'Preferences...'    ^false      ^PRJPROJ_CB_PREFS}
        {^(projMenu <> 'renameButton') 'Rename...'  ^false      ^PRJPROJ_CB_RENAME}
        ^menuSeparator
        {^(projMenu <> 'closeButton') 'Close'       ^false      ^PRJPROJ_CB_CLOSE}
    },

    projselect_menu_buttons = {
        'Selected'
        ^selectMenu
        ^selectmenu_cb
        {^selnOpenName      'Open'          ^true       ^PRJSELN_CB_OPEN}
        {^selnViewName      'View'          ^false      ^PRJSELN_CB_VIEW}
        {^selnSaveName      'Save'          ^false      ^PRJSELN_CB_SAVE}
        {^selnCloseName     'Close'         ^false      ^PRJSELN_CB_CLOSE}
        ^menuSeparator
        {^selnCompileName   'Compile'       ^false      ^PRJSELN_CB_COMPILE}
        {^selnRunName       'Run'           ^false      ^PRJSELN_CB_RUN}
        ^menuSeparator
        {^selnFileInfoName  'File Info...'      ^false  ^PRJSELN_CB_FILEINFO}
    },

    projedit_menu_buttons = {
        'Edit'
        ^editMenu
        ^editmenu_cb
        {^editAddName       'Add'           ^true       ^PRJEDIT_CB_ADD}
        {^editAddAfterName  'Add After'     ^false      ^PRJEDIT_CB_ADDAFTER}
        {^editAddBeforeName 'Add Before'    ^false      ^PRJEDIT_CB_ADDBEFORE}
        {^editRenameName    'Rename'        ^false      ^PRJEDIT_CB_RENAME}
        {^editRemoveName    'Remove'        ^false      ^PRJEDIT_CB_REMFILE}
        ^menuSeparator
        {^editRefreshName   'Refresh'       ^false      ^PRJEDIT_CB_REFRESH}
    },

    projedit_menu_extras = {
        ;;; Motif only: OLIT doesn't have a good enough API to implement
        ;;; these reliably...
        ^menuSeparator
        {^editSelectAllName 'Select All'    ^false      ^PRJEDIT_CB_SELECTALL}
        {^editDeselectAllName 'Deselect All'    ^false  ^PRJEDIT_CB_DESELECTALL}
    },

    projbuild_menu_buttons = {
        'Build'
        ^buildMenu
        ^buildmenu_cb
        {^buildCompileAllName   'Compile All'   ^false      ^PRJBUILD_CB_COMPILEALL}
        {^buildSavedImageName   'Make Image'   ^false      ^PRJBUILD_CB_MKIMAGE}
        {^buildImageScriptsName 'Make Scripts'   ^false  ^PRJBUILD_CB_GENSCRIPTS}
        ^menuSeparator
        {^buildImageToolName  'Image Tool...'    ^false     ^PRJBUILD_CB_IMAGETOOL}
    },

    projhelp_menu_buttons = {
        'Help'
        ^helpMenu
        ^helpmenu_cb
        {^(helpMenu <> 'aboutProjectButton') 'About Project'    ^true       ^PRJHELP_CB_PROJECT}
        {^(helpMenu <> 'aboutProjectToolButton') 'About Project Tool'   ^false       ^PRJHELP_CB_TOOL}
        {^(helpMenu <> 'aboutPoplogButton') 'About Poplog'      ^false      ^PRJHELP_CB_POPLOG}
        ^menuSeparator
        {^(helpMenu <> 'helpToolButton') 'Search...'         ^false      ^PRJHELP_CB_HELPTOOL}
    },
;


/* ---------------------------------------------------------------
    File List Callbacks
   --------------------------------------------------------------- */

;;; used to set the sensitivity of the menu options before a menu is
;;; popped up
;;;
define lconstant menupopup_cb;
    erasenum(3);
    SetMenuSensitivity();
enddefine;


;;; client is the operation to be performed, item is the most recently
;;; selected item and projtool is the specific projectool object
;;;
define lconstant fileseln_cb(client, item);
    lvars client, item;

    define lconstant DoOpen(item); lvars item;
        DoSingleFile(item, OpenFile, false, true);
    enddefine;

    go_on client to SELECT ACTION;

    SELECT:
        return;

    ACTION:
        ProjDefer(DoOpen(%item%));
        return;
enddefine;


/* ---------------------------------------------------------------
    Top-level Project Manipulation
   --------------------------------------------------------------- */

;;; New Project. takes name/path of new project and an option
;;;
define CreateProject(name) -> project;
lvars name, useproject = false, project;
dlocal XptBusyCursorOn = true;

    if isProject(name) then
        name -> (name, useproject);
    endif;

    check_string(name);
    initProject() -> project;        ;;; create Project structure
    initialise_new_project(project, name, useproject);  ;;; set up custom values
enddefine;


;;; Read Project
;;;
define pop_ui_read_project(filename) -> project;
    lvars filename, project;
    dlocal XptBusyCursorOn = true;
    check_string(filename);
    read_project_description(filename) -> project;
enddefine;


define lconstant save_project_wspace(report);
lvars procedure report, workspace = create_project_workspace();

    lvars fname = gen_wspace_filename(current_project);
    report(fname);
    write_project_workspace(workspace, fname);
    sys_grbg_list(workspace);
enddefine;


define lconstant restore_project_wspace();
lvars workspace =
    read_project_workspace(
        gen_wspace_filename(current_project));

    if workspace then
        restore_project_workspace(workspace);
        sys_grbg_list(workspace);
    endif;
enddefine;


define auto_save_project;

    define lconstant do_autosave();
        dlocal XptBusyCursorOn = true;
        AutosaveProjMsg();
        save_project(current_project, erase);
        OkayMsg();
    enddefine;

    if current_project then
        do_autosave();

        ;;; "manually" reset the timer - if an error occurs this code
        ;;; won't be reached which may be a good thing...
        ;;;
        if current_project.Proj_auto_prefs.ProjAP_autosave then
            current_project.Proj_auto_prefs.ProjAP_asavetime
                -> sys_timer(auto_save_project);
        endif;
    endif;
enddefine;


define auto_update_project;
dlocal XptBusyCursorOn;

    if current_project then
        true -> XptBusyCursorOn;
        UpdateProjTool();

        ;;; "manually" reset the timer - if an error occurs this code
        ;;; won't be reached which may be a good thing...
        ;;;
        if current_project.Proj_auto_prefs.ProjAP_autoupdate then
            current_project.Proj_auto_prefs.ProjAP_aupdatetime
                -> sys_timer(auto_update_project);
        endif;
    endif;
enddefine;


define global pop_ui_save_project();

    define lconstant do_save;
        dlocal XptBusyCursorOn = true;
        save_project(current_project, SavingMsg);
        save_project_wspace(SavingMsg);
        OkayMsg();
    enddefine;

    if current_project then
        ProjectProtect(do_save, InterruptedMsg);
    endif;
enddefine;

    ;;; what to do on exit from Poplog: save the current project, but
    ;;; with minimum interaction with the tool windows because we can't
    ;;; be sure what state things are in
define pop_ui_kill_project();
    if current_project then
        save_project(current_project, erase);
        save_project_wspace(erase);
        ;;; zap the current project so that it won't be saved again
        false -> current_project;
        ;;; if there's no current project there shouldn't be a project
        ;;; tool window; it will be destroyed if the exit completes
        ;;; normally but we'll pop it down now in case the exit is
        ;;; interrupted
        lvars shell = p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL);
        if XptIsLiveType(shell, "Widget") then
            XtPopdown(shell);
        endif;
    endif;
enddefine;

define pop_ui_projecttool();
lvars project;
dlocal XptBusyCursorOn = true;

    unless (pop_ui_current_project ->> project) then
        mishap(0, 'No current project');
    endunless;

    unless projecttool_win then
        p_PROJECTTOOL('(untitled)',
            project_menu_buttons,
            projselect_menu_buttons,
            projedit_menu_buttons,
            if testdef popxlink_motif then
                ;;; add Motif-only extras
                () <> projedit_menu_extras
            endif,
            projbuild_menu_buttons,
            projhelp_menu_buttons,
            menupopup_cb,
            fileseln_cb, PROJ_EV_SELECT, PROJ_EV_ACTION,
            pop_ui_close_project,
            UserCancel_cb,
            pop_ui_app_shell) -> projecttool_win;
        popexit <> pop_ui_kill_project -> popexit;
    endunless;

    ;;; setup the file description tables, set the files list
    ;;; and restore the workspace as needed
    ;;;
    UpdateProjTool();
    p_PROJECTTOOL_OP(project.Proj_desc_data.ProjD_name, projecttool_win,
        PROJ_WIN_SETTITLE);
    p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_DISPLAY);
    restore_project_wspace();

    ;;; enable auto-saving and auto-update
    ;;;
    project.Proj_auto_prefs.ProjAP_asave
        -> project.Proj_auto_prefs.ProjAP_autosave;

    project.Proj_auto_prefs.ProjAP_aupdate
        -> project.Proj_auto_prefs.ProjAP_autoupdate;
enddefine;



define close_projecttool_window();
    if projecttool_win then
        ;;; stop auto-saving
        false ->> sys_timer(auto_save_project)
                -> sys_timer(auto_update_project);

        ;;; close the window
        p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_CLOSE);

        ;;; clear the mappings between file structures and descriptions
        clearproperty(project_file_to_string);
        clearproperty(project_string_to_file);
    endif;
enddefine;


/* ---------------------------------------------------------------

    Project Preferences And Make Image Tools

   --------------------------------------------------------------- */


defclass lconstant PrefsTool {
    Prefs_pages,
    Prefs_buttons,
    Prefs_templates,
    Prefs_box,
    Prefs_indexsheet,
    Prefs_widgets,
    Prefs_current,
};

lconstant PrefsTool_data = {
    ^Prefs_pages        ^false
    ^Prefs_buttons      ^false
    ^Prefs_templates    ^false
    ^Prefs_box          ^false
    ^Prefs_indexsheet   ^false
    ^Prefs_widgets      ^false
    ^Prefs_current      ^nullstring
};


define lconstant createPrefsTool(pages, buttons, templates, tsize) -> t;
    lvars pages, buttons, templates, tsize, t;

    lvars i;
    fast_for i from 2 by 2 to length(PrefsTool_data) do
        subscrv(i, PrefsTool_data);
    endfast_for;

    consPrefsTool() -> t;
    pages -> t.Prefs_pages;
    buttons -> t.Prefs_buttons;
    newmapping(templates, tsize, false, true) -> t.Prefs_templates;
    newmapping([], tsize, false, true) -> t.Prefs_widgets;
enddefine;



lconstant
    CREATE_BTN = "Create",
    APPLY_BTN = "Apply",
    RESET_BTN = "Reset",
    DONE_BTN = "Done",
    UPDATE_BTN = "Update",
    CANCEL_BTN = "Cancel",
;


/* ---------------------------------------------------------------
    Specific Sheet Field Accepters
   --------------------------------------------------------------- */

;;; Note that in the page setup routine, page can be a single
;;; sheet or or a list of sheets
;;;
define lconstant AutoPages_setup(page); lvars page;
    current_project.Proj_auto_prefs.ProjAP_autosave
        -> propsheet_sensitive(hd(page), AUTOSAVETIME_FIELD);
    current_project.Proj_auto_prefs.ProjAP_autoupdate
        -> propsheet_sensitive(hd(page), AUTOUPDATETIME_FIELD);
enddefine;

define lconstant ProjSavePages_setup(page); lvars page;
    if current_project.Proj_save_prefs.ProjSV_ed_savewhat = NONE_LABEL then
        false
    else
        true
    endif -> propsheet_sensitive(hd(tl(page)), SAVEEDWINPOS_FIELD);
enddefine;

define lconstant GenscriptPage_setup(page); lvars page;
    current_project.Proj_script_prefs.ProjGS_genloaderfile
        -> propsheet_sensitive(hd(tl(page)), ADDLOADER_FIELD);

    current_project.Proj_script_prefs.ProjGS_genmkscript
        -> propsheet_sensitive(hd(tl(page)), ADDMKIMAGE_FIELD);
enddefine;


define lconstant SaveEditor_accepter(sheet, field, new_value, proj_ident) -> new_value;
lvars sheet field new_value proj_ident;

    if new_value = NONE_LABEL then
        ;;; disable the save editor info menu
        false
    else
        true
    endif -> propsheet_sensitive(sheet, SAVEEDWINPOS_FIELD);
enddefine;


define lconstant LoaderFile_accepter(sheet, field, new_value) -> new_value;
lvars sheet field new_value;

    if sys_fname_extn(new_value) /= '.p' then
        propsheet_undef -> new_value;

        pop_ui_message('Loader file name must have a ".p" extension', false,
            XptShellOfObject(sheet),
            PRJ_ERRORTITLE);
    endif;
enddefine;


define lconstant AutoSave_accepter(sheet, field, new_value) -> new_value;
lvars sheet field new_value;

    new_value -> propsheet_sensitive(sheet, AUTOSAVETIME_FIELD);
enddefine;

define lconstant AutoSaveTime_accepter(sheet, field, new_value) -> new_value;
lvars sheet field new_value;

    min(max(1, new_value), 60) -> new_value;
enddefine;


define lconstant AutoUpdate_accepter(sheet, field, new_value) -> new_value;
lvars sheet field new_value;

    new_value -> propsheet_sensitive(sheet, AUTOUPDATETIME_FIELD);
enddefine;

define lconstant AutoUpdateTime_accepter(sheet, field, new_value) -> new_value;
lvars sheet field new_value;

    min(max(1, new_value), 60) -> new_value;
enddefine;

define lconstant GenLoaderFile_accepter(sheet, field, new_value) -> new_value;
lvars sheet field new_value;

    new_value -> propsheet_sensitive(sheet, ADDLOADER_FIELD);
enddefine;

define lconstant GenMkscriptFile_accepter(sheet, field, new_value) -> new_value;
lvars sheet field new_value;

    new_value -> propsheet_sensitive(sheet, ADDMKIMAGE_FIELD);
enddefine;


    ;;; popmemlim and popminmemlim are displayed in Kbytes
lconstant WORDS_PER_KBYTE = 1024/SIZEOFTYPE(:full); ;;; 1024/SIZEOFTYPE(:word);
;;;
define lconstant memlim_converter(kb) /* -> words */;
    lvars kb;
    kb * WORDS_PER_KBYTE
enddefine;
;;;
define updaterof memlim_converter(words) /* -> kb */;
    lvars words;
    (words + #_<WORDS_PER_KBYTE/2>_#) div WORDS_PER_KBYTE;
enddefine;

    ;;; ensure popmemlim and popminmemlim can never go below 0
define lconstant memlim_accepter(ps, button, val) -> val;
    lvars ps, button, val, real_val;
    if isreal(val) then
        intof(val) -> val;
        if propsheet_acceptreason == "increment" then
            val + 99 -> val;
        elseif propsheet_acceptreason == "decrement" then
            val - 99 -> val;
        endif;
        ;;; verify new value
        memlim_converter(val) -> real_val;
        if real_val < 0 then
            0 -> val;
        endif;
    else
        ;;; illegal value
        propsheet_undef -> val;
    endif;
enddefine;


/* ---------------------------------------------------------------
    Sheet Definitions
   --------------------------------------------------------------- */

;;; Preferences Tool Sheets
;;;
lvars
    GeneralPage =
        [
            [^NAME_FIELD message ^nullstring
                ( nodefault, columns = 15 )
            ]
            [^DIRECTORY_FIELD message ^nullstring
                    ( nodefault, columns = 40 )
            ]
            [^DESCRIPTION_FIELD ^nullstring
                ( nodefault, columns = 40 )
            ]
        ],

    ViewPage =
        [
            'Time & Date'
            [
                [^TIMEFORMAT_FIELD menuof ^time_formats]
                [^DATEFORMAT_FIELD menuof ^date_formats]
            ]
            'File Display'
            [^FILESHOW_FIELD someof
                        [^PRJ_FCATEGORY_LABEL]
                        [^PRJ_FTYPE_LABEL ^PRJ_FDESC_LABEL]
                        [^PRJ_FUPDATE_LABEL ^PRJ_FSIZE_LABEL]
            ]
        ],

    AutoPages =
        [
            'Automatic Saving & Update'
            [
                [^AUTOSAVE_FIELD ^true
                    (accepter = ^AutoSave_accepter)]
                +[^AUTOSAVETIME_FIELD 10 minutes
                    (aligned = ^false, accepter = ^AutoSaveTime_accepter,
                     label = 'every', columns = 10)]
                [^AUTOUPDATE_FIELD ^true
                    (accepter = ^AutoUpdate_accepter)]
                +[^AUTOUPDATETIME_FIELD 10 minutes
                    (aligned = ^false, accepter = ^AutoUpdateTime_accepter,
                     label = 'every', columns = 10)]
            ]
        ],

    ProjSavePages =
        [
            'Project Tool'
            [
                [^SAVEPROJTOOL_FIELD ^true]
            ]
            'Ved Windows'
            [
                [^SAVEEDITOR_FIELD menuof ^window_savewhat_opts
                    ( accepter = ^(SaveEditor_accepter(%ident current_project%)) )
                ]
                [^SAVEEDWINPOS_FIELD ^true]
            ]
        ],

;


;;; Make Image Tool Sheets
;;;
lvars
    ImagePage =
        [
            'Image Setup'
            [
                [^IMAGEPATH_FIELD ^nullstring
                    (columns = 30) ]
                [^BUILDON_FIELD menuof ^mkimage_buildon_opts
                    (columns = 15) ]
                [^LANGUAGES_FIELD someof [^POP11_LABEL ^LISP_LABEL]
                                    [^PROLOG_LABEL ^ML_LABEL]
                    ( nodefault ) ]
            ]
        ],

    RuntimePage =
        [
            [^TOPLEVEL_FIELD menuof ^ss_language_opts
                ( default = 1 )
            ]
            [^STARTUPPROC_FIELD ^nullstring
                ( nodefault, columns = 30 )
            ]
            [^MINMEM_FIELD 0 'Kb'
                  ( columns = 16,
                    converter = ^memlim_converter,
                    accepter = ^memlim_accepter,
                  )
            ]
            [^MAXMEM_FIELD 0 'Kb'
                  ( columns = 16,
                    converter = ^memlim_converter,
                    accepter = ^memlim_accepter,
                  )
            ]
            [^GCTYPE_FIELD menuof [^GCCOPY_LABEL ^GCNONCOPY_LABEL]
            ]
            [^GCRATIO_FIELD 1-64
                ( default = ^popgcratio, width = 30 )
            ]
        ],

    GenscriptPage =
        [   'Script pathnames'
            [
                [^MKIMAGE_FIELD ^nullstring
                    (columns = 30) ]
                [^LOADERFILE_FIELD ^nullstring
                    (accepter = ^LoaderFile_accepter, columns = 30) ]
            ]
            'Generate'
            [
                [^MKIMAGE_FIELD ^true (accepter = ^GenMkscriptFile_accepter)]
                +[^ADDMKIMAGE_FIELD ^true
                    (label = 'add to project')]
                [^LOADERFILE_FIELD ^true (accepter = ^GenLoaderFile_accepter)]
                +[^ADDLOADER_FIELD ^true
                    (label = 'add to project')]
            ]
        ],

;


;;; the various pages making up the project information sheets
lconstant prefstool_pages =
    [%
        PRJ_DESC_LABEL,
        PRJ_VIEW_LABEL,
        PRJ_AUTO_LABEL,
        PRJ_SAVE_LABEL,
    %];

;;; the various pages making up the project information sheets
lconstant mkimagetool_pages =
    [%
        PRJ_RT_LABEL,
        PRJ_IMAGE_LABEL,
        PRJ_GENSCRIPT_LABEL,
    %];


/* ---------------------------------------------------------------
    Local Variables
   --------------------------------------------------------------- */

lvars
    prefstool = writeable createPrefsTool(prefstool_pages,
                    [^APPLY_BTN ^RESET_BTN ^DONE_BTN ^CANCEL_BTN],
                       [
                        [^PRJ_DESC_LABEL ^(conspair(GeneralPage, false))]
                        [^PRJ_VIEW_LABEL ^(conspair(ViewPage, false))]
                        [^PRJ_AUTO_LABEL ^(conspair(AutoPages, AutoPages_setup))]
                        [^PRJ_SAVE_LABEL ^(conspair(ProjSavePages, ProjSavePages_setup))]
                       ], 4);

lvars
    mkimagetool = writeable createPrefsTool(mkimagetool_pages,
                    [^APPLY_BTN ^RESET_BTN ^DONE_BTN ^CANCEL_BTN],
                       [
                        [^PRJ_RT_LABEL ^(conspair(RuntimePage, false))]
                        [^PRJ_IMAGE_LABEL ^(conspair(ImagePage, false))]
                        [^PRJ_GENSCRIPT_LABEL ^(conspair(GenscriptPage, GenscriptPage_setup))]
                       ], 4);


/* ---------------------------------------------------------------
    Selection and property tool manipulation
   --------------------------------------------------------------- */

;;; used to set and get the procedures which should be run when the
;;; user modifies preferences associated with the project.
;;;
define lconstant set_configbox_actions(on_apply, on_action, on_done,
            on_cancel, on_help, box);
lvars on_apply, on_action, on_done, on_cancel, on_help, box;
    on_apply, on_action, on_done, on_cancel, on_help
        -> explode(propsheet_user_data(box));
enddefine;

define lconstant get_configbox_actions(box)
    /* -> (on_apply, on_action, on_done, on_cancel, on_help) */;
lvars box;
    explode(propsheet_user_data(box));
enddefine;


;;; looks through the project data sheet and returns the slot accessor
;;; in the project containing the data to be displayed and the
;;; data structure descriptor
;;;
define lconstant get_page_data(sheet_name) -> accessor -> sheet_data;
lvars sheet_name data accessor = false, sheet_data = false, i;

    fast_for i from 3 by 3 to length(Project_data) do
        if (subscrv(i, Project_data) ->> data) and
                subscrv(1, data) = sheet_name then
            subscrv(i-2, Project_data) -> accessor;
            subscrv(2, data) -> sheet_data;
            quitloop;
        endif;
    endfast_for;
enddefine;


define lconstant procedure show_sheet_values(struct, sheet_data, sheet, setup_pdr);
lvars struct sheet sheet_data setup_pdr;

    lvars i, s, f, field, accessor;

    fast_for i from 1 by 3 to length(sheet_data) do

        ;;; is it displayed?
        ;;;
        if (subscrv(i+2, sheet_data) ->> field) then
            subscrv(i, sheet_data) -> accessor;

            if isvector(field) then
                sheet(subscrv(1, field)) -> s;
                subscrv(2, field) -> f;
            else
                sheet -> s;
                field -> f;
            endif;
            struct.accessor -> propsheet_field_value(s, f);
        endif;
    endfast_for;

    ;;; if there are special states (e.g. sensitivity) which need
    ;;; to be set, there should be a setup procedure to do it
    ;;;
    if isprocedure(setup_pdr) then
        setup_pdr(sheet);
    endif;
enddefine;


;;; ensures the visible sheet is displaying the current values
;;;
define lconstant show_multisheet_values(struct, curr_sheet, sheet, template);
lvars struct, curr_sheet, sheet, template, accessor, sheet_data, sub_struct,
    i, sub_accessor, sub_field, s, f
    focus_sheet = false, focus_field = false;

    ;;; look up the struct sheet data. accessor is the Project structure
    ;;; accessor to extract data about the current sheet, while sheet_data
    ;;; is the vector containing
    get_page_data(curr_sheet) -> accessor -> sheet_data;

    ;;; get the sub-component for this sheet (either a slot accessor
    ;;; or table lookup procedure)
    ;;;
    struct.accessor -> sub_struct;

    show_sheet_values(sub_struct, sheet_data, sheet, back(template));
enddefine;


define lconstant set_sheet_values(struct, sheet_data, sheet) -> changed;
lvars struct sheet sheet_data changed = false;

    lvars i, field, accessor, newval;
    fast_for i from 1 by 3 to length(sheet_data) do

        ;;; is it displayed?
        ;;;
        if (subscrv(i+2, sheet_data) ->> field) then
            subscrv(i, sheet_data) -> accessor;

            if isvector(field) then
                propsheet_field_value(sheet(fast_subscrv(1, field)),
                            fast_subscrv(2, field))
            else
                propsheet_field_value(sheet, field)
            endif -> newval;

            ;;; check if any values have changed
            ;;;
            unless changed then
                newval /= struct.accessor -> changed;
            endunless;

            newval -> struct.accessor;
        endif;
    endfast_for;
enddefine;


;;; transfers values from a sheet to the struct sub-structure
;;;
define lconstant set_multisheet_values(struct, curr_sheet, sheet) -> changed;
lvars struct, curr_sheet, sheet, changed = false,
    accessor, sheet_data, sub_struct, newval,
    i, sub_accessor, sub_field;

    get_page_data(curr_sheet) -> accessor -> sheet_data;

    ;;; get the sub-component for this sheet (either a slot accessor
    ;;; or table lookup procedure)
    ;;;
    struct.accessor -> sub_struct;

    set_sheet_values(sub_struct, sheet_data, sheet) -> changed;
enddefine;



;;; change which set of variables are currently being displayed
;;;
define lconstant multisheettool_select(ps, button, val, tool) -> val;
lvars ps button val tool;

    lvars
        box = tool.Prefs_box,
        sheetname = tool.Prefs_current,
        widgets = tool.Prefs_widgets,
        templates = tool.Prefs_templates,
        indexsheet = tool.Prefs_indexsheet,
    ;

    lvars
        sheet = widgets(val),
        template = templates(val);

    if sheet and val /= sheetname and templates(val) then

        lvars changed;

        ;;; if we're changing sheet, don't force the user to press on
        ;;; Apply to save the current settings
        ;;;
        set_multisheet_values(current_project, sheetname, widgets(sheetname))
            -> changed;

        propsheet_save(box, true);

        ;;; call the apply procedure if there is one
        ;;;
        lvars on_apply;
        get_configbox_actions(box) -> (on_apply ,,,,);

        if isprocedure(on_apply) then
            on_apply(current_project, sheetname, changed, false) ->;
        endif;

        ;;; now update the screen
        ;;;
        propsheet_hide(widgets(sheetname));
        propsheet_show(sheet);
        val ->> sheetname
            ->> tool.Prefs_current
            -> propsheet_field_value(indexsheet, "Options");

        ;;; update the display
        ;;;
        show_multisheet_values(current_project, sheetname,
            sheet, template);
        propsheet_save(box);
    endif;
enddefine;

define lconstant multisheettool_buttons_cb(box, btn, tool) -> val;
lvars box, btn, tool, val = false;
lvars on_apply, on_action, on_done, on_cancel, on_help, changed;

    lvars
        pages = tool.Prefs_pages,
        box = tool.Prefs_box,
        sheetname = tool.Prefs_current,
        widgets = tool.Prefs_widgets,
        templates = tool.Prefs_templates,
        indexsheet = tool.Prefs_indexsheet,
    ;

    get_configbox_actions(box)
            -> (on_apply, on_action, on_done, on_cancel, on_help);

    if btn == APPLY_BTN then
        set_multisheet_values(current_project, sheetname, widgets(sheetname))
            -> changed;
        propsheet_save(box, true);

        if isprocedure(on_apply) then
            on_apply(current_project, sheetname, changed, false)
        else
            false
        endif -> val;

    elseif btn == RESET_BTN then
        show_multisheet_values(current_project, sheetname,
                widgets(sheetname), templates(sheetname));
        propsheet_save(box, true);

    elseif btn == DONE_BTN then
        set_multisheet_values(current_project, sheetname, widgets(sheetname))
            -> changed;

        if isprocedure(on_done) then
            on_done(current_project, sheetname, changed, true)
        else
            true
        endif -> val;

    elseif btn == CANCEL_BTN then
        if isprocedure(on_cancel) then
            on_cancel(current_project, sheetname, false, true)
        else
            true
        endif -> val;
    endif;

    if val and testdef popxlink_openlook then
        propsheet_hide(box);
    endif;
enddefine;


/* ---------------------------------------------------------------
    Adding and Removing Options Sheets
   --------------------------------------------------------------- */

define lconstant create_sheet(title, display, tool);
lvars title display tool;

    lvars
        pages = tool.Prefs_pages,
        box = tool.Prefs_box,
        sheetname = tool.Prefs_current,
        widgets = tool.Prefs_widgets,
        templates = tool.Prefs_templates,
        indexsheet = tool.Prefs_indexsheet,
    ;

    lvars sheet options
        page_desc = templates(title),
        options = front(page_desc),
        setup_pdr = back(page_desc),
        curr_list show_list;

    if options then
        propsheet_field(indexsheet,
                        [Options menuof ^pages
                        (nodefault, accepter= ^(multisheettool_select(%tool%)))]);

        sheetname -> propsheet_field_value(indexsheet, "Options");

        if (widgets(title) ->> show_list) then
            ;;; may a have a number of sheets to destroy
            propsheet_destroy(if ispair(dup(show_list)) then dl() endif);
        endif;

        if not(islist(hd(options))) then
            [%  while options /== nil do
                    propsheet_new(hd(options), box, hd(tl(options)));
                    tl(tl(options)) -> options;
                endwhile;
            %] -> widgets(title);
        else
            propsheet_new(title, box, options) -> widgets(title);
        endif;

        if display then
            [%  indexsheet,
                if (widgets(sheetname) ->> sheet) then
                    if ispair(dup(sheet)) then
                        ;;; a list of sheets so display them all
                        dl();
                    endif;
                endif,
                box %] -> show_list;

            propsheet_show(show_list);
            sys_grbg_list(show_list);
        endif;
    endif;
enddefine;


/* ---------------------------------------------------------------
    Initialisation And Creation Routines
   --------------------------------------------------------------- */

;;; sets the busy cursor and position of the dialog.
define lconstant set_propbox_attributes(box, parent); lvars box, parent;
    XtRealizeWidget(box);
    true ->> XptBusyCursorFeedback(box) -> XptGarbageCursorFeedback(box);

    if parent then
        l_typespec posn { x:XptPosition, y:XptPosition };
        lvars posn = EXPTRINITSTR(:posn);
        fast_XtTranslateCoords(parent, 0,0, exacc[@]posn.x, exacc[@]posn.y);
        (exacc posn.x, exacc posn.y, false, false) -> XptWidgetCoords(box);
        XptMaxWidgetVisibility(box);
    else
        if testdef popxlink_motif then
            ;;; dimensions of the enclosing DialogShell are of no use
            ;;; until the child is managed, i.e. popped up. To position
            ;;; it before pop-up, we need to work on the RowColumn child
            lvars pw, w = propsheet_subpart(box, "upper_controls");
            while (XtParent(w) ->> pw) and pw /== box do pw -> w endwhile;
            if w then XtRealizeWidget(w ->> box) endif;
        endif;
        XptCenterWidgetOn(box, "screen");
    endif;
enddefine;


define multisheettool_init(title, tool);
lvars title tool;

    propsheet_init();

    lvars
        pages = tool.Prefs_pages,
        buttons = tool.Prefs_buttons,
        widgets = tool.Prefs_widgets,
        parent,
        box;

    propsheet_new_box(title,
        (p_PROJECTTOOL_OP(projecttool_win, PROJ_WIN_GETSHELL) ->> parent),
        multisheettool_buttons_cb(%tool%), buttons) ->> box -> tool.Prefs_box;

    propsheet_new(false, box,
                   [Options menuof ^(tool.Prefs_pages)
                    (nodefault, accepter= ^(multisheettool_select(%tool%))])
        -> tool.Prefs_indexsheet;

    hd(pages) -> tool.Prefs_current;

    ;;; create sheets for all titles in the tool pages
    ;;;
    for title in pages do
        unless widgets(title) then
            create_sheet(title, false, tool);
        endunless;
    endfor;

    propsheet_save(box);

    ;;; initialise the store for the action procedures
    ;;;
    initv(5) -> propsheet_user_data(box);
    set_propbox_attributes(box, parent);
enddefine;


define lconstant update_multisheettool_window(toolident); lvars toolident;
lvars tool = idval(toolident),
      box = tool.Prefs_box;

    if box and propsheet_visible(tool.Prefs_box) then
        lvars
            sheetname = tool.Prefs_current,
            widgets = tool.Prefs_widgets,
            templates = tool.Prefs_templates,
        ;
        show_multisheet_values(current_project, sheetname,
                widgets(sheetname), templates(sheetname));
        propsheet_save(box, true);
    endif;
enddefine;

define lconstant update_prefstool_window = update_multisheettool_window(%ident prefstool%);
enddefine;

define lconstant update_mkimagetool_window = update_multisheettool_window(%ident mkimagetool%);
enddefine;


define lconstant close_multisheettool_window(tool); lvars tool;
    if tool.idval.Prefs_box then
        propsheet_hide(tool.idval.Prefs_box);
    endif;
enddefine;

define lconstant close_prefstool_window = close_multisheettool_window(%ident prefstool%);
enddefine;

define lconstant close_mkimagetool_window = close_multisheettool_window(%ident mkimagetool%);
enddefine;


define open_multisheettool_window(title, initial_sheet,
    on_apply, on_action, on_done, on_cancel, on_help, tool);
lvars title, initial_sheet, on_apply, on_action, on_done,
    on_cancel, on_help, tool;
lvars curr_sheet show_list;

    define lconstant hide_config_sheet(prop);
    lvars prop;
        if prop then
            propsheet_hide(prop);
        endif;
        erase();        ;;; the sheet argument
    enddefine;

    ;;; if no box, then need to create the tool
    ;;;
    lvars set_cursors = false;
    unless tool.Prefs_box then
        multisheettool_init(title, tool);
    endunless;

    if initial_sheet and member(initial_sheet, tool.Prefs_pages) then
        if initial_sheet /= tool.Prefs_current then
            initial_sheet -> tool.Prefs_current;
            appproperty(tool.Prefs_widgets, hide_config_sheet);
        endif;
    elseif initial_sheet then
        warning(initial_sheet, 1, 'No such property sheet');
    endif;

    set_configbox_actions(on_apply, on_action, on_done, on_cancel,
        on_help, tool.Prefs_box);

    lvars
        widgets = tool.Prefs_widgets,
        templates = tool.Prefs_templates,
    ;

    [% tool.Prefs_indexsheet,
        if ispair(dup(widgets(tool.Prefs_current))) then
            dl()
        endif,
        tool.Prefs_box %] -> show_list;

    propsheet_show(show_list);
    sys_grbg_list(show_list);

    ;;; update the display
    show_multisheet_values(current_project, tool.Prefs_current,
            widgets(tool.Prefs_current), templates(tool.Prefs_current));
    propsheet_save(tool.Prefs_box);

    if tool.Prefs_current /= nullstring then
        tool.Prefs_current -> (tool.Prefs_indexsheet)("Options");
    endif;
enddefine;


;;; pop_ui_projectprefstool takes an intial sheet name (a string)
;;; and procedures to be called when particular buttons are pressed
;;;
define pop_ui_projectprefstool(initial_sheet,
    on_apply, on_action, on_done, on_cancel, on_help);
dlocal XptBusyCursorOn = true;
lvars project initial_sheet on_apply on_action on_cancel on_done on_help;

    unless (pop_ui_current_project ->> project) then
        mishap(0, 'No current project');
    endunless;

    open_multisheettool_window('Poplog: Project Preferences', initial_sheet,
        on_apply, on_action, on_done, on_cancel, on_help, prefstool);
enddefine;


;;; pop_ui_projectmkimagetool takes an intial sheet name (a string)
;;; and procedures to be called when particular buttons are pressed
;;;
define pop_ui_projectmkimagetool(initial_sheet, on_apply, on_action, on_done,
                    on_cancel, on_help);
dlocal XptBusyCursorOn = true;
lvars project initial_sheet on_apply on_action on_cancel on_done on_help;

    unless (pop_ui_current_project ->> project) then
        mishap(0, 'No current project');
    endunless;

    open_multisheettool_window('Poplog: Image Tool', initial_sheet,
        on_apply, on_action, on_done, on_cancel, on_help, mkimagetool);
enddefine;


/* ---------------------------------------------------------------

    New Project Dialog
        Creates a non-modal dialog which can be used to preferences a
        new project. When the "Create" button is pressed, any existing
        project is closed and the new one opened in its place.

   --------------------------------------------------------------- */

;;; New Project Tool

lvars
    newproj_box = false,
    newproj_sheet = false,
    newproj_status = false,
    ;

lconstant
    NEWSTATUS_FIELD = "newprojstate",
    EMPTYNAME_STRING = 'Please enter a project name',
    INVALIDNAME_STRING = 'Project name is invalid (illegal characters or format)',
    INVALIDDIR_STRING = 'Base directory is invalid or doesn\'t exist',
    CREATENEWPROJ_STRING = 'Creating new project...',
;

define lconstant verify_new_project(name, dir) -> valid;
lvars name, dir, valid = false;

    if name = nullstring then
        EMPTYNAME_STRING -> propsheet_field_value(newproj_status, NEWSTATUS_FIELD);
        return;
    endif;

    unless issimplename(name) then
        INVALIDNAME_STRING -> propsheet_field_value(newproj_status, NEWSTATUS_FIELD);
        return;
    endunless;

    unless (sysfileok(dir) ->> dir) and sys_dir_name(dir) then
        INVALIDDIR_STRING -> propsheet_field_value(newproj_status, NEWSTATUS_FIELD);
        return;
    endunless;

    true -> valid;
enddefine;


define lconstant newproj_button_cb(box, btn) -> val;
lvars box, btn, val = false, name, dir;

    switchon btn
        case = CREATE_BTN then
            propsheet_field_value(newproj_sheet, NAME_FIELD) -> name;
            propsheet_field_value(newproj_sheet, DIRECTORY_FIELD) -> dir;
            if verify_new_project(name, dir) then

                CREATENEWPROJ_STRING -> propsheet_field_value(newproj_status, NEWSTATUS_FIELD);
                lvars proj = CreateProject(name,
                                    if current_project then
                                        current_project
                                    endif);

                sys_dir_name(dir) -> proj.Proj_desc_data.ProjD_dir;

                if current_project then
                    pop_ui_save_project();
                    if close_project_edit_windows(true) then
                        close_project_subwindows();
                    else
                        nullstring -> propsheet_field_value(newproj_status, NEWSTATUS_FIELD);
                        return;
                    endif;
                endif;
                proj -> pop_ui_current_project;
                pop_ui_projecttool();
                true -> val;
            endif;
        case = CANCEL_BTN then
            true -> val;
    endswitchon;

    if val and testdef popxlink_openlook then
        propsheet_hide(box);
    endif;
enddefine;


define pop_ui_new_project();

    unless newproj_box then
        propsheet_new_box('Poplog: New Project',
                    pop_ui_app_shell, newproj_button_cb,
                    [^CREATE_BTN ^CANCEL_BTN]) -> newproj_box;

        propsheet_new(false, newproj_box,
            [
                [^NAME_FIELD ^nullstring
                    ( nodefault, columns = 30 )
                ]
                [^DIRECTORY_FIELD ^nullstring
                    ( nodefault, columns = 30 )
                ]
            ]) -> newproj_sheet;
        current_directory -> propsheet_field_value(newproj_sheet,
                DIRECTORY_FIELD);
        propsheet_show(newproj_sheet);

        propsheet_new(false, newproj_box,
            [
                [^NEWSTATUS_FIELD message ^nullstring
                    ( nolabel, aligned = ^false, nodefault, columns = 40 )
                ]
            ]) -> newproj_status;
        propsheet_show(newproj_status);

        set_propbox_attributes(newproj_box, false);
    endunless;

    nullstring -> propsheet_field_value(newproj_sheet, NAME_FIELD);
    nullstring -> propsheet_field_value(newproj_status, NEWSTATUS_FIELD);

    propsheet_show(newproj_box);
enddefine;

;;; see also OpenProject...
;;;
define pop_ui_open_project();
    lvars name = ChooseProject('Poplog: Open Project', 'Open', false, false);
    returnunless(name);
    lvars proj = pop_ui_read_project(name);
    if proj then
        DoOpenProject(proj);
    else
        pop_ui_message(sprintf(name,
                    'Can\'t read project file\n\n    %S'), false,
                    pop_ui_app_shell, PRJ_ERRORTITLE);
    endif;
enddefine;


/* ---------------------------------------------------------------
    Changing Information Associated With A File Descriptor
   --------------------------------------------------------------- */

lconstant filefield_datavec = newassoc([
        [^File_name         ^FILE_ST_NAME]
        [^File_category     ^FILE_ST_CATEGORY]
        [^File_contents     ^FILE_ST_TYPE]
        [^File_description  ^FILE_ST_DESC]
    ]);

lconstant filedesc_fields =
    [% appproperty(filefield_datavec,
                procedure(x);lvars x;
                    ;;; remove the key (access procedure), leaving
                    ;;; the fieldname or <undef> if there is no sheet field
                    erase();
                    x;
                endprocedure) %];


;;; used to associate a file with a propsheet box
;;;
lvars file_info_propsheet = newproperty([], 20, false, "perm");


;;; used to show the values of a particular target in an existing sheet
;;;
define lconstant show_file_vals(file_desc, sheets);
lvars file_desc, sheets;

    lvars sheet = hd(sheets);

    define lconstant show_val(accessor, fieldname);
    lvars accessor, fieldname;

        ;;; check the structure field has an entry in the property sheet
        unless fieldname == pop_undef then
            file_desc.accessor -> propsheet_field_value(sheet, fieldname);
        endunless;
    enddefine;

    ;;; update the different fields
    ;;;
    appproperty(filefield_datavec, show_val);
enddefine;


;;; used to transfer the displayed values in the target sheet
;;; to the target data. Returns true if a new file has been added
;;; or false otherwise.
;;;
define lconstant set_file_vals(file_desc, sheets);
lvars file_desc, sheets;

    lvars sheet = hd(sheets);

    define lconstant set_val(accessor, fieldname);
    lvars accessor, fieldname;

        ;;; check the structure field has an entry in the property sheet
        unless fieldname == pop_undef then
            propsheet_field_value(sheet, fieldname) -> file_desc.accessor;
        endunless;
    enddefine;

    ;;; update the different fields
    ;;;
    appproperty(filefield_datavec, set_val);
enddefine;


define update_project_file_sheet(file_desc);
lvars file_desc, sheets;

    if (file_info_propsheet(file_desc) ->> sheets) then
        show_file_vals(file_desc, sheets);
    endif;
enddefine;


define destroy_project_file_sheet(file_desc);
lvars file_desc, sheets;

    if (file_info_propsheet(file_desc) ->> sheets) then
        propsheet_destroy(sheets);
        false -> file_info_propsheet(file_desc);
        sys_grbg_list(sheets);
    endif;
enddefine;


define close_all_project_file_sheets();
    define lconstant destroy_sheet;
        erase();
        destroy_project_file_sheet();
    enddefine;

    appproperty(file_info_propsheet, destroy_sheet);
enddefine;

define lconstant filetool_button_cb(box, btn) -> val;
lvars box, btn, val = true, file_desc;

    propsheet_user_data(box) -> file_desc;

    if btn == UPDATE_BTN then
        set_file_vals(file_desc, file_info_propsheet(file_desc));

        ;;; ensure other information about this file descriptor is updated
        ;;;
        UpdateProjFile(false, false, file_desc);

    elseif btn == CANCEL_BTN then
        ;;; will pop-down by default
        destroy_project_file_sheet(file_desc);
    endif;

    if val and testdef popxlink_openlook then
        propsheet_hide(box);
    endif;
enddefine;


lvars last_filebox = false;

;;; the count argument here is used to decide whether the new dialog
;;; should be offset from the previous one created
;;;
define lconstant create_file_sheet(file_desc, parent, count);
lvars file_desc, parent, count, box, sheet;

    propsheet_new_box('Poplog: File Information',
                parent, filetool_button_cb,
                [^UPDATE_BTN ^CANCEL_BTN]) -> box;

    propsheet_new(false, box,
        [
            [^FILE_ST_NAME message ^(file_desc.File_name)]
            [^FILE_ST_CATEGORY menuof ^file_category_opts]
            [^FILE_ST_TYPE menuof ^filetype_ordering]
            [^FILE_ST_DESC ^(file_desc.File_description)
                    (columns = 30)]
        ]) -> sheet;

    file_desc.File_category -> propsheet_field_value(sheet, FILE_ST_CATEGORY);
    file_desc.File_contents -> propsheet_field_value(sheet, FILE_ST_TYPE);

    ;;; make sure we can find out what file this sheet is displaying
    ;;;
    file_desc -> propsheet_user_data(box);

    lvars sheets = [^sheet ^box];
    sheets -> file_info_propsheet(file_desc);

    propsheet_show(sheet);

    if count > 1 and last_filebox then
        set_propbox_attributes(box, last_filebox);
    else
        set_propbox_attributes(box, parent);
    endif;

    propsheet_show(box ->> last_filebox);
enddefine;

define show_project_file_sheet(file_desc, parent, count);
lvars count, file_desc, parent, file_name, sheets;

    unless (file_info_propsheet(file_desc) ->> sheets) then
        create_file_sheet(file_desc, parent, count);
    else
        show_file_vals(file_desc, sheets);
        propsheet_show(sheets);
    endunless;
enddefine;


/* ---------------------------------------------------------------
    Window Manipulation
   --------------------------------------------------------------- */

define lconstant update_project_subwindows();
    update_prefstool_window();
    update_mkimagetool_window();
enddefine;


define lconstant close_project_subwindows();
    close_all_project_file_sheets();
    close_prefstool_window();
    close_mkimagetool_window();
enddefine;


define lconstant close_project_windows();
    close_project_subwindows();
    close_projecttool_window();
enddefine;


;;; pop_ui_close_project should not call itself recursively since
;;; it can be called when the project window is closed and when
;;; explicitly called from a menu option
;;;
define pop_ui_close_project();
dlocal XptBusyCursorOn;
    if pop_ui_current_project and not(iscaller(pop_ui_close_project, 2)) then
        true -> XptBusyCursorOn;
        pop_ui_save_project();
        if close_project_edit_windows(true) then
            close_project_windows();
            false -> pop_ui_current_project;
        endif;
    endif;
enddefine;


/* ---------------------------------------------------------------
    Ved Command
   --------------------------------------------------------------- */

define lconstant get_fnames_from_ved(starter, endl);
lvars starter, endl, line;

    define lconstant get_ved_line();
    lvars ch;
        vedtextleft();
        consstring(#|
            while (vedrepeater() ->> ch) /== 10 do
                returnif(ch == termin)(nullstring);
                ch;
            endwhile |#);
    enddefine;

    vedpositionpush();
    starter();
    [% while (vedline <= endl) do
        unless length(get_ved_line() ->> line) == 0 then
            line;
        endunless;
    endwhile %];
    vedpositionpop();
enddefine;

define global ved_project;
dlocal vedargument, current_directory;
lvars file, files, len;

    define lconstant check_current_project();
        unless current_project then
            vedputmessage('No open project');
            exitfrom(ved_project);
        endunless;
    enddefine;

    length(vedargument) -> len;

    if isstartstring('addmr', vedargument) then
        check_current_project();
        if vvedmarklo > vvedmarkhi then
            vedputmessage('project: no marked range');
        else
            get_fnames_from_ved(vedmarkfind, vvedmarkhi) -> files;
            get_project_basedir(current_project) -> current_directory;
            for file in files do
                DoAddFile(file, 0, false, false, false);
            endfor;
            sys_grbg_list(files);
        endif;

    elseif isstartstring('addall', vedargument) then
        check_current_project();
        get_fnames_from_ved(vedtopfile, vvedbuffersize) -> files;
        get_project_basedir(current_project) -> current_directory;
        for file in files do
            DoAddFile(file, 0, false, false, false);
        endfor;
        sys_grbg_list(files);

    elseif vedargument = 'add' then
        check_current_project();
        get_project_basedir(current_project) -> current_directory;
        unless vedcurrent = nullstring then
            DoAddFile(vedpathname, 0, false, false, false);
        endunless;

    elseif isstartstring('add ', vedargument) then
        check_current_project();
        tl(sysparse_string(vedargument)) -> files;
        get_project_basedir(current_project) -> current_directory;
        for file in files do
            unless file = nullstring then
                DoAddFile(file, 0, false, false, false);
            endunless;
        endfor;
        sys_grbg_list(files);

    elseif vedargument = 'new' then
        pop_ui_new_project();

    elseif vedargument = 'open' then
        OpenProject();

    elseif isstartstring('open ', vedargument) then
        tl(sysparse_string(vedargument)) -> files;
        returnif(null(files));
        hd(files)->file;
        if sys_file_exists(file) then
            lvars proj = pop_ui_read_project(file);
            if proj then
                DoOpenProject(proj);
            else
                vedputmessage('can\'t read project from file');
            endif;
        else
            vedputmessage('can\'t open project: no such file or directory');
        endif;
        sys_grbg_list(files);

    elseif vedargument = 'save' then
        check_current_project();
        pop_ui_save_project();

    elseif vedargument = 'close' then
        check_current_project();
        pop_ui_close_project();

    else
        vedputmessage(sprintf(vedargument, 'project: unknown command "%P"'));

    endif;
enddefine;


SET_GUI(projecttool_switch_vec, projecttool_xm, projecttool_xol,
    'pop_ui_projecttool');


endexload_batch;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul 29 1995
        Added dummy definitions at start of file for Popc
--- Robert John Duncan, Jul 18 1995
        Substituted Julian's projfile_vedpresent for the standard vedpresent
--- Robert John Duncan, Jul  7 1995
        Replaced compile-time tests for widget set with run-time tests.
        Changed set_propbox_attributes to centre Motif propsheets properly
        and reorganised pop_ui_new_project so that all the component sheets
        of the NewProject box are managed before set_propbox_attributes is
        called to position it.
        Fixed <ENTER> project add command.
        Added pop_ui_kill_project for brutal termination of a project on
        exit from Poplog.
        Changed file-selection mechanism to use pop_ui_choose_file and to
        create a different instance of the selector for choosing projects
        (because the directory and filter will typically be different to
        those for normal files).
        Changed some labels.
        Added Julian's fixes.
 */
