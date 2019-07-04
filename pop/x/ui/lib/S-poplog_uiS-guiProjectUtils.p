/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-guiProjectUtils.p
 > Purpose:         ProjectTool utilities
 > Author:          Julian Clinton, April 1995 (see revisions)
 > Documentation:
 > Related Files:
*/

#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;

section $-poplog_ui;

global vars project_protect = true;

include sysdefs;
include subsystem;
include pop_uiP;

uses
    $-poplog_ui$-guiFileutils,
    $-poplog_ui$-guiTimeUtils,
    datafile,
    sprintf,
    switchon,
;


/* ---------------------------------------------------------------
    Label Definitions
   --------------------------------------------------------------- */

constant
    POP11_LABEL = 'Pop-11',
    PROLOG_LABEL = 'Prolog',
    LISP_LABEL = 'Common Lisp',
    ML_LABEL = 'Standard ML',
    VED_LABEL = 'Ved',

    HELP_LABEL = 'Help',
    REF_LABEL = 'Reference',
    TEACH_LABEL = 'Teach',
    DOC_LABEL = 'Document',

    NONE_LABEL = 'none',
    YES_LABEL = 'yes',
    NO_LABEL = 'no',

    USETARG_LABEL = 'default for target',

    GCCOPY_LABEL = 'copying',
    GCNONCOPY_LABEL = 'non-copying',
;

;;; Project file information
constant
    FILE_ST_NAME = 'Name',
    FILE_ST_CATEGORY = 'Category',
    FILE_ST_TYPE = 'Type',
    FILE_ST_DESC = 'Purpose',
;


;;; executable names
constant
    BASEPOP11_EXE = 'basepop11',
    POP11_EXE = 'pop11',
    CLISP_EXE = 'clisp',
    PROLOG_EXE = 'prolog',
    PML_EXE = 'pml',
;

;;; category of file in a project
constant
    SOURCE_LABEL = "source",
    UTILITY_LABEL = "utility",
    DOCUMENTATION_LABEL = "documentation",
    SCRIPT_LABEL = "'command script'",
;


;;; types of file in a project
constant
    POP11SRC_LABEL = "'Pop-11 source'",
    PROLOGSRC_LABEL = "'Prolog source'",
    LISPSRC_LABEL = "'Lisp source'",
    MLSRC_LABEL = "'ML source'",

    CSRC_LABEL = "'C source'",
    CINC_LABEL = "'C include'",
    F77SRC_LABEL = "'Fortran 77 source'",

    TEXT_LABEL = "'text'",

    SH_LABEL = "'Bourne shell script'",
    CSH_LABEL = "'C shell script'",
    DCL_LABEL = "'DCL script'",

    OBJECT_LABEL = "'object file'",
    EXE_LABEL = "'executable'",
    SIMAGE_LABEL = "'saved image'",
;


constant
    pop_ui_projecttool_version = 1000,      ;;; in case we need to re-organize

    PRJ_ROOT_LABEL = 'Root',

    PRJ_DESC_LABEL = "'General'",
    PRJ_AUTO_LABEL = "'Auto Save/Update'",
    PRJ_SAVE_LABEL = "'Save Workspace'",
    PRJ_VIEW_LABEL = "'View'",

    PRJ_IMAGE_LABEL = "'Image'",
    PRJ_RT_LABEL = "'Runtime'",
    PRJ_GENSCRIPT_LABEL = "'Scripts'",

    PRJ_NEVER_LABEL = 'never',
    PRJ_ALL_EDFILES_LABEL = 'all windows',
    PRJ_PROJ_EDFILES_LABEL = 'project windows',

    PRJ_FCATEGORY_LABEL = 'category',
    PRJ_FTYPE_LABEL = 'type',
    PRJ_FDESC_LABEL = 'description',
    PRJ_FUPDATE_LABEL = 'last update',
    PRJ_FSIZE_LABEL = 'size',

    PRJ_NONE_LABEL = 'none',
    PRJ_EMPTY_LABEL = '     ',
;


constant
    NAME_FIELD = 'Name',
    DIRECTORY_FIELD = 'Directory',
    DESCRIPTION_FIELD = 'Description',

    IMAGEPATH_FIELD = 'Image path',
    BUILDON_FIELD = 'Build using',
    LANGUAGES_FIELD = 'Languages',
    MKIMAGE_FIELD = 'Build script',
    LOADERFILE_FIELD = 'Loader file',

    ADDLOADER_FIELD = 'Add loader',
    ADDMKIMAGE_FIELD = 'Add build script',

    STARTUPPROC_FIELD = 'Eval on startup',
    TOPLEVEL_FIELD = 'Top-level',
    MINMEM_FIELD = 'Min. heap size',
    MAXMEM_FIELD = 'Max. heap size',
    GCTYPE_FIELD = 'GC type',
    GCRATIO_FIELD = 'GC ratio',

    TIMEFORMAT_FIELD = 'Time format',
    DATEFORMAT_FIELD = 'Date format',
    AUTOSAVE_FIELD = 'Save project',
    AUTOSAVETIME_FIELD = 'Save time',
    AUTOUPDATE_FIELD = 'Update project',
    AUTOUPDATETIME_FIELD = 'Update time',

    SAVEPROJTOOL_FIELD = 'Window location',
    SAVEEDITOR_FIELD = 'Save which',
    SAVEEDWINPOS_FIELD = 'Window location',

    FILESHOW_FIELD = 'Show',
;


constant date_formats = [
    'Mon DD YYYY'
    'Mon DD YY'
    'MM/DD/YYYY'
    'MM/DD/YY'
    'MM-DD-YYYY'
    'MM-DD-YY'
    'DD Mon YYYY'
    'DD Mon YY'
    'DD/Mon/YYYY'
    'DD/Mon/YY'
    'DD-Mon-YYYY'
    'DD-Mon-YY'
    'DD/MM/YYYY'
    'DD/MM/YY'
    'DD-MM-YYYY'
    'DD-MM-YY'
    ];

constant time_formats = [
    'HH:MM (24 hr)'
    'HH:MM:SS (24 hr)'
    'HH.MM (24 hr)'
    'HH.MM.SS (24 hr)'
    'HH:MM (am/pm)'
    'HH:MM:SS (am/pm)'
    'HH.MM (am/pm)'
    'HH.MM.SS (am/pm)'
    ];


constant ss_language_opts = [
    ^POP11_LABEL
    ^PROLOG_LABEL
    ^LISP_LABEL
    ^ML_LABEL
    ];

constant yes_no_list = [
    ^YES_LABEL ^NO_LABEL
    ];


constant window_savewhat_opts = [
    ^PRJ_PROJ_EDFILES_LABEL
    ^PRJ_ALL_EDFILES_LABEL
    ^PRJ_NONE_LABEL
    ];

constant file_category_opts = [
    ^SOURCE_LABEL
    ^UTILITY_LABEL
    ^DOCUMENTATION_LABEL
    ^SCRIPT_LABEL
    ];


;;; executable names
constant mkimage_buildon_opts = [
    ^BASEPOP11_EXE
    ^POP11_EXE
    ^CLISP_EXE
    ^PROLOG_EXE
    ^PML_EXE
    ];
;


/* ---------------------------------------------------------------
    File Type Utilities
   --------------------------------------------------------------- */

defclass FileType {
    FType_name,
    FType_subsystem,        ;;; word giving Poplog subsystem or <false>
    FType_extns,            ;;; possible extension on this file (list or ident)
    FType_defextn,          ;;; default file extension (may be a procedure)
    FType_defaction,        ;;; default action when double-clicked (not used)
    FType_editable,         ;;; can it be run in a text editor?
    FType_editor,           ;;; what text editor
    FType_viewable,         ;;; can it be viewed?
    FType_viewer,           ;;; what viewer?
    FType_compilable,       ;;; can it be compiled?
    FType_compcomm,         ;;; if so, how?
    FType_executable,       ;;; can it be executed?
    FType_execcomm,         ;;; if so, how?
};


;;; ProjectProtect allows a user procedure to be run when interrupt
;;; is called.
;;;
define ProjectProtect(op, on_intr); lvars op, on_intr;
lvars oldintr = interrupt;
dlocal interrupt;

    define lconstant catch_interrupt;
        if isprocedure(on_intr) then
            on_intr();
        endif;
        chainfrom(ProjectProtect, oldintr);
    enddefine;

    if project_protect then
        catch_interrupt -> interrupt;
    endif;
    op();
enddefine;


;;; some values returned by vedfileprops can be used as commands
;;; (e.g. ref, help) while others need converting (e.g. lib -> showlib)
;;;
define lconstant get_vedprops_command(props);
lvars props;
    switchon props
        case = "lib" then
            "showlib";
        case = "include" then
            "showinclude";
        else
            props or "ved";
    endswitchon;
enddefine;

;;; open a Ved window on a file.
;;;
define open_ved_window(basedir, ss, props, file, can_edit);
    lvars basedir, ss, props, file, can_edit, wininfo = false, filepos = false;
    if islist(can_edit) then
        ;;; optional window and file position
        ((), (), basedir, ss, props, file, can_edit) ->
            (basedir, ss, props, file, can_edit, wininfo, filepos);
    endif;

    define lconstant Open();
        lconstant command = '%p %p %p', resources = [x y numRows numColumns];
        dlocal current_directory = basedir, subsystem = ss;
        lvars iconic = false;
        if wininfo then
            dl(wininfo) -> iconic -> xved_value("nextWindow", resources);
        endif;
        if props then
            veddo(sprintf(file, get_vedprops_command(props), ss, command));
            ;;; can't stop the window taking focus, so we don't try to
            ;;; iconify it
        elseif iconic then
            true -> xved_value("nextWindow", "iconic");
            vededit(file, false);
        else
            vededit(file, true);
            unless wininfo then
                wved_raise_window(wvedwindow);
            endunless;
        endif;
        can_edit -> vedwriteable;
        if filepos then
            lvars (topline, leftcol, row, col) = dl(filepos);
            topline -> vedlineoffset;
            leftcol -> vedcolumnoffset;
            vedjumpto(row, col);
            vedrefresh();
        endif;
    enddefine;

    call_ved(Open);
enddefine;


;;; given a Ved file buffer, get its window details
;;;
define get_ved_window_params(filebuff) -> (wx, wy, rows, cols, isiconic);
lvars filebuff, wx, wy, rows, cols, isiconic;
dlocal ved_current_file;

    filebuff -> ved_current_file;
    xved_value("currentWindow", [x y numRows numColumns iconic]) ->
        (wx, wy, rows, cols, isiconic);
enddefine;


;;; given a Ved file buffer, get the position of the file and cursor
;;; within the window
;;;
define get_ved_file_params(filebuff) -> (topline, leftcol, row, col);
lvars filebuff, topline, leftcol, row, col;
dlocal ved_current_file;

    filebuff -> ved_current_file;
    vedlineoffset -> topline;
    vedcolumnoffset -> leftcol;
    vedline -> row;
    vedcolumn -> col;
enddefine;


define filetype_datavec = newproperty([
        [^POP11SRC_LABEL
                ^(consFileType(POP11SRC_LABEL, "pop11",
                    ['.p' '.ph'],
                    subscr_subsystem(%SS_FILE_EXTN, "pop11"%),
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    true, subsystem_compile(%"pop11"%),
                    false, false))]

        [^PROLOGSRC_LABEL
                ^(consFileType(PROLOGSRC_LABEL, "prolog",
                    ident prologfiletypes,
                    subscr_subsystem(%SS_FILE_EXTN, "prolog"%),
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    true, subsystem_compile(%"prolog"%),
                    false, false))]

        [^LISPSRC_LABEL
                ^(consFileType(LISPSRC_LABEL, "lisp",
                    ident lispfiletypes,
                    subscr_subsystem(%SS_FILE_EXTN, "lisp"%),
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    true, subsystem_compile(%"lisp"%),
                    false, false))]

        [^MLSRC_LABEL
                ^(consFileType(MLSRC_LABEL, "ml",
                    ['.ml' '.sig'],
                    subscr_subsystem(%SS_FILE_EXTN, "ml"%),
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    true, subsystem_compile(%"ml"%),
                    false, false))]

        [^TEXT_LABEL
                ^(consFileType(TEXT_LABEL, false,
                    [], '',
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    false, false,
                    false, false))]

        [^CSRC_LABEL
                ^(consFileType(CSRC_LABEL, false,
                    ['.c'], '.c',
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    true, false,
                    false, false))]

        [^CINC_LABEL
                ^(consFileType(CINC_LABEL, false,
                    ['.h'], '.h',
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    true, false,
                    false, false))]

        [^F77SRC_LABEL
                ^(consFileType(F77SRC_LABEL, false,
                    ['.f' '.for'], '.f',
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    true, false,
                    false, false))]

        [^DCL_LABEL
                ^(consFileType(DCL_LABEL, false,
                    ['.com'], '.com',
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    false, false,
                    true, sysobey))]

        [^SH_LABEL
                ^(consFileType(SH_LABEL, false,
                    ['.sh'], nullstring,
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    false, false,
                    true, sysobey))]

        [^CSH_LABEL
                ^(consFileType(CSH_LABEL, false,
                    ['.csh'], nullstring,
                    false,          ;;; default file action
                    true, "ved",
                    true, "pved",
                    false, false,
                    true, sysobey))]
    ], 16, false, "perm")
enddefine;


vars filetype_ordering = [
        ^POP11SRC_LABEL
        ^PROLOGSRC_LABEL
        ^LISPSRC_LABEL
        ^MLSRC_LABEL
        ^TEXT_LABEL
        ^CSRC_LABEL
        ^CINC_LABEL
        ^F77SRC_LABEL
        ^SH_LABEL
        ^CSH_LABEL
        ^DCL_LABEL
    ];


/* ---------------------------------------------------------------
    Mapping Between Project Values & "Useful" Values
   --------------------------------------------------------------- */


define lconstant current_username;
#_IF DEF UNIX
    sysgetusername(popusername);
#_ELSE
    popusername;
#_ENDIF
enddefine;

define lconstant ss_to_label(ss); lvars ss;
    switchon ss
        case = "pop11" then
            POP11_LABEL;
        case = "top" then
            PROLOG_LABEL;
        case = "prolog" then
            PROLOG_LABEL;
        case = "lisp" then
            LISP_LABEL;
        case = "ml" then
            ML_LABEL;
        else
            false
    endswitchon;
enddefine;


define lconstant label_to_ss(label); lvars label;
    switchon label
        case = POP11_LABEL then
            "pop11"
        case = PROLOG_LABEL then
            "prolog"
        case = LISP_LABEL then
            "lisp"
        case = ML_LABEL then
            "ml"
        else
            false
    endswitchon;
enddefine;


define lconstant list_loaded_subsystems;

    define lconstant check_ss_label(label); lvars label;
        if is_subsystem_loaded(label_to_ss(label)) then
            label
        endif;
    enddefine;

    [% applist(ss_language_opts, check_ss_label) %];
enddefine;


define lconstant label_to_ss_lib(label); lvars label;
    switchon label
        case = POP11_LABEL then
            "pop11"
        case = PROLOG_LABEL then
            "prolog"
        case = LISP_LABEL then
            "clisp"
        case = ML_LABEL then
            "ml"
        else
            false
    endswitchon;
enddefine;


define lconstant label_to_ss_com(label); lvars label;
    switchon label
        case = POP11_LABEL then
            POP11_EXE
        case = PROLOG_LABEL then
            PROLOG_EXE
        case = LISP_LABEL then
            CLISP_EXE
        case = ML_LABEL then
            PML_EXE
        else
            BASEPOP11_EXE
    endswitchon;
enddefine;

define lconstant ss_com_to_label(com); lvars com;
    switchon com
        case = POP11_EXE then
            POP11_LABEL
        case = BASEPOP11_EXE then
            POP11_LABEL
        case = PROLOG_EXE then
            PROLOG_LABEL
        case = CLISP_EXE then
            LISP_LABEL
        case = PML_EXE then
            ML_LABEL
        else
            mishap(com, 1, 'Unknown executable type');
    endswitchon;
enddefine;


/* ---------------------------------------------------------------
    Project Data Structures
   --------------------------------------------------------------- */

;;; Each structure which is used to display data has an associated vector.
;;; The vector contains groups of 3 bits of data:
;;;     the structure accessor procedure
;;;     the default value for the field
;;;     the name of the property field which displays that value, or
;;;
;;;         false if the value is not displayed, or
;;;
;;;         for pages with multiple sheets, a vector containing the
;;;         integer offset of the sheet and the field name within the sheet
;;;
defclass ProjCreate {
    ProjC_creator,
    ProjC_createtime,
    };

constant ProjCreate_data = {
    ^ProjC_creator          ^current_username   ^false
    ^ProjC_createtime       ^sys_real_time      ^false
    };


defclass ProjDesc {
    ProjD_name,
    ProjD_dir,
    ProjD_desc,
    };

constant ProjDesc_data = {
    ^ProjD_name         ^nullstring         ^NAME_FIELD
    ^ProjD_dir          ^nullstring         ^DIRECTORY_FIELD
    ^ProjD_desc         ^nullstring         ^DESCRIPTION_FIELD
    };


defclass ProjAutoPrefs {
    ProjAP_asave,
    ProjAP_asavetime,
    ProjAP_aupdate,
    ProjAP_aupdatetime,
    };


constant procedure auto_save_project;   ;;; defined in pop_ui_projecttool.p
constant procedure auto_update_project;   ;;; defined in pop_ui_projecttool.p

define ProjAP_autosave(aprefs); lvars aprefs;
    aprefs.ProjAP_asave;
enddefine;

define updaterof ProjAP_autosave(t, aprefs); lvars t, aprefs;
    ;;; if we've got a non-false value and the timer for auto_save_project
    ;;;
    if t then
        aprefs.ProjAP_asavetime -> sys_timer(auto_save_project);
    else
        false -> sys_timer(auto_save_project);
    endif;
    t -> aprefs.ProjAP_asave;
enddefine;


define ProjAP_autosavetime(aprefs); lvars aprefs;
    number_coerce(aprefs.ProjAP_asavetime/6e7, 1);
enddefine;

define updaterof ProjAP_autosavetime(t, aprefs); lvars t, aprefs;
    t * 6e7 -> aprefs.ProjAP_asavetime;
    if aprefs.ProjAP_asave then
        aprefs.ProjAP_asavetime -> sys_timer(auto_save_project);
    endif;
enddefine;


define ProjAP_autoupdate(aprefs); lvars aprefs;
    aprefs.ProjAP_aupdate;
enddefine;

define updaterof ProjAP_autoupdate(t, aprefs); lvars t, aprefs;
    ;;; if we've got a non-false value and the timer for auto_update_project
    ;;;
    if t then
        aprefs.ProjAP_aupdatetime -> sys_timer(auto_update_project);
    else
        false -> sys_timer(auto_update_project);
    endif;
    t -> aprefs.ProjAP_aupdate;
enddefine;


define ProjAP_autoupdatetime(aprefs); lvars aprefs;
    number_coerce(aprefs.ProjAP_aupdatetime/6e7, 1);
enddefine;

define updaterof ProjAP_autoupdatetime(t, aprefs); lvars t, aprefs;
    t * 6e7 -> aprefs.ProjAP_aupdatetime;
    if aprefs.ProjAP_aupdate then
        aprefs.ProjAP_aupdatetime -> sys_timer(auto_update_project);
    endif;
enddefine;


constant ProjAutoPrefs_data = {
    ^ProjAP_autosave        ^false                      {1 ^AUTOSAVE_FIELD}
    ^ProjAP_autosavetime    6e8                         {1 ^AUTOSAVETIME_FIELD}
    ^ProjAP_autoupdate      ^false                      {1 ^AUTOUPDATE_FIELD}
    ^ProjAP_autoupdatetime  6e8                         {1 ^AUTOUPDATETIME_FIELD}
    };


defclass ProjSavePrefs {
    ProjSV_tool_saveprefs,
    ProjSV_ed_savewhat,
    ProjSV_ed_saveprefs,
    };

constant ProjSavePrefs_data = {
    ^ProjSV_tool_saveprefs  ^true               {1 ^SAVEPROJTOOL_FIELD}
    ^ProjSV_ed_savewhat     ^(hd(window_savewhat_opts)) {2 ^SAVEEDITOR_FIELD}
    ^ProjSV_ed_saveprefs    ^true               {2 ^SAVEEDWINPOS_FIELD}
    };


defclass ProjViewPrefs {
    ProjVP_timeformat,
    ProjVP_dateformat,
    ProjVP_fileprefs,
    };


;;; set defaults to be closest to operating system display format
;;;
#_IF DEF VMS

constant ProjViewPrefs_data = {
    ^ProjVP_timeformat      'HH:MM:SS (24 hr)'  {1 ^TIMEFORMAT_FIELD}
    ^ProjVP_dateformat      'DD-Mon-YYYY'       {1 ^DATEFORMAT_FIELD}
    ^ProjVP_fileprefs       [^PRJ_FUPDATE_LABEL ^PRJ_FSIZE_LABEL]   {2 ^FILESHOW_FIELD}
    };

#_ELSE

constant ProjViewPrefs_data = {
    ^ProjVP_timeformat      ^(hd(time_formats))         {1 ^TIMEFORMAT_FIELD}
    ^ProjVP_dateformat      ^(hd(date_formats))         {1 ^DATEFORMAT_FIELD}
    ^ProjVP_fileprefs       [^PRJ_FUPDATE_LABEL ^PRJ_FSIZE_LABEL]   {2 ^FILESHOW_FIELD}
    };

#_ENDIF


defclass ProjImage {
    ProjIM_imagename,       ;;; name of saved image
    ProjIM_buildon,         ;;; which command to build the image on
    ProjIM_langs,           ;;; list of languages required
    };

constant ProjImage_data = {
    ^ProjIM_imagename       ^nullstring             {1 ^IMAGEPATH_FIELD}
    ^ProjIM_buildon         ^(label_to_ss_com(ss_to_label(subsystem)))
                                                    {1 ^BUILDON_FIELD}
    ^ProjIM_langs           ^(list_loaded_subsystems())   {1 ^LANGUAGES_FIELD}
    };


defclass ProjRuntime {
    ProjRT_toplevel,        ;;; what is the top-level compiler
    ProjRT_startup,         ;;; name of procedure to invoke on startup
    ProjRT_memmin,          ;;; value of popminmemlim at startup
    ProjRT_memmax,          ;;; value of popmemlim at startup
    ProjRT_gctype,          ;;; default GC type
    ProjRT_gcratio          ;;; default GC ratio
    };

constant ProjRuntime_data = {
    ^ProjRT_toplevel    ^(ss_to_label(subsystem))   ^TOPLEVEL_FIELD
    ^ProjRT_startup     ^nullstring         ^STARTUPPROC_FIELD
    ^ProjRT_memmin      0                   ^MINMEM_FIELD
    ^ProjRT_memmax      262144              ^MAXMEM_FIELD
    ^ProjRT_gctype      ^GCCOPY_LABEL       ^GCTYPE_FIELD
    ^ProjRT_gcratio     25                  ^GCRATIO_FIELD
    };


defclass ProjGenscript {
    ProjGS_loaderfile,      ;;; name of loader file
    ProjGS_mkscript,        ;;; name of mkimage script
    ProjGS_genloaderfile,   ;;; generate loader file?
    ProjGS_addloaderfile,   ;;; add loader file to project?
    ProjGS_genmkscript,     ;;; generate mkimage script?
    ProjGS_addmkscript,     ;;; add mkimage script to project?
    };

constant ProjGenscript_data = {
    ^ProjGS_loaderfile      ^nullstring             {1 ^LOADERFILE_FIELD}
    ^ProjGS_mkscript        ^nullstring             {1 ^MKIMAGE_FIELD}
    ^ProjGS_genloaderfile   ^true                   {2 ^LOADERFILE_FIELD}
    ^ProjGS_addloaderfile   ^true                   {2 ^ADDLOADER_FIELD}
    ^ProjGS_genmkscript     ^true                   {2 ^MKIMAGE_FIELD}
    ^ProjGS_addmkscript     ^true                   {2 ^ADDMKIMAGE_FIELD}
    };


defclass FileDesc {
    File_name,
    File_category,
    File_contents,
    File_owner,
    File_description,
    File_lastupdate,
    File_editargs,
    File_viewargs,
    File_compargs,
    File_execargs,
    File_defaultaction,
    };

constant FileDesc_data = {
    ^File_name              ^nullstring         ^false
    ^File_category          ^false              ^false
    ^File_contents          ^nullstring         ^false
    ^File_owner            ^false              ^false
    ^File_description       ^nullstring         ^false
    ^File_lastupdate        0                   ^false
    ^File_editargs          ^nullstring         ^false
    ^File_viewargs          ^nullstring         ^false
    ^File_compargs          ^nullstring         ^false
    ^File_execargs          ^nullstring         ^false
    ^File_defaultaction     ^false              ^false
    };


/* ---------------------------------------------------------------
    Creating Structures
   --------------------------------------------------------------- */

define lconstant initInstance(class_data, constructor);
lvars class_data constructor i val;

    ;;; the second item in each set of 3 bits of data is the default
    ;;; value for the field. Put all these on the stack and then
    ;;; call the constructor
    ;;;
    constructor(
        for i from 2 by 3 to length(class_data) do
            if isprocedure(subscrv(i, class_data) ->> val) then
                val();
            else
                val;
            endif;
        endfor);
enddefine;

constant procedure (
    initProjCreate = initInstance(% ProjCreate_data, consProjCreate %),
    initProjDesc = initInstance(% ProjDesc_data, consProjDesc %),
    initProjImage = initInstance(% ProjImage_data, consProjImage %),
    initProjGenscript = initInstance(% ProjGenscript_data, consProjGenscript %),
    initProjRuntime = initInstance(% ProjRuntime_data, consProjRuntime %),
    initProjAutoPrefs = initInstance(% ProjAutoPrefs_data, consProjAutoPrefs %),
    initProjViewPrefs = initInstance(% ProjViewPrefs_data, consProjViewPrefs %),
    initProjSavePrefs = initInstance(% ProjSavePrefs_data, consProjSavePrefs %),
    initFileDesc = initInstance(% FileDesc_data, consFileDesc %),
);


;;; A Project1000 is the holding place for all the data that is written
;;; to disk as the project description. Call it Project1000 so we can look
;;; at the datafile and get what type of object we're creating...
;;;
defclass Project1000 {
    Proj_tool_version,
    Proj_type,              ;;; root or sub-project (currently always root)
    Proj_create_data,       ;;; who created the project and when
    Proj_desc_data,         ;;; misc info about the project
    Proj_auto_prefs,        ;;; customise the behaviour of the tool
    Proj_save_prefs,        ;;; customise what is saved when the project saved
    Proj_view_prefs,        ;;; customise how files appear in the project
    Proj_files_list,        ;;; list of filenames associated with the project
    Proj_files_count,       ;;; how many files
    Proj_files_table,       ;;; table mapping file names to file structs
    Proj_image_prefs,
    Proj_run_data,          ;;; runtime startup info
    Proj_script_prefs,
    };


;;; The Project structure holds the whole thing together. The third item
;;; is the index label of the sheet used to display the attibutes
;;; and a pointer to the data vector for the display contents. An optional
;;; third item in the vector is the procedure that should be called with
;;; the project structure to return the appropriate set of data
;;;

define global isProject = isProject1000;
enddefine;


/* ---------------------------------------------------------------
    Information About A Project
   --------------------------------------------------------------- */

define lconstant isabs_path(path) /* -> boole */ ;
lvars path;

    returnunless(path)(false);

#_IF DEF VMS
    lvars dir = sys_fname_path(path);

    if dir = nullstring then
        false
    else
        (datalength(dir) fi_> 1) and not(isstartstring('[.', dir))
    endif;
#_ELSE
    (datalength(path) fi_> 0) and
            (fast_subscrs(1, path) == `/` or fast_subscrs(1, path) == `$`
             or fast_subscrs(1, path) == `~`)
#_ENDIF
enddefine;


;;; get_project_basedir takes aproject and returne the path of
;;; the root directory or the name of the workspace variable
;;;
define get_project_basedir(project) -> dir; lvars project, dir = false;
    if project then
        project.Proj_desc_data.ProjD_dir -> dir;
    endif;
enddefine;


define gen_project_abspath(project, fname) -> path; lvars project, fname, path;
    sysfileok(fname) -> fname;
    if isabs_path(fname) then
        fname
    else
        get_project_basedir(project) dir_>< fname
    endif -> path;
enddefine;


define gen_project_relpath(project, fname) -> fname; lvars project, fname;
    lvars basedir = get_project_basedir(project);

    ;;; if the file chosen is below the project root directory, make the
    ;;; filename relative to this
    ;;;
    sysfileok(fname) -> fname;
    if isstartstring(basedir, fname) then
        allbutfirst(length(basedir), fname) -> fname;
    endif;
enddefine;


constant Project_data = {
    ^Proj_tool_version  ^pop_ui_projecttool_version     ^false
    ^Proj_type          ^PRJ_ROOT_LABEL     ^false
    ^Proj_create_data   ^initProjCreate     ^false
    ^Proj_desc_data     ^initProjDesc       {^PRJ_DESC_LABEL    ^ProjDesc_data}
    ^Proj_auto_prefs    ^initProjAutoPrefs  {^PRJ_AUTO_LABEL    ^ProjAutoPrefs_data}
    ^Proj_save_prefs    ^initProjSavePrefs  {^PRJ_SAVE_LABEL    ^ProjSavePrefs_data}
    ^Proj_view_prefs    ^initProjViewPrefs  {^PRJ_VIEW_LABEL    ^ProjViewPrefs_data}
    ^Proj_files_list    []                  ^false
    ^Proj_files_count   0                   ^false
    ^Proj_files_table   ^false              ^false
    ^Proj_image_prefs   ^initProjImage      {^PRJ_IMAGE_LABEL   ^ProjImage_data}
    ^Proj_run_data      ^initProjRuntime    {^PRJ_RT_LABEL      ^ProjRuntime_data}
    ^Proj_script_prefs  ^initProjGenscript  {^PRJ_GENSCRIPT_LABEL    ^ProjGenscript_data}
    };


constant procedure
    initProject = initInstance(% Project_data, consProject1000 %);


/* ---------------------------------------------------------------
    File Type Utilities
   --------------------------------------------------------------- */

;;; filetype_pred is used as the basis for closures on file type
;;; information. This can be passed either the type name (string)
;;; or a FileDesc structure
;;;
define lconstant filetype_pred(type, accessor);
lvars type accessor;

    if isFileDesc(type) then
        type.File_contents -> type;
    endif;

    (filetype_datavec(type) ->> type) and (type.accessor);
enddefine;

define is_filetype_editable = filetype_pred(%FType_editable%);
enddefine;

define is_filetype_viewable = filetype_pred(%FType_viewable%);
enddefine;

define is_filetype_compilable = filetype_pred(%FType_compilable%);
enddefine;

define is_filetype_executable = filetype_pred(%FType_executable%);
enddefine;

define is_filetype_poplog = filetype_pred(%FType_subsystem%);
enddefine;


;;; when the user adds a new file, produce a default type
;;;
define filetypeinfo_from_extn(name) -> category -> contents;
lvars name category = DOCUMENTATION_LABEL, contents = TEXT_LABEL;

    lvars ftype, info, extn = sys_fname_extn(name);

    for ftype in filetype_ordering do
        if (filetype_datavec(ftype) ->> info) then
            if isident(info.FType_extns ->> info) then
                idval(info) -> info;
            endif;
            if info and member(extn, info) then
                ftype -> contents;
                quitloop;
            endif;
        endif;
    endfor;

    switchon contents
        case = POP11SRC_LABEL then
            SOURCE_LABEL
        case = PROLOGSRC_LABEL then
            SOURCE_LABEL
        case = LISPSRC_LABEL then
            SOURCE_LABEL
        case = MLSRC_LABEL then
            SOURCE_LABEL
        case = TEXT_LABEL then
            DOCUMENTATION_LABEL
        case = CSRC_LABEL then
            SOURCE_LABEL
        case = CINC_LABEL then
            SOURCE_LABEL
        case = F77SRC_LABEL then
            SOURCE_LABEL
        case = SH_LABEL then
            SCRIPT_LABEL
        case = CSH_LABEL then
            SCRIPT_LABEL
        case = DCL_LABEL then
            SCRIPT_LABEL
    endswitchon -> category;
enddefine;



/* ---------------------------------------------------------------
    Misc. Utilities
   --------------------------------------------------------------- */

define gen_prj_filename(proj);
lvars proj;
    sprintf(proj.Proj_desc_data.ProjD_dir dir_><
                proj.Proj_desc_data.ProjD_name, '%S.prj');
enddefine;

define gen_wspace_filename(proj);
lvars proj;
    sprintf(proj.Proj_desc_data.ProjD_dir dir_><
                proj.Proj_desc_data.ProjD_name, '%S.pws');
enddefine;


define gen_file_description(project, fstruct, timeformat, dateformat, fileprefs);

lvars project, fstruct, fileprefs, timeformat, dateformat;

    lvars fname = fstruct.File_name,
        fullname = gen_project_abspath(project, fname);

    consstring(#|
        explode(fname),

        if member(PRJ_FCATEGORY_LABEL, fileprefs)  then
            ` `,`[`, explode(fstruct.File_category), `]`;
        endif;

        lvars dotype = false, dodesc = false;

        if (member(PRJ_FTYPE_LABEL, fileprefs) ->> dotype) or
                (member(PRJ_FDESC_LABEL, fileprefs) ->> dodesc) then

            ` `, `(`;

            member(PRJ_FDESC_LABEL, fileprefs) -> dodesc;

            if dotype then
                explode(fstruct.File_contents);
                if dodesc then
                    `,`, ` `;
                endif;
            endif;

            if dodesc then
                explode(fstruct.File_description);
            endif;
            `)`;
        endif;

        lvars mtime, fsize;
        if isfile(fullname) then
            sysmodtime(fullname) ->> fstruct.File_lastupdate -> mtime;
            sysfilesize(fullname) -> fsize;
        else
            sys_real_time() -> mtime;
            0 -> fsize;
        endif;

        if member(PRJ_FUPDATE_LABEL, fileprefs) then
            `,`, ` `;
            gen_date_chars(mtime, timeformat, dateformat, ', ');
        endif;

        if member(PRJ_FSIZE_LABEL, fileprefs) then
            `,`, ` `;
            procedure(n); lvars n; dlocal cucharout = identfn;
                spr(n);
            endprocedure(fsize);
            `b`, `y`, `t`, `e`, `s`;
        endif;

        |#);
enddefine;


;;; initialises a new project
;;;
define initialise_new_project(project, fname, useproject);
lvars project, fname, useproject;

    lvars
        name = sys_fname_nam(fname),
        dir = sys_fname_path(fname),
        extn = sys_fname_extn(fname),
    ;

    if dir = nullstring then
        current_directory -> dir;
    endif;

    name -> project.Proj_desc_data.ProjD_name;
    sys_dir_name(dir) -> project.Proj_desc_data.ProjD_dir;

    newmapping([], 32, false, true) -> project.Proj_files_table;

    sprintf(name, '%S.psv') -> project.Proj_image_prefs.ProjIM_imagename;
    sprintf(name, 'load%S.p') -> project.Proj_script_prefs.ProjGS_loaderfile;

#_IF DEF VMS
    sprintf(name, 'mk%S.com') -> project.Proj_script_prefs.ProjGS_mkscript;
#_ELSE
    sprintf(name, 'mk%S') -> project.Proj_script_prefs.ProjGS_mkscript;
#_ENDIF

    if useproject then
        ;;; copy project preferences from an existing project
        ;;;
        explode(useproject.Proj_auto_prefs) -> explode(project.Proj_auto_prefs);
        explode(useproject.Proj_save_prefs) -> explode(project.Proj_save_prefs);
        explode(useproject.Proj_view_prefs) -> explode(project.Proj_view_prefs);
    endif;
enddefine;


;;; initialises a file structure from the filename
;;;
define initialise_projfiledesc(fname, fstruct, proj);
lvars fname, fstruct, proj;

    fname -> fstruct.File_name;
    filetypeinfo_from_extn(fname)
        -> fstruct.File_category -> fstruct.File_contents;
enddefine;

;;; once the project table has been initialised or restored,
;;; the initialisation list can be reclaimed
;;;
define lconstant reclaim_projtable_filelists(lists);
lvars lists list;

    for list in lists do
        sys_grbg_list(list);
    endfor;
    sys_grbg_list(lists);
enddefine;


/* ---------------------------------------------------------------
    File I/O
   --------------------------------------------------------------- */

;;; reads the project, sets the project directory so we know
;;; where to save it to and re-creates the newmapping property
;;; containing the file information...
;;;
define read_project_description(filename) -> project;
    lvars filename, project = false;
    if sys_file_exists(filename) then
        datafile(filename) -> project;
        if isProject(project) then
            ;;; set project directory to location of file
            lvars path;
            if (sys_fname_path(filename) ->> path) /= nullstring and
                    (sys_dir_name(path) ->> path) then
                sysfileok(path)
            else
                current_directory
            endif -> project.Proj_desc_data.ProjD_dir;
            ;;; the file table will be in list form; convert to a property
            lvars flist = project.Proj_files_table;
            newmapping(flist, 32, false, true) -> project.Proj_files_table;
            reclaim_projtable_filelists(flist);
        else
            false -> project;
        endif;
    endif;
enddefine;

define write_project_description(project, filename);
    lvars project, filename;
    ;;; convert file table to list form for datafile
    copy(project) -> project;
    datalist(project.Proj_files_table) -> project.Proj_files_table;
    project -> datafile(filename);
    reclaim_projtable_filelists(project.Proj_files_table);
enddefine;


define read_project_workspace(filename) -> workspace;
lvars filename workspace = false;

    if sys_file_exists(filename) then
        datafile(filename)
    else
        false
    endif -> workspace;
enddefine;

define write_project_workspace(workspace, filename);
lvars workspace filename;

    workspace -> datafile(filename);
enddefine;


lvars saving_project = false;
;;;
define save_project(project, report);
    lvars project, procedure report;
    unless saving_project then
        dlocal saving_project = true;
        lvars fname = gen_prj_filename(project);
        report(fname);
        write_project_description(project, fname);
    endunless;
enddefine;


/* ---------------------------------------------------------------
    (More) List Utilities
   --------------------------------------------------------------- */

;;; member_before searches a list for an item using comparison operator op
;;; and returns the pair BEFORE the pair containing the item (apart
;;; from when the item is at the head of the list) and the index of
;;; the item before (so index 0 implies item is at the front of the
;;; list)
;;;
define member_before(item, list, op) -> list -> index;
lvars item list op index = 0;

    repeat forever
        ;;; if list is empty, can't be found
        ;;;
        if list == [] then
            false -> list; return;
        endif;

        ;;; if the head of the list is the item then return the item
        ;;; (should only occur if the item is at the head of list)
        ;;;
        returnif(op(item, hd(list)));

        ;;; check to see if the list is at least 2 items long and whether
        ;;; the second from front is the item we're looking for
        ;;;
        index + 1 -> index;
        returnif(not(null(tl(list))) and op(item, hd(tl(list))));

        tl(list) -> list;
    endrepeat;

enddefine;


;;; nthpair returns the pair at index n (n = 1 returns the first pair)
;;;
define nthpair(n, list) -> list;
lvars n, list;

    lvars i;
    fast_for i from 2 to n do
        tl(list) -> list;
    endfast_for;
enddefine;


/* ---------------------------------------------------------------

    Generating Command Scripts To:

        - create a source loader file

        - build a saved image

   --------------------------------------------------------------- */

/* ---------------------------------------------------------------
    Script Utilities
   --------------------------------------------------------------- */

define lconstant gen_comment_chars(type) -> (starter, ender);
lvars type, starter, ender;

    if type = DCL_LABEL then
        '$! ', nullstring
    elseif type = POP11SRC_LABEL then
        ';;; ', nullstring
    elseif type = PROLOGSRC_LABEL then
        '% ', nullstring
    elseif type = LISPSRC_LABEL then
        ';;;; ', nullstring
    elseif type = MLSRC_LABEL then
        '(* ', ' *)'
    else
        '# ', nullstring
    endif -> (starter, ender);
enddefine;

define lconstant gen_linestart_chars(type);
lvars type;

    if type = DCL_LABEL then
        '$ ';
    else
        ;;; /bin/sh or /bin/csh
        ;;;
        nullstring;
    endif;
enddefine;

define lconstant gen_script_header(type);
lvars type;

    switchon type
        case = SH_LABEL then
            '#!/bin/sh\n';
        case = CSH_LABEL then
            '#!/bin/csh -f\n';
        else
            nullstring;
    endswitchon;
enddefine;


;;; proj is a project structure
;;; name is the name of the file
;;; purpose is a string giving the purpose of the file
;;; type is the type of script (SH_LABEL, CSH_LABEL or DCL_LABEL)
;;; outfile is the character consumer for the file
;;;
define lconstant gen_script_banner(project, name, purpose, type, outfile);
lvars project, name, purpose, type, outfile;
dlocal cucharout;

    lvars
        comment_start,
        comment_end;

    gen_comment_chars(type) -> (comment_start, comment_end);

    lvars
        text = consstring(#|
            explode(gen_script_header(type));
            explode(comment_start);
            explode('File name      :    ');
            explode(name); explode(comment_end); `\n`;
            explode(comment_start);
            explode('Project        :    ');
            explode(project.Proj_desc_data.ProjD_name); explode(comment_end); `\n`;
            explode(comment_start);
            explode('Purpose        :    ');
            explode(purpose); explode(comment_end); `\n`;
            explode(comment_start);
            explode('Generation Date:    ');
            gen_date_chars(
                    sys_real_time(),
                    project.Proj_view_prefs.ProjVP_timeformat,
                    project.Proj_view_prefs.ProjVP_dateformat,
                    ', '); explode(comment_end); `\n`;
            |#);
    outfile -> cucharout;
    sys_syspr(text);
enddefine;

define gen_mkimage_ss_opts(project, exepath, top_level) -> string;
lvars project, exepath, top_level, string;

lvars
    exelang = ss_com_to_label(exepath),
    toplang = ss_to_label(top_level),
    projlangs = copylist(project.Proj_image_prefs.ProjIM_langs);

    ;;; first, make sure the top level language is included
    unless member(toplang, projlangs) then
        toplang :: projlangs -> projlangs;
    endunless;

    ;;; new remove Pop-11 which is included in any language
    ncdelete(POP11_LABEL, projlangs) -> projlangs;

    ;;; ...and remove the language associated with the command we're
    ;;; using to build the image
    ncdelete(exelang, projlangs) -> projlangs;

    if null(projlangs) then
        nullstring
    else
        lvars lang;
        consstring(#|
            for lang in projlangs do
                explode(label_to_ss_lib(lang)); ` `;
            endfor;
            |#)
    endif -> string;
enddefine;


define gen_builder_invoke(project, exepath, imagepath, loadpath,
    top_level, invoke_expr, add_breaks, script_type, outfile);
lvars project, exepath, imagepath, loadpath, top_level, invoke_expr,
    add_breaks, script_type, outfile;

    lvars langlibs = gen_mkimage_ss_opts(project, exepath, top_level);

#_IF DEF VMS
    lconstant
        continue_line = '-\n    ',
        blank_line = '$\n';
#_ELSE
    lconstant
        continue_line = '\\\n    ',
        blank_line = '\n';
#_ENDIF

dlocal cucharout = outfile;

    lvars do_continue, do_newline, do_blankline;

    if add_breaks then
        continue_line, blank_line, '\n'
    else
        nullstring, nullstring, nullstring
    endif -> (do_continue, do_blankline, do_newline);

    sys_syspr(sprintf(do_newline, invoke_expr, loadpath, do_continue,
        langlibs, imagepath, do_continue, top_level, exepath, do_blankline,
#_IF DEF VMS
        '%S$ %S \\%%nort mkimage -subsystem %S %S%S %S%S%S ": %S"%S'
#_ELSE
        '%S$popsys/%S %%nort mkimage -subsystem %S %S%S %S%S%S ": %S"%S'
#_ENDIF
        ));
enddefine;


define lconstant set_execute_permission(file); lvars file;
#_IF DEF UNIX
    if isfile(file) then
        sysfilemode(file) || 8:111 -> sysfilemode(file);
    endif;
#_ENDIF;
enddefine;


/* ---------------------------------------------------------------
    Generic Script Generator Routines
   --------------------------------------------------------------- */

;;; In the following procedures:
;;;     project is the project being generated for
;;;     targdata is the information which describes the current target
;;;
define gen_loader_file(project, script_type, fname);
lvars project, script_type, fname;

lvars
    charcons = discout(fname);

dlocal cucharout;

    gen_script_banner(project, fname,
        'compiles languages, files etc., and sets the startup workspace',
        POP11SRC_LABEL, charcons);

    charcons -> cucharout;

    ;;; Load the project files. Files are compiled in the order
    ;;; that they appear in the project. Only files with a Poplog
    ;;; subsystem type are included. There's a simple check to make
    ;;; sure the loader file isn't added to the list of files to be
    ;;; compiled.
    ;;;
    lvars
        files = project.Proj_files_list,
        ftable = project.Proj_files_table,
    ;

    define lconstant gen_file_compile(file); lvars file, ftype;
        if (ftable(file) ->> ftype) and
                ftype.File_category == SOURCE_LABEL and
                (filetype_datavec(ftype.File_contents) ->> ftype) and
                isword(ftype.FType_subsystem) then
            printf(ftype.FType_subsystem, gen_project_abspath(project, file),
                'subsystem_compile(\'%S\', "%P");\n');
        endif;
    enddefine;

    sys_syspr('\n;;; Compile project files\n');
    applist(files, gen_file_compile);

    ;;; Finally set the default startup paremeters for memory,
    ;;; GC parameters etc.
    ;;;
    sys_syspr('\n;;; Set runtime memory, GC parameters\n');

    sys_syspr('sys_runtime_apply(\n    procedure;\n');
    printf(project.Proj_run_data.ProjRT_memmax,
            '        max(%P, popmemlim) -> popmemlim;\n');

    printf(project.Proj_run_data.ProjRT_memmin,
            '        max(%P, popminmemlim) -> popminmemlim;\n');

    printf(if project.Proj_run_data.ProjRT_gctype = GCCOPY_LABEL then
            "true" else "false" endif,
            '        %P -> pop_gc_copy;\n');

    printf(project.Proj_run_data.ProjRT_gcratio,
            '        %P -> popgcratio;\n');

    sys_syspr('    endprocedure);\n');

    charcons(termin);
enddefine;



define gen_build_image_script(project, script_type, fname);
lvars project, script_type, fname,
    iprefs = project.Proj_image_prefs,
    sprefs = project.Proj_script_prefs,
    rprefs = project.Proj_run_data,
    charcons = discout(fname);

    gen_script_banner(project, fname,
        'builds the application saved image',
        script_type, charcons);

    gen_builder_invoke(project,
            iprefs.ProjIM_buildon,
            gen_project_abspath(project, iprefs.ProjIM_imagename),
            gen_project_abspath(project, sprefs.ProjGS_loaderfile),
            label_to_ss(rprefs.ProjRT_toplevel),
            rprefs.ProjRT_startup,
            true,
            script_type,
            charcons);

    charcons(termin);
    set_execute_permission(fname);
enddefine;


;;; instead if simply building command file, this generates a
;;; temporary loaderfile and a command to run it and returns
;;; the name of the temporary loader file and an appopriate command
;;; to invoke it
;;;
define gen_build_image_command(project, script_type)
                -> (loaderfile, command);
lvars project, script_type, command,
    loaderfile = systmpfile(false, 'load', '.p'),
    iprefs = project.Proj_image_prefs,
    rprefs = project.Proj_run_data,
    ;

    gen_loader_file(project, script_type, loaderfile);

    consstring(#|
        gen_builder_invoke(project,
            iprefs.ProjIM_buildon,
            gen_project_abspath(project, iprefs.ProjIM_imagename),
            loaderfile,
            label_to_ss(rprefs.ProjRT_toplevel),
            rprefs.ProjRT_startup,
            false,
            script_type,
            identfn)|#) -> command;
enddefine;

constant guiProjectUtils = true;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  7 1995
        Installed Julian's fixes for open_ved_window, etc.
--- Robert John Duncan, Jul  5 1995
        Fix to ProjCreate_data so that creator name and creation time are
        reset for each project instead of being constants
 */
