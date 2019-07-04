/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/ui/lib/S-poplog_uiS-filetool_xol.p
 > Purpose:         Open Look file tool
 > Author:          Julian Clinton, Aug 1991 (see revisions)
 > Documentation:   REF *POP_UI
 > Related Files:   pop_ui_filetool.p
 */
compile_mode :pop11 +strict;

uses-now Xol;

section $-poplog_ui;

include pop_uiP.ph;
include xt_constants.ph;
include xpt_coretypes.ph;
include XolConstants.ph;

uses
    xt_init,
    xt_widget,
    xt_composite,
    xt_callback,
    xt_event,
    xt_grab,
    xt_popup,
    xt_widgetinfo,
    xt_resource,
    xt_appcon,

    xpt_cursorplane,
    xpol_listutils,

    xolTextFieldWidget,
    xolCaptionWidget,
    xolOblongButtonWidget,
    xolControlAreaWidget,
    xolStaticTextWidget,
    xolScrollingListWidget,

    $-poplog_ui$-guiActions,
    $-poplog_ui$-guiUtils,
    $-poplog_ui$-guiFileutils,
    $-poplog_ui$-guiXlibs,
    $-poplog_ui$-guiMouseEvents,
;

lconstant filetool_switch_vec   = INIT_filetool_switch_vec;
constant filetool_xol           = filetool_switch_vec;

lvars
    allow_new_file      = true,
    file_chooser_done   = true,
    file_chosen         = false,
;

    ;;; the FileTool record allows for multiple instances of the tool
defclass lconstant FileTool {
    ;;; component widgets
    ftShell,
    ftDirectoryTextWidget,
    ftPatternTextWidget,
    ftFileTextWidget,
    ftInfoTextWidget,
    ftFooterTextWidget,
    ftFileListWidget,
    ftActionButtonWidget,
    ftFilterButtonWidget,
    ;;; parameters of most recent search
    ftSearchDirectory,
    ftSearchPattern,
    ftSearchModTime,
    ;;; most recent list selection (for spotting double-click)
    ftSelection,
    ftSelectionTime,
    ;;; default footer text
    ftDefaultMessage,
};
;;;
define lconstant newFileTool() -> tool;
    lvars tool = consFileTool(
        repeat #_<datalength(FileTool_key)>_# times false endrepeat
    );
    0 ->> ftSearchModTime(tool) -> ftSelectionTime(tool);
    nullstring -> ftDefaultMessage(tool);
enddefine;

    ;;; some abbreviations for tool resource access
define :inline lconstant FT_TITLE(tool);
    (XptVal (ftShell(tool))(XtN title:XptString))
enddefine;

define :inline lconstant FT_DIRECTORY(tool);
    (XptVal (ftDirectoryTextWidget(tool))(XtN string:XptString))
enddefine;

define :inline lconstant FT_PATTERN(tool);
    (XptVal (ftPatternTextWidget(tool))(XtN string:XptString))
enddefine;

define :inline lconstant FT_FILE(tool);
    (XptVal (ftFileTextWidget(tool))(XtN string:XptString))
enddefine;

define :inline lconstant FT_INFO(tool);
    (XptVal (ftInfoTextWidget(tool))(XtN string:XptString))
enddefine;

define :inline lconstant FT_FOOTER(tool);
    (XptVal (ftFooterTextWidget(tool))(XtN string:XptString))
enddefine;

define :inline lconstant FT_LABEL(tool);
    (XptVal (ftActionButtonWidget(tool))(XtN label:XptString))
enddefine;

define :inline lconstant FT_ACTION_BUSY(tool);
    (XptVal (ftActionButtonWidget(tool))(XtN busy:XptBoolean))
enddefine;

define :inline lconstant FT_FILTER_BUSY(tool);
    (XptVal (ftFilterButtonWidget(tool))(XtN busy:XptBoolean))
enddefine;

define lconstant set_footer(msg, tool);
    lvars msg, tool;
    msg -> FT_FOOTER(tool);
enddefine;

define lconstant clear_footer(tool);
    lvars tool;
    set_footer(ftDefaultMessage(tool), tool);
enddefine;

    ;;; update the file list in tool with files in directory matching
    ;;; pattern
define lconstant update_file_list(directory, pattern, tool);
    lvars directory, pattern, tool;
    procedure();    ;;; this procedure needed because of the dlocal
        dlocal % XptCursorPlaneOn(ftShell(tool)) % = true;
        dlocal XptBusyCursorOn = true, pop_pr_quotes = false;

        XptAppTryEvents(XptCurrentAppContext);
        XFlush(XptDefaultDisplay);

        lvars dir;
        if sys_dir_name(directory) ->> dir then
            lconstant statbuf = writeable initv(2);
            if sys_file_stat(dir, statbuf) then
                lvars modtime = statbuf(2);
                ;;; only do the search if the directory's changed
                unless dir = ftSearchDirectory(tool)
                and pattern = ftSearchPattern(tool)
                and modtime = ftSearchModTime(tool)
                then
                    true -> FT_FILTER_BUSY(tool);
                    XptAppTryEvents(XptCurrentAppContext);
                    nullstring -> FT_FILE(tool);
                    dir -> FT_DIRECTORY(tool);
                    lvars (files, nfiles, ndirs) = sys_get_files(dir, pattern);
                    XptAppTryEvents(XptCurrentAppContext);
                    false -> FT_FILTER_BUSY(tool);
#_IF XOL_VERSION < 3000
                    lvars len = ndirs + nfiles;
                    if len > 900 then
                        set_footer('Too many files - list truncated', tool);
                        allbutlast(len - 900, files) -> files;
                    endif;
#_ENDIF
                    ((ndirs == 0 and 'No') or ndirs) sys_><
                     ((ndirs == 1 and ' directory, ') or ' directories, ')
                        sys_><
                    ((nfiles == 0 and 'no') or nfiles) sys_><
                     ((nfiles == 1 and ' file') or ' files')
                        -> FT_INFO(tool);

                    files -> XpolListItems(ftFileListWidget(tool));
                    dir -> ftSearchDirectory(tool);
                    pattern -> ftSearchPattern(tool);
                    modtime -> ftSearchModTime(tool);
                endunless;
            else
                set_footer('Directory is unreadable', tool);
            endif;
        else
            set_footer('No such directory', tool);
        endif;
    endprocedure();
enddefine;

    ;;; respond to selection of file in tool
define lconstant select_file(file, tool);
    lvars file, tool;
    procedure();    ;;; this procedure needed because of the dlocal
        dlocal %XptCursorPlaneOn(ftShell(tool))% = true;
        lvars dir, orgdir = FT_DIRECTORY(tool);
        dlocal current_directory = orgdir;
        clear_footer(tool);
        if file = nullstring then
            set_footer('Please enter a filename', tool);
        elseif sys_dir_name(file) ->> dir then
            ;;; if given a directory then update the scrolling list
            update_file_list(dir, FT_PATTERN(tool), tool);
        else
            true -> FT_ACTION_BUSY(tool);
            XptAppTryEvents(XptCurrentAppContext);
            sys_fname_path(file) -> dir;
            if dir = nullstring then
                ;;; must be referring to the currently listed directory
                orgdir -> dir;
            endif;
            if sys_dir_name(dir) ->> dir then
                ;;; if given a valid directory then process the filename
                dir dir_>< sys_fname_namev(file) -> file;
                if allow_new_file or isfile(file) then
                    ;;; return the selected file
                    file ->> file_chosen -> file_chooser_done;
                else
                    ;;; new file not allowed
                    set_footer('No such file', tool);
                endif;
            else
                ;;; otherwise the pathname is illegal
                set_footer('No such directory', tool);
            endif;
            XptAppTryEvents(XptCurrentAppContext);
            false -> FT_ACTION_BUSY(tool);
        endif;
    endprocedure();
enddefine;

    ;;; callback for selection from file list: clientdata is the file tool
define lconstant list_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata, tool = clientdata;
    clear_footer(tool);
    calldata -> XpolCurrentListItem(widget);
    lvars selection = XpolListTokenToItem(calldata);
    if (is_doubleclick(XptDefaultDisplay, ftSelectionTime(tool))
                                                    -> ftSelectionTime(tool))
    and selection = ftSelection(tool)
    then
        false -> ftSelection(tool);
        select_file(selection, tool);
    else
        selection ->> ftSelection(tool) -> FT_FILE(tool);
    endif;
enddefine;

    ;;; callback for Action button: clientdata is the file tool
define lconstant action_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;
    select_file(FT_FILE(clientdata), clientdata);
enddefine;

    ;;; callback for Filter button: clientdata is the file tool
define lconstant filter_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;
    update_file_list(FT_DIRECTORY(clientdata), FT_PATTERN(clientdata),
        clientdata);
enddefine;

    ;;; callback for Cancel button (or any other termination condition)
define lconstant cancel_cb(widget, clientdata, calldata);
    lvars widget, clientdata, calldata;
    true -> file_chooser_done;
enddefine;

define :macexpr p_FILETOOL(parent, title, action, directory, pattern, file,
                           buffer, allow_new_file) -> choice;
    lvars parent, title, action, directory, pattern, file, buffer, choice;
    dlocal allow_new_file, XptBusyCursorOn = true;

    define lconstant cache =
        newproperty([], 8, false, "tmparg");
    enddefine;

    lvars tool;
    unless (cache(parent) ->> tool)
    and XptIsLiveType(ftShell(tool), "Widget")
    then
        ;;; create new
        newFileTool() ->> tool -> cache(parent);
        lvars shell = XtVaCreatePopupShell('fileTool',
            xtTransientShellWidget, parent, (#|
                XtN transientFor,   parent,
            |#));
        shell -> ftShell(tool);

        lvars dialog = XtVaCreateWidget('dialog',
            xolControlAreaWidget, shell, (#|
                XtN layoutType,     OL_FIXEDCOLS,
                XtN sameSize,       OL_NONE,
                XtN center,         true,
                XtN hPad,           10,
                XtN vPad,           10,
                XtN vSpace,         10,
            |#));

        ;;; user input (search parameters)
        lvars input_area = XtVaCreateWidget('inputArea',
            xolControlAreaWidget, dialog, (#|
                XtN layoutType,     OL_FIXEDCOLS,
                XtN alignCaptions,  true,
            |#));
        lvars file_caption = XtVaCreateManagedWidget('File:',
            xolCaptionWidget, input_area, XptVaArgList([{%
                XtN font,           'variable',
            %}]));
        lvars file_text = XtVaCreateManagedWidget('file',
            xolTextFieldWidget, file_caption, (#|
                XtN charsVisible,   24,
            |#));
        file_text -> ftFileTextWidget(tool);
        lvars pattern_caption = XtVaCreateManagedWidget('Filter:',
            xolCaptionWidget, input_area, XptVaArgList([{%
                XtN font,           'variable',
            %}]));
        lvars pattern_text = XtVaCreateManagedWidget('pattern',
            xolTextFieldWidget, pattern_caption, (#|
                XtN charsVisible,   24,
            |#));
        pattern_text -> ftPatternTextWidget(tool);
        XtManageChild(input_area);
        ;;; tool output (search results)
        lvars output_area = XtVaCreateWidget('outputArea',
            xolControlAreaWidget, dialog, (#|
                XtN layoutType,     OL_FIXEDCOLS,
            |#));
        lvars dir_text = XtVaCreateManagedWidget('directory',
            xolStaticTextWidget, output_area, (#|
            |#));
        dir_text -> ftDirectoryTextWidget(tool);
        lvars info_text = XtVaCreateManagedWidget('info',
            xolStaticTextWidget, output_area, (#|
            |#));
        info_text -> ftInfoTextWidget(tool);
        lvars file_list = XtVaCreateManagedWidget('list',
            xolScrollingListWidget, output_area, (#|
                XtN viewHeight,     8,
                XtN recomputeWidth, true,
                XtN selectable,     false,
            |#));
        file_list -> ftFileListWidget(tool);
        lvars footer_text = XtVaCreateManagedWidget('footer',
            xolStaticTextWidget, output_area, (#|
            |#));
        footer_text -> ftFooterTextWidget(tool);
        ;;; get file list to a reasonable initial size
        [% consstring(#| repeat 30 times `_` endrepeat |#) %]
            -> XpolListItems(file_list);
        ['../'] -> XpolListItems(file_list);
        XtManageChild(output_area);

        ;;; control area
        lvars control_area = XtVaCreateWidget('controlArea',
            xolControlAreaWidget, dialog, (#|
                XtN layoutType,     OL_FIXEDROWS,
                XtN hSpace,         25,
            |#));
        lvars action_button = XtVaCreateManagedWidget('________',
            xolOblongButtonWidget, control_area, (#|
                XtN default,        true,
            |#));
        action_button -> ftActionButtonWidget(tool);
        lvars filter_button = XtVaCreateManagedWidget('Filter',
            xolOblongButtonWidget, control_area, (#|
            |#));
        filter_button -> ftFilterButtonWidget(tool);
        lvars cancel_button = XtVaCreateManagedWidget('Cancel',
            xolOblongButtonWidget, control_area, (#|
            |#));
        XtManageChild(control_area);

        XtManageChild(dialog);

        XtAddCallback(file_text, XtN verification, action_cb, tool);
        XtAddCallback(pattern_text, XtN verification, filter_cb, tool);
        XtAddCallback(file_list, XtN userMakeCurrent, list_cb, tool);
        XtAddCallback(action_button, XtN select, action_cb, tool);
        XtAddCallback(filter_button, XtN select, filter_cb, tool);
        XtAddCallback(cancel_button, XtN select, cancel_cb, tool);
        XtAddCallback(shell, XtN popdownCallback, cancel_cb, tool);
        XtAddCallback(shell, XtN destroyCallback, cancel_cb, tool);

        XtRealizeWidget(shell);
        XptCenterWidgetOn(shell, "screen");

        ;;; set up cursors
        true ->> XptBusyCursorFeedback(shell)
             ->  XptGarbageCursorFeedback(shell);
        false -> XptCursorPlaneOn(shell);

        ;;; set up initial defaults
        unless title then
            'Poplog: ' <> (action or 'Choose') <> ' File' -> title;
        endunless;
        file_search_defaults(directory, pattern, file)
            -> (directory, pattern, file);
        if directory = nullstring then
            current_directory -> directory;
        endif;
        unless action then
            'OK' -> action;
        endunless;
    endunless;

    if title then
        title -> FT_TITLE(tool);
    endif;
    if action then
        action -> FT_LABEL(tool);
    endif;
    if directory and directory /= nullstring then
        directory -> FT_DIRECTORY(tool);
    endif;
    if pattern then
        pattern -> FT_PATTERN(tool);
    endif;
    if file then
        file -> FT_FILE(tool);
    endif;
    if buffer then
        'Current buffer: ' <> buffer
    else
        nullstring
    endif -> ftDefaultMessage(tool);

    clear_footer(tool);

    procedure() -> file_chosen;
        dlocal file_chooser_done = false, file_chosen = false;
        dlocal 0 %
            XtPopup(ftShell(tool), XtGrabExclusive),
            XtPopdown(ftShell(tool))
        %;
        ;;; do this after popup - give user something to look at
        update_file_list(FT_DIRECTORY(tool), FT_PATTERN(tool), tool);
        until file_chooser_done do
            syshibernate();
        enduntil;
    endprocedure() -> choice;
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 10 1995
        Fixed doubleclick test in list_cb so that the selection time is
        reset on each call and no double clicks get missed.
--- Robert John Duncan, May  4 1995
        Now allows multiple instances of the tool, cached against the parent
        widget. p_FILETOOL takes extra title and buffer arguments for extra
        customisation: if buffer is given, it's displayed as the default
        string in the message area.
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr  9 1993
        Uses Xol/xol*Widget instead of XptW*idgetSet
--- John Williams, Oct 13 1992
        Julian's new version installed at Sussex. Not tested.
--- Julian Clinton, Oct 13 1992
        Made list truncation specific to OLIT versions < 3000.
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- Adrian Howard, Aug 20 1992
        Set -pop_pr_quotes- false in appropriate places to stop quotes being placed
        around filenames.
--- Simon Nichols, Dec 13 1991
        Changed button label from 'Update' to 'Filter'.
Julian Clinton, 18/10/91
    Also sets busy cursor on when updating the file list.
Julian Clinton, 14/10/91
    Now sets busy cursor on during tool creation.
Julian Clinton, 11/10/91
    Changed -update_file_list- so that it now checks if the directory
    is readable.
    Changed callbacks which do not access their call args to remove them
    from the stack using -erasenum- or by directly calling another callback.
--- Integral Solutions Ltd, Sep 18 1991 (Julian Clinton)
    Changed directory modification test.
    Changed padding for action buttons.
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Centered shell widget on screen.
    Now only refreshes the list if a using different directory, file filter
    or if the directory has been updated since it was last looked at.
--- Jonathan Meyer, Sep  2 1991 Added stuff for busy/gc cursors
--- Jonathan Meyer, Aug 29 1991
        XpolDefaultSetup -> XptDefaultSetup
        Made use of XpolCurrentListItem so that multiple scrolling list
        items cannot be selected.
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
Julian Clinton, 19/8/91
    Files now selectable with a double mouse click on the list.
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
Julian Clinton, 13/8/91
    Set upper control area hPad and vPad to 10.
 */
