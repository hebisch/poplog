/* --- Copyright University of Sussex 1999.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-filetool_xm.p
 > Purpose:         Motif file tool
 > Author:          Julian Clinton, Aug 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now Xm;

section $-poplog_ui;

include sysdefs.ph;
include pop_uiP.ph;
include xt_constants.ph;
include XmConstants.ph;

uses
    xt_init,
    xt_widget,
    xt_callback,
    xt_event,
    xt_grab,
    xt_popup,
    xt_widgetinfo,
    xt_resource,
    xt_composite,
    xt_util,
    xpt_cursorplane,
    xmLabelWidget,
    xmTextFieldWidget,
    xmListWidget,
    xmFormWidget,
    xmFileSelectionBoxWidget,
    xmMessageBoxWidget,
    $-poplog_ui$-guiFileutils,
;

lconstant filetool_switch_vec   = INIT_filetool_switch_vec;
constant filetool_xm            = filetool_switch_vec;

lvars
    allow_new_file      = true,
    file_chooser_mapped = false,
    file_chosen         = false,
;

    ;;; callback for OK button
define lconstant ok_cb(w, clientdata, calldata);
    lvars w, clientdata, calldata;
    procedure();    ;;; this procedure needed because of the dlocal
        dlocal % XptCursorPlaneOn(w) % = true;
        l_typespec calldata :XmFileSelectionBoxCallbackStruct;
        lvars file = XpmCoerceString(exacc calldata.value);
#_IF DEF WIN32
        NutConvert(file) -> file;
#_ENDIF
        unless isabsolute_pathname(file) then
            ;;; relative to search directory -- expand to absolute path
            lvars dir = XpmCoerceString(exacc calldata.dir);
#_IF DEF WIN32
            NutConvert(dir) -> dir;
#_ENDIF
            dir dir_>< file -> file;
        endunless;
        if sysisdirectory(file) then
            ;;; search that directory
            sysfileok(file) -> XptVal w(XmN directory:XpmCopiedString);
            return;
        elseif sys_file_exists(file)
        or allow_new_file and sysisdirectory(sys_fname_path(file))
        then
            ;;; OK
            file -> file_chosen;
            XtUnmanageChild(w);
        else
            lconstant msgTitle = 'File Selection Error';
            lvars msgText = 'File not found:\n' <> file;
            lvars msg = XmCreateErrorDialog(w, 'fileToolError', ConsArgList(#|
                XmN dialogTitle,        ->XpmCoerceCopiedString(msgTitle),
                XmN dialogStyle,        XmDIALOG_PRIMARY_APPLICATION_MODAL,
                XmN noResize,           true,
                XmN messageAlignment,   XmALIGNMENT_CENTER,
                XmN messageString,      ->XpmCoerceCopiedString(msgText),
            |#));
            ;;; leave just the OK button
            XtUnmanageChild(XmMessageBoxGetChild(msg, XmDIALOG_HELP_BUTTON));
            XtUnmanageChild(XmMessageBoxGetChild(msg, XmDIALOG_CANCEL_BUTTON));
            XtManageChild(msg);
        endif;
    endprocedure();
enddefine;

    ;;; callback for Cancel button
define lconstant cancel_cb(w, clientdata, calldata);
    lvars w, clientdata, calldata;
    XtUnmanageChild(w);
enddefine;

    ;;; callback for Unmap, including after XtUnmanageChild
define lconstant unmap_cb(w, clientdata, calldata);
    lvars w, clientdata, calldata;
    false -> file_chooser_mapped;
enddefine;

    ;;; callback for destroy
define lconstant destroy_cb(w, clientdata, calldata);
    lvars w, clientdata, calldata;
    false -> file_chooser_mapped;
enddefine;

define :macexpr p_FILETOOL(parent, title, action, directory, pattern, file,
                           buffer, allow_new_file) -> choice;
    lvars parent, title, action, directory, pattern, file, buffer, choice;
    dlocal allow_new_file, XptBusyCursorOn = true;

    define lconstant cache =
        newproperty([], 8, false, "tmparg");
    enddefine;

    lvars box;
    unless XptIsLiveType(cache(parent) ->> box, "Widget") then
        ;;; create new
        XmCreateFileSelectionDialog(parent, 'fileTool', ConsArgList(#|
            XmN dialogStyle,            XmDIALOG_FULL_APPLICATION_MODAL,
            XmN resizePolicy,           XmRESIZE_GROW,
            XmN listVisibleItemCount,   10,
        |#)) ->> box -> cache(parent);
        ;;; remove the Help button
        XtUnmanageChild(XmFileSelectionBoxGetChild(box, XmDIALOG_HELP_BUTTON));
        ;;; add another text field to display the current Ved buffer
        ;;; name, though initially unmanaged
        lvars work_area = XtVaCreateWidget('workArea', xmFormWidget, box, (#|
            |#));
        lvars buffer_label = XtVaCreateManagedWidget('bufferLabel',
            xmLabelWidget, work_area, (#|
                XmN labelString,        ->XpmCoerceCopiedString('Current Buffer'),
                XmN alignment,          XmALIGNMENT_BEGINNING,
                XmN topAttachment,      XmATTACH_FORM,
                XmN leftAttachment,     XmATTACH_FORM,
            |#));
        lvars buffer_text = XtVaCreateManagedWidget('bufferText',
            xmTextFieldWidget, work_area, (#|
                XmN cursorPositionVisible, false,
                XmN editable,           false,
                XmN traversalOn,        false,
                XmN topAttachment,      XmATTACH_WIDGET,
                XmN topWidget,          buffer_label,
                XmN topOffset,          0,
                XmN bottomAttachment,   XmATTACH_FORM,
                XmN leftAttachment,     XmATTACH_FORM,
                XmN rightAttachment,    XmATTACH_FORM,
            |#));
        ;;; add callbacks
        XtAddCallback(box, XmN okCallback, ok_cb, false);
        XtAddCallback(box, XmN cancelCallback, cancel_cb, false);
        XtAddCallback(box, XmN unmapCallback, unmap_cb, false);
        XtAddCallback(box, XmN destroyCallback, destroy_cb, false);
        ;;; subsequent code needs the dialog to be realized (though not
        ;;; yet mapped)
        XtRealizeWidget(box);
        ;;; enable cursors
        true ->> XptBusyCursorFeedback(box) -> XptGarbageCursorFeedback(box);
        ;;; ... but only when we want them
        false -> XptCursorPlaneOn(box);
        if parent == pop_ui_app_shell then
            ;;; no visible reference point, so give it an explicit initial
            ;;; position in the centre of the screen
            false -> XptVal box(XmN defaultPosition:XptBoolean);
            XptCenterWidgetOn(box, "screen");
        endif;
        ;;; set initial defaults
        unless title then
            'Poplog: ' <> (action or 'Choose') <> ' File' -> title;
        endunless;
        file_search_defaults(directory, pattern, file)
            -> (directory, pattern, file);
    endunless;

    if title then
        title -> XptVal box(XmN dialogTitle:XpmCopiedString);
    endif;
    if action then
        lvars button = XmFileSelectionBoxGetChild(box, XmDIALOG_OK_BUTTON);
        action -> XptVal button(XmN labelString:XpmCopiedString);
    endif;
    if directory and directory /= nullstring then
        sysfileok(directory) -> XptVal box(XmN directory:XpmCopiedString);
    endif;
    if pattern then
        pattern -> XptVal box(XmN pattern:XpmCopiedString);
    endif;
    if file = nullstring then
        ;;; no initial selection
        XmListDeselectAllItems(XmFileSelectionBoxGetChild(box, XmDIALOG_LIST));
        XmStringFree(XptVal box(XmN directory:XmString) ->> XptVal box(XmN dirSpec:XmString));
    elseif file then
        file -> XptVal box(XmN dirSpec:XpmCopiedString);
    endif;

#_IF not(DEF LINUX)
    if buffer then
        ;;; show buffer name
        XtVaSetValues(XtNameToWidget(box, '*bufferText'), (#|
            XmN value,          buffer,
            XmN cursorPosition, datalength(buffer),
        |#));
        XtManageChild(XmFileSelectionBoxGetChild(box, XmDIALOG_WORK_AREA));
    else
        ;;; don't!
        XtUnmanageChild(XmFileSelectionBoxGetChild(box, XmDIALOG_WORK_AREA));
    endif;
#_ENDIF

    procedure() -> file_chosen;
        dlocal file_chooser_mapped = true, file_chosen = false;
        dlocal 0 % XtManageChild(box), XtUnmanageChild(box) %;
        while file_chooser_mapped do
            syshibernate();
        endwhile;
    endprocedure() -> choice;
enddefine;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Nov  6 1999
        On advice from Julian clinton inserted #_IF not(DEF LINUX) because
        LessTif doesn't seem to have XmDIALOG_WORK_AREA in the file widget
        Should really test for Lesstif.

--- Robert Duncan, May 30 1996
        Added Win32 code (used in the NuTCRACKER environment) for converting
        NuTCRACKER filenames to proper Windows format
--- Robert John Duncan, Jun 22 1995
        Fixed to use XpmCoerceCopiedString
--- Robert John Duncan, May  4 1995
        Now allows multiple instances of the tool, cached against the parent
        widget. p_FILETOOL takes extra title and buffer arguments for extra
        customisation: if buffer is given, it's displayed in an additional
        text field in the window.
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr 16 1993
        Uses Xm/xm*Widget etc.
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- Integral Solutions Ltd, Jun  2 1992 (Julian Clinton)
    Improved behaviour of Motif FileTool:
        - entering a relative directory now displays that directory
          rather than returning the directory as a file (see isl-fr.4447)
        - added call to isabsolute_pathname (defined in guiFileutils)
        - a couple of other minor modifications (relocating the conversion
          of search_directory to an XmString and updating the widget)
        - filetool now resizable by the user
    Changed refs of -isdirectory- to -sysisdirectory-.
--- Simon Nichols, Feb 10 1992
        Inserted Julian's fix from bugreport isl-fr.4408.
--- Simon Nichols, Dec 13 1991
        Fixed -xmfiletool- to pass directory argument to the Motif file
        selection widget (fix courtesy of Julian Clinton).
Julian Clinton, 10/12/91
    Set XptWMProtocols -false-, modified deleteResponse and removed shell
    popdown callback.
Julian Clinton, 14/11/91
    Now sets busy cursor on during tool creation.
    Removed call to -XmFileSelectionDoSearch- in filetool_action_cb.
Julian Clinton, Tom Khabaza, 11/10/91
    If a filename is given, the search directory is added to the
    start of it when the selection is added to the XmDIALOG_TEXT field.
    Modified behaviour so that if the directory path of the file selection
    field is empty, the XmNdirectory resource is accessed.
--- Jonathan Meyer, Sep 16 1991 Erased argument left on stack by
        XmProcessTraversal
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Centered shell widget on screen.
--- Jonathan Meyer, Sep  2 1991 Added stuff for busy/gc cursors
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Removed popmemlim increment (now done in pop_ui_popcontroltool.p).
    File tool displays any file name passed as a parameter.
    Changed to use XmDIALOG_FULL_APPLICATION_MODAL instead of
    XmDIALOG_SYSTEM_MODAL.
Julian Clinton, 16/8/91
    Modified so that simply giving the file name without a path adds the
    currently listed directory path to the front.
    Changed to re-use existing fileselection box.
    Updates directory listing when first popped up.
Julian Clinton, 14/8/91
    Altered so that entering a directory pathname into the selection
    field now selects that directory.
    Now always sets mustMatch to <false> and modified callbacks to
    allow files in other directories to be selected directly rather
    having to select the directory first.
Julian Clinton, 13/8/91
    Changed to use guiShells and pop_ui_app_shell.
 */
