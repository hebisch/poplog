/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-guiShells.p
 > Purpose:         Poplog UI shell utilities
 > Author:          Julian Clinton, August 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui;

exload_batch

include pop_uiP.ph;

uses xt_init, xt_widget, xt_widgetclass;

define guiRealizeWidget(w, del_response);
    lvars w, del_response;
    dlocal XptWMProtocols = false;
    XtRealizeWidget(w);
    if testdef popxlink_motif then
        del_response -> XptVal[fast] (XptShellOfObject(w))(XtN deleteResponse)
    else
        ;;; OLIT
        if del_response == Xm_DESTROY then
            XtDestroyWidget
        elseif del_response == Xm_UNMAP then
            XtUnmapWidget
        else
            ;;; Xm_DO_NOTHING
            erase
        endif -> XptShellDeleteResponse(w)
    endif
enddefine;

    ;;; hidden Application Shell providing a parent for all UI tools
    ;;; and dialogs; resources can be specified as:
    ;;;     Poplog.tool.resource: value
lvars app_shell = false;
define active pop_ui_app_shell;
    unless XptIsLiveType(app_shell, "Widget") then
        XptDefaultSetup();
        XtVaAppCreateShell('popUI', 'Poplog', xtApplicationShellWidget,
            XptDefaultDisplay, (#|
                XtN width,              1,
                XtN height,             1,
                XtN mappedWhenManaged,  false,
            |#)) -> app_shell;
        XtRealizeWidget(app_shell);
    endunless;
    app_shell;
enddefine;
;;;
;;; do we ever want to update?
define updaterof active pop_ui_app_shell(shell);
    lvars shell;
    XptLiveTypeCheck(shell, "Widget") -> shell;
    unless XtIsApplicationShell(shell) then
        mishap(shell, 1, 'Application Shell needed');
    endunless;
    shell -> app_shell;
enddefine;

constant guiShells = true;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Apr 10 1995
        Got rid of everything except the application shell
--- John Gibson, Feb  3 1994
        Set OLIT shell delete response in guiRealizeWidget
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr  6 1993
        Uses xt widgetclass libraries instead of XptW*idgetSet
Julian Clinton, 21/10/91
    Shells are now no longer centered on screen automatically.
Julian Clinton, 14/10/91
    Modified name strings.
--- Integral Solutions Ltd, Sep 18 1991 (Julian Clinton)
    Removed setting of x and y resources to 0.
--- Integral Solutions Ltd, Sep 12 1991 (Julian Clinton)
    Centered shell widget on screen.
--- Jonathan Meyer, Aug 29 1991
        removed test and call of XpolDefaultSetup.
 */
