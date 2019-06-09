/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/src/xt_init.p
 > Purpose:         X Toolkit - initialising the toolkit
 > Author:          Roger Evans, Jul  5 1988 (see revisions)
 > Documentation:   REF xt_procs
 > Related Files:   xt_*.p
 */

#_INCLUDE 'xt_declare.ph'


section $-Sys$-Xt =>
                        fast_XptToolkitPreInitialize
                        fast_XptToolkitPostInitialize
                        fast_XptCustomInitialize,
                        fast_XtToolkitInitialize,
                        fast_XtInitializeWidgetClass,
                        XptToolkitInitialize
;


;;; initialising the pop-specific aspects of the interface
;;; two routines - one to precede toolkit initialisation, one to
;;; follow it

lvars ToolkitPreInitDone = false;
;;;
define fast_XptToolkitPreInitialize();
    unless ToolkitPreInitDone then
        if pop_runtime then
            ;;; say X init run
            "x" -> pop_runtime
        else
            mishap(0, 'CAN\'T INITIALISE X TOOLKIT WHEN pop_runtime IS FALSE')
        endif;
        X_apply(0, EXTERN_PTR(PopXtDefaultLanguageProc), 0, _3,
                                _extern XtSetLanguageProc) -> ;
        Init_appcon();      ;;; xt_appcon.p
        Init_error();       ;;; xt_error.p
        Init_popup();       ;;; xt_popup.p
        Init_widget();      ;;; xt_widget.p
        true -> ToolkitPreInitDone;
    endunless;
enddefine;

lvars ToolkitPostInitDone = false;
;;;
define fast_XptToolkitPostInitialize;
    unless ToolkitPostInitDone then
        true -> ToolkitPostInitDone;
    endunless;
enddefine;

lvars ToolkitInitDone = false;
;;;
define fast_XptCustomInitialize(proc);
    lvars proc;
    unless ToolkitInitDone then
        fast_XptToolkitPreInitialize();
        proc();
        fast_XptToolkitPostInitialize();
        true -> ToolkitInitDone;
    endunless;
enddefine;

    /*  Declared incremental in xt_declare.ph */
protected
define vars XptToolkitInitialize();
    X_apply(_0, _extern XtToolkitInitialize) -> ;
enddefine;

define fast_XtToolkitInitialize();

    fast_XptCustomInitialize(XptToolkitInitialize)
enddefine;


define fast_XtInitializeWidgetClass() with_nargs 1;
    X_apply(_1, _extern XtInitializeWidgetClass) -> ;
enddefine;

/*
 *  Use the 3 XLINK_ identifiers defined automatically by poplink (this
 *  makes their declarations available for extraction by popc).
 */
constant $-XLINK_TYPE;
vars $-XLINK_EXLIBDIRS, $-XLINK_EXLIBFILES;

uses (XLINK_TYPE, XLINK_EXLIBDIRS, XLINK_EXLIBFILES);

/*
 *  A dummy exload to incorporate dummy env vars into the link process
 *  (thus forcing the 3 XLINK_ identifiers above to be defined)
 */
exload xt_init ['==POP_XLINK_EXLIBDIRS' '==POP_XLINK_EXLIBFILES']
    ;;; anything will do
    lconstant dummy;
endexload;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  2 1997
        Added call to XtSetLanguageProc in fast_XptToolkitPreInitialize
--- John Gibson, Jun  8 1993
        Added uses for the XLINK_ identifiers
--- John Gibson, May  9 1993
        Added dummy exload
--- John Gibson, Apr  9 1993
        Added vars procedure XptToolkitInitialize to call
         _extern XtToolkitInitialize, and made fast_XtToolkitInitialize
        use that instead.
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- John Gibson, Dec 11 1991
        Made fast_XptToolkitPreInitialize assign "x" to pop_runtime
        when it's true
--- John Gibson, Dec  9 1991
        Added check for pop_runtime in fast_XptToolkitPreInitialize
--- Roger Evans, Jun 28 1991 removed Init_action, Init_timeout, Init_input
--- Roger Evans, Jun 25 1991 added Init_widget
--- Roger Evans, Feb  5 1991 added fast_XptCustomInitialize
--- Roger Evans, Nov 21 1990 added Init_appcon
--- Roger Evans, Nov  4 1990
    added Init_action
    exported pre and post initialize procedures
--- Roger Evans, Oct 11 1990 Much revised
--- Roger Evans, Jul  4 1990 changed to use X_apply
--- Roger Evans, Jun 23 1990 changed to use Cons_widgetclass
 */
