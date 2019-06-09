/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_widgetinfo.p
 > Purpose:         Fast routines that give info on the status of the widget
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XT_WIDGETINFO
 > Related Files:   C.x/x/pop/lib/xt_widgetinfo.p
 */

compile_mode:pop11 +strict;

section;

include xpt_coretypes.ph;
include xpt_xtypes.ph;

;;; Load the external "raw" procedures
XptLoadProcedures 'xt_widgetinfo'
    lvars
        XtIsRealized
        XtScreen
        XtWindow
        XtScreenOfObject
        XtWindowOfObject
        XtName
        XtParent
;

;;; Returns -true- if widget has been realized - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define global fast_XtIsRealized() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsRealized();
enddefine;

;;; Returns the screen pointer for the specified widget - 17/07/90
;;; Input - <Widget>, Output - <ScreenPtr>
define global fast_XtScreen() with_nargs 1;
    exacc (1):XptScreenPtr raw_XtScreen();
enddefine;

;;; Returns the screen pointer for the object if it's a widget, otherwise the
;;; screen pointer of its nearest widget ancestor - 17/07/90
;;; Input - <Widget>, Output - <ScreenPtr>
define global fast_XtScreenOfObject() with_nargs 1;
    exacc (1):XptScreenPtr raw_XtScreenOfObject();
enddefine;

;;; Return the window of the specified widget - 27/07/90
;;; Input - <Widget>, Output - <Window>
define global fast_XtWindow() with_nargs 1;
    exacc (1):XptWindow raw_XtWindow();
enddefine;

;;; Returns the window of the object if it's a widget, otherwise the
;;; window of its nearest widget ancestor - 27/07/90
;;; Input - <Widget>, Output - <Window>
define global fast_XtWindowOfObject() with_nargs 1;
    exacc (1):XptWindow raw_XtWindowOfObject();
enddefine;

;;; Returns a pointer to the instance name of an object - 31/08/90
;;; Input - <Widget>, Output - <String>
define global fast_XtName() with_nargs 1;
    exacc (1):exptr.exacc_ntstring raw_XtName();
enddefine;

;;; Returns the parent of a specified widget - 31/08/90
;;; Input - <Widget>, Output - <Widget | FALSE>
define global fast_XtParent() with_nargs 1;
    exacc (1):XptWidget raw_XtParent();
enddefine;

;;; So uses works OK
constant fast_xt_widgetinfo = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Aug 25 1992
        -fast_XtDisplayOfObject- made an autoloadable so it can be used in
        LIB *FAST_XT_APPINIT
--- John Gibson, Nov  3 1991
        includes xpt_coretypes.ph
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Feb  7 1991 change duse of XPtString to explicit
        exacc_ntstring
--- Roger Evans, Nov 19 1990 tidied up
--- Roger Evans, Oct 19 1990 changed to use XptLoadProcedures
--- Roger Evans, Oct 11 1990 changed to use exacc
--- Adrian Howard, Sep 13 1990 : Removed request for raw_XtDisplay. Also
            removed a bug-fix in -fast_XtParent- made redundant by changes
            to the XptImport procedures
--- James Goodlet, Sep 13 1990 - removed fast_XtDisplay - now in system.
--- James Goodlet, Sep 12 1990 - all functions made "global" to export from
        top level section.
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  6 1990 : Altered calls to XptCoerce/Import to make
        compatable with format of new external conversion/access procs
--- Adrian Howard, Aug 31 1990 : Added XtName and XtParent on RobDu's request
 */
