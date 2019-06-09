/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_widgetclass.p
 > Purpose:         Fast versions of various routines related to the class
 >                  and type of widgets
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XT_WIDGETCLASS
 > Related Files:   C.x/x/pop/lib/xt_widgetclass.p
 */
compile_mode:pop11 +strict;

section;
exload_batch;

include xpt_coretypes.ph;

;;; Load the external "raw" procedures
XptLoadProcedures fast_xt_widgetclass
    lvars
        XtClass
        XtSuperclass
        XtIsObject
        XtIsRectObj
        XtIsComposite
        XtIsConstraint
        XtIsOverrideShell
        XtIsWMShell
        XtIsVendorShell
        XtIsTransientShell
        XtIsTopLevelShell
        XtIsApplicationShell
;

;;; Returns the class of a widget - 17/07/90
;;; Input - <Widget>, Output - <WidgetClass>
define fast_XtClass() with_nargs 1;
    exacc (1):XptWidgetClass raw_XtClass();
enddefine;

;;; Returns the superclass of a widget - 17/07/90
;;; Input - <Widget>, Output - <WidgetClass>
define fast_XtSuperclass() with_nargs 1;
    exacc (1):XptWidgetClass raw_XtSuperclass();
enddefine;

;;; Returns -true- if widget is a subclass of the Object class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define fast_XtIsObject() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsObject();
enddefine;

;;; Returns -true- if widget is a subclass of the RectObj class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define fast_XtIsRectObj() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsRectObj();
enddefine;

;;; Returns -true- if widget is a subclass of the Composite class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define fast_XtIsComposite() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsComposite();
enddefine;

;;; Returns -true- if widget is a subclass of the Constraint class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define fast_XtIsConstraint() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsConstraint();
enddefine;

;;; Returns -true- if widget is a subclass of the Shell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
/* (Now in the system)
define fast_XtIsShell() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsShell();
enddefine;
*/

;;; Returns -true- if widget is a subclass of OverrideShell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define fast_XtIsOverrideShell() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsOverrideShell();
enddefine;

;;; Returns -true- if widget is a subclass of WMShell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define fast_XtIsWMShell() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsWMShell();
enddefine;

;;; Returns -true- if widget is a subclass of TransientShell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define fast_XtIsTransientShell() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsTransientShell();
enddefine;

;;; Returns -true- if widget is a subclass of TopLevelShell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define fast_XtIsTopLevelShell() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsTopLevelShell();
enddefine;

;;; Returns -true- if widget a subclass of ApplicationShell class - 17/07/90
;;; Input - <Widget>, Output - <BOOL>
define fast_XtIsApplicationShell() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsApplicationShell();
enddefine;

;;; Returns -true- if widget a subclass of VendorShell class - 09/08/90
;;; Input - <Widget>, Output - <BOOL>
define fast_XtIsVendorShell() with_nargs 1;
    exacc (1):XptBoolean raw_XtIsVendorShell();
enddefine;

constant fast_xt_widgetclass= true;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Nov 10 1997
        Moved fast_X*tIsShell into the system
--- John Gibson, Apr 14 1993
        Made fast_XtIsSubclass autoloadable
--- Adrian Howard, May 28 1992
        Shifted -fast_XtIsWidget- into an autoloadable
--- John Gibson, Nov  3 1991
        includes xpt_coretypes.ph
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Nov 19 1990 tidied up
--- Roger Evans, Oct 19 1990 changed to use XptLoadProcedure
--- Roger Evans, Oct 14 1990 fixed bug in XtClass, removed XptExternalPtr

--- Roger Evans, Oct 11 1990 changed to use exacc
--- James Goodlet, Sep 12 1990 - made definitions global.
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  6 1990 : Altered calls to XptCoerce/Import to make
        compatable with format of new external conversion/access procs
 */
