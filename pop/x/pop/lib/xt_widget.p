/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_widget.p
 > Purpose:         Widget creation, management, mapping etc
 > Author:          Adrian Howard, Aug  8 1990 (see revisions)
 > Documentation:   REF *XT_WIDGET
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p
 >                  C.x/x/pop/lib/fast_xt_widget.p
 */
compile_mode:pop11 +strict;

section;
exload_batch;

uses fast_xt_widget.p;     ;;; Get the fast versions of the procedures

include xpt_constants.ph;
include xpt_generaltypes.ph;


;;; Create a widget - 08/08/90
;;; Input - <String> <WidgetClass> <Widget> <ArgList> <Cardinal>
;;; Output - <Widget>
define XtCreateWidget() with_nargs 4;

    lvars ( name_string, widgetclass, widget,
            arglist, cardinal ) = XptCheckArgListAndCardinal();

    fast_XtCreateWidget(    XptCheckString(name_string),
                            XptCheckWidgetClass(widgetclass),
                            XptCheckWidget(widget),
                            arglist, cardinal ;;; already checked
                       );
enddefine;


;;; Create a widget (VarArgs option) - 08/08/90
;;; Input - <String> <WidgetClass> <Widget> <ArgVarargs>, Output - <Widget>
define XtVaCreateWidget(count);
    lvars count;
    XptCheckVarargs(count);
    XptCheckWidget(subscr_stack(count fi_+ 1)) ->;
    XptCheckWidgetClass(subscr_stack(count fi_+ 2)) ->;
    XptCheckString(subscr_stack(count fi_+ 3)) ->;
    fast_XtVaCreateWidget(count);
enddefine;


;;; Create an application shell instance - 08/08/90
;;; Input - <String> <String> <WidgetClass> <DisplayPtr> <ArgList> <Cardinal>
;;; Output - <Widget>
define XtAppCreateShell() with_nargs 5;

    lvars ( name_string, class_string, widgetclass, displayptr,
            arglist, cardinal ) = XptCheckArgListAndCardinal();

    fast_XtAppCreateShell(  if name_string then XptCheckString(name_string)
                            else false
                            endif,
                            XptCheckString(class_string),
                            XptCheckWidgetClass(widgetclass),
                            XptCheckDisplayPtr(displayptr),
                            arglist, cardinal ;;; already checked
                         );
enddefine;


;;; Create an application shell instance (varargs option) - 08/08/90
;;; Input - <String> <String> <WidgetClass> <DisplayPtr> <ArgVarargs>
;;; Output - <Widget>
define XtVaAppCreateShell(count);
    lvars count;
    XptCheckVarargs(count);
    XptCheckDisplayPtr(subscr_stack(count fi_+ 1)) -> ;
    XptCheckWidgetClass(subscr_stack(count fi_+ 2)) -> ;
    XptCheckString(subscr_stack(count fi_+ 3)) -> ;
    XptCheckString(subscr_stack(count fi_+ 4)) -> ;
    fast_XtVaAppCreateShell(count);
enddefine;


;;; Realize a widget instance - 08/08/90
;;; Input - <Widget>
define XtRealizeWidget() with_nargs 1;
    fast_XtRealizeWidget(XptCheckWidget());
enddefine;


;;; Destroy the windows associated with a widget and its children - 08/08/90
;;; Input - <Widget>
define XtUnrealizeWidget() with_nargs 1;
    fast_XtUnrealizeWidget(XptCheckWidget());
enddefine;


;;; Destroy a widget instance - 08/08/90
;;; Input - <Widget>
define XtDestroyWidget() with_nargs 1;
    fast_XtDestroyWidget(XptTypeCheck(XDT_WIDGET));
enddefine;


;;; Create and manage a child widget - 08/08/90
;;; Input - <String> <WidgetClass> <Widget> <ArgList> <Cardinal>
;;; Output - <Widget>
define XtCreateManagedWidget() with_nargs 4;

    lvars ( name, widgetclass, parent_widget,
            arglist, cardinal ) = XptCheckArgListAndCardinal();

    fast_XtCreateManagedWidget( XptCheckString(name),
                                XptCheckWidgetClass(widgetclass),
                                XptCheckWidget(parent_widget),
                                arglist, cardinal ;;; already checked
                              );
enddefine;


;;; Create and manage a child widget (varargs option) - 08/08/90
;;; Input - <String> <WidgetClass> <Widget> <ArgVarargs>, Output - <Widget>
define XtVaCreateManagedWidget(count);
    lvars count;
    XptCheckVarargs(count);
    XptCheckWidget(subscr_stack(count fi_+ 1)) ->;
    XptCheckWidgetClass(subscr_stack(count fi_+ 2)) ->;
    XptCheckString(subscr_stack(count fi_+ 3)) ->;
    fast_XtVaCreateManagedWidget(count);
enddefine;


;;; Explicitly map a widget - 08/08/90
;;; Input - <Widget>
define XtMapWidget() with_nargs 1;
    fast_XtMapWidget(XptCheckWidget());
enddefine;


;;; Explicitly unmap a widget - 08/08/90
;;; Input - <Widget>
define XtUnmapWidget() with_nargs 1;
    fast_XtUnmapWidget(XptCheckWidget());
enddefine;


;;; So uses works OK
constant xt_widget= true;


endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1991
        includes xpt_generaltypes.ph
--- John Gibson, Jul 18 1991
        Allowed XtAppCreateShell to take false for name string
--- Roger Evans, Jun 18 1991 added XptCheckArgListAndCardinal code
--- Jonathan Meyer, May 29 1991
        Made XtDestroyWidget accept dead widgets and live widgets
--- Roger Evans, Nov 19 1990 tidied up
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
