/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_popup.p
 > Purpose:         Management of popup widgets
 > Author:          Adrian Howard, Aug  8 1990 (see revisions)
 > Documentation:   REF *XT_POPUP
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p
 >                  C.x/x/pop/lib/fast_xt_popup.p
 */

compile_mode:pop11 +strict;

section;

uses xpt_typecheck.p;        ;;; Get the type checking routines
uses fast_xt_popup.p;       ;;; Get the fast versions of the procedures

include xpt_generaltypes.ph;


;;; Creation of a popup shell - 08/08/90
;;; Input - <String> <WidgetClass> <Widget> <ArgList> <Cardinal>
;;; Output - <Widget>
define global XtCreatePopupShell() with_nargs 4;

    lvars ( name_string, widgetclass, parent_widget,
            arglist, cardinal ) = XptCheckArgListAndCardinal();

    fast_XtCreatePopupShell(    XptCheckString(name_string),
                                XptCheckWidgetClass(widgetclass),
                                XptCheckWidget(parent_widget),
                                arglist, cardinal ;;; already checked
                           );
enddefine;


;;; Creation of a popup shell (varargs version) - 08/08/90
;;; Input - <String> <WidgetClass> <Widget> <ArgVarargs>, Output - <Widget>
define global XtVaCreatePopupShell(count);
    lvars count;
    XptCheckVarargs(count);
    XptCheckWidget(subscr_stack(count fi_+ 1)) ->;
    XptCheckWidgetClass(subscr_stack(count fi_+ 2)) ->;
    XptCheckString(subscr_stack(count fi_+ 3)) ->;
    fast_XtVaCreatePopupShell(count);
enddefine;


;;; Map a popup shell from within an application - 08/08/90
;;; Input - <Widget> <XtGrabKind>
define global XtPopup(popup_shell, grabkind);
    lvars popup_shell, grabkind;
    fast_XtPopup(   XptCheckWidget(popup_shell),
                    XptCheckGrabKind(grabkind)
                );
enddefine;


;;; Map a spring-loaded popup shell from within an application - 08/08/90
;;; Input - <Widget>
define global XtPopupSpringLoaded() with_nargs 1;
    fast_XtPopupSpringLoaded(XptCheckWidget());
enddefine;


;;; Convenience callback procedure to map a pop-up with XtGrabNone - 08/08/90
;;; Input - <Widget> <XtPointer> <XtPointer>
define global XtCallbackNone(widget, xtpointer_to_popup, call_data);
    lvars widget, xtpointer_to_popup, call_data;
    fast_XtCallbackNone(    XptCheckWidget(widget),
                            xtpointer_to_popup,
                            call_data
                       );
enddefine;


;;; Convenience callback procedure to map a pop-up with XtGrabNonexclusive
;;; - 08/08/90
;;; Input - <Widget> <XtPointer> <XtPointer>
define global XtCallbackNonexclusive(widget, xtpointer_to_popup, call_data);
    lvars widget, xtpointer_to_popup, call_data;
    fast_XtCallbackNonexclusive(    XptCheckWidget(widget),
                                    xtpointer_to_popup,
                                    call_data
                               );
enddefine;


;;; Convenience callback procedure to map a pop-up with XtGrabExclusive
;;; - 08/08/90
;;; Input - <Widget> <XtPointer> <XtPointer>
define global XtCallbackExclusive(widget, xtpointer_to_popup, call_data);
    lvars widget, xtpointer_to_popup, call_data;
    fast_XtCallbackExclusive(   XptCheckWidget(widget),
                                xtpointer_to_popup,
                                call_data
                            );
enddefine;


;;; Pop down a pop-up that has been popped up with one of the above callback
;;; procedures (wow!) - 08/08/90
;;; Input - <Widget> <XtPointer> <XtPointer>
define global XtCallbackPopdown(widget, xtpointer_to_popdownid, call_data);
    lvars widget, xtpointer_to_popdownid, call_data;
    fast_XtCallbackPopdown(   XptCheckWidget(widget),
                              xtpointer_to_popdownid,
                              call_data
                          );
enddefine;


;;; Unmap a pop-up from within an application - 08/08/90
;;; Input - <Widget>
define global XtPopdown() with_nargs 1;
    fast_XtPopdown(XptCheckWidget());
enddefine;


;;; So uses works OK
constant xt_popup= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1991
        includes xpt_generaltypes.ph
--- Roger Evans, Jun 18 1991 added XptCheckArgListAndCardinal code
--- Roger Evans, Nov 19 1990 added xpt_generaltypes
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
