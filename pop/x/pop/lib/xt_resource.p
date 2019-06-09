/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_resource.p
 > Purpose:         Resource Management
 > Author:          Adrian Howard, Aug  9 1990 (see revisions)
 > Documentation:   REF *XT_RESOURCE
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p
 >                  C.x/x/pop/lib/fast_xt_resource.p
 */

compile_mode:pop11 +strict;

section;

include xpt_constants.ph
include xpt_generaltypes.ph;

uses xpt_typecheck.p;        ;;; Get the type checking routines
uses fast_xt_resource.p;    ;;; Get the fast versions of the procedures

shadowclass XptResourcePtr {:XptResource};
shadowclass XptResourceList #_< [props ^XDT_RESOURCELIST] >_#
    :XptResource[];

;;; Get the resource list structure for a particular class - 09/08/90
;;; Input - <WidgetClass> <XtResourceListPtr> <CardinalPtr>
define XtGetResourceList(widgetclass, resourcelistptr, cardinalptr);
    lvars widgetclass, resourcelistptr, cardinalptr;
    fast_XtGetResourceList( XptCheckWidgetClass(widgetclass),
                            XptCheckResourceListPtr(resourcelistptr),
                            XptCheckCardinalPtr(cardinalptr)
                          );
enddefine;


;;; Get the constraint resource list structure for a widget class - 09/08/90
;;; Input - <Widget> <XtResourceListPtr> <CardinalPtr>
define XtGetConstraintResourceList(widgetclass, resourcelistptr,
            cardinalptr);
    lvars widgetclass, resourcelistptr, cardinalptr;
    fast_XtGetConstraintResourceList(XptCheckWidgetClass(widgetclass),
                                     XptCheckResourceListPtr(resourcelistptr),
                                     XptCheckCardinalPtr(cardinalptr)
                                    );
enddefine;


;;; Get the overall resources of an application - 09/08/90
;;; Input - <Widget> <XtPointer> <XtResourceList> <Cardinal> <ArgList>
;;; <Cardinal>
define XtGetApplicationResources() with_nargs 5;

    lvars ( widget, base_xtpointer, resourcelist,
            resource_cardinal, arglist, argc ) = XptCheckArgListAndCardinal();

    fast_XtGetApplicationResources( XptCheckWidget(widget),
                                    base_xtpointer,
                                    XptCheckResourceListAndLength(resourcelist, resource_cardinal),
                                    arglist, argc ;;; already checked
                                  );
enddefine;


;;; Get the overall resources of an application (varargs version) - 09/08/90
;;; Input - <Widget> <XtPointer> <XtResourceList> <Cardinal> <ArgVarargs>
define XtVaGetApplicationResources(count);
    lvars count;
    XptCheckVarargs(count);
    XptCheckResourceListAndLength(
        subscr_stack(count fi_+ 2),
        subscr_stack(count fi_+ 2)
    ) -> ->;
    XptCheckWidget(subscr_stack(count fi_+ 4)) ->;
    fast_XtVaGetApplicationResources(count);
enddefine;


;;; Get the values of a widgets resources - 09/08/90
;;; Input - <Widget> <ArgList> <Cardinal>
define XtGetValues() with_nargs 2;

    lvars ( widget, arglist, cardinal) = XptCheckArgListAndCardinal();

    fast_XtGetValues(   XptCheckWidget(widget),
                        arglist, cardinal ;;; already checked
                    );
enddefine;


;;; Get the values of a widgets resources (varargs version) - 09/08/90
;;; Input - <Widget> <ArgVarargs>
define XtVaGetValues(count);
    lvars count;
    XptCheckVarargs(count);
    XptCheckWidget(subscr_stack(count fi_+ 1)) ->;
    fast_XtVaGetValues(count);
enddefine;


;;; Get the values of non-widget resource data - 09/08/90
;;; Input - <XtPointer> <XtResourceList> <Cardinal> <ArgList> <Cardinal>
define XtGetSubvalues() with_nargs 4;

    lvars ( base_xtpointer, resourcelist, cardinal,
            arglist, argc ) = XptCheckArgListAndCardinal();

    fast_XtGetSubvalues(    base_xtpointer,
                            XptCheckResourceListAndLength(resourcelist, cardinal),
                            arglist, argc   ;;; already checked
                       );
enddefine;


;;; Get the values of non-widget resource data (varargs version) - 09/08/90
;;; Input - <XtPointer> <XtResourceList> <Cardinal> <ArgVarargs>
define XtVaGetSubvalues(count);
    lvars count;
    XptCheckVarargs(count);
    XptCheckResourceListAndLength(
        subscr_stack(count fi_+ 2),
        subscr_stack(count fi_+ 2)
    ) -> ->;
    fast_XtVaGetSubvalues(count);
enddefine;


;;; Modify a widgets resource values - 09/08/90
;;; Input - <Widget> <ArgList> <Cardinal>
define XtSetValues() with_nargs 3;

    lvars (widget, arglist, cardinal) = XptCheckArgListAndCardinal();

    fast_XtSetValues(   XptCheckWidget(widget),
                        arglist, cardinal ;;; already checked
                    );
enddefine;


;;; Modify a widgets resource values (varargs version) - 09/08/90
;;; Input - <Widget> <ArgVarargs>
define XtVaSetValues(count);
    lvars count;
    XptCheckVarargs(count);
    XptCheckWidget(subscr_stack(count fi_+ 1)) ->;
    fast_XtVaSetValues(count);
enddefine;


;;; Modify a widgets non-widget resources (!) - 09/08/90
;;; Input - <XtPointer> <XtResourceList> <Cardinal> <ArgList> <Cardinal>
define XtSetSubvalues()  with_nargs 4;

    lvars ( base_xtpointer, resourcelist, cardinal,
            arglist, argc ) = XptCheckArgListAndCardinal();

    fast_XtSetSubvalues(    base_xtpointer,
                            XptCheckResourceListAndLength(resourcelist, cardinal),
                            arglist, argc ;;; already checked
                       );
enddefine;


;;; Modify a widgets non-widget resources (!) [varargs version] - 09/08/90
;;; Input - <XtPointer> <XtResourceList> <Cardinal> <ArgVarargs>
define XtVaSetSubvalues(count);
    lvars count;
    XptCheckVarargs(count);
    XptCheckResourceListAndLength(
        subscr_stack(count fi_+ 2),
        subscr_stack(count fi_+ 2)
    ) -> ->;
    fast_XtVaSetSubvalues(count);
enddefine;


;;; Get resource database of a display
define XtDatabase() with_nargs 1;
    fast_XtDatabase(XptCheckDisplayPtr());
enddefine;


;;; So uses works OK
constant xt_resource= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 30 1993
        Added XtDatabase
--- John Gibson, Nov  3 1991
        typespecs moved to xpt_generaltypes.ph
--- Adrian Howard, Nov  1 1991
    o Removed -XptResourceListPtr- shadowclass, equivalent of -XptResourceList-
    o -XptResource- now a typespec
    o -XptResource- shadowclass renamed -XptResourcePtr-
--- Adrian Howard, Sep 11 1991 : Added checks for resource list length
--- Roger Evans, Jun 18 1991 added XptCheckArgListAndCardinal code
--- Ian Rogers, Mar 13 1991
        Corrected XptResourceListPtr cf. BR ianr.18
--- Roger Evans, Feb 10 1991 changed XptPopObj to XptPointer
--- Roger Evans, Feb  7 1991 removed 'Pop' version of shadowclasses, since
        XptString can handle pop strings properly now
--- Roger Evans, Jan 26 1991 changed to new shadowclass props format
--- Roger Evans, Nov 19 1990 added shadowclass defs
--- Ian Rogers, Nov  9 1990
        Corrected typos in fast_XtGetResourceList
        & fast_XtGetConstraintResourceList
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
