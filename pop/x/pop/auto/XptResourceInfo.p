/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptResourceInfo.p
 > Purpose:         Resource information database for widgets
 > Author:          Jonathan Meyer, Jan 29 1991 (see revisions)
 > Documentation:
 > Related Files:   LIB *XptSpecifyResourceInfo
 */
compile_mode :pop11 +strict;

/********************************************************************
 *      MAINTAINING A RESOURCE DATABASE FOR WIDGET CLASSES
 ********************************************************************/

include xpt_generaltypes.ph;
include xpt_constants.ph;

section $-Xpt => XptResourceInfo;
exload_batch;

uses
    fast_xt_resource,           ;;; XtGetResourceList etc.
    fast_xt_widgetclass,        ;;; XtClass
    fast_xt_widgetinfo,         ;;; XtParent
    fast_xt_util,               ;;; XtFree
    XptSpecifyResourceInfo,
;


define :inline lconstant NEW_PROP(num);
    newanyproperty([], num, 1,
                    intof(num * 0.75)+1, false, false,
                    "perm", false, false)
enddefine;

constant procedure (WidgetResources, ConstraintResources);
declare_incremental property (
    ;;; n.b. NEW_PROP must be a macro here, not a procedure
    WidgetResources     = NEW_PROP(20),
    ConstraintResources = NEW_PROP(5),
);


/***
    get class resources and put them in properties, making props if nec.
 ***/
define Get_class_resources(class, prop, constraint_res) -> prop;
    lvars
        class prop constraint_res
        procedure res_pdr = if constraint_res then
                                fast_XtGetConstraintResourceList
                            else
                                fast_XtGetResourceList
                            endif,
        num, ptr, i, resource, name,
    ;
    lconstant
        res_return = EXPTRINITSTR(:exptr),
        num_return = EXPTRINITSTR(:XptCardinal),
    ;
    l_typespec resource :XptResource;

    XptCheckWidgetClass(class)->;
    fast_XtInitializeWidgetClass(class);

    if not(prop) or prop(RES_NOT_SET) then
        res_pdr(class, res_return, num_return);
        exacc :XptCardinal num_return -> num;
        exacc :exptr res_return -> ptr;
        if num == 0 then
            unless prop then
                NEW_PROP(1) -> prop;
            endunless
        else
            unless prop then NEW_PROP(num) -> prop; endunless;
            ;;; read resources into pop property,
            ;;; sharing resource structs if poss
            fast_for i to num do
                exacc :resource[] ptr[i] -> resource;
                consword(exacc resource.XptRName) -> name;
                unless prop(name) then
                    Get_resource_struct(
                        consword(exacc resource.XptRType),
                        exacc resource.XptRsize fi_<< 3 ;;; in bits
                    ) -> prop(name);
                endunless;
            endfast_for;
            fast_XtFree(ptr);
        endif;
        false -> prop(RES_NOT_SET);
    endif;
enddefine;

define lconstant Check_name(name) -> name;
    lvars name;
    unless name.isword or name.isstring then
        mishap(name,1,'WORD or STRING NEEDED');
    endunless;
    if name.isstring then
        consword(#|explode(name); if .dup == 0 then -> endif; |#) -> name;
    endif;
enddefine;

/************
    These two are caches for the last widget accessed.
    This allows an optimistation that assumes a typical program
    will want information on a series of resources on the same widget
 ************/
lvars   last_wid = false,
        wid_prop
    ;

define XptResourceInfo(widget, name) -> info;
    lvars   widget, class, info,
            name     = Check_name(name),
            con_prop = false,
        ;
    XptIsLiveType(widget, XDT_WIDGETCLASS) or XptCheckWidget(widget) -> ;

    /********
        fastest lookup - getting another resource from last seen widget
        (we use exacc ^uint to avoid keeping handle on the widget)
     ********/
    returnif(
        exacc [fast] ^uint widget == last_wid and wid_prop(name) ->> info
    );

    exacc [fast] ^uint widget -> last_wid;

    if XptIsType(widget, XDT_WIDGETCLASS) then
        ;;; already got the class
        widget -> class;
        false -> widget;
    else
        fast_XtClass(widget) -> class;
    endif;

    /********
        next fastest lookup - we've got property table before
     ********/
    returnif(
        (WidgetResources(class) ->> wid_prop)
        and (wid_prop(name) ->> info)
    );

    if wid_prop == false or wid_prop(RES_NOT_SET) then
        Get_class_resources(class, wid_prop, false)
                ->> wid_prop -> WidgetResources(class);
        returnif(wid_prop(name) ->> info);
    endif;

    if widget and fast_XtIsConstraint(fast_XtParent(widget) ->> widget) then
        fast_XtClass(widget) -> class;
        ;;; parent is a constraint widget - try that
        returnif(
            (ConstraintResources(class) ->> con_prop)
            and (con_prop(name) ->> info)
        );

        if con_prop == false or con_prop(RES_NOT_SET) then
            ;;; get resources out of widget
            Get_class_resources(class, con_prop, true)
                ->> con_prop -> ConstraintResources(class);
            con_prop(name) -> info;
        endif;
    endif;
enddefine;

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 13 1995
        Now in section $-Xpt
--- John Gibson, Apr  1 1995
        Replaced use of int*vecs with EXPTRINITSTRs for correct types
--- Ian Rogers, May  7 1993
        Made res_return and num_return writeable
--- John Gibson, Apr 13 1993
        o  Moved Get_resource_struct to XptSpecifyResourceInfo.p and made
           this file use that.
        o  Because XtNLookup strings won't be unique with POPC, changed
           resource properties to be keyed on words instead (using the
           strings was completely pointless, since XtNLookup's internal
           property was holding on to all the words anyway).
        o  Added incremental declarations for outer properties.
--- John Gibson, Mar 23 1993
        Made RES_NOT_SET just a constant rather than a macro (can't be used
        as a macro in other files with POPC).
--- Ian Rogers, Feb 17 1993
        Much tidying.
        Allowed -XptResourceInfo- to take a class as well as a widget.
        Fixed bug in -Get_class_resources- which would have only occured if
        a class had no resources
        Ensured that the widget class is initialised before attempting to
        extract the resources.
--- John Gibson, Nov  3 1991
        Uses resource typespec definition from xpt_generaltypes.ph
--- Jonathan Meyer, Mar 11 1991 Removed erroneous comment
 */
