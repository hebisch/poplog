/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptSpecifyResourceInfo.p
 > Purpose:         Overrides/add resource info for class
 > Author:          Jonathan Meyer, Jan 30 1991 (see revisions)
 > Documentation:   REF *XPT_RESOURCE
 > Related Files:   LIB *XptResourceInfo
 */
compile_mode :pop11 +strict;

/********************************************************************
 *      SPECIFYING RESOURCE INFORMATION FOR A WIDGET CLASS
 *
 *          (N.B. This file should contain only the procedure
 *          XptSpecifyResourceInfo and nothing else -- so it will load
 *          into corepop11)
 *
 ********************************************************************/

include xpt_constants.ph;

section $-Xpt => XptSpecifyResourceInfo;

uses-by_name from XptResourceInfo ($-Xpt$-WidgetResources,
                                   $-Xpt$-ConstraintResources);

    /*  Bit sizes of some standard types (ideally these should be derived
        from include xpt_xtypes.ph etc, but those won't compile in
        corepop11).
    */
l_typespec XID :ulong, Pointer :exptr;

lconstant standard_type_sizes = [
    Bitmap      ^SIZEOFTYPE(:XID,:1)
    Boolean     ^SIZEOFTYPE(:byte,:1)
    Font        ^SIZEOFTYPE(:XID,:1)
    Int         ^SIZEOFTYPE(:int,:1)
    Pixel       ^SIZEOFTYPE(:XID,:1)
    Pixmap      ^SIZEOFTYPE(:XID,:1)
    Pointer     ^SIZEOFTYPE(:Pointer,:1)
    Short       ^SIZEOFTYPE(:short,:1)
    String      ^SIZEOFTYPE(:Pointer,:1)
    Widget      ^SIZEOFTYPE(:Pointer,:1)
    WidgetClass ^SIZEOFTYPE(:Pointer,:1)
];

constant RES_NOT_SET = -1;

lconstant macro (
    WIDPROP = [sys_current_val("ident $-Xpt$-WidgetResources")],
    CONPROP = [sys_current_val("ident $-Xpt$-ConstraintResources")],
);

define lconstant New_prop(num);
    lvars num;
    newanyproperty([], num, 1,
                    intof(num * 0.75)+1, false, false,
                    "perm", false, false);
enddefine;

/***
    save us from remaking same struct over and over
    (many resources have same type/size values)
    used LIB *XptResourceInfo
 ***/
lvars resource_structs = [];

define Get_resource_struct(type, size) -> s;
    lvars s, type, size;

    fast_for s in resource_structs do
        returnif(type == fast_subscrv(1, s)
                 and size == fast_subscrv(2, s))
    endfast_for;

    ;;; doesn't exist, so make a new struct  prospectous
    ({^type ^size} ->> s) :: resource_structs -> resource_structs;
enddefine;

define XptSpecifyResourceInfo(class, resources);
    lvars   class, resources, key, val, prop, wid_res, con_res;

    define lconstant Add_to_prop(item);
        lvars item, vec, type, size, l;
        item(2) -> vec;
        if isword(vec) and (fast_lmember(vec, standard_type_sizes) ->> l) then
            vec -> type;
            l(2) -> size
        elseunless isvector(vec) and datalength(vec) == 2
        and isword(vec(1) ->> type) and isinteger(vec(2) ->> size)
        then
            mishap (vec, 1, 'INVALID RESOURCE ENTRY SPECIFICATION');
        endif;
        Get_resource_struct(type, size) -> prop(front(item));
    enddefine;

    XptTypeCheck(class, XDT_WIDGETCLASS) -> ;

    ;;; first widget resources
    if resources(1) ->> wid_res then
        unless WIDPROP(class) ->> prop then
            New_prop(length(wid_res) + 1) ->> prop -> WIDPROP(class);
            true -> prop(RES_NOT_SET);
        endunless;
        applist(wid_res, Add_to_prop)
    endif;

    ;;; now constraint resources
    if resources(2) ->> con_res then
        unless CONPROP(class) ->> prop then
            New_prop(length(con_res)+1) ->> prop -> CONPROP(class);
            true -> prop(RES_NOT_SET);
        endunless;
        applist(con_res, Add_to_prop);
    endif;
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 13 1995
        Changed Add_to_prop to allow a resource entry spec to be a standard
        typename in standard_type_sizes as an alternative to a 2-element
        vector.
        Now in section $-Xpt.
--- John Gibson, Apr 13 1993
        Reorganised to work with POPC at compile-time. Props now keyed on
        words, not XtNLookup strings (see revision note in XptResourceInfo.p)
--- Ian Rogers, Feb 17 1993
        Much tidying.
        Removed duplicate definitions of -WidgetResources-
        -ConstraintResources- and -Get_resource_struct- (in
        LIB * XptResourceInfo)
--- Roger Evans, May 29 1991 changed widgetclass typecheck not to require
        a LIVE widgetclass, since it might not be if loads are batched
--- Jonathan Meyer, Mar 12 1991
--- Jonathan Meyer, Mar 12 1991 Added uses XtN
 */
