/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/ToolkitWidgetSet.p
 > Purpose:         Minimal information on the basic Xt Toolkit widget-set
 > Author:          Tom Khabaza, Aug 14 1990 (see revisions)
 > Documentation:
 > Related Files:   LIB * XptWidgetSet
 */
#_TERMIN_IF DEF POPC_COMPILING

uses XptWidgetSet;

lconstant ws = XptNewWidgetSet("Toolkit");

[
    CompositeWidget
    ConstraintWidget
    CoreWidget
    Widget
    Object
    RectObj
    ShellWidget
    OverrideShellWidget
    WmShellWidget
    TransientShellWidget
    TopLevelShellWidget
    ApplicationShellWidget
    VendorShellWidget
] -> ws("WidgetSetMembers");

procedure(wsname, wcname);
    lvars wsname, wcname;
    consword('xt' sys_>< wcname)    ;;; to make it use useslib
endprocedure -> ws("WidgetSetFileMapping");


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  6 1993
        Changed WidgetSetFileMapping to load libraries
--- Jonathan Meyer, Oct 23 1990
        Added list of widget set members - disallows loading of invalid
        widget classes. turned off any attempts to load libraries
--- Jonathan Meyer, Oct 22 1990
    XptWidgetSet now does all the work for a basic library
 */
