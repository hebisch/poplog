/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptValue.p
 > Purpose:         Access and update widget resources by name
 > Author:          Jonathan Meyer, Sept 7 1990 (see revisions)
 > Documentation:   HELP *XptValue
 > Related Files:   LIB *fast_XptValue
 */
compile_mode :pop11 +strict;

section;

weak constant procedure fast_XptValue;

define XptValue(widget,name);
    lvars widget, name, coerce = "int";
    unless XptIsType(widget, "Widget") then
        (widget,name) -> (widget,name,coerce);
    endunless;
    fast_chain(XptCheckWidget(widget), ->XptCoerceString(name), coerce,
                #_< fast_XptValue >_#);
enddefine;

define updaterof XptValue(value, widget, name);
    lvars value, widget, name, coerce = "int";
    unless XptIsType(widget, "Widget") then
        (value,widget,name) -> (value,widget,name,coerce);
    endunless;
    fast_chain(value, XptCheckWidget(widget), ->XptCoerceString(name), coerce,
            #_< updater(fast_XptValue) >_#);
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Aug 19 1991
        Now coerces strings
--- Jonathan Meyer, Jun  5 1991
        Added type checking
 */
