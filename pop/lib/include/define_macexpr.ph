/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/include/define_macexpr.ph
 > Purpose:         Define form for defining a procedure whose name is a
 >                  macro expression
 > Author:          John Gibson, Jun  1 1993
 > Documentation:
 */

#_TERMIN_IF DEF DEFINE_MACEXPR_INCLUDED

section;

define :define_form iconstant macexpr;
    lvars mac = readitem(), idname;
    if mac == "updaterof" then
        readitem() -> mac;
        "macexpr_" <> mac -> idname;
        [define updaterof ^idname ^^proglist] -> proglist;
        pop11_comp_expr();
        [; updater(^idname) -> updater(^mac);]
    else
        "macexpr_" <> mac -> idname;
        [define lconstant ^idname ^^proglist] -> proglist;
        pop11_comp_expr();
        [; ^idname -> ^mac;]
    endif nc_<> proglist -> proglist
enddefine;

/*
 *  Define a set of vector subscript expression macros (which can then be
 *  assigned to with define :macexpr ... or otherwise).
 */
define iconstant macro GEN_VECSUB_MACROS vecname fieldlist;
    lvars field, n = 0, vecname, fieldlist;
    [iconstant macro (].dl;
    for field in fieldlist do
        n+1 -> n;
        [^field = [fast_subscrv(^n,^vecname)],].dl
    endfor;
    [^("INIT_" <> vecname) = [initv(^n)] );].dl
enddefine;

iconstant DEFINE_MACEXPR_INCLUDED = true;

endsection;
