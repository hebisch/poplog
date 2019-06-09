/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptValTestTypedArg.p
 > Purpose:         Procedure called by XptVal to test for typed arg
 > Author:          John Gibson, Sep  9 1992
 > Documentation:   None -- purely for use of XptVal
 > Related Files:   LIB *XptVal
 */
compile_mode :pop11 +strict :vm -pentch;

section;

include xt_constants.ph;

lconstant macro (
    TYPED_ARG_STRING        = 2:1e0,
    TYPED_ARG_INTEGER       = 2:1e1,
    );

define global XptValTestTypedArg(name, item, flags);
    lvars name, item, flags;
    if      flags &&/=_0 TYPED_ARG_STRING and isstring(item) then
        XtVaTypedArg, name, XtR String, -> exval_to_string(item),
                                datalength(item) fi_+ 1, true
    elseif  flags &&/=_0 TYPED_ARG_INTEGER and isintegral(item) then
        XtVaTypedArg, name, XtR Int, item, SIZEOFTYPE(:int), true
    else
        ;;; last result false means name, item returned unchanged
        name, item, false
    endif
enddefine;

endsection;
