/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptPopImportProcedure.p
 > Purpose:         Turn a function exptr into a pop procedure
 > Author:          John Gibson, Mar 29 1993
 > Documentation:   REF * XPT_COERCE
 */
compile_mode :pop11 +strict;

section;

define XptPopImportProcedure(exptr, name_or_clos, call_p, nargs);
    lvars exptr, name_or_clos, call_p, nargs, p;

    ;;; when a widgetclass proc is called, coerce arguments.
    ;;; Future versions will allow nargs to be a list of argument checking procs
    define lconstant process_args_and_call(proc, call_p, nargs);
        lvars i, proc, nargs, arg, key, call_p;
        unless nargs then .dup + 1 -> nargs endunless;
        if stacklength() fi_< nargs then
            clearstack() ->;    ;;; generate a ste mishap
        endif;
        fast_for i from 1 to nargs do
            subscr_stack(i) -> arg;
            ;;; integers and external pointers left alone
            nextif(arg.isnumber or arg.isexternal_ptr_class
                    or arg.isexfunc_closure);
            if arg.isstring then
                ;;; null terminate strings
                ->XptCoerceTmpString(arg) -> subscr_stack(i);
            elseif arg.isboolean then
                ;;; convert booleans
                ->XptCoerceBoolean(arg) -> subscr_stack(i);
            elseif class_attribute(datakey(arg)->>key,"external_deref")
                    or key == vector_key then
                ;;; do nothing
            elseif (class_field_spec(key) ->> key) == "full" or key == false or
                    key.islist and fast_lmember("full", key) then
                ;;; can't process arg:
                mishap(arg,1,'INVALID DATATYPE FOR EXTERNAL WIDGET PROCEDURE');
            endif;
        endfor;
        fast_chain(proc, call_p);
    enddefine;

    XptImportProcedure(exptr) -> exptr;
    if isclosure(name_or_clos) then
        ;;; name_or_clos is the 3-element closure of myself that this call
        ;;; resulted from (and has name as pdprops). Turn it into the result
        ;;; but DON'T return it (must be writeable, of course).
        name_or_clos -> p;
        exptr -> frozval(1,p);
        process_args_and_call -> pdpart(p);
    else
        ;;; create new closure, and return it
        process_args_and_call(%exptr, call_p, nargs%) ->> p;
        name_or_clos -> pdprops(p)
    endif;
enddefine;

endsection;
