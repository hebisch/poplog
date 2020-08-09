/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/define_generic.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

section $-objectclass => define_generic;

;;; I want this visible for debugging.
define generic_load_table =
    newproperty([], 16, false, "tmparg");
enddefine;

define lconstant check_generic(name, value);
    if value.isgeneric then
        lvars f = generic_load_table( value );
        if f and f /= popfilename then
            warning( 'ocgpd: GENERIC PROCEDURE ALREADY DECLARED IN ANOTHER FILE', [^name ^f] )
        endif;
        popfilename -> value.generic_load_table;
    elseunless value.isundef then
        warning( 'ocgpn: GENERIC PROCEDURE WAS DEFINED AS NON-METHOD', [^name ^value] )
    endif
enddefine;

define :define_form global generic;
    lvars mode = pop11_try_nextreaditem("updaterof").flag_to_mode;
    lvars decl = read_identspec(default_default_idspec());
    lvars name = readitem();
    define_hook("generic", name);
    identspec_declare(decl)(name, "procedure");
    lvars ilocals = false;
    if nextreaditem() == "(" then
        get_locals(false) -> (ilocals,);
    endif;
    lvars with_stuff = read_with_stuff();
    lvars props = with_stuff("pdprops");
    lvars arity =
        with_stuff("with_nargs") or
        ilocals and ilocals.length or
        "unbound";
    if with_stuff("with_combination") then
        ;;; This is where we deal with before & after methods.
        warning('UNIMPLEMENTED FEATURE: with_combination', []);
    endif;
    pop11_need_nextreaditem(";").erase;
    pop11_need_nextreaditem("enddefine").erase;
    bind_method(name, mode);
    lvars m = currentval(name);
    check_generic(name, m);
    lvars mt = method_table(m);
    if mode == CALL_MODE then
        arity -> cArityMethodTable(mt);
        if props then props -> pdprops(m) endif;
    else
        arity -> uArityMethodTable(mt);
        if props then
            mishap('CANNOT COMBINE updaterof AND with_props', []);
        endif;
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 22 1996
        Restored old behaviour whereby define:generic with no argument list
        leaves arity unbound (instead of forcing subsequent method
        definitions also to have no arguments, which is pretty pointless).
--- Robert John Duncan, Nov 28 1995
        Changed define_generic to read input and output locals using new
        procedures from "method_form.p", i.e. to be the same as
        define_method, except for typed arguments which don't make sense
        for a generic.
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
