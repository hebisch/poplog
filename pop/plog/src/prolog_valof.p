/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/prolog_valof.p
 > Purpose:         Prolog: converting Pop procedures to Prolog predicates
 > Author:          Robert Duncan, Apr 27 1993
 > Documentation:
 > Related Files:
 */

section prolog;

constant
    procedure ( alias, unwrapped, is_special_functor, cons_special_closure,
        cons_undefined_closure, );

weak constant
    procedure ( predicate_record, );

vars
    procedure ( prolog_invisible, );

;;; ========================================================================

;;; converter_to_contn:
;;;     convert a Prolog procedure (i.e. one using the continuation stack)
;;;     to a Pop continuation-passing procedure. The contn is declared
;;;     "procedure" to get an automatic procedure check, allowing the
;;;     continuation to be fast_apply; arity is redundant.

define converter_to_contn(contn, pdr, arity);
    lvars procedure contn, pdr, arity;
    SAVE;
    prolog_push_continuation(contn, "fast_apply", 2);
    pdr();
    RESTORE;
enddefine;

;;; converter_from_contn:
;;;     convert a Pop continuation-passing procedure to a Prolog procedure.
;;;     The argument arity is redundant.

define converter_from_contn(pdr, arity);
    lvars pdr, arity;
    chain(prolog_apply_continuation, pdr);
enddefine;

;;; prolog_valof:
;;;     access/update the value of a Prolog procedure from Pop. The Pop
;;;     procedure is a continuation-passing one which must be converted
;;;     to (or from) the Prolog version. Invisible wrappers on
;;;     procedures are ignored.

define prolog_valof(fn, arity) -> proc;
    lvars fn, arity, proc;
    lvars name = alias(fn, arity);
    ;;; this first step is the same as predicate_valof, except that we
    ;;; don't check the functor name
    unless (isdefined(name) ->> proc)
    and isprocedure(idval(proc) ->> proc)
    and not(isundef(proc))
    then
        if is_special_functor(fn, arity) then
            cons_special_closure(fn, arity)
        else
            cons_undefined_closure(fn, arity)
        endif -> proc;
    endunless;
    unwrapped(proc) -> proc;
    if pdpart(proc) == converter_from_contn then
        ;;; previously imported from Pop
        frozval(1, proc) -> proc;
    else
        ;;; pure Prolog procedure
        writeable converter_to_contn(% proc, arity %) -> proc;
        name -> pdprops(proc);
    endif;
enddefine;
;;;
define updaterof prolog_valof(proc, fn, arity);
    lvars proc, fn, arity;
    lvars name = alias(fn, arity);
    if is_special_functor(fn, arity) then
        mishap(proc, name, 2, 'ILLEGAL ASSIGNMENT TO SYSTEM PREDICATE');
    endif;
    ;;; declare an identifier for the predicate
    lvars id;
    if isdefined(name) ->> id then
        ;;; already defined, but make sure it's a procedure
        unless identtype(id) == "procedure" then
            ident_declare(name, "procedure", isconstant(id));
        endunless;
    elseif isdeclared(name) ->> id then
        ;;; declared "weak", but not yet defined: declare it properly
        ident_declare(name, "procedure", isconstant(id));
    else
        ;;; new identifier needed: vars is the default
        ident_declare(name, "procedure", false);
    endif;
    ;;; convert the new value to Prolog form
    if pdpart(proc) == converter_to_contn then
        ;;; Prolog procedure wrapped up by previous -prolog_valof- call
        frozval(1, proc) -> proc;
    else
        ;;; Pop continuation-passing procedure
        writeable converter_from_contn(% proc, arity %) -> proc;
        name -> pdprops(proc);
    endif;
    ;;; do the update -- might mishap if the identifier is
    ;;; non-assignable
    proc -> unwrapped(valof(name)) -> valof(name);
    ;;; create a predicate record, if applicable
    if testdef predicate_record then
        weakref predicate_record(fn, arity, true) -> ;
    endif;
enddefine;

;;; prolog_raw_valof:
;;;     like prolog_valof, but without ignoring invisible wrappers

define prolog_raw_valof() with_nargs 2;
    dlocal prolog_invisible = not;
    prolog_valof();
enddefine;
;;;
define updaterof prolog_raw_valof() with_nargs 3;
    dlocal prolog_invisible = not;
    -> prolog_valof();
enddefine;

endsection;     /* prolog */
