/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/typecheck.p
 > Purpose:         PML: Static analysis
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

vars
    ml_warnings,        ;;; forward from "compile.p"
;

constant procedure (    ;;; forward from this file
    typecheck_exp,
    typecheck_dec,
    typecheck_strdec,
    typecheck_spec,
);


lvars
    this_dec,
        ;;; declaration number, for identifying where type variables were
        ;;; created
    this_strname,
        ;;; identifier of the current structure; <false> at top level
    overloadings,
        ;;; list of forms with overloading constraints still to be
        ;;; checked, including wildcard record patterns
    types_sharing,
    structures_sharing,
        ;;; lists of sharing constraints in a signature
;


;;; next_decnum:
;;;     generates a sequence of unique declaration numbers

lvars
    decnum,
;

define lconstant next_decnum();
    decnum, decnum + 1 -> decnum;
enddefine;


;;; typeof:
;;;     get/set the type attached to a parse tree node

define typeof =
    nodetype(%%);
enddefine;
;;;
define updaterof typeof(/*ty,*/ node);
    lvars node;
    type_deref(/*ty*/) -> nodetype(node);
enddefine;

;;; find_instance:
;;;     checks that an overloaded variable has been used at one of a given
;;;     set of valid types

define find_instance(var, ty, linenum, tys);
    lvars i, var, ty, linenum, tys;
    unless is_ground_type(ty) then
        ml_error('cannot determine a type for overloaded identifier\n%s\n',
            [% '\tval %p : %p', val_name(var), ty %], popfilename, linenum);
    endunless;
    For i in tys do
        returnif(test_unify(ty, i))(i);
    endfor;
    ml_error('overloaded identifier used with invalid type\n%s\n',
        [% '\tval %p : %p', val_name(var), ty %], popfilename, linenum);
enddefine;

;;; check_overloading:
;;;     checks that an overloaded type in a declaration has been
;;;     properly instantiated.

define lconstant check_overloading(x);
    lvars x, (node, ty) = Destpair(x);
    SWITCH node
    CASE VarExp(var) then
        ;;; overloaded operator: the -val_overloading- field should
        ;;; define a specialisation procedure which when applied to the
        ;;; var will return a specialised copy or give an error
        ;;; (e.g. see -find_instance- above)
        lvars ov = val_overloading(var);
        if isprocedure(ov) then
            ov(var, ty, nodeline(node)) -> first(node);
        endif;
    CASE WRecordPat(_, _, _) then
        if is_wildcard_recordtype(ty) then
            ml_error('cannot determine a type for record pattern\n%s\n',
                ['\t%p' ^ty], popfilename, nodeline(node));
        endif;
        ;;; fill in the complete set of labels
        lvars labs = type_labels(recordtype_deref(ty));
        if isinteger(labs) then fromto(1, labs) else Back(labs) endif
            -> third(node);
    CASE SelectorExp(lab, _) then
        if is_wildcard_recordtype(type_domain(ty)) then
            ml_error('cannot determine a type for record selector\n%s\n',
                ['\t#%p : %p' ^lab ^ty], popfilename, nodeline(node));
        endif;
        ;;; fill in the complete set of labels
        lvars labs = type_labels(recordtype_deref(type_domain(ty)));
        if isinteger(labs) then fromto(1, labs) else Back(labs) endif
            -> second(node);
    ENDSWITCH;
enddefine;

;;; bad_arity:
;;;     reports a constructor used with the wrong number of arguments

define lconstant bad_arity(msg, id, n, linenum);
    lvars msg, id, n, linenum;
    ml_error('%S to %S\n\t%p\n', [%
        if n > 1 then
            'unexpected arguments'
        elseif n == 1 then
            'unexpected argument'
        elseif n == -1 then
            'missing argument'
        else
            'missing arguments'
        endif, msg, id
    %], popfilename, linenum);
enddefine;

;;; check_type:
;;;     checks that the type of sub_form (type_found) matches the type
;;;     implied by the enclosing form (type_wanted)

define lconstant check_type(form, sub_form, type_wanted, type_found);
    lvars form, sub_form, type_wanted, type_found;
    if unify_types(type_wanted, type_found) then
        ;;; types unify, so make them identical
        type_wanted -> typeof(sub_form);
    else
        ;;; reason for unification failure is already on the stack;
        ;;; push additional arguments and call unify_error
        unify_error(/*reason*/, form, sub_form);
    endif;
enddefine;

;;; check_types:
;;;     checks the types of a list of sub_forms

define lconstant check_types(form, sub_forms, type_wanted, types_found);
    lvars form, sub_forms, type_wanted, types_found;
    lvars sub_form, type_found;
    For sub_form, type_found in sub_forms, types_found do
        check_type(form, sub_form, type_wanted, type_found);
    endfor;
enddefine;


/*
 *  Special constants
 */

define lconstant constant_type(k);
    lvars k;
    lvars t = scon_type(k);
    if t == "int" then
        inttype;
    elseif t == "real" then
        realtype;
    elseif t == "string" then
        stringtype;
    else
        mishap(k, 1, 'ILLEGAL CONSTANT');
    endif;
enddefine;


/*
 *  Types
 */

define typecheck_tyexp(tyexp) -> ty;
    lvars tyexp, ty;
    SWITCH tyexp
    CASE VarTyexp(v) then
        tyvar_type(v) -> ty;
    CASE RecordTyexp(labs, tyexps) then
        recordtype(conspair(false, labs), map(tyexps, typecheck_tyexp)) -> ty;
    CASE TupleTyexp(tyexps) then
        tupletype(map(tyexps, typecheck_tyexp)) -> ty;
    CASE FunTyexp(dom_tyexp, ran_tyexp) then
        funtype(typecheck_tyexp(dom_tyexp), typecheck_tyexp(ran_tyexp)) -> ty;
    CASE ConsTyexp(tycon, tyexps) then
        unless llength(tyexps) == tycon_arity(tycon) then
            bad_arity('type constructor', tycon_name(tycon),
                llength(tyexps) - tycon_arity(tycon), nodeline(tyexp));
        endunless;
        constype(tycon, map(tyexps, typecheck_tyexp)) -> ty;
    ENDSWITCH;
    ty -> typeof(tyexp);
enddefine;


/*
 *  Patterns
 */

;;; mark_nonexpansive:
;;;     propagates the "non-expansive" attribute through the variables in
;;;     a pattern

define lconstant mark_nonexpansive(pat);
    lvars pat;
    SWITCH pat
    CASE VarPat(var) then
        true -> val_nonexpansive(var);
    CASE LayeredPat(var, pat) then
        true -> val_nonexpansive(var);
        mark_nonexpansive(pat);
    CASE TypedPat(pat, _) then
        mark_nonexpansive(pat);
    else
    ENDSWITCH;
enddefine;

define typecheck_pat(pat) -> ty;
    lvars pat, ty;
    SWITCH pat
    CASE ConstPat(scon) then
        constant_type(scon) -> ty;
    CASE WildCardPat() then
        anytype(this_dec) -> ty;
    CASE VarPat(var) then
        anytype(this_dec) ->> ty -> val_type(var);
        this_strname -> val_parent(var);
    CASE ConPat(con) then
        unless val_arity(con) == 0 then
            bad_arity('constructor', val_name(con), -1, nodeline(pat));
        endunless;
        type_instance(val_type(con), this_dec) -> ty;
    CASE ExnPat(exn) then
        unless val_arity(exn) == 0 then
            bad_arity('exception constructor', val_name(exn), -1,
                nodeline(pat));
        endunless;
        exntype -> ty;
    CASE RecordPat(labs, pats) then
        recordtype(conspair(false, labs), map(pats, typecheck_pat)) -> ty;
    CASE WRecordPat(labs, pats, _) then
        recordtype(conspair(true, labs), map(pats, typecheck_pat)) -> ty;
        acons(pat, ty, overloadings) -> overloadings;
    CASE UnitPat() then
        unittype -> ty;
    CASE TuplePat(pats) then
        tupletype(map(pats, typecheck_pat)) -> ty;
    CASE ListPat(pats) then
        anytype(this_dec) -> ty;
        check_types(pat, pats, ty, map(pats, typecheck_pat));
        listtype(ty) -> ty;
    CASE ConAppPat(con, apat) then
        unless val_arity(con) == 1 then
            bad_arity('constructor', val_name(con), 1, nodeline(pat));
        endunless;
        type_instance(val_type(con), this_dec) -> ty;
        check_type(pat, apat, type_domain(ty), typecheck_pat(apat));
        type_range(ty) -> ty;
    CASE ExnAppPat(exn, apat) then
        unless val_arity(exn) == 1 then
            bad_arity('exception constructor', val_name(exn), 1,
                nodeline(pat));
        endunless;
        check_type(pat, apat, type_domain(val_type(exn)), typecheck_pat(apat));
        exntype -> ty;
    CASE TypedPat(pat1, tyexp) then
        typecheck_tyexp(tyexp) -> ty;
        check_type(pat, pat1, ty, typecheck_pat(pat1));
    CASE LayeredPat(var, pat1) then
        typecheck_pat(pat1) ->> ty -> val_type(var);
        this_strname -> val_parent(var);
    ENDSWITCH;
    ty -> typeof(pat);
enddefine;


/*
 *  Expressions
 */

define typecheck_rule(rule) -> ty;
    lvars rule, ty;
    SWITCH rule
    CASE Rule(pat, e) then
        funtype(typecheck_pat(pat), typecheck_exp(e)) -> ty;
    ENDSWITCH;
    ty -> typeof(rule);
enddefine;

define typecheck_match(e, match) -> ty;
    lvars rule, e, match, ty;
    SWITCH match
    CASE Match(rules) then
        funtype(anytype(this_dec), anytype(this_dec)) -> ty;
        check_types(e, rules, ty, map(rules, typecheck_rule));
    ENDSWITCH;
    ty -> typeof(match);
enddefine;

define typecheck_exp(e) -> ty;
    lvars e, ty;
    SWITCH e
    CASE ConstExp(scon) then
        constant_type(scon) -> ty;
    CASE VarExp(var) then
        type_instance(val_type(var), this_dec) -> ty;
        if val_overloading(var) then
            acons(e, ty, overloadings) -> overloadings;
        endif;
    CASE ConExp(con) then
        type_instance(val_type(con), this_dec) -> ty;
    CASE ExnExp(exn) then
        val_type(exn) -> ty;
    CASE RecordExp(labs, exps) then
        recordtype(conspair(false, labs), map(exps, typecheck_exp)) -> ty;
    CASE UnitExp() then
        unittype -> ty;
    CASE TupleExp(exps) then
        tupletype(map(exps, typecheck_exp)) -> ty;
    CASE SeqExp(exps) then
        lvars e1;
        For e1 in exps do typecheck_exp(e1) -> ty endfor;
    CASE ListExp(exps) then
        anytype(this_dec) -> ty;
        check_types(e, exps, ty, map(exps, typecheck_exp));
        listtype(ty) -> ty;
    CASE SelectorExp(lab, _) then
        anytype(this_dec) -> ty;
        funtype(recordtype([^true ^lab], [^ty]), ty) -> ty;
        acons(e, ty, overloadings) -> overloadings;
    CASE LetExp(decs, e1) then
        app(decs, typecheck_dec);
        typecheck_exp(e1) -> ty;
    CASE AppExp(e1, e2) then
        lvars ty1 = typecheck_exp(e1);
        lvars ty2 = typecheck_exp(e2);
        funtype(anytype(this_dec), anytype(this_dec)) -> ty;
        if test_unify(ty, ty1) then
            ;;; ty1 is a function type: check that the argument's ok
            check_type(e, e2, type_domain(ty), ty2);
        else
            ;;; not a function at all: this will give an appropriate
            ;;; error
            check_type(e, e1, funtype(ty2, type_range(ty)), ty1);
        endif;
        type_range(ty) -> ty;
    CASE TypedExp(e1, tyexp) then
        typecheck_tyexp(tyexp) -> ty;
        check_type(e, e1, ty, typecheck_exp(e1));
    CASE AndExp(e1, e2) or OrExp(e1, e2) then
        lvars ty1 = typecheck_exp(e1);
        lvars ty2 = typecheck_exp(e2);
        booltype -> ty;
        check_type(e, e1, booltype, ty1);
        check_type(e, e2, booltype, ty2);
    CASE IfExp(e1, e2, e3) then
        lvars ty1 = typecheck_exp(e1);
        lvars ty2 = typecheck_exp(e2);
        typecheck_exp(e3) -> ty;
        check_type(e, e1, booltype, ty1);
        check_type(e, e3, ty2, ty);
    CASE WhileExp(e1, e2) then
        lvars ty1 = typecheck_exp(e1);
        lvars () = typecheck_exp(e2);
        check_type(e, e1, booltype, ty1);
        unittype -> ty;
    CASE CaseExp(e1, match) then
        lvars ty1 = typecheck_exp(e1);
        lvars ty2 = typecheck_match(e, match);
        ;;; typecheck_match always returns a funtype
        check_type(e, e1, type_domain(ty2), ty1);
        type_range(ty2) -> ty;
    CASE FnExp(match) then
        typecheck_match(e, match) -> ty;
    CASE RaiseExp(e1) then
        check_type(e, e1, exntype, typecheck_exp(e1));
        anytype(this_dec) -> ty;
    CASE HandleExp(e1, match) then
        typecheck_exp(e1) -> ty;
        check_type(e, e1, funtype(exntype, ty), typecheck_match(e, match));
    ENDSWITCH;
    ty -> typeof(e);
enddefine;


/*
 *  Core Declarations
 */

;;; Value and function declarations:

;;; bind_tyvar:
;;;     creates a new type variable for a tyvar which is scoped in a
;;;     "val" or "fun" declaration (where "scoped" means it occurs
;;;     unguarded in the declaration and hasn't yet been bound)

define bind_tyvar(tyvar);
    lvars tyvar;
    unless tyvar_type(tyvar) then
        ;;; not yet bound
        constraint_type(tyvar_name(tyvar), this_dec) -> tyvar_type(tyvar);
        tyvar;
    endunless;
enddefine;

;;; unbind_tyvar:
;;;     unbinds a type variable scoped in this declaration. The type
;;;     variable can't be free in the current environment (indicated by
;;;     a non-false declaration number).

define unbind_tyvar(tyvar);
    lvars tyvar;
    if type_decnum(tyvar_type(tyvar)) then
        ml_error('illegal free user type variable\n\t%p\n',
            [% tyvar_name(tyvar) %], false, false);
    endif;
    false -> tyvar_type(tyvar);
enddefine;

;;; closure:
;;;     generalises the type of a variable bound in the current declaration

define closure(var);
    lvars var;
    type_generalise(val_type(var), this_dec, not(val_nonexpansive(var)));
enddefine;


define typecheck_valbind(vb);
    lvars vb;
    SWITCH vb
    CASE ValBind(pat, e) then
        check_type(vb, e, typecheck_pat(pat), typecheck_exp(e));
        if nonexpansive(e) then mark_nonexpansive(pat) endif;
    ENDSWITCH;
enddefine;

define typecheck_recvalbinds(vbs);
    lvars vbs;
    returnif(vbs == []);
    lvars (vb, vbs) = Destpair(vbs);
    SWITCH vb
    CASE ValBind(pat, e) then
        lvars ty = typecheck_pat(pat);
        typecheck_recvalbinds(vbs);
        check_type(vb, e, ty, typecheck_exp(e));
        ;;; all recursive value bindings must be non-expansive
        mark_nonexpansive(pat);
    ENDSWITCH;
enddefine;

define typecheck_valdec(valdec);
    lvars   valdec, scoped_tyvars;
    dlocal  overloadings = [];
    map(second(valdec), bind_tyvar) -> scoped_tyvars;
    app(third(valdec), typecheck_valbind);
    typecheck_recvalbinds(fourth(valdec));
    app(overloadings, check_overloading);
    app(first(valdec), closure);
    app(scoped_tyvars, unbind_tyvar);
enddefine;

define typecheck_clause(clause) -> ty;
    lvars clause, ty;
    SWITCH clause
    CASE Clause(pats, e, tyexp) then
        lvars n_args = (#| app(pats, typecheck_pat) |#);
        if tyexp then
            ;;; constraint on result type
            typecheck_tyexp(tyexp) -> ty;
            check_type(clause, e, ty, typecheck_exp(e));
        else
            typecheck_exp(e) -> ty;
        endif;
        Repeat n_args times funtype((), ty) -> ty endrepeat;
    ENDSWITCH;
    ty -> typeof(clause);
enddefine;

define typecheck_fbs(fbs);
    lvars fb, fbs;
    ;;; initialise the type and arity of each function for possible
    ;;; forward references
    For fb in fbs do
        lvars var = first(fb);
        anytype(this_dec) -> val_type(var);
        second(fb) -> val_arity(var);
    endfor;
    ;;; now for the real types ...
    For fb in fbs do
        SWITCH fb
        CASE FunBind(var, _, clauses) then
            /*
            ;;; determine types of all clauses
            lvars clause;
            For clause in clauses do
                typecheck_clause(clause) -> ;
            endfor;
            ;;; take first as canonical
            Destpair(clauses) -> (clause, clauses);
            lvars ty = typeof(clause);
            ;;; check remaining ones agree: we do this one argument at a
            ;;; time to constrain any errors that do come up
            For clause in clauses do
                lvars ty1 = ty;
                SWITCH clause
                CASE Clause(pats, e, _) then
                    lvars pat;
                    For pat in pats do
                        check_type(clause, pat, type_domain(ty1), typeof(pat));
                        type_range(ty1) -> ty1;
                    endfor;
                    ;;; result types
                    check_type(clause, e, ty1, typeof(e));
                ENDSWITCH;
                ty -> typeof(clause);
            endfor;
            */
            ;;; check types of all clauses
            lvars (ty, tys) = Destpair(map(clauses, typecheck_clause));
            check_types(fb, Back(clauses), ty, tys);
            ;;; check type of definition agrees with type of use
            ty -> typeof(fb);
            check_type(fb, fb, ty, val_type(var));
            ;;; update var type to type of definition, which could
            ;;; involve type abbreviations
            ty -> val_type(var);
            this_strname -> val_parent(var);
            true -> val_nonexpansive(var);
        ENDSWITCH;
    endfor;
enddefine;

define typecheck_fundec(fundec);
    lvars   fundec, scoped_tyvars;
    dlocal  overloadings = [];
    map(second(fundec), bind_tyvar) -> scoped_tyvars;
    typecheck_fbs(third(fundec));
    app(overloadings, check_overloading);
    app(first(fundec), closure);
    app(scoped_tyvars, unbind_tyvar);
enddefine;


;;; Type declarations:

define typecheck_typebind(tb);
    lvars tycon, tyvar, ty, boundvars, tb;
    first(tb) -> tycon;
    [%  For tyvar in second(tb) do
            anytype(false) ->> tyvar_type(tyvar);
        endfor
    %] -> boundvars;
    typecheck_tyexp(third(tb)) -> ty;
    this_strname -> tycon_parent(tycon);
    new_type_alias(boundvars, ty) -> tycon_function(tycon);
enddefine;

define typecheck_typedec(typedec);
    lvars typedec;
    app(second(typedec), typecheck_typebind);
enddefine;


;;; Datatype and abstype declarations:

;;; init_tycon:
;;;     initialise the type constructor bound by -db-

define lconstant init_tycon(db);
    lvars tycon, db;
    first(db) -> tycon;
    this_strname -> tycon_parent(tycon);
    new_type_name(llength(second(db)), true) -> tycon_function(tycon);
    if bound_names then make_bound(tycon_function(tycon)) endif;
    erase(%%) -> tycon_printer(tycon);
enddefine;

;;; compute_equality:
;;;     computes the equality attributes for the type constructors bound
;;;     by -dbs-. This is an iterative process, since the equality
;;;     attributes of mutually recursive datatypes are related.

define lconstant compute_equality(dbs);
    lvars db, tycon, dbs, changed = true;

    define lconstant db_equality(db);
        lvars cb, con, db;
        For cb in third(db) do
            first(cb) -> con;
            if val_arity(con) == 1 then
                returnunless(is_equality_type(type_domain(val_type(con))))(false);
            endif;
        endfor;
        true;
    enddefine;

    ;;; The equality attribute has been set (by -init_tycon-) to true for
    ;;; each tycon; we make repeated passes over the tycons, recomputing
    ;;; equality until nothing changes. Since the equality attribute can
    ;;; only decrease (from true to false), this process has to finish
    while changed do
        false -> changed;
        For db in dbs do
            first(db) -> tycon;
            if tycon_equality(tycon) and not(db_equality(db)) then
                false -> tycon_equality(tycon);
                true -> changed;
            endif;
        endfor;
    endwhile;
enddefine;

;;; make_printable:
;;;     compiles a printing function for a datatype constructor and
;;;     assigns it to the -tycon_printer- field. The existing dummy
;;;     closure put there by -init_tycon- is overwritten.

define lconstant make_printable(db);
    lvars tycon, tyvars, cons, db;
    first(db) -> tycon;
    second(db) -> tyvars;
    map(third(db), first) -> cons;
    compile_data_printer(tycon, tyvars, cons)
        ->> pdpart(tycon_printer(tycon))
        ->  tycon_printer(tycon);
enddefine;

define typecheck_conbinds(tycon, tyvars, cbs);
    lvars con, cb, i, ty, rep, tycon, tyvars, cbs;
    constype(tycon, map(tyvars, tyvar_type)) -> ty;
    0 -> i;
    data_rep(cbs) -> rep;
    For cb in cbs do
        i fi_+ 1 -> i;
        first(cb) -> con;
        this_strname -> val_parent(con);
        rep -> val_datarep(con);
        if second(cb) then
            ;;; unary constructor
            1 -> val_arity(con);
            funtype(typecheck_tyexp(second(cb)), ty) -> val_type(con);
            data_value(rep, i, 1) -> val_value(con);
        else
            ;;; nullary constructor
            0 -> val_arity(con);
            ty -> val_type(con);
            data_value(rep, i, 0) -> val_value(con);
        endif;
    endfor;
    rep -> tycon_datarep(tycon);
enddefine;

define typecheck_databind(db);
    lvars tyvar, tv, db;
    For tyvar in second(db) do
        constraint_type(tyvar_name(tyvar), false) -> tv;
        tv -> tyvar_type(tyvar);
        if type_eqtyvar(tv) or type_imptyvar(tv) then
            /*
            ;;; Comment this out until proper warnings enabled ...
            ml_warning('attributes of type variable ignored\n\t%p\n',
                [^tv], popfilename, nodeline(db));
            */
            false ->> type_eqtyvar(tv) -> type_imptyvar(tv);
            false -> fast_cont(tv); ;;; clear the name field
        endif;
    endfor;
    typecheck_conbinds(first(db), second(db), third(db));
enddefine;

define typecheck_datatypedec(datatypedec);
    lvars dbs, datatypedec;
    third(datatypedec) -> dbs;
    app(dbs, init_tycon);
    app(fourth(datatypedec), typecheck_typebind);
    app(dbs, typecheck_databind);
    compute_equality(dbs);
    app(dbs, make_printable);
enddefine;

define typecheck_abstypedec(abstypedec);
    lvars atycon, tycon, abstypedec, datatypedec;
    second(abstypedec) -> datatypedec;
    typecheck_datatypedec(datatypedec);
    app(third(abstypedec), typecheck_dec);
    ;;; Fill in the abstract copies of the declared tycons
    For atycon, tycon in first(abstypedec), first(datatypedec) do
        unless atycon == tycon then
            ;;; make the tycon abstract, then fill in its exported copy
            [] -> tycon_cons(tycon);
            false -> tycon_equality(tycon);
            false -> tycon_printer(tycon);
            explode(tycon) -> explode(atycon);
        endunless;
    endfor;
enddefine;


;;; Exception declarations:

define typecheck_exnbind(eb);
    lvars exn, ty, eb;
    first(eb) -> exn;
    if second(eb) then
        ;;; exception has an argument:
        ;;; force its type to be imperative by unifying with an imperative
        ;;; type variable
        anytype(this_dec) -> ty;
        true -> type_imptyvar(ty);
        unless test_unify(ty, typecheck_tyexp(second(eb)) ->> ty) then
            ml_error('exception type must be imperative\n%s\n',
                [% '\texception %p of %p', val_name(exn), ty %],
                popfilename, nodeline(eb));
        endunless;
        funtype(ty, exntype) -> val_type(exn);
        compile_toplevel_printer(ty) -> val_printer(exn);
        1 -> val_arity(exn);
    elseif third(eb) then
        val_type(third(eb)) -> val_type(exn);
        val_printer(third(eb)) -> val_printer(exn);
        val_arity(third(eb)) -> val_arity(exn);
    else
        exntype -> val_type(exn);
    endif;
    this_strname -> val_parent(exn);
enddefine;

define typecheck_exceptiondec(exceptiondec);
    lvars exceptiondec;
    app(second(exceptiondec), typecheck_exnbind);
enddefine;


;;; Local declarations:

define typecheck_localdec(localdec);
    lvars localdec;
    app(first(localdec), typecheck_dec);
    app(second(localdec), typecheck_dec);
enddefine;


;;; Core declarations:

define typecheck_dec(dec);
    lvars   dec;
    dlocal  this_dec = next_decnum();
    if isValDec(dec) then
        typecheck_valdec(dec);
    elseif isFunDec(dec) then
        typecheck_fundec(dec);
    elseif isTypeDec(dec) then
        typecheck_typedec(dec);
    elseif isDatatypeDec(dec) then
        typecheck_datatypedec(dec);
    elseif isAbstypeDec(dec) then
        typecheck_abstypedec(dec);
    elseif isExceptionDec(dec) then
        typecheck_exceptiondec(dec);
    elseif isLocalDec(dec) then
        typecheck_localdec(dec);
    elseif isOpenDec(dec) or isDirective(dec) then
        ;;; nothing to do
    else
        bad_syntax_tree(dec);
    endif;
enddefine;


/*
 *  Signature and Structure Expressions
 */

;;; check_side_condition:
;;;     checks the "side condition" on a declaration that it should
;;;     contain no free imperative type variables. Strictly, this should
;;;     apply to top-level declarations only (see -make_topdec- below),
;;;     but it's convenient to apply it to every dec in a generative
;;;     structure expression too, to avoid having to cope with free variables
;;;     in signature matching. This will forbid more programs than the
;;;     semantics require, but (we hope) only non-useful ones.
;;;     We also take the opportunity to compact all var and exn types to
;;;     remove chains of type variables from the global or structure env,
;;;     and to mark all exported vars as "used".

define lconstant check_side_condition(dec, msg);
    lvars entry, ty, dec, msg;
    For entry in first(dec) do
        val_type(entry) -> ty;
        unless is_generic_type(ty) then
            ml_error('%S\n\t%p\n', [^msg ^entry], popfilename,
                nodeline(dec));
        endunless;
        type_full_deref(ty) -> val_type(entry);
    endfor;
enddefine;

define lconstant make_strdec(dec);
    lvars dec;
    if isValDec(dec) or isFunDec(dec) or isExceptionDec(dec) then
        check_side_condition(dec, 'illegal imperative type in structure');
    elseif isAbstypeDec(dec) then
        app(third(dec), make_strdec);
    elseif isLocalDec(dec) or isLocalStrDec(dec) then
        app(second(dec), make_strdec);
    endif;
enddefine;

define typecheck_sigexp(sigexp) -> strenv;
    lvars   sigexp, strenv;
    dlocal  this_strname, bound_names = true, types_sharing = [],
            structures_sharing = [];
    if isIdSigexp(sigexp) then
        ;;; the parser will have created a unique instance of this signature;
        ;;; return its environment
        sig_env(first(sigexp)) -> strenv;
    elseif isGenSigexp(sigexp) then
        first(sigexp) -> strenv;
        ;;; create a new (anonymous) structure name for the new structure
        new_strname(false, false) -> this_strname;
        make_bound(this_strname);
        this_strname -> structenv_strname(strenv);
        app(second(sigexp), typecheck_spec);
        make_shared(types_sharing, structures_sharing, nodeline(sigexp));
    else
        bad_syntax_tree(sigexp);
    endif;
enddefine;

define typecheck_strexp(strexp) -> strenv;
    lvars   strexp, strenv;
    dlocal  this_strname;
    if isIdStrexp(strexp) then
        str_env(first(strexp)) -> strenv;
    elseif isGenStrexp(strexp) then
        first(strexp) -> strenv;
        ;;; create a new (anonymous) structure name for the new structure
        new_strname(false, this_strname) -> this_strname;
        if bound_names then make_bound(this_strname) endif;
        this_strname -> structenv_strname(strenv);
        app(second(strexp), typecheck_strdec);
        app(second(strexp), make_strdec);
    elseif isAppStrexp(strexp) then
        ;;; the parser will have created a unique instance of the functor:
        ;;; return its result environment
        fnc_env(first(strexp)) -> strenv;
        signature_match(
            strexp,
            fnc_arg(first(strexp)),
            typecheck_strexp(second(strexp)));
    elseif isLetStrexp(strexp) then
        app(first(strexp), typecheck_strdec);
        typecheck_strexp(second(strexp)) -> strenv;
    else
        bad_syntax_tree(strexp);
    endif;
enddefine;


/*
 *  Structure Declarations
 */

define typecheck_strbind(strb);
    lvars str, strenv1, strenv2, strb;
    first(strb) -> str;
    if second(strb) then typecheck_sigexp(second(strb)) -> endif;
    typecheck_strexp(third(strb)) -> strenv1;
    str_env(str) -> strenv2;
    unless strenv1 == strenv2 then
        signature_match(strb, strenv2, strenv1);
    endunless;
    if strname_name(structenv_strname(strenv2)) then
        new_strname(str_name(str), this_strname) -> str_strname(str);
        if bound_names then make_bound(str_strname(str)) endif;
    else
        structenv_strname(strenv2) -> str_strname(str);
        str_name(str) -> strname_name(str_strname(str));
    endif;
enddefine;

define typecheck_structuredec(structuredec);
    lvars structuredec;
    app(second(structuredec), typecheck_strbind);
enddefine;

define typecheck_localstrdec(strdec);
    lvars strdec;
    app(first(strdec), typecheck_strdec);
    app(second(strdec), typecheck_strdec);
enddefine;

define typecheck_strdec(dec);
    lvars dec;
    if isStructureDec(dec) then
        typecheck_structuredec(dec);
    elseif isLocalStrDec(dec) then
        typecheck_localstrdec(dec);
    else
        typecheck_dec(dec);
    endif;
enddefine;


/*
 *  Specifications
 */

define typecheck_valspec(valspec);
    lvars tyvar, var, valdesc, valspec;
    For tyvar in second(valspec) do
        constraint_type(tyvar_name(tyvar), false) -> tyvar_type(tyvar);
    endfor;
    For valdesc in third(valspec) do
        first(valdesc) -> var;
        this_strname -> val_parent(var);
        typecheck_tyexp(second(valdesc)) -> val_type(var);
    endfor;
enddefine;

define typecheck_typespec(typespec, equality);
    lvars tycon, typedesc, equality, typespec;
    For typedesc in second(typespec) do
        first(typedesc) -> tycon;
        this_strname -> tycon_parent(tycon);
        new_type_name(llength(second(typedesc)), equality) -> tycon_function(tycon);
        make_bound(tycon_function(tycon));
    endfor;
enddefine;

define typecheck_datatypespec(datatypespec);
    lvars datadescs, datatypespec;
    third(datatypespec) -> datadescs;
    app(datadescs, init_tycon);
    app(datadescs, typecheck_databind);
    compute_equality(datadescs);
    app(datadescs, make_printable);
enddefine;

define typecheck_exceptionspec(exceptionspec);
    lvars exn, ty, exndesc, exceptionspec;
    For exndesc in second(exceptionspec) do
        first(exndesc) -> exn;
        if second(exndesc) then
            typecheck_tyexp(second(exndesc)) -> ty;
            funtype(ty, exntype) -> val_type(exn);
            compile_toplevel_printer(ty) -> val_printer(exn);
            1 -> val_arity(exn);
        else
            exntype -> val_type(exn);
        endif;
        this_strname -> val_parent(exn);
    endfor;
enddefine;

define typecheck_strdesc(strdesc);
    lvars str, strname, strdesc;
    first(strdesc) -> str;
    typecheck_sigexp(second(strdesc)) -> ;
    ;;; the structure name of the environment of -str- will be anonymous
    ;;; (because it's generated from a signature); make it look like -str-
    structenv_strname(str_env(str)) ->> strname -> str_strname(str);
    str_name(str) -> strname_name(strname);
    this_strname -> strname_parent(strname);
enddefine;

define typecheck_structurespec(structurespec);
    lvars structurespec;
    app(second(structurespec), typecheck_strdesc);
enddefine;

define typecheck_sharingspec(sharingspec);
    lvars shareq, sharingspec;
    For shareq in first(sharingspec) do
        first(shareq) -> shareq;
        if istycon(Front(shareq)) then
            conspair(shareq, types_sharing) -> types_sharing;
        else
            conspair(shareq, structures_sharing) -> structures_sharing;
        endif;
    endfor;
enddefine;

define typecheck_includespec(includespec);
    lvars sig, includespec;
    For sig in first(includespec) do
        ;;; share the environment name with the current structure name:
        ;;; these will become the same at the next instance
        this_strname -> fast_cont(structenv_strname(sig_env(sig)));
    endfor;
enddefine;

define typecheck_localspec(localspec);
    lvars localspec;
    app(first(localspec), typecheck_spec);
    app(second(localspec), typecheck_spec);
enddefine;

define typecheck_spec(spec);
    lvars spec;
    if isValSpec(spec) then
        typecheck_valspec(spec);
    elseif isTypeSpec(spec) then
        typecheck_typespec(spec, false);
    elseif isEqtypeSpec(spec) then
        typecheck_typespec(spec, true);
    elseif isDatatypeSpec(spec) then
        typecheck_datatypespec(spec);
    elseif isExceptionSpec(spec) then
        typecheck_exceptionspec(spec);
    elseif isStructureSpec(spec) then
        typecheck_structurespec(spec);
    elseif isSharingSpec(spec) then
        typecheck_sharingspec(spec);
    elseif isOpenSpec(spec) then
        ;;; nothing to do
    elseif isIncludeSpec(spec) then
        typecheck_includespec(spec);
    elseif isLocalSpec(spec) then
        typecheck_localspec(spec);
    else
        bad_syntax_tree(spec);
    endif;
enddefine;


/*
 *  Top Level Declarations
 */

;;; make_topdec:
;;;     enforces the side condition on top-level declarations that they
;;;     should contain no free imperative type variables, and marks any
;;;     structures opened at the top-level as being open for ever.

define lconstant make_topdec(dec);
    lvars str, dec;
    if isValDec(dec) or isFunDec(dec) or isExceptionDec(dec) then
        check_side_condition(dec, 'illegal imperative type at top-level');
    elseif isOpenDec(dec) or isPervasiveDec(dec) then
        For str in first(dec) do
            true -> strname_open(structenv_strname(str_env(str)));
        endfor;
    elseif isAbstypeDec(dec) then
        app(third(dec), make_topdec);
    elseif isLocalDec(dec) or isLocalStrDec(dec) then
        app(second(dec), make_topdec);
    endif;
enddefine;

define typecheck_sigbind(sigb);
    lvars sig, strenv, sigb;
    first(sigb) -> sig;
    typecheck_sigexp(second(sigb)) -> strenv;
    new_signame(sig_name(sig)) -> sig_signame(sig);
    ;;; if the signature's a new one, give it a name
    unless structenv_signame(strenv) then
        sig_signame(sig) -> structenv_signame(strenv);
    endunless;
enddefine;

define typecheck_signaturedec(signaturedec);
    lvars signaturedec;
    app(second(signaturedec), typecheck_sigbind);
enddefine;

define typecheck_fncbind(fncb);
    lvars   fncb, str, strenv1, strenv2;
    dlocal  bound_names = true;
    typecheck_sigexp(third(fncb)) ->> strenv1 -> fnc_arg(first(fncb));
    if second(fncb) ->> str then
        structenv_strname(strenv1) -> str_strname(str);
        str_name(str) -> strname_name(str_strname(str));
    endif;
    if fourth(fncb) then typecheck_sigexp(fourth(fncb)) -> endif;
    typecheck_strexp(fifth(fncb)) -> strenv1;
    fnc_env(first(fncb)) -> strenv2;
    unless strenv1 == strenv2 then
        signature_match(fncb, strenv2, strenv1);
    endunless;
enddefine;

define typecheck_functordec(functordec);
    lvars functordec;
    app(second(functordec), typecheck_fncbind);
enddefine;

define typecheck_topdec(dec);
    lvars   dec;
    dlocal  this_strname = false;
    ;;; NB: first(dec) may be a dynamic list
    if isStrTopDec(dec) then
        applist(first(dec), typecheck_strdec);
    elseif isSigTopDec(dec) then
        applist(first(dec), typecheck_signaturedec);
    elseif isFncTopDec(dec) then
        applist(first(dec), typecheck_functordec);
    elseif isPerTopDec(dec) or isExtTopDec(dec) then
        ;;; nothing to do
    else
        bad_syntax_tree(dec);
    endif;
    applist(first(dec), make_topdec);
enddefine;

define typecheck(dec);
    lvars   dec;
    dlocal  decnum = 1;
    typecheck_topdec(dec);
enddefine;


/*
 *  External Declarations
 *  (see "external.p")
 */

define typecheck_external_val(var, tyvars, tyexp);
    lvars tyvar, var, tyvars, tyexp;
    For tyvar in tyvars do
        constraint_type(tyvar_name(tyvar), false) -> tyvar_type(tyvar);
    endfor;
    typecheck_tyexp(tyexp) -> val_type(var);
    type_generalise(val_type(var), false, false);
enddefine;

define typecheck_external_exception(exn, tyexp);
    lvars exn, ty, tyexp;
    returnunless(tyexp)(exntype -> val_type(exn));
    typecheck_tyexp(tyexp) -> ty;
    type_generalise(ty, false, false);
    funtype(ty, exntype) -> val_type(exn);
    compile_toplevel_printer(ty) -> val_printer(exn);
    1 -> val_arity(exn);
enddefine;

define typecheck_external_structure(str, sig, strenv);
    lvars str, sig, strenv;
    if sig then
        signature_match(false, sig_env(sig), strenv);
        sig_env(sig) -> strenv;
    endif;
    strenv -> str_env(str);
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Sep 18 1995
        Fixed arguments to check_type for HandleExp causing mishap when
        error-reporting
--- Robert John Duncan, Jul  3 1995
        Changed node-matching syntax to SWITCH/CASE/ENDSWITCH
--- Robert John Duncan, Apr 27 1995
        New name for unification procedure
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Oct 31 1994
        Fixed a bug in the treatment of overloading, consequent on the last
        change
--- Robert John Duncan, Oct 24 1994
        Major revision of error reporting. Unification now goes through
        procedure check_type which validates a condition on an inference
        rule; unification failures are reported through a new explain
        procedure defined in "type_errors.p"
--- Robert John Duncan, Nov  1 1991
        Renamed warning and error procedures.
--- Simon Nichols, Jun 26 1991
        Removed assignments to the usage count of values (now managed in
        "env.p").
--- Robert John Duncan, Mar 19 1991
        Fixed -typecheck_fbs- to check bindings in the right order; also
        rearranged the order of checks to maximise the amount of type
        information available within each clause body.
        -check_unify- now expects the complete node as its last argument
        rather than just the line number.
--- Robert John Duncan, Feb 11 1991
        Exceptions, constructors and variables now represented by a common
        "val" record.
        Datarep set for each constructor in a databind.
        New style error messages
--- Robert John Duncan, Feb  4 1991
        Changed to use boolean flags.
--- Robert John Duncan, Jan 21 1991
        Removed match-checking code - now done by -pregen-
--- Robert John Duncan, Oct  2 1990
        Added local structure declarations.
--- Rob Duncan, Jan 31 1990
        Modified -typecheck_fbs- so that the type of a function is
        determined by its definition rather than its use: these must unify
        anyway, but the former may involve abbreviation types from explicit
        constraints.
        Commented out warning about type variables in -typecheck_databind-
        until warnings properly enabled.
--- Rob Duncan, Nov  9 1989
        Fixed -typecheck_clause- to return the constraint type if given.
--- Rob Duncan, Sep 12 1989
        Changed -typecheck_topdec- to cope with TopDec sequences.
 */
