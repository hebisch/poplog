/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/pml/src/pregen.p
 > Purpose:         Transformation pass prior to code generation
 > Author:          Robert John Duncan, Dec 10 1990 (see revisions)
 > Documentation:
 > Related Files:
 */


section $-ml;

/*
 *  Checking a Match for Exhaustiveness and Irredundancy
 */

;;; forward references
lconstant procedure (
    reduce,
    reduce1,
);

;;; make_unmatched:
;;;     turn an ordinary pattern into an unmatched pattern

define lconstant make_unmatched(p);
    lvars p;
    if isConstPat(p) or isWildCardPat(p) then
        p;
    elseif isVarPat(p) then
        wildcard_node;
    elseif isConPat(p) then
        ;;; expand it into a proper constructor node with wildcard argument
        mkConAppPat(false, first(p), wildcard_node);
    elseif isConAppPat(p) then
        mkConAppPat(false, first(p), make_unmatched(second(p)));
    elseif isExnPat(p) then
        mkExnAppPat(false, first(p), wildcard_node);
    elseif isExnAppPat(p) then
        mkExnAppPat(false, first(p), make_unmatched(second(p)));
    elseif isTuplePat(p) then
        mkTuplePat(false, map(first(p), make_unmatched));
    elseif isLayeredPat(p) then
        make_unmatched(second(p));
    else
        bad_syntax_tree(p);
    endif;
enddefine;

;;; reduce_constructors:
;;;     reduces the unmatched pattern -u- by the pattern whose constructor
;;;     is -con- and whose argument is -p-

define lconstant reduce_constructors(con, p, u);
    lvars c, p, u, con, name, ulist;

    define lconstant constructors_of(con);
        lvars ty, con;
        val_type(con) -> ty;
        if isfuntype(ty) then type_range(ty) -> ty endif;
        ;;; must use the original constructors, in case this is an abstype
        type_orig_cons(ty);
    enddefine;

    val_name(con) -> name;
    if isWildCardPat(u) then
        ;;; expand it out to a list of constructor patterns
        [%
            For c in constructors_of(con) do
                if val_name(c) == name then
                    For u in reduce1(p, wildcard_node) do
                        mkConAppPat(false, con, u);
                    endfor;
                else
                    mkConAppPat(false, c, wildcard_node);
                endif;
            endfor
        %];
    elseif val_name(first(u)) == name
    and (reduce1(p, second(u)) ->> ulist)
    then
        ;;; constructors match, and -p- has reduced -u-'s argument
        [%
            For u in ulist do
                mkConAppPat(false, con, u);
            endfor
        %];
    else
        ;;; no reduction made
        false;
    endif;
enddefine;

;;; reduce1:
;;;     reduces the unmatched pattern -u- by the pattern -p-. Returns a list
;;;     of patterns which remain unmatched, or <false> if no reduction made.

define lconstant reduce1(p, u);
    lvars p, u, unmatched;
    if isConstPat(p) then
        if isWildCardPat(u) then
            [% mkConstPat(false, [% first(p) %]) %];
        elseif not(ispair(first(u))) then
            ;;; it represents a single, unmatched value
            first(p) = first(u) and [];
        elseif member(first(p), first(u)) then
            ;;; we've already matched this value
            false;
        else
            [% mkConstPat(false, conspair(first(p), first(u))) %];
        endif;
    elseif isWildCardPat(p) or isVarPat(p) then
        [];
    elseif isConPat(p) then
        reduce_constructors(first(p), wildcard_node, u);
    elseif isConAppPat(p) then
        reduce_constructors(first(p), second(p), u);
    elseif isExnPat(p) or isExnAppPat(p) then
        [^u];   ;;; no reduction, but not redundant! Could be better ...
    elseif isTuplePat(p) then
        if isWildCardPat(u) then
            ;;; expand it into a tuple of wildcards: (_, .., _)
            [% Repeat listlength(first(p)) times wildcard_node endrepeat %];
        else
            first(u);
        endif -> unmatched;
        if reduce(first(p), unmatched) ->> unmatched then
            [% For u in unmatched do mkTuplePat(false, u) endfor %];
        else
            false;
        endif;
    elseif isLayeredPat(p) then
        reduce1(second(p), u);
    else
        bad_syntax_tree(p);
    endif;
enddefine;

;;; reduce:
;;;     reduces the unmatched pattern list -ulist- by the pattern list
;;;     -plist-. The reduction is done component-wise and the results
;;;     combined to take account of cross effects.

define lconstant reduce(plist, ulist);
    lvars plist, ulist, ulist1, unmatched, p, u;

    define lconstant cross(l1, l2);
        lvars i1, i2, l1, l2;
        For i1 in l1 do
            For i2 in l2 do
                conspair(i1, i2);
            endfor;
        endfor
    enddefine;

    returnif(ulist == [])([]);
    Destpair(plist) -> plist -> p;
    Destpair(ulist) -> ulist -> u;
    unless (reduce1(p, u) ->> ulist1)
    and (reduce(plist, ulist) ->> unmatched)
    then
        return(false);
    endunless;
    [%
        if ulist1 == [] then
            if unmatched /== [] then cross(conspair(u, []), unmatched) endif;
        elseif unmatched == [] then
            cross(ulist1, conspair(ulist, []));
        else
            cross(ulist1, conspair(ulist, [])),
            cross(conspair(make_unmatched(p), []), unmatched)
        endif;
    %];
enddefine;

;;; reduce_unmatched:
;;;     reduces the unmatched set -unmatched- by the pattern list -plist-.
;;;     This involves reducing each sublist of -unmatched- and amalgamating
;;;     the results. If no sublist is reduced, <false> is returned.

define lconstant reduce_unmatched(plist, unmatched);
    lvars ulist, plist, unmatched, has_reduced = false;
    [%
        For ulist in unmatched do
            if reduce(plist, ulist) ->> unmatched then
                dl(unmatched);
                true -> has_reduced;
            else
                ulist;
            endif;
        endfor
    %],
    unless has_reduced then ->, false endunless;
enddefine;

;;; check_match:
;;;     check a match for exhaustiveness and irredundancy.
;;;     A match has the form:
;;;         [[p(1,1), ..., p(1,m), e(1)], ..., [p(n,1), ..., p(n,m), e(n)]]

define lconstant check_match(match) -> exhaustive -> redundant;
    lvars   rule, unmatched, tmp, match, ruleno = 1, redundant = [],
            exhaustive = false;
    returnif(match == []);
    [% [% Repeat listlength(Front(match)) - 1 times wildcard_node endrepeat
    %] %] -> unmatched;
    For rule in match do
        if reduce_unmatched(rule, unmatched) ->> tmp then
            tmp -> unmatched;
        else
            [^^redundant ^ruleno] -> redundant;
        endif;
        ruleno fi_+ 1 -> ruleno;
    endfor;
    (unmatched == []) -> exhaustive;
enddefine;


/*
 *  Constant Folding
 */

;;; try_static_eval:
;;;     try evaluating f(x), returning <false> if any errors occur

lvars
    static_eval_proc = false,
;

define lconstant try_static_eval(var, x);
    lvars var, x;

    define lconstant static_eval_loop();
        define lconstant safe_apply with_nargs 2;
            dlocal exn_pdr = false;
            define dlocal pop_exception_handler(n, mess, idstring, severity);
                clearstack();
                exitfrom(false, 1, safe_apply);
            enddefine;
            apply(), true, 2;
        enddefine;
        repeat
            suspend(safe_apply());
        endrepeat;
    enddefine;

    unless isprocess(static_eval_proc) and isliveprocess(static_eval_proc)
    then
        consproc(0, static_eval_loop) -> static_eval_proc;
    endunless;
    if val_tupled(var) then
        ml_desttuple(x) -> ;
    elseif val_arity(var) == 1 then
        x;
    else
        return(false);
    endif;
    runproc(val_value(var), val_arity(var) fi_+ 1, static_eval_proc);
enddefine;


/*
 *  Transformation of the Parse Tree
 *  (Incorporating match-checking and constant-folding)
 */

;;; forward references
lconstant procedure (
    pregen_pat,
    pregen_exp,
    pregen_lastexp,
    pregen_decs,
    pregen_strdecs,
);


;;; Special constants:

define lconstant pregen_const(k);
    if is_real_scon(k) then
        ;;; check for real overflow
        define dlocal pop_exception_handler(n, mess, idstring, severity);
            if isendstring(':arith-fltovf', idstring) then
                ml_error('real overflow\n\t%p\n', [^k], false, false);
            endif;
            false;
        enddefine;
    endif;
    scon_value(k);
enddefine;


;;; Patterns:

;;; pregen_list_pat:
;;;     turns a ListPat node into a chain of cons nodes
;;;     i.e. [p1, .., pn] --> (p1 :: .. :: pn :: nil)

define lconstant pregen_list_pat(pat) -> pat;
    lvars x, pat;
    popstackmark, app(first(pat), pregen_pat);
    nil_node -> pat;
    until (->> x) == popstackmark do
        mkTuplePat(false, [% x, pat %]) -> x;
        copy(::_node) -> pat;
        x -> second(pat);
    enduntil;
enddefine;

;;; pregen_record_pat:
;;;     turns a record pattern into a tuple. Wildcard records have any
;;;     missing fields filled in with wildcards.

define lconstant pregen_record_pat(labels, pats, wlabels);
    lvars label, labels, wlabels, pats;
    returnif(labels == [])(wildcard_node);
    nc_map(pats, pregen_pat);
    if wlabels then
        ;;; fill in missing patterns with wildcards
        [%  For label in labels do
                if wlabels /== [] and label == Front(wlabels) then
                    Destpair(pats) -> pats;
                    Back(wlabels) -> wlabels;
                else
                    wildcard_node;
                endif;
            endfor;
        %] -> pats;
    endif;
    mkTuplePat(false, pats);
enddefine;

define lconstant pregen_pat(pat) -> pat;
    lvars pat;
    if isConstPat(pat) then
        pregen_const(first(pat)) -> first(pat);
    elseif isConPat(pat) then
        if val_datarep(first(pat)) == T_UNIT then
            wildcard_node -> pat;
        endif;
    elseif isVarPat(pat) then
        if val_usage(first(pat)) == 0 then wildcard_node -> pat endif;
    elseif isUnitPat(pat) then
        wildcard_node -> pat;
    elseif isRecordPat(pat) then
        pregen_record_pat(first(pat), second(pat), false) -> pat;
    elseif isWRecordPat(pat) then
        pregen_record_pat(third(pat), second(pat), first(pat)) -> pat;
    elseif isTuplePat(pat) then
        nc_map(first(pat), pregen_pat);
    elseif isListPat(pat) then
        pregen_list_pat(pat) -> pat;
    elseif isConAppPat(pat) and val_datarep(first(pat)) == T_IDENT then
        pregen_pat(second(pat)) -> pat;
    elseif isConAppPat(pat) or isExnAppPat(pat) or isLayeredPat(pat) then
        pregen_pat(second(pat)) -> second(pat);
    elseif isTypedPat(pat) then
        pregen_pat(first(pat)) -> pat;
    endif;
enddefine;


;;; Expressions:

lvars
    lastexp = false,
        ;;; indicates the last expression in a construct
        ;;; for possible last-call optimisation
    is_leaf_fun = true,
        ;;; indicates that the current function binding has no
        ;;; nested functions
;

define lconstant pregen_val_exp(e) -> e;
    lvars e, val = first(e);
    if val_idkind(val) == ID_VAL and not(val_tracing(val)) then
        val_value(val, true) -> val;
        unless val == "undef" then
            mkConstExp(false, val) -> e;
        endunless;
    endif;
enddefine;

define lconstant pregen_record_exp(e);
    lvars e, es = second(e);
    nc_map(es, pregen_exp);
    if all(es, isConstExp) then
        mkConstExp(false, ml_constuple(#| app(es, first) |#))
    else
        mkTupleExp(false, es)
    endif;
enddefine;

define lconstant pregen_tuple_exp(e) -> e;
    lvars e, es = first(e);
    nc_map(es, pregen_exp);
    if all(es, isConstExp) then
        mkConstExp(false, ml_constuple(#| app(es, first) |#)) -> e;
    endif;
enddefine;

define lconstant pregen_seq_exp(e) -> e;
    lvars e, es = first(e);
    if back(es) == [] then
        ;;; single expression in a sequence - must be the body of a "let"
        pregen_lastexp(Front(es)) -> e;
        return;
    endif;
    repeat
        pregen_exp(Front(es)) -> Front(es);
        Back(es) -> es;
        quitif(Back(es) == []);
    endrepeat;
    pregen_lastexp(Front(es)) -> Front(es);
enddefine;

define lconstant pregen_list_exp(e) -> e;
    lvars x, e;
    popstackmark;
    app(first(e), pregen_exp);
    mkConstExp(false, []) -> e;
    until (->> x) == popstackmark or not(isConstExp(x)) do
        conspair(first(x), first(e)) -> first(e);
    enduntil;
    until x == popstackmark do
        mkTupleExp(false, [% x, e %]) -> e -> x;
    enduntil;
enddefine;

define lconstant pregen_let_exp(e) -> e;
    lvars e;
    pregen_decs(first(e)) -> first(e);
    pregen_lastexp(second(e)) -> second(e);
    if first(e) == [] then second(e) -> e endif;
enddefine;

define lconstant pregen_match(match, ishandler) -> match;
    lvars rule, match, ishandler, exhaustive, redundant, rules;
    first(match) -> rules;
    [%  For rule in rules do
            [% pregen_pat(first(rule)), pregen_lastexp(second(rule)) %];
        endfor;
    %] -> first(match);
    if ml_warnings then
        check_match(first(match)) -> exhaustive -> redundant;
        For rule in redundant do
            ml_warning('redundant rule (%p) in <match>\n', [^rule],
                popfilename, nodeline(rules(rule)));
        endfor;
        unless ishandler or exhaustive then
            ml_warning('possible Match exception', popfilename,
                nodeline(match));
        endunless;
    endif;
enddefine;

define lconstant pregen_handler(match);
    lvars match;
    pregen_match(match, true);
enddefine;

define lconstant pregen_fn(match);
    lvars   match;
    dlocal  lastexp = true;
    pregen_match(match, false);
enddefine;

define lconstant pregen_app_exp(e) -> e;
    lvars fn, arg1, e, args = [];

    define lconstant make_case_exp(e, match, lastexp);
        lvars   e, match;
        dlocal  lastexp;
        mkCaseExp(false, e, pregen_match(match, false));
    enddefine;

    pregen_exp(second(e)) -> arg1;
    first(e) -> fn;
    while isAppExp(fn) do
        conspair(arg1, args) -> args;
        pregen_exp(second(fn)) -> arg1;
        first(fn) -> fn;
    endwhile;
    if isVarExp(fn) then
        lblock;
            lvars var = first(fn);
            if  isConstExp(arg1)
            and args == []
            and val_isprimitive(var)
            and try_static_eval(var, first(arg1))
            then
                ;;; -try_static_eval- leaves its result on the stack
                () -> first(arg1);
                (arg1, false) -> (fn, arg1);
            endif;
        endlblock;
    elseif isConExp(fn) then
        lblock;
            lvars con = first(fn);
            if val_datarep(con) == T_IDENT then
                (arg1, false) -> (fn, arg1);
            elseif isConstExp(arg1) and val_datarep(con) /== T_REF then
                val_value(con)(first(arg1)) -> first(arg1);
                (arg1, false) -> (fn, arg1);
            endif;
        endlblock;
    elseif isFnExp(fn) then
        ;;; call of anonymous function becomes a case
        (make_case_exp(arg1, first(fn), lastexp and args == []), false)
            -> (fn, arg1);
    else
        pregen_exp(fn) -> fn;
    endif;
    if arg1 then conspair(arg1, args) -> args endif;
    if args == [] then
        fn -> e;
    else
        (fn, args) -> (first(e), second(e));
        ;;; mark a last-call node
        if lastexp then true -> nodeprops(e) endif;
    endif;
enddefine;

define lconstant pregen_lastexp(e) -> e;
    lvars e;
    if isConstExp(e) then
        pregen_const(first(e)) -> first(e);
    elseif isVarExp(e) then
        pregen_val_exp(e) -> e;
    elseif isConExp(e) then
        mkConstExp(false, val_value(first(e))) -> e;
    elseif isExnExp(e) then
        pregen_val_exp(e) -> e;
    elseif isUnitExp(e) then
        mkConstExp(false, ml_unit) -> e;
    elseif isRecordExp(e) then
        pregen_record_exp(e) -> e;
    elseif isTupleExp(e) then
        pregen_tuple_exp(e) -> e;
    elseif isSeqExp(e) then
        pregen_seq_exp(e) -> e;
    elseif isListExp(e) then
        pregen_list_exp(e) -> e;
    elseif isLetExp(e) then
        pregen_let_exp(e) -> e;
    elseif isAppExp(e) then
        pregen_app_exp(e) -> e;
    elseif isTypedExp(e) then
        pregen_lastexp(first(e)) -> e;
    elseif isAndExp(e) or isOrExp(e) then
        pregen_exp(first(e)) -> first(e);
        pregen_lastexp(second(e)) -> second(e);
    elseif isWhileExp(e) then
        pregen_exp(first(e)) -> first(e);
        pregen_exp(second(e)) -> second(e);
    elseif isIfExp(e) then
        pregen_exp(first(e)) -> first(e);
        pregen_lastexp(second(e)) -> second(e);
        pregen_lastexp(third(e)) -> third(e);
    elseif isCaseExp(e) then
        pregen_exp(first(e)) -> first(e);
        pregen_match(second(e), false) -> second(e);
    elseif isFnExp(e) then
        pregen_fn(first(e)) -> first(e);
        false -> is_leaf_fun;
    elseif isRaiseExp(e) then
        pregen_exp(first(e)) -> first(e);
    elseif isHandleExp(e) then
        pregen_exp(first(e)) -> first(e);
        pregen_handler(second(e)) -> second(e);
    endif;
enddefine;

define lconstant pregen_exp(e);
    lvars   e;
    dlocal  lastexp = false;
    pregen_lastexp(e);
enddefine;


;;; Declarations and Bindings:

define lconstant pregen_val_bind(vb);
    lvars vb;
    SWITCH vb
    CASE ValBind(pat, e) then
        lvars pat1, e1;
        pregen_pat(pat) ->> pat1 -> first(vb);
        pregen_exp(e) ->> e1 -> second(vb);
        SWITCH pat1
        CASE VarPat(var) then
            SWITCH e1
            CASE ConstExp(k) then
                ;;; bind now and discard the node
                SWITCH e
                CASE VarExp(evar) then
                    ;;; copy attributes as well as value
                    explode(val_ident(evar)) -> explode(val_ident(var));
                else
                    k -> val_value(var);
                ENDSWITCH;
                return;
            else
            ENDSWITCH;
        CASE WildCardPat then
            ;;; skip exhaustiveness check
        else
/*
            if ml_warnings then
                lvars (, exhaustive) = check_match([[^pat1 ^false]]);
                unless exhaustive then
                    ml_warning('possible Bind exception', popfilename,
                        nodeline(vb));
                endunless;
            endif;
*/
        ENDSWITCH
    ENDSWITCH;
    vb;
enddefine;

define lconstant pregen_val_dec(dec);
    lvars dec, vbs, recvbs;
    map(third(dec), pregen_val_bind) ->> vbs -> third(dec);
    map(fourth(dec), pregen_val_bind) ->> recvbs -> fourth(dec);
    returnunless(vbs == [] and recvbs == [])(dec);
enddefine;

define lconstant pregen_fun_bind(fb);
    lvars   clause, match, fb, exhaustive, redundant, clauses;
    dlocal  lastexp = true, is_leaf_fun = true;

    define lconstant is_tuple_rule(/* rule */) with_nargs 1;
        lvars pat = Front(/* rule */);
        if isTuplePat(pat) then
            ;;; if it has a line number, it must be a real tuple;
            ;;; otherwise it was created from a RecordPat
            nodeline(pat);
        else
            false;
        endif;
    enddefine;

    third(fb) -> clauses;
    [%  For clause in clauses do
            [%  app(first(clause), pregen_pat),
                pregen_lastexp(second(clause))
            %];
        endfor;
    %] ->> match -> third(fb);
    if ml_warnings then
        check_match(match) -> exhaustive -> redundant;
        For clause in redundant do
            ml_warning('redundant clause (%p) in function\n\t%p\n',
                [% clause, val_name(first(fb)) %], popfilename,
                nodeline(clauses(clause)));
        endfor;
        unless exhaustive then
            ml_warning('possible Match exception in function\n\t%p\n',
                [% val_name(first(fb)) %], popfilename, nodeline(fb));
        endunless;
    endif;
    ;;; is this a tupled function?
    if second(fb) == 1 and all(match, is_tuple_rule) then
        llength(first(Front(Front(match)))) -> val_arity(first(fb));
        true -> val_tupled(first(fb));
        nc_map(match,
            procedure(rule);
                lvars rule;
                first(Front(rule)) <> Back(rule);
            endprocedure);
    endif;
    if is_leaf_fun then "leaf" -> nodeprops(fb) endif;
enddefine;

define lconstant pregen_dec(dec);
    lvars dec;
    if isValDec(dec) then
        pregen_val_dec(dec);
    elseif isFunDec(dec) then
        app(third(dec), pregen_fun_bind);
        false -> is_leaf_fun;
        dec;
    elseif isExceptionDec(dec) then
        dec;
    elseif isAbstypeDec(dec) then
        copy(dec) -> dec;
        pregen_decs(third(dec)) -> third(dec);
        dec;
    elseif isLocalDec(dec) then
        copy(dec) -> dec;
        pregen_decs(first(dec)) -> first(dec);
        pregen_decs(second(dec)) -> second(dec);
        dec;
    endif;
enddefine;

define lconstant pregen_decs(decs);
    lvars decs;
    map(decs, pregen_dec);
enddefine;


;;; Modules:

define lconstant pregen_strexp(strexp) -> strexp;
    lvars strexp;
    if isAppStrexp(strexp) then
        pregen_strexp(second(strexp)) -> second(strexp);
    elseif isGenStrexp(strexp) then
        pregen_strdecs(second(strexp)) -> second(strexp);
    elseif isLetStrexp(strexp) then
        pregen_strdecs(first(strexp)) -> first(strexp);
        pregen_strexp(second(strexp)) -> second(strexp);
    endif;
enddefine;

define lconstant pregen_str_bind(sb);
    lvars sb;
    pregen_strexp(third(sb)) -> third(sb);
enddefine;

define lconstant pregen_strdec(dec);
    lvars dec;
    if isStructureDec(dec) then
        app(second(dec), pregen_str_bind);
        dec;
    elseif isLocalStrDec(dec) then
        copy(dec) -> dec;
        pregen_strdecs(first(dec)) -> first(dec);
        pregen_strdecs(second(dec)) -> second(dec);
        dec;
    else
        pregen_dec(dec);
    endif;
enddefine;

define lconstant pregen_strdecs(decs);
    lvars decs;
    map(decs, pregen_strdec);
enddefine;

define lconstant pregen_fnc_dec(dec) -> dec;
    lvars bind, dec;
    For bind in second(dec) do
        pregen_strexp(fifth(bind)) -> fifth(bind);
    endfor;
enddefine;


/*
 *  Transformation Pass Prior to Code Generation
 */

;;; pregen:
;;;     returns a list of declarations.

define pregen(topdec);
    lvars   topdec;
    dlocal  static_eval_proc = false, lastexp = false, is_leaf_fun = true;
    if isFncTopDec(topdec) then
        maplist(first(topdec), pregen_fnc_dec);
    elseif isStrTopDec(topdec) then
        maplist(first(topdec), pregen_strdec);
    else
        [];
    endif;
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Apr 26 1996
        Changes for new Poplog exception handling
--- Robert John Duncan, Jul  3 1995
        Changed node-matching syntax to SWITCH/CASE/ENDSWITCH
--- Robert John Duncan, Dec 20 1994
        Suppressed constant replacement of traced functions so that the code
        generator can supply a more specific type for them later
--- Robert John Duncan, Dec 20 1994
        Changes for the new treatment of wrapped/unwrapped function values:
        every function now has only a single unwrapped value. try_static_eval
        changed to work with the unwrapped value, and pregen_val_bind
        extended to copy the unwrapped value rather than the wrapped version.
        Also, val_isprimitive is now recorded directly in the identifier, so
        no need for a separate table here.
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Oct 24 1994
        Changes to error reporting
--- Robert John Duncan, Jan  7 1993
        Changed to copy local and abstype declaration nodes before updating
        to prevent bindings being lost from top-level printing.
--- Robert John Duncan, Nov  1 1991
        Renamed warning and error procedures.
--- Robert John Duncan, Sep 27 1991
        Fixed the match-checking code to work for abstract types by using
        the original constructors from tycons
--- Robert John Duncan, Sep 24 1991
        Extended -pregen_app_exp- to transform application of FnExp to
        CaseExp (previously done by -gen_app_exp-).
--- Simon Nichols, Jun 27 1991
        Changes to support the detection of leaf functions.
--- Robert John Duncan, Feb 11 1991
        Exceptions, constructors and variables now represented by a common
        "val" record.
        New style warning messages
--- Robert John Duncan, Jan 21 1991
        Change to implementation of exception handling: code for a handler
        is now in-line, so it must inherit the -lastexp- attribute from its
        caller.
 */
