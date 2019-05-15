/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:           C.all/plog/src/gencode.p
 > Purpose:        Prolog: compile Prolog to VM code.
 > Author:         John Gibson, Robert Duncan and Simon Nichols, 1992 (see revisions)
 >                  (based on Mellish and Laventhol, 1985)
 > Documentation:  REF * VMCODE
 */


include vm_flags

section prolog;

constant
    procedure ( transform_body, isground, prolog_doeval, pred_spec, ),
    prolog_variable_tok_key,
;

weak global vars
    $-pop_show_code,
;

;;; ========================================================================

vars
    prolog_show_code = false,
        ;;; flag for tracing code-planting
    wascut,
        ;;; for a dynamic predicate, indicates whether the last clause
        ;;; "interpreted" was cut
;

;;; Variables relating to the current predicate procedure
lvars
    curr_pred_record,       ;;; predicate record
    curr_pred_alias,        ;;; alias name
    curr_pred_arity,        ;;; arity
    curr_pred_arg_vars,     ;;; list of formal parameter lvars
    curr_pred_start_lab,    ;;; procedure start label
    curr_pred_end_lab,      ;;; procedure end label
;

;;; Variables relating to the current clause
lvars
    curr_clause_vars_seen,
        ;;; lex/perm vars representing variables encountered so far, for
        ;;; distinguishing initial from subsequent occurrences of a variable
    curr_clause_usage_env,
        ;;; environment mapping variables to their usage (see below)
    curr_clause_was_cut,
        ;;; flags whether a top level cut has been seen during code planting
    curr_clause_embedded_cut,
        ;;; initially <false> for each clause, it is assigned the argument
        ;;; to prolog_cut/1 (a variable) if the clause has an embedded cut
    curr_clause_fail_lab,
        ;;; label to be placed at the next fail point
;

lconstant
    MAX_INLINE_ARGS = 10,
        ;;; maximum number of arguments we're prepared to match inline
;

;;; The usage environment maps prologvars to their usage. Initially this
;;; is either "single" or "multiple", depending on how many times the
;;; variable occurs in the term. Variables with multiple usage (i.e.,
;;; non-singleton variables) are eventually updated with the names of
;;; the VM variables (lexical or permanent) used to represent them.

;;; make_usage_env:
;;;     creates an environment mapping each unbound variable in a clause
;;;     to its "usage" (single or multiple). The clause is given as a
;;;     head term plus a list of body goals.

define make_usage_env(head, body) -> env;
    lvars head, body, env = [];

    define lconstant find_vars(/* term */);
        lvars i, term = prolog_deref(/* term */);

        PROLOG_GO_ON term;

        PT_VAR:
            For i in env do
                returnif(Front(i) == term)("multiple" -> Back(i));
            endfor;
            conspair(term, "single") :: env -> env;
            return;

        PT_PAIR:
            find_vars(Front(term));
            chain(Back(term), find_vars);

        PT_TERM:
            For i to fast_prolog_arity(term) do
                find_vars(fast_prolog_arg(i, term));
            endfor;
            return;

        PT_NIL:
        PT_CONST:
        PT_OTHER:
            return;
    enddefine;

    find_vars(head);
    applist(body, find_vars);
enddefine;

;;; var_usage:
;;;     returns the usage for the prolog variable -var-.

define lconstant var_usage(var);
    lvars entry, var;
    For entry in curr_clause_usage_env do
        returnif(Front(entry) == var)(Back(entry));
    endfor;
    mishap(var, 1, 'NO USAGE FOR PROLOG VARIABLE');
enddefine;
;;;
define updaterof var_usage(usage, var);
    lvars entry, var, usage;
    ;;; If there is an existing entry for -var-, update it
    For entry in curr_clause_usage_env do
        returnif(Front(entry) == var)(usage -> Back(entry));
    endfor;
    ;;; No existing entry, so add a new one
    conspair(var, usage) :: curr_clause_usage_env -> curr_clause_usage_env;
enddefine;

;;; prolog_fail_label:
;;;     returns next fail point label, creating it if necessary.

define prolog_fail_label();
    if curr_clause_was_cut then
        curr_pred_end_lab;
    elseif curr_clause_fail_lab then
        curr_clause_fail_lab;
    else
        sysNEW_LABEL() ->> curr_clause_fail_lab;
    endif;
enddefine;


;;; -- Pseudo Prolog VM instructions --------------------------------------

;;; sysPLOG_IFNOT_UNIFY:
;;;     similar to sysPLOG_IFNOT_ATOM in the core system, except using
;;;     -prolog_unify- to match.

define vars sysPLOG_IFNOT_UNIFY(/* arg_var, arg_qual, */ item, item_qual,
                                fail_lab) with_nargs 5;
    lvars item, item_qual, fail_lab;
    sysPLOG_ARG_PUSH(/* arg_var, arg_qual */);
    sysPLOG_ARG_PUSH(item, item_qual);
    sysCALL("prolog_unify");
    sysIFNOT(fail_lab);
enddefine;

;;; sysPLOG_IFNOT_FAIL:
;;;     jumps to the next fail point label if the top item on the stack is
;;;     <false>. The item is always removed.

define lconstant sysPLOG_IFNOT_FAIL();
    sysIFNOT(prolog_fail_label());
enddefine;


;;; -- Generate code to construct a Prolog term ---------------------------

;;; comp_build_term, comp_build_term_nd:
;;;     generate code to reconstruct a Prolog term, with and without
;;;     dereferencing variables

define lconstant comp_term(/* term, */ needs_deref) with_nargs 2;
    lvars needs_deref;

    define lconstant do_comp_term(/* term */) with_nargs 1;
        lvars term = prolog_deref(/* term */);

        returnif(isground(term))(sysPUSHQ(prolog_full_deref(term)));
        PROLOG_GO_ON term;

        PT_VAR:
            lvars var = var_usage(term);
            if var == "single" then
                sysCALL("prolog_newvar");
            elseif Lmember(var, curr_clause_vars_seen) then
                ;;; subsequent occurrence of a variable
                sysPUSH(var);
                if needs_deref then sysCALL("prolog_deref") endif;
            else
                ;;; first occurrence of a variable
                var :: curr_clause_vars_seen -> curr_clause_vars_seen;
                sysCALL("prolog_newvar");
                sysPOP(var);
                sysPUSH(var);
            endif;
            return;

        PT_PAIR:
            do_comp_term(Front(term));
            do_comp_term(Back(term));
            sysCALL("conspair");
            return;

        PT_TERM:
            prolog_appargs_nd(term, do_comp_term);
    #_IF PROLOG_FIXED_FUNCTORS
            sysPUSHQ(fast_prolog_functor(term));
    #_ELSE
            do_comp_term(fast_prolog_functor(term));
    #_ENDIF
            sysPUSHQ(fast_prolog_arity(term));
            sysCALL("consprologterm");
            return;

        PT_NIL:
        PT_CONST:
        PT_OTHER:
            /* these cases should have been covered by -isground- */
            mishap(0, 'SYSTEM ERROR IN comp_term');
    enddefine;

    do_comp_term(/* term */);
enddefine;

define lconstant comp_build_term =
    comp_term(% true %);
enddefine;

define lconstant comp_build_term_nd =
    comp_term(% false %);
enddefine;

;;; comp_eval_term:
;;;     compile code for a Prolog term interpreted as a Pop-11
;;;     expression.

define lconstant comp_eval_term(/* term */);
    lvars term = prolog_deref(/* term */);

    PROLOG_GO_ON term;

    PT_VAR:
        comp_build_term_nd(term);
        sysCALL("ident prolog_doeval");
        return;

    PT_PAIR:
        comp_eval_term(Front(term));
        comp_eval_term(Back(term));
        sysCALL("conspair");
        return;

    PT_TERM:
        lvars (fn, arity) = prolog_termspec(term);
        if fn == "quote" then
            prolog_appargs_nd(term, comp_build_term);
        elseif fn == "quote_nd" then
            prolog_appargs_nd(term, comp_build_term_nd);
        elseif fn == "valof" and arity == 1 then
            lvars arg = prolog_arg(1, term);
            if isword(arg) then
                sysPUSH(arg);
            else
                comp_build_term(arg);
                sysCALL("valof");
            endif;
        elseif fn == "apply" then
            lvars i;
            For i to arity fi_- 1 do
                comp_eval_term(fast_prolog_arg(i, term));
            endfor;
            lvars upd, arg = prolog_arg(arity, term);
            if prolog_checkspec(arg, "updater", 1) ->> upd then
                prolog_arg(1, arg) -> arg;
            endif;
            if prolog_checkspec(arg, "valof", 1)
            and isword(prolog_arg(1, arg))
            then
                if upd then sysUCALL else sysCALL endif
                    (prolog_arg(1, arg));
            else
                comp_eval_term(arg);
                if upd then sysUCALLS else sysCALLS endif(0);
            endif;
        elseif fn == "-" and arity == 1 then
            comp_eval_term(fast_prolog_arg(1, term));
            sysCALL("negate");
        elseif fn == "." and arity == 2 then
            comp_eval_term(fast_prolog_arg(1, term));
            comp_eval_term(fast_prolog_arg(2, term));
            sysCALL("conspair");
        else
            prolog_appargs_nd(term, comp_eval_term);
            sysCALL(fn);
        endif;
        return;

    PT_NIL:
    PT_CONST:
        sysPUSHQ(term);
        return;

    PT_OTHER:
        comp_build_term(term);
        return;

enddefine;


;;; -- Generate compact head matching code --------------------------------

;;; comp_unify_patt:
;;;     generate code for pattern unification.

define lconstant comp_unify_patt(arg);
    lvars
        entry, patt, arg,
        var_ref_list        = [],
            ;;; maps variables to unique references, which are both embedded
            ;;; in the pattern and also used at compile time to ensure that a
            ;;; var/ref pair only occurs once in -update_before_match-
        update_before_match = [],
            ;;; var/ref pairs for variables which occur before the pattern
        update_after_match  = [],
            ;;; var/ref pairs for variables which occur first in the pattern
    ;

    ;;; returns the reference for a variable seen before in the clause
    define lconstant var_ref(var) -> ref;
        lvars var, ref, list;
        if (Lmember(var, var_ref_list) ->> list) then
            ;;; already seen in the pattern
            Front(Back(list)) -> ref;
        else
            ;;; not seen in the pattern but seen in a previous argument
            consref(true) -> ref;
            var :: (ref :: var_ref_list) -> var_ref_list;
        endif;
    enddefine;

    ;;; returns a new reference for a variable not seen before
    define lconstant new_var_ref(var) -> ref;
        lvars var, ref = consref(false);
        var :: (ref :: var_ref_list) -> var_ref_list;
    enddefine;

    ;;; creates the pattern and the update lists, returning the pattern
    define lconstant create_pattern(/* term */) with_nargs 1;
        lvars term = prolog_deref(/* term */), var, ref;
        lconstant
            CONS        = 1,
            TERM        = 2,
            IGNORE      = 3,
            FIRSTVAR    = 4,
            NTHVAR      = 5,
            CONSTANT    = 6,
        ;
        PROLOG_GO_ON term;

        PT_VAR:
            var_usage(term) -> var;
            if var == "single" then
                IGNORE;
            elseif Lmember(var, curr_clause_vars_seen) then
                ;;; subsequent occurrence of a variable
                var_ref(term) -> ref;
                if Cont(ref) then
                    ;;; first occurrence in pattern
                    conspair(var, ref) :: update_before_match
                        -> update_before_match;
                    false -> Cont(ref);
                endif;
                NTHVAR, ref;
            else
                ;;; first occurrence of a variable
                var :: curr_clause_vars_seen -> curr_clause_vars_seen;
                new_var_ref(term) -> ref;
                conspair(var, ref) :: update_after_match -> update_after_match;
                FIRSTVAR, ref;
            endif;
            return;

        PT_PAIR:
            CONS, create_pattern(Front(term)), create_pattern(Back(term));
            return;

        PT_TERM:
            TERM, datalength(term),     ;;; Note: length NOT arity
#_IF PROLOG_FIXED_FUNCTORS
            fast_prolog_functor(term),
#_ELSE
            create_pattern(fast_prolog_functor(term)),
#_ENDIF
            prolog_appargs_nd(term, create_pattern);
            return;

        PT_NIL:
        PT_CONST:
        PT_OTHER:
            CONSTANT, term;
            return;

    enddefine;

    {% create_pattern(arg) %} -> patt;
    For entry in update_before_match do
        ;;; update the reference with the value from the variable
        sysPUSH(Front(entry)), sysPUSHQ(Back(entry)), sysUCALL("fast_cont");
    endfor;
    ;;; Do the pattern unification
    sysPUSHQ(patt), sysCALLQ(prolog_unify_patt);
    For entry in update_after_match do
        ;;; update the variable with the value from the reference
        sysPUSHQ(Back(entry)), sysCALL("fast_cont"), sysPOP(Front(entry));
    endfor
enddefine;

;;; comp_compact_unify:
;;;     generates compact matching code for -arg- (an argument of a complex
;;;     term in the head of a clause). If -arg- contains variables, pattern
;;;     unification is used.

define lconstant comp_compact_unify(arg_var, arg);
    lvars arg_var, arg;
    if curr_clause_usage_env == [] or isground(arg) then
        sysPLOG_IFNOT_UNIFY(arg_var, true, prolog_full_deref(arg), false,
            prolog_fail_label());
    else
        sysPUSH(arg_var);
        comp_unify_patt(arg);
        sysPLOG_IFNOT_FAIL();
    endif;
enddefine;


;;; -- Generate head matching code ----------------------------------------

lconstant procedure comp_match_complex_term;    ;;; forward declaration

;;; comp_match_complex_term_arg:
;;;     generates code for matching an argument of a complex term in the head
;;;     of a clause.

define lconstant comp_match_complex_term_arg(/* arg_var, arg_qual, term */)
                                                                with_nargs 3;
    lvars term = prolog_deref(/* term */), var;
    PROLOG_GO_ON term;

    PT_VAR:
        returnif((var_usage(term) ->> var) == "single") (-> (,));
            ;;; nothing to do -- erase arg_var, arg_qual and return
        if Lmember(var, curr_clause_vars_seen) then
            ;;; subsequent occurrence of a variable
            sysPLOG_IFNOT_UNIFY(/* arg_var, arg_qual, */ var, true,
                prolog_fail_label());
        else
            ;;; first occurrence  of a variable
            sysPLOG_ARG_PUSH(/* arg_var, arg_qual */);
            sysPOP(var);
            var :: curr_clause_vars_seen -> curr_clause_vars_seen;
        endif;
        return;

    PT_PAIR:
    PT_TERM:
        comp_match_complex_term(/* arg_var, arg_qual, */ term);
        return;

    PT_NIL:
    PT_CONST:
        sysPLOG_IFNOT_ATOM(/* arg_var, arg_qual, */ term, false,
            prolog_fail_label());
        return;

    PT_OTHER:
        sysPLOG_IFNOT_UNIFY(/* arg_var, arg_qual, */ term, false,
            prolog_fail_label());
        return;

enddefine;


;;; comp_match_complex_term:
;;;     generates code for matching a complex term in the head of a clause.

define lconstant comp_match_complex_term(/* arg_var, arg_qual, */ term)
                                                                with_nargs 3;
    lvars arg, i, term, var_lab, success_lab, save_vars_seen;
    sysPLOG_TERM_SWITCH(/* arg_var, arg_qual */ term,
        sysNEW_LABEL() ->> var_lab, prolog_fail_label());
    ;;; Control drops through if the argument is a structure or pair, leaving
    ;;; the dereferenced argument on the stack.
    sysPOP(sysNEW_LVAR() ->> arg);
    ;;; Generate code to match the arguments of the structure/pair.
    ;;; Don't record vars seen because we want to start from the same point
    ;;; when generating code to construct the term, below.
    curr_clause_vars_seen -> save_vars_seen;
    if ispair(term) then
        comp_match_complex_term_arg(arg, "fast_front", Front(term));
        comp_match_complex_term_arg(arg, "fast_back", Back(term));
    else
        For i to fast_prolog_arity(term) do
            comp_match_complex_term_arg(arg, i, fast_prolog_arg(i, term))
        endfor;
    endif;
    save_vars_seen -> curr_clause_vars_seen;
    ;;; Jump round term constructing code.
    sysGOTO(sysNEW_LABEL() ->> success_lab);
    ;;; Generate code to construct the structure/pair.
    ;;; Control reaches here if -arg- is an unbound variable (which is left
    ;;; on the stack for -prolog_assign-).
    sysLABEL(var_lab);
    comp_build_term_nd(term);
    sysCALL("prolog_assign");
    ;;; Success.
    sysLABEL(success_lab);
enddefine;


;;; comp_match_head_arg:
;;;     generates code for matching an argument in the head of a clause.

define lconstant comp_match_head_arg(/* arg, */ arg_var);
    lvars arg = prolog_deref(/* arg */), arg_var, var;

    define lconstant compact_code_needed(term);
        lvars i, term, arity = prolog_arity(term);
        For i to arity do
            returnif(prolog_complexterm(prolog_arg(i, term)))(true);
        endfor;
        false;
    enddefine;

    PROLOG_GO_ON arg;

    PT_VAR:
        var_usage(arg) -> var;
        unless var == "single" or var == arg_var then
            sysPLOG_IFNOT_UNIFY(var, true, arg_var, true, prolog_fail_label());
        endunless;
        return;

    PT_PAIR:
    PT_TERM:
        if compact_code_needed(arg) then
            comp_compact_unify(arg_var, arg);
        else
            comp_match_complex_term(arg_var, true, arg);
        endif;
        return;

    PT_NIL:
    PT_CONST:
        sysPLOG_IFNOT_ATOM(arg_var, true, arg, false, prolog_fail_label());
        return;

    PT_OTHER:
        sysPLOG_IFNOT_UNIFY(arg_var, true, arg, false, prolog_fail_label());
        return;

enddefine;


;;; comp_match_head:
;;;     generates code for matching the head of a clause.

define lconstant comp_match_head(head);
    lvars head, i, arg_var, arg_vars = curr_pred_arg_vars;
    if curr_pred_arity > MAX_INLINE_ARGS then
        For i to MAX_INLINE_ARGS do
            Destpair(arg_vars) -> (arg_var, arg_vars);
            comp_match_head_arg(prolog_arg(i, head), arg_var);
        endfor;
        comp_compact_unify(
            Front(arg_vars),
            consprologterm(
                For i from MAX_INLINE_ARGS fi_+ 1 to curr_pred_arity do
                    prolog_arg(i, head)
                endfor,
                "DUMMY",
                curr_pred_arity fi_- MAX_INLINE_ARGS));
    else
        1 -> i;
        until arg_vars == [] do
            Destpair(arg_vars) -> (arg_var, arg_vars);
            comp_match_head_arg(prolog_arg(i, head), arg_var);
            i fi_+ 1 -> i;
        enduntil;
    endif
enddefine;


;;; -- Generate code for the body of a clause -----------------------------

;;; inline_expansion:
;;;     mapping from predicate names to inline expansions

define inline_expansion =
    newproperty([], 64, false, "perm");
enddefine;

;;; expand_inline:
;;;     generates the inline expansion of a term

define expand_inline(term, expansion);
    lvars term, expansion;
    ;;; create initial environment mapping vars in the clause head to
    ;;; arguments in the term
    lvars i, (head, goals) = Destpair(expansion);
    dlvars env = [%
        For i to prolog_arity(term) do
            conspair(prolog_arg(i, head), prolog_arg(i, term));
        endfor;
    %];
    ;;; copy goals from the clause body, replacing variables from the
    ;;; environment (this is identical to the code from prolog_instance)
    define lconstant do_var_tok(tok);
        lvars n, tok;
    compile_mode:vm -pentch -bjmpch;
        For n in env do
            returnif(Front(n) == tok)(Back(n), false)
        endfor;
        prolog_newvar() -> n;
        conspair(conspair(tok, n), env) -> env;
        n, false        ;;; false = changed, don't unpeel recursively
    enddefine;
    For term in goals do
        Prolog_unpeel(term, prolog_variable_tok_key, do_var_tok);
    endfor;
enddefine;

;;; comp_inline:
;;;     try compiling a goal as inline code

define lconstant comp_inline(goal) -> done;
    lvars goal, done = true;

    define lconstant unify_result(arg);
        lvars arg, var;
        if isprologvar(arg)
        and not(Lmember(var_usage(arg) ->> var, curr_clause_vars_seen))
        then
            ;;; first occurrence of a variable
            if var == "single" then
                sysERASE(0);
            else
                sysPOP(var);
                var :: curr_clause_vars_seen -> curr_clause_vars_seen;
            endif;
        else
            comp_build_term_nd(arg);
            sysCALL("prolog_unify");
            sysPLOG_IFNOT_FAIL();
        endif;
    enddefine;

    lvars name = fast_prolog_functor(goal);
    if name == "ident fail\/0" then
        sysGOTO(prolog_fail_label());
    elseif name == "ident prolog_eval\/1" then
        comp_eval_term(fast_prolog_arg(1, goal));
    elseif name == "ident prolog_eval\/2" then
        comp_eval_term(fast_prolog_arg(1, goal));
        unify_result(fast_prolog_arg(2, goal));
    elseif name == "ident prolog_evaltrue\/1" then
        comp_eval_term(fast_prolog_arg(1, goal));
        sysPLOG_IFNOT_FAIL();
    elseif name == "ident prolog_val\/2" then
        lvars var = fast_prolog_arg(1, goal);
        if isword(var) then
            sysPUSH(var);
        else
            comp_build_term(var);
            sysCALL("valof");
        endif;
        unify_result(fast_prolog_arg(2, goal));
    elseif name == "ident prolog_setq\/2" then
        lvars var = fast_prolog_arg(1, goal);
        comp_eval_term(fast_prolog_arg(2, goal));
        if isword(var) then
            sysPOP(var);
        else
            comp_build_term(var);
            sysUCALL("valof");
        endif;
    else
        ;;; can't inline
        false -> done;
    endif;
enddefine;

;;; comp_body_goals:
;;;     generates code for each goal in the clause body.

define lconstant comp_body_goals(goals, last_clause);
    lvars goals, last_clause;
    lvars first_call = false; ;;; first call in the body or after a cut
    lvars n_contns = 0; ;;; number of continuations after first call

    define lconstant push_contns(n_contns);
        lvars n_contns;
        if n_contns == 1 then
            sysCALL("prolog_push_continuation");
        elseif n_contns /== 0 then
            sysPUSHQ(n_contns), sysCALL("prolog_push_continuations");
        endif
    enddefine;

    until goals == [] do
        lvars goal, expansion;
        Destpair(goals) -> (goal, goals);
        if goal == "!" then
            if first_call then
                sysPUSHQ("prolog_own_exit_on_success"), sysPUSHQ(1);
                push_contns(n_contns fi_+ 1);
                sysPUSH(first_call), sysCALL("prolog_own_invoke");
                sysPLOG_IFNOT_FAIL();
            endif;
            true -> curr_clause_was_cut;
            false -> first_call;
            0 -> n_contns;
        elseif first_call then
            ;;; continuation goal
            prolog_appargs_nd(goal, comp_build_term_nd);
            sysPUSHQ(fast_prolog_functor(goal));
            sysPUSHQ(fast_prolog_arity(goal) fi_+ 1);
            n_contns fi_+ 1 -> n_contns;
        elseif inline_expansion(fast_prolog_functor(goal)) ->> expansion then
            [^(expand_inline(goal, expansion)) ^^goals] -> goals;
        elseunless comp_inline(goal) then
            ;;; first real call
            prolog_appargs_nd(goal, comp_build_term_nd);
            fast_prolog_functor(goal) -> first_call;
        endif;
    enduntil;
    push_contns(n_contns);
    ;;; Plant code to call the predicate named by -first_call-
    unless first_call then
        "prolog_apply_continuation" -> first_call;
    endunless;
    if not(curr_clause_was_cut or last_clause) then
        ;;; there's a choice point
        sysCALL(first_call);
    elseif curr_clause_embedded_cut then
        ;;; must leave the stack frame for prolog_cut/1 to chain back to
        sysCALL(first_call);
        sysGOTO(curr_pred_end_lab);
    elseif identof(first_call) == identof(curr_pred_alias)
    and pred_system(curr_pred_record)
    then
        ;;; re-save choice point values and jump back to procedure start
        ;;; NOTE: this optimisation precludes spying
        sysPLOG_RESTART(curr_pred_start_lab);
    else
        sysPUSH(first_call), sysCALL("chain");
    endif;
enddefine;


;;; -- Generate code for a clause -----------------------------------------

;;; precomp_clause:
;;;     transform a clause and determine its variable usage before
;;;     generating any code

define lconstant precomp_clause(clause) -> (head, body, env, cut_var);
    lvars clause;
    lvars head = prolog_head(clause);
    lvars (body, cut_var) = transform_body(prolog_body(clause));
    lvars env = make_usage_env(head, body);
enddefine;

;;; setup_clause_lvars:
;;;     For each non-singleton variable, updates its usage to be the name of
;;;     the lexical variable representing it.

define lconstant setup_clause_lvars(head);
    lvars head, i, entry, arg, arity = min(curr_pred_arity, MAX_INLINE_ARGS);
    ;;; get argument variable lvars from -curr_pred_arg_vars-
    unless curr_pred_arg_vars == [] then
        For i from arity by -1 to 1 do
            prolog_arg(i, head) -> arg;
            if isprologvar(arg) and var_usage(arg) == "multiple" then
                Subscrl(i, curr_pred_arg_vars) -> var_usage(arg);
            endif;
        endfor;
    endunless;
    ;;; create new lvars for remaining (non-argument) variables
    For entry in curr_clause_usage_env do
        if Back(entry) == "multiple" then
            sysNEW_LVAR() -> Back(entry);
        endif;
    endfor;
enddefine;

;;; comp clause:
;;;     generates code for a single clause.

define lconstant comp_clause(head, body, env, cut_var, last_clause);
    lvars head, body, env, cut_var, last_clause;
    dlocal
        curr_clause_usage_env       = env,
        curr_clause_vars_seen       = curr_pred_arg_vars,
        curr_clause_embedded_cut    = cut_var,
        curr_clause_was_cut         = false,
        curr_clause_fail_lab        = false,
        pop_new_lvar_list,
    ;
    unless last_clause then sysPLOG_SAVE() endunless;
    setup_clause_lvars(head);
    comp_match_head(head);
    if cut_var then
        ;;; clause has an embedded cut: save the current (relative)
        ;;; callstack length in a temporary variable
        lvars stacklen_v = sysNEW_LVAR();
        sysPUSHQ(1), sysCALL("callstacklength");
        sysPUSH("prolog_barrier_apply"), sysCALL("callstacklength");
        sysCALL("fi_-");
        sysPOP(stacklen_v);
        ;;; bind the temporary to cut_var
        stacklen_v -> var_usage(cut_var);
        conspair(stacklen_v, curr_clause_vars_seen) -> curr_clause_vars_seen;
    endif;
    comp_body_goals(body, last_clause == true);
    if curr_clause_fail_lab then
        sysLABEL(curr_clause_fail_lab);
    endif;
    unless last_clause then sysPLOG_RESTORE() endunless;
enddefine;


;;; -- Generate code for a unit clause group ------------------------------

;;; comp_unit_clause_group:
;;;     generates code for a list of unit clauses all of the same kind.

define lconstant comp_unit_clause_group(clauses, last, unify_pdrs, body, cut);
    lvars   i, arg_v, arg_vs, clause_v, clauses_v, fail_lab, loop_lab,
            clauses, last, unify_pdrs, body, cut;
    dlocal
        pop_vm_flags = pop_vm_flags fi_|| VM_NO_BACK_JUMP_CHECKS,
        pop_new_lvar_list,
    ;
    sysNEW_LABEL() -> fail_lab;
    sysNEW_LVAR() -> clause_v;
    sysPUSHQ(clauses), sysPOP(sysNEW_LVAR() ->> clauses_v);
    ;;; Start of loop to match all but the last clause
    sysLABEL(sysNEW_LABEL() ->> loop_lab);
    sysPUSH(clauses_v), sysCALL("fast_destpair"), sysPOP(clauses_v);
    sysPOP(clause_v);
    if body then
        ;;; clause is of the form (H :- B) so get the head
        sysPLOG_ARG_PUSH(clause_v, 1), sysPOP(clause_v);
    endif;
    sysPLOG_SAVE();
    ;;; Apply match code-planting procedure from -unify_pdrs- (either
    ;;; sysPLOG_IFNOT_ATOM or sysPLOG_IFNOT_UNIFY) for each argument
    curr_pred_arg_vars -> arg_vs;
    if curr_pred_alias = "\.\/2" then
        Subscrv(1, unify_pdrs)(arg_vs(1), true, clause_v, "fast_front", fail_lab);
        Subscrv(2, unify_pdrs)(arg_vs(2), true, clause_v, "fast_back", fail_lab);
    elseif curr_pred_arity > MAX_INLINE_ARGS then
        ;;; last argument variable will hold a compound term
        For i to MAX_INLINE_ARGS do
            Destpair(arg_vs) -> (arg_v, arg_vs);
            Subscrv(i, unify_pdrs)(arg_v, true, clause_v, i, fail_lab);
        endfor;
        Front(arg_vs) -> arg_v;
        For i from MAX_INLINE_ARGS fi_+ 1 to curr_pred_arity do
            Subscrv(i, unify_pdrs)(arg_v, i fi_- MAX_INLINE_ARGS,
                clause_v, i, fail_lab);
        endfor;
    else
        For i to curr_pred_arity do
            Destpair(arg_vs) -> (arg_v, arg_vs);
            Subscrv(i, unify_pdrs)(arg_v, true, clause_v, i, fail_lab);
        endfor;
    endif;
    ;;; Success
    if cut then
        sysPUSH("prolog_apply_continuation"), sysCALL("chain");
    else
        sysCALL("prolog_apply_continuation");
    endif;
    ;;; Failure
    sysLABEL(fail_lab);
    sysPLOG_RESTORE();
    ;;; End of loop: continue until we have reached the last clause
    sysPUSH(clauses_v), sysPUSHQ(last), sysCALL("=="), sysIFNOT(loop_lab);
enddefine;

define lconstant group_unit_clauses(clauses, at_end);
    lvars clauses, at_end;

    define lconstant has_cut(term);
        lvars term;
        if term == "!" then
            true;
        elseif prolog_checkspec(term, ",", 2) then
            ;;; have to allow for silly cases like: foo :- true, !.
            has_cut(fast_prolog_arg(1, term))
            or has_cut(fast_prolog_arg(2, term));
        else
            false;
        endif;
    enddefine;

    define lconstant match_pdr(term);
        lvars term;
        if isword(term) or isinteger(term) then
            sysPLOG_IFNOT_ATOM;
        else
            sysPLOG_IFNOT_UNIFY;
        endif;
    enddefine;

    define lconstant match_spec(clause) -> (match, body, cut);
        lvars clause, match, body, cut;
        if prolog_checkspec(clause, ":-", 2) then
            has_cut(fast_prolog_arg(2, clause)) -> cut;
            if cut then #_<[!]>_# else [] endif -> body;
            fast_prolog_arg(1, clause) -> clause;
        else
            false ->> body -> cut;
        endif;
        {% prolog_appargs_nd(clause, match_pdr) %} -> match;
    enddefine;

    define lconstant same_spec(clause, match, body, cut);
        lvars clause, match, body, cut;
        if prolog_checkspec(clause, ":-", 2) then
            returnunless(body
                and cut == has_cut(fast_prolog_arg(2, clause)))(false);
            fast_prolog_arg(1, clause) -> clause;
        else
            returnif(body)(false);
        endif;
        lvars i;
        For i to datalength(match) do
            returnunless(match_pdr(prolog_arg_nd(i, clause)) ==
                            Subscrv(i, match))(false);
        endfor;
        true;
    enddefine;

    repeat
        lvars (first, last) = (clauses, clauses);
        lvars (match, body, cut) = match_spec(Front(clauses));
        lvars n = 1;
        Back(clauses) -> clauses;
        while clauses /== []
        and same_spec(Front(clauses), match, body, cut)
        do
            clauses -> last;
            n fi_+ 1 -> n;
            Back(clauses) -> clauses;
        endwhile;
        ;;; sublist of clauses from first to last inclusive is a
        ;;; unit-clause group of length n
        if n fi_< 5 then
            ;;; compile each one separately
            first -> clauses;
            fast_repeat n times
                lvars clause;
                Destpair(clauses) -> (clause, clauses);
                comp_clause(prolog_head(clause), body or [], [], false,
                    clauses == [] and at_end);
            endrepeat;
            quitif(clauses == []);
        elseif clauses == [] then
            comp_unit_clause_group(first, last, match, body, cut);
            ;;; compile the last one specially
            comp_clause(prolog_head(Front(last)), body or [], [], false,
                at_end);
            quitloop;
        else
            comp_unit_clause_group(first, clauses, match, body, cut);
        endif;
    endrepeat;
enddefine;


;;; -- Generate code for a predicate --------------------------------------

;;; show_code:
;;;     returns <true> if code-planting is traced for a current predicate.

define lconstant show_code(pred);
    lvars pred;
    testdef pop_show_code and weakref pop_show_code
    or prolog_show_code == true
    or islist(prolog_show_code)
        and member(conspair(pred_spec(pred)), prolog_show_code);
enddefine;

;;; comp_predicate:
;;;     compiles a set of clauses into a Prolog procedure

define lconstant comp_predicate(clauses, comp_clauses, pred) -> p;
    lvars clauses, procedure comp_clauses, pred, p;
    dlocal
        curr_pred_record        = pred,
        curr_pred_alias         = pred_alias(pred),
        curr_pred_arity         = pred_arity(pred),
        curr_pred_arg_vars,
        curr_pred_start_lab,
        curr_pred_end_lab,
        pop_debugging           = DEF prolog_debugging,
        weakref pop_show_code   = show_code(pred),
    ;
    sysPROCEDURE(curr_pred_alias, curr_pred_arity);
    sysLABEL(sysNEW_LABEL() ->> curr_pred_start_lab);
    sysNEW_LABEL() -> curr_pred_end_lab;

    lvars arity = curr_pred_arity;
    if arity > MAX_INLINE_ARGS then
        sysPUSHQ("DUMMY");
        sysPUSHQ(arity fi_- MAX_INLINE_ARGS);
        sysCALL("consprologterm");
        MAX_INLINE_ARGS fi_+ 1 -> arity;
    endif;
    [% repeat arity times sysNEW_LVAR() endrepeat %] -> curr_pred_arg_vars;
    applist(rev(curr_pred_arg_vars), sysPOP);

    comp_clauses(clauses) -> clauses;

    sysLABEL(curr_pred_end_lab);
    sysENDPROCEDURE() -> p;
    ;;; stash any frozen clauses in the pdprops of the procedure (yuk!)
    if clauses then conspair(pdprops(p), clauses) -> pdprops(p) endif;
enddefine;

;;; comp_static_predicate:
;;;     compile a list of clauses into a single, static predicate
;;;     definition

define:inline lconstant IS_UNIT_CLAUSE(env, body);
    (env == [] and (body == [] or Front(body) == "!" and Back(body) == []))
enddefine;

define comp_static_predicate(clauses, pred);
    lvars clauses, pred;

    define lconstant comp_clauses(clauses);
        lvars clauses;
        lvars last = conspair(undef, []);
        lvars text = pred_notext(curr_pred_record) /== true and last;
        until null(clauses) do
            lvars clause = Front(clauses);
            lvars (head, body, env, cut_var) = precomp_clause(clause);
            if IS_UNIT_CLAUSE(env, body) then
                lvars unit_clauses = last;
                repeat
                    conspair(prolog_full_deref(clause), []) ->> Back(last)
                        -> last;
                    Back(clauses) -> clauses;
                    quitif(null(clauses));
                    Front(clauses) -> clause;
                    precomp_clause(clause) -> (head, body, env, cut_var);
                    quitunless(IS_UNIT_CLAUSE(env, body));
                endrepeat;
                if null(clauses) then
                    group_unit_clauses(Back(unit_clauses), true);
                    quitloop;
                else
                    group_unit_clauses(Back(unit_clauses), false);
                endif;
            endif;
            ;;; non-unit clause
            Back(clauses) -> clauses;
            comp_clause(head, body, env, cut_var, null(clauses));
            if text then
                conspair(prolog_generalise(clause), []) ->> Back(last) -> last;
            endif;
        enduntil;
        ;;; return saved clauses (if any)
        text and Back(text);
    enddefine;

    comp_predicate(clauses, comp_clauses, pred);
enddefine;

;;; comp_dynamic_predicate:
;;;     compile a single clause into a procedure for incorporation into a
;;;     dynamic predicate definition

define comp_dynamic_predicate(clause, pred);
    lvars clause, pred;

    define lconstant comp_clauses(clause);
        lvars clause;
        comp_clause(precomp_clause(clause), "dynamic");
        ;;; if we get here, then the clause wasn't cut
        sysPUSHQ(false);
        sysPOP("ident wascut");
        ;;; return a copy of the clause
        prolog_generalise(clause);
    enddefine;

    comp_predicate(clause, comp_clauses, pred);
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 20 1993
        Fixed bug in group_unit_clauses
--- Robert John Duncan, Aug 19 1993
        Fixed test for whether to keep the clauses in comp_static_predicate
--- Robert John Duncan, Jul 15 1993
        More reorganisation:
        o   transformation of body goals moved out to new file "transform.p";
            new procedure precomp_clause does initial transformation and
            computes the usage env
        o   old inlining strategy abandoned: now only a few special functors
            are recognised (mainly to do with evaluation) and inlined; other
            cases are handled by inline_expansion which replaces a goal by
            an expansion of its body
        o   code generation from "procedures.p" moved into here
        o   unit-clause stuff rewritten to use precomp_clause
        o   variable c*ut_stack_length deleted: treatment of embedded cuts
            works just as well with a lexical variable
        o   changed comp_clause to omit save/restore pair for dynamic clauses
            because the interpreter does that anyway
--- Simon Nichols, Apr  7 1992
        -prolog_predof- now takes predicate name and arity as arguments.
--- Simon Nichols, Feb 12 1992
        Substantially reorganized and rewritten. In particular, the
        compilation of body goals has been made much more straightforward
        (with no loss of efficiency); tests on the type and structure of
        terms uses 'go_on prolog_type(term) ...' wherever possible; the
        names of procedures have been made more informative and much more
        consistent.
--- John Gibson, Aug 30 1989
        - In -Make_usage_env-, changed PAIR case to chain on the back of
        the pair instead of using an unnecessary recursive call.
        - In -compact_unify-, replaced test 'isground(arg)' with
        'curr_clause_env == [] or isground(arg)'.
--- Rob Duncan, Aug  8 1989
        - Moved in -compile_inline_call- and associated procedures from
          "popcalls.p";
        - moved out -prolog_instance- etc. to "util.p";
        - added some declarations at the top of the file previously in
          "prolog.p";
        - renamed -prolog_code_for_args- to -MAX_INLINE_ARGS- and
          -fixed_functors- to -PROLOG_FIXED_FUNCTORS-;
        - did more tidying up.
--- John Gibson, Jun  9 1989
        Changed compile of vm_flags.ph to #_INCLUDE.
            Made -pop_debugging- locally false inside
        -comp_unit_clause_group-, so that VM_NO_BACK_JUMPS is effective
        (providing -prolog_debugging- false at compile_time). Similarily
        for -comp_predicate- (where it replaces old -pop_optimise-).
--- John Gibson, Apr 27 1989
        -sysPLOG_RESTART- changed to include a GOTO as well (and so now
        takes label argument). Changed use in -makecall- accordingly.
--- Rob Duncan, Aug 31 1988
        Added -curr_pred_record- and made -makecall- look at that rather
        than at -prolog_syspredicate- when deciding whether to do a RESTART
--- Rob Duncan, Jun 10 1988
        Modified -comp_unit_clause_group- to check for predicates with
        arity > MAX_INLINE_ARGS and to plant appropriate code to deal
        with the dummy term which will be constructed to hold the
        additional arguments. See bugreport <robd@psunf.uucp.1>
--- Rob Duncan, Mar 16 1988
        Renamed from xbcomp.p
--- Simon Nichols, Nov  2 1987
        Changed -this_section- to -prolog_section-, -oeos_word- to
        "prolog_own_exit_on_success" and -own_invoke- to -prolog_own_invoke-.
        Tidied-up some of the declarations, adding/expanding comments.
--- John Gibson, Sep 20 1987
        Combined -compstart- and -compend- into a single procedure
        -comp_predicate-, which then takes an extra code-planting procedure
        argument to call in between the start and end code; this means that
        the whole compilation of a predicate is done inside -comp_predicate-,
        which can then have all the predicate-global variables as dlocal.
        (Necessary because the old arrangement meant that garbage VM labels
        containing codelists were kept hanging around after the predicate was
        finished compiling, and, for a large predicate, using up lots of
        space). Similarily, made all the clause-global variables dlocal to
        -compclause- (renamed -comp_clause-).
            Made all the globals lvars where possible and renamed them
        consistently as curr_pred_... and curr_clause_...  Removed several
        global vars that weren't used anywhere.
--- John Gibson, Mar 12 1987
        Added -comp_unit_clause_group- and improved interface to Prolog VM
        instructions. Also made -prolog_generalise-, -prolog_instance- not copy
        structures unless necessary (similarily for -prolog_full_deref- in the
        core system).
--- John Gibson, Mar 31 1986
        Reorganised procedures, making most lconstants, etc.
        Changed -expand_match- to use new prolog VM instructions
--- John Gibson, Apr 11 1986
        Added use of sysPLOG_RESTART
 */
