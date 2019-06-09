/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/type_errors.p
 > Purpose:         PML: verbose printing of type errors
 > Author:          Robert John Duncan, Oct 21 1994 (see revisions)
 */


section $-ml;

/*
 *  Simple printer for syntax forms
 */

lconstant macro (
    _ANDALSO_   = '\{b}andalso',
    _AS_        = '\{b}as',
    _BAR_       = '\{b}|',
    _CASE_      = '\{b}case',
    _DO_        = '\{b}do',
    _ELSE_      = '\{b}else',
    _END_       = '\{b}end',
    _FN_        = '\{b}fn',
    _HANDLE_    = '\{b}handle',
    _IF_        = '\{b}if',
    _IN_        = '\{b}in',
    _LET_       = '\{b}let',
    _OF_        = '\{b}of',
    _OP_        = '\{b}op',
    _ORELSE_    = '\{b}orelse',
    _RAISE_     = '\{b}raise',
    _THEN_      = '\{b}then',
    _WHILE_     = '\{b}while',

    VEDERRS     = [cucharout==vedcharinsert],
    _EXP_       = [if VEDERRS then '\{i}exp' else '<exp>' endif],
    _MATCH_     = [if VEDERRS then '\{i}match' else '<match>' endif],
    _PAT_       = [if VEDERRS then '\{i}pat' else '<pat>' endif],
    _DEC_       = [if VEDERRS then '\{i}dec' else '<dec>' endif],

    _LET_IN_    = _LET_ <> ' ' <> _DEC_ <> ' ' <> _IN_,
);

lconstant procedure (pr_pat, pr_exp);   ;;; forward

define lconstant pr_id(var);
    lvars var, id = fullname(val_parent(var), val_name(var));
    if lookup_opr(id) then
        pr(_OP_); pr(space);
    endif;
    pr(id);
enddefine;

define lconstant pr_record(labs, nodes, print, wild, depth);
    lvars labs, nodes, procedure print, wild, depth;
    pr("{");
    lvars n = length(nodes), nprinted = 0;
    unless n == 0 then
        depth + 1 -> depth;
        repeat
            pr(front(labs)); pr("="); print(front(nodes), depth);
            nprinted + 1 -> nprinted;
            if nprinted == n then
                quitloop;
            elseif nprinted == 3 and n > 4 then
                pr(","); pr(space); pr("__");
                quitloop;
            else
                pr(","); pr(space);
            endif;
            back(labs) -> labs; back(nodes) -> nodes;
        endrepeat;
    endunless;
    if wild then
        pr(if nprinted > 0 then ', ...' else '...' endif);
    endif;
    pr("}");
enddefine;

define lconstant pr_sequence(nodes, print, depth, chars);
    lvars nodes, procedure print, depth, chars;
    lvars (opener, closer, separator) = explode(chars);
    cucharout(opener);
    lvars n = length(nodes), nprinted = 0;
    depth + 1 -> depth;
    repeat
        print(front(nodes), depth);
        nprinted + 1 -> nprinted;
        if nprinted == n then
            quitloop;
        elseif nprinted == 3 and n > 4 then
            cucharout(separator); pr(space); pr("__");
            quitloop;
        else
            cucharout(separator); pr(space);
        endif;
        back(nodes) -> nodes;
    endrepeat;
    cucharout(closer);
enddefine;

define lconstant pr_apat(pat, depth);
    lvars pat, depth;
    SWITCH pat
    CASE ConstPat(scon) then
        pr(scon);
    CASE WildCardPat() then
        pr("_");
    CASE VarPat(var)
      or ConPat(var)
      or ExnPat(var)
    then
        pr_id(var);
    CASE RecordPat(labs, pats) then
        pr_record(labs, pats, pr_pat, false, depth);
    CASE WRecordPat(labs, pats, _) then
        pr_record(labs, pats, pr_pat, true, depth);
    CASE UnitPat() then
        pr('()');
    CASE TuplePat(pats) then
        pr_sequence(pats, pr_pat, depth, '(),');
    CASE ListPat(pats) then
        pr_sequence(pats, pr_pat, depth, '[],');
    else
        pr("("); pr_pat(pat, depth+1); pr(")");
    ENDSWITCH;
enddefine;

define lconstant pr_apppat(pat, depth);
    lvars pat, depth, as_inf_pat = false;
    if isboolean(depth) then
        ;;; optional flag as_inf_pat
        ((), pat, depth) -> (pat, depth, as_inf_pat);
    endif;
    SWITCH pat
    CASE ConAppPat(con, pat)
      or ExnAppPat(con, pat)
    then
        lvars id = fullname(val_parent(con), val_name(con));
        if lookup_opr(id)
        and isTuplePat(pat) and length(first(pat)) == 2
        then
            lvars (pat1, pat2) = dl(first(pat));
            unless as_inf_pat then pr("(") endunless;
            pr_apppat(pat1, depth);
                pr(space); pr(id); pr(space);
            pr_apppat(pat2, depth);
            unless as_inf_pat then pr(")") endunless;
            return;
        endif;
        pr_id(con); pr(space); pr_apat(pat, depth);
    else
        pr_apat(pat, depth);
    ENDSWITCH;
enddefine;

define lconstant pr_typed_pat(pat, depth);
    lvars pat, depth;
    SWITCH pat
    CASE TypedPat(pat, _) then
        pr_typed_pat(pat, depth);
    else
        pr_apppat(pat, depth, true);    ;;; infix application
    ENDSWITCH;
enddefine;

define lconstant pr_pat(pat, depth);
    lvars pat, depth;
    returnif(depth > 1)(pr(_PAT_));
    SWITCH pat
    CASE LayeredPat(var, pat) then
        pr_id(var); pr(space); pr(_AS_); pr(space); pr_pat(pat, depth);
    else
        pr_typed_pat(pat, depth);
    ENDSWITCH;
enddefine;

define lconstant pr_aexp(e, depth);
    lvars e, depth;
    SWITCH e
    CASE ConstExp(scon) then
        pr(scon);
    CASE VarExp(var)
      or ConExp(var)
      or ExnExp(var)
    then
        pr_id(var);
    CASE RecordExp(labs, exps) then
        pr_record(labs, exps, pr_exp, false, depth);
    CASE UnitExp() then
        pr('()');
    CASE TupleExp(exps) then
        pr_sequence(exps, pr_exp, depth, '(),');
    CASE ListExp(exps) then
        pr_sequence(exps, pr_exp, depth, '[],');
    CASE SeqExp(exps) then
        pr_sequence(exps, pr_exp, depth, '();');
    CASE SelectorExp(lab, _) then
        pr("#"); pr(lab);
    CASE LetExp(_, e) then
        SWITCH e
        CASE SeqExp(exps) then
            if back(exps) == [] then front(exps) -> e endif;
        else
        ENDSWITCH;
        pr(_LET_IN_); pr(space); pr_exp(e, depth+1); pr(space); pr(_END_);
    else
        pr("("); pr_exp(e, depth+1); pr(")");
    ENDSWITCH;
enddefine;

define lconstant pr_appexp(e, depth);
    lvars e, depth, as_inf_exp = false;
    if isboolean(depth) then
        ;;; optional flag as_inf_exp
        ((), e, depth) -> (e, depth, as_inf_exp);
    endif;
    SWITCH e
    CASE AppExp(e1, e2) then
        if isVarExp(e1) or isConExp(e1) or isExnExp(e1) then
            lvars var = first(e1);
            lvars id = fullname(val_parent(var), val_name(var));
            if lookup_opr(id)
            and isTupleExp(e2) and length(first(e2)) == 2
            then
                dl(first(e2)) -> (e1, e2);
                unless as_inf_exp then pr("(") endunless;
                pr_appexp(e1, depth);
                    pr(space); pr(id); pr(space);
                pr_appexp(e2, depth);
                unless as_inf_exp then pr(")") endunless;
                return;
            endif;
        endif;
        pr_appexp(e1, depth); pr(space); pr_aexp(e2, depth);
    else
        pr_aexp(e, depth);
    ENDSWITCH;
enddefine;

define lconstant pr_typed_exp(e, depth);
    lvars e, depth;
    SWITCH e
    CASE TypedExp(e, _) then
        pr_typed_exp(e, depth);
    else
        pr_appexp(e, depth, true);  ;;; infix application
    ENDSWITCH;
enddefine;

define lconstant pr_andalso_exp(e, depth);
    lvars e, depth;
    while isAndExp(e) do
        pr_typed_exp(first(e), depth);
        pr(space); pr(_ANDALSO_); pr(space);
        second(e) -> e;
    endwhile;
    pr_typed_exp(e, depth);
enddefine;

define lconstant pr_orelse_exp(e, depth);
    lvars e, depth;
    while isOrExp(e) do
        pr_andalso_exp(first(e), depth);
        pr(space); pr(_ORELSE_); pr(space);
        second(e) -> e;
    endwhile;
    pr_andalso_exp(e, depth);
enddefine;

define lconstant pr_rule(rule, depth);
    lvars rule, depth;
    SWITCH rule
    CASE Rule(pat, e) then
        pr_pat(pat, depth); pr(space); pr("=>"); pr(space);
        pr_exp(e, depth);
    ENDSWITCH;
enddefine;

define lconstant pr_match(match, depth);
    lvars match, depth;
    returnif(depth > 0)(pr(_MATCH_));
    depth + 1 -> depth;
    SWITCH match
    CASE Match(rules) then
        repeat
            pr_rule(destpair(rules) -> rules, depth);
            quitif(rules == []);
            if depth == 1 then
                pr(newline); pr(tab); pr(_BAR_); pr(space); pr(space);
            else
                pr(_BAR_); pr(space);
            endif;
        endrepeat;
    ENDSWITCH;
enddefine;

define lconstant pr_exp(e, depth);
    lvars e, depth;
    returnif(depth > 1)(pr(_EXP_));
    SWITCH e
    CASE IfExp(e1, e2, e3) then
        pr(_IF_); pr(space); pr_exp(e1, depth+1);
        if depth == 0 then pr(newline), pr(tab) else pr(space) endif;
        pr(_THEN_); pr(space); pr_exp(e2, depth+1);
        if depth == 0 then pr(newline), pr(tab) else pr(space) endif;
        pr(_ELSE_); pr(space); pr_exp(e3, depth+1);
    CASE WhileExp(e1, e2) then
        pr(_WHILE_); pr(space); pr_exp(e1, depth+1);
        if depth == 0 then pr(newline), pr(tab) else pr(space) endif;
        pr(_DO_); pr(space); pr_exp(e2, depth+1);
    CASE CaseExp(e, match) then
        pr(_CASE_); pr(space); pr_exp(e, depth+1);
        if depth == 0 then pr(newline), pr(tab) else pr(space) endif;
        pr(_OF_); pr(space); pr_match(match, depth);
    CASE FnExp(match) then
        pr(_FN_); pr(space); pr_match(match, depth);
    CASE RaiseExp(e) then
        pr(_RAISE_); pr(space); pr_exp(e, depth+1);
    CASE HandleExp(e, match) then
        pr_orelse_exp(e, depth);
        if depth == 0 then pr(newline), pr(tab) else pr(space) endif;
        pr(_HANDLE_); pr(space); pr_match(match, depth);
    else
        pr_orelse_exp(e, depth);
    ENDSWITCH;
enddefine;

define lconstant pr_clause(clause, depth);
    lvars clause, depth;
    SWITCH clause
    CASE Clause(pats, e, _) then
        lvars pat;
        for pat in pats do
            pr_apat(pat, depth); pr(space);
        endfor;
        pr("=>"); pr(space); pr_exp(e, depth+1);
    ENDSWITCH;
enddefine;

define lconstant pr_valbind(valbind, depth);
    lvars valbind, depth;
    SWITCH valbind
    CASE ValBind(pat, e) then
        pr_pat(pat, depth+1); pr(space); pr("="); pr(space);
        pr_exp(e, depth+1);
    ENDSWITCH;
enddefine;

define lconstant pr_form(form, depth, print, ty, init);
    lvars form, depth, print, ty, init;
    pr(tab); print(form, depth);
    if ty then
        pr(space); pr(":"); pr(space); print_type(ty, init);
    endif;
    pr(newline);
enddefine;


/*
 *  Error reporting
 */

;;; unify_error:
;;;     explain an error resulting from a unification failure

define lconstant explain(reason, form, sub_form);
    lvars reason, form, sub_form;
    SWITCH form
    CASE ListPat(items) then
        printf('in pattern\n');
        pr_form(form, 0, pr_pat, false, false);
        printf('Not all the list members have the same type:\n');
        pr_form(hd(items), 1, pr_pat, nodetype(hd(items)), true);
        pr_form(sub_form, 1, pr_pat, nodetype(sub_form), false);
    CASE ConAppPat(con, pat) or ExnAppPat(con, pat) then
        printf('in pattern\n');
        pr_form(form, 0, pr_pat, false, false);
        printf('Cannot apply constructor\n');
        pr_form(mkVarPat(false, con), 0, pr_apat, val_type(con), true);
        printf('to argument\n');
        pr_form(pat, 0, pr_apat, nodetype(pat), false);
    CASE TypedPat(pat, ty) then
        printf('in type constraint\n');
        pr_form(pat, 0, pr_typed_pat, nodetype(ty), true);
        printf('The actual type is\n');
        pr_form(pat, 0, pr_typed_pat, nodetype(pat), true);
    CASE ListExp(items) then
        printf('in expression\n');
        pr_form(form, 0, pr_exp, false, false);
        printf('Not all the list members have the same type:\n');
        pr_form(hd(items), 1, pr_exp, nodetype(hd(items)), true);
        pr_form(sub_form, 1, pr_exp, nodetype(sub_form), false);
    CASE AppExp(e1, e2) then
        printf('in expression\n');
        pr_form(form, 0, pr_exp, false, false);
        if sub_form == e1 then
            ;;; type of e1 is not a function type
            printf('The value\n');
            pr_form(e1, 0, pr_appexp, nodetype(e1), true);
            printf('is not a function\n');
        else
            ;;; bad argument type
            printf('Cannot apply function\n');
            pr_form(e1, 0, pr_appexp, nodetype(e1), true);
            printf('to argument\n');
            pr_form(e2, 0, pr_aexp, nodetype(e2), false);
        endif;
    CASE TypedExp(e, ty) then
        printf('in type constraint\n');
        pr_form(e, 0, pr_typed_exp, nodetype(ty), true);
        printf('The actual type is\n');
        pr_form(e, 0, pr_typed_exp, nodetype(e), true);
    CASE AndExp(e1, e2) or OrExp(e1, e2) then
        printf('in expression\n');
        pr_form(form, 0, pr_exp, false, false);
        printf('The value\n');
        pr_form(sub_form, 0, pr_typed_exp, nodetype(sub_form), true);
        printf('is not a boolean\n');
    CASE IfExp(e1, e2, e3) then
        printf('in expression\n');
        pr_form(form, 0, pr_exp, false, false);
        if sub_form == e1 then
            printf('The value\n');
            pr_form(e1, 1, pr_exp, nodetype(e1), true);
            printf('is not a boolean\n');
        else
            printf('The two alternatives have different types:\n');
            pr_form(e2, 1, pr_exp, nodetype(e2), true);
            pr_form(e3, 1, pr_exp, nodetype(e3), false);
        endif;
    CASE WhileExp(e1, e2) then
        printf('in expression\n');
        pr_form(form, 0, pr_exp, false, false);
        printf('The value\n');
        pr_form(sub_form, 1, pr_exp, nodetype(sub_form), true);
        printf('is not a boolean\n');
    CASE CaseExp(e1, match) then
        printf('in expression\n');
        pr_form(form, 0, pr_exp, false, false);
        unless sub_form == e1 then
            ;;; bad match
            chain(reason, match, sub_form, explain);
        endunless;
        printf('Cannot apply\n');
        pr_form(match, 1, pr_match, nodetype(match), true);
        printf('to argument\n');
        pr_form(sub_form, 1, pr_exp, nodetype(sub_form), true);
    CASE FnExp(match) then
        printf('in expression\n');
        pr_form(form, 0, pr_exp, false, false);
        ;;; bad match
        chain(reason, match, sub_form, explain);
    CASE RaiseExp(e1) then
        printf('in expression\n');
        pr_form(form, 0, pr_exp, false, false);
        printf('The value\n');
        pr_form(sub_form, 1, pr_exp, nodetype(sub_form), true);
        printf('is not an exception\n');
    CASE HandleExp(e1, match) then
        printf('in expression\n');
        pr_form(form, 0, pr_exp, false, false);
        unless sub_form == e1 then
            ;;; bad match
            chain(reason, match, sub_form, explain);
        endunless;
        printf('The handler\n');
        pr_form(match, 1, pr_match, nodetype(match), true);
        printf('should have type\n');
        printf('\t'); pr(funtype(exntype, nodetype(e1))); printf('\n');
    CASE ValBind(pat, e) then
        printf('in value binding\n');
        pr_form(form, 0, pr_valbind, false, false);
        printf('Cannot bind\n');
        pr_form(pat, 1, pr_pat, nodetype(pat), true);
        printf('to\n');
        pr_form(e, 1, pr_exp, nodetype(e), false);
    CASE Clause(_, e, ty) then
        printf('in type constraint\n');
        pr_form(e, 0, pr_typed_exp, nodetype(ty), true);
        printf('The actual type is\n');
        pr_form(e, 0, pr_typed_exp, nodetype(e), true);
    CASE FunBind(var, _, clauses) then
        printf('in definition of function\n\t%p\n', [% val_name(var) %]);
        if sub_form == form then
            ;;; type of definition does not agree with type of use
            printf('The type derived from the definition\n');
            printf('\t'); print_type(nodetype(form), true); printf('\n');
            printf('is not the same as the type of use\n');
            printf('\t'); print_type(val_type(var), false); printf('\n');
        else
            ;;; some clause has a different type
            printf('Not all the clauses have the same type:\n');
            pr_form(hd(clauses), 0, pr_clause, nodetype(hd(clauses)), true);
            pr_form(sub_form, 0, pr_clause, nodetype(sub_form), false);
        endif;
    CASE Match(rules) then
        ;;; special case -- called recursively to finish off Case, Fn
        ;;; and Handle
        printf('Not all the match rules have the same type:\n');
        pr_form(hd(rules), 1, pr_rule, nodetype(hd(rules)), true);
        pr_form(sub_form, 1, pr_rule, nodetype(sub_form), false);
    ENDSWITCH;
    ;;; show the reason for failure (if it's interesting)
    if reason == TYPERR_CTS then
        printf('(unification would create a cyclic type structure)\n');
    elseif reason == TYPERR_TCV then
        printf('(unification would bind an explicit type variable)\n');
    elseif reason == TYPERR_ETN then
        printf('(equality type needed)\n');
    elseif reason == TYPERR_ITN then
        printf('(imperative type variable needed)\n');
    endif;
enddefine;

define unify_error(reason, form, sub_form);
    lvars reason, form, sub_form;
    ml_error(reason, form, sub_form, explain, popfilename, nodeline(form));
enddefine;

;;; sigmatch_error
;;;     explain an error resulting from a signature match failure

vars sigmatch_node = false;
define sigmatch_error(sig_entry, str_entry);
    lvars sig_entry, str_entry, linenum;
    if isstructenv(sig_entry) then
        structenv_strname(sig_entry) -> sig_entry;
    endif;
    if isstructenv(str_entry) then
        structenv_strname(str_entry) -> str_entry;
    endif;
    ml_error(
        'in signature match - %s',
        [%  if str_entry then
                ;;; specification mismatch
                'specification match failure\n\tWanted: %p\n\tFound : %p\n',
                sig_entry, str_entry
            else
                ;;; missing name
                'unmatched specification\n\t%p\n', sig_entry
            endif
        %],
        popfilename,
        sigmatch_node and nodeline(sigmatch_node) or poplinenum);
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  3 1995
        Changed node-matching syntax to SWITCH/CASE/ENDSWITCH
--- Robert John Duncan, Mar 30 1995
        Changed the definition of LET_IN to highlight the dec component
        properly
--- Robert John Duncan, Nov 24 1994
        Sectionised
 */
