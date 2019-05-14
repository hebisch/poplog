/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:           C.all/lib/lib/ploginpop.p
 > Purpose:        Pop-11 interface to Prolog
 > Author:         Roger Evans, May 1984; John Williams, Apr 30 1987 (see revisions)
 > Documentation:  HELP * PLOGINPOP
 > Related Files:  LIB * PLOG_GOALS
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode:pop11 +strict;

section;

uses plog_goals;

/* Compiling Plog goals */

:- op(20, fx, '?').
:- op(30, fx, '^').

define plog_goal_newvar(item, goal) -> var;
    lvars env = plog_goal_env(goal);
    if fast_lmember(item, env) ->> var then
        fast_front(fast_back(var)) -> var;
    else
        prolog_newvar() -> var;
        [^item ^var ^^env] -> plog_goal_env(goal);
    endif;
enddefine;

define compile_plog_goal(term, goalvar);

    define cache =
        ;;; remembers interesting things about components of term
        newproperty([], 32, false, "tmparg");
    enddefine;

    define analyse(term) -> const;
        ;;; check over a term for prologvars and constants
        lvars const = false;
        if prolog_complexterm(term) then
            lvars (fn, arity) = prolog_termspec(term);
            unless (fn == "?" or fn == "^") and arity == 1 then
                lvars i, const = true;
                fast_for i to arity do
                    unless analyse(prolog_arg(i, term)) then
                        false -> const;
                    endunless;
                endfor;
                if const then true -> cache(term) endif;
            endunless;
        elseif isprologvar(term) then
            ;;; we use the cache to count occurrences
            lvars n = cache(term);
            if n then n + 1 else 1 endif -> cache(term);
        else
            true ->> const -> cache(term);
        endif;
    enddefine;

    define plant(term, goalvar);
        ;;; generate code to rebuild a goal
        lvars kind = cache(term);
        if kind == true then
            ;;; constant
            sysPUSHQ(term);
        elseif kind == false then
            ;;; complex term to be constructed at run-time
            lvars (fn, arity) = prolog_termspec(term);
            if fn == "?" and arity == 1 then
                sysIDENT(prolog_arg(1, term)), sysPUSH(goalvar),
                    sysCALL("plog_goal_newvar");
            elseif fn == "^" and arity == 1 then
                lvars arg = prolog_arg(1, term);
                if isword(arg) then
                    define comp_expr(expr);
                        dlocal proglist_state = proglist_new_state(expr);
                        pop11_comp_stmnt_seq_to(termin) -> ;
                    enddefine;
                    comp_expr(stringin(arg));
                elseif prolog_complexterm(arg) then
                    mishap(arg, 1, 'Invalid expression after ^')
                else
                    sysPUSHQ(arg);
                endif
            else
                lvars i;
                fast_for i to arity do
                    plant(prolog_arg(i, term), goalvar);
                endfor;
                sysPUSHQ(fn), sysPUSHQ(arity), sysCALL("prolog_maketerm");
            endif
        elseif isinteger(kind) then
            ;;; first occurrence of a variable
            sysCALL("prolog_newvar");
            unless kind == 1 then
                ;;; more occurrences later
                sysPOP(sysNEW_LVAR() ->> kind), sysPUSH(kind);
                kind -> cache(term);
            endunless;
        elseif isword(kind) then
            ;;; subsequent occurrence of a variable
            sysPUSH(kind);
        else
            mishap('Unexpected cache entry', [^term --> ^kind]);
        endif;
    enddefine;

    clearproperty(cache);
    analyse(term) -> ;
    plant(term, goalvar);
enddefine;


/* Reading Prolog goals in Pop-11 using |< ... >| */

global vars syntax >| ;

define vars syntax |< ;
    dlocal
        pop_new_lvar_list,
        3 % $-prolog$-prolog_switch_itemiser() %,
        1 % item_chartype(`|`, readitem) % = 3
    ;
    lvars term = prolog_readterm_to(">|");
    lvars goalvar = sysNEW_LVAR();
    sysPUSHQ("undef"), sysPUSHQ([]), sysPUSHQ(false),
        sysCALL("consplog_goal"), sysPOP(goalvar);
    compile_plog_goal(term, goalvar);
    sysPUSH(goalvar), sysUCALL("plog_goal_term");
    (sysPUSH, goalvar) -> (pop_expr_inst, pop_expr_item);
enddefine;

procedure(goal);
    lvars goal;
    appdata('|< ', cucharout);
    prolog_write(plog_goal_term(goal));
    appdata(' >|', cucharout);
endprocedure -> class_print(plog_goal_key);


/* plogwhile <goal> do ... endplogwhile */

global vars syntax endplogwhile;

define vars syntax plogwhile;
    lvars body goal lab1 lab2 loop loopend var varlist;

    sysNEW_LVAR() -> goal;
    pop11_comp_expr_to([do then]) ->;
    sysPOP(goal);

    sysPROCEDURE(false, 0);
        sysNEW_LABEL() -> loop;
        sysNEW_LABEL() -> loopend;
        pop11_loop_start(loop);
        pop11_loop_end(loopend);
        sysCALL("plog_goal_env_bind");
        pop11_comp_stmnt_seq_to("endplogwhile") ->;
        sysGOTO(loop);
        sysLABEL(loopend);
        sysPUSHQ(true);
        sysPUSHQ(1);
        sysCALLQ(suspend);
        sysLABEL(loop);
    sysENDPROCEDURE() -> body;

    sysPUSH(goal);
    sysPUSHQ(body);
    sysCALL("plog_goal_invoke");
    sysERASE(1);

enddefine;


global vars ploginpop = true;       /* for uses */


endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 17 1995
        Fixed compile_plog_goal to take account of prologvars occurring
        explicitly in a goal
--- John Williams, May 10 1995
        Now declares an identifier ploginpop (cf. BR isl-fr.4565).
--- John Gibson, Aug 13 1989
        Replaced old sys- compiler procedures with pop11_ ones.
--- John Williams, Feb  1 1989
        No longer uses -Lisp_list_assoc_val-
        Changes -item_chartype- ok even if saved image is shareable
 */
