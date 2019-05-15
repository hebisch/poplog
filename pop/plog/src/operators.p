/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:        C.all/plog/src/operators.p
 > Purpose:     Prolog: declaring operators
 > Author:      Robert Duncan & Simon Nichols, February 1987 (see revisions)
 */


section prolog;

constant
    procedure ( prefix_prec, prefix_rprec, postfix_prec, postfix_lprec,
        infix_prec, infix_lprec, infix_rprec, bad_goal, ),
;

weak constant
    procedure ( prologc_property_update, );

;;; ========================================================================

;;; operator_assign:
;;;     adds an entry to a precedence table.

define operator_assign(prec, op, table_name);
    lvars prec, op, table_name;
    prec -> valof(table_name)(op);
    if testdef prologc_property_update then
        weakref prologc_property_update(prec, op, table_name);
    endif;
enddefine;

;;; op/3:
;;;     declares new operators.

define op\/3(prec, type, ops);
    lvars prec, type, ops;

    define lconstant declare_op(prec, lprec, rprec, type, op);
        lvars prec, lprec, rprec, type, op;
        unless isword(op) then
            mishap(op, 1, 'ILLEGAL OPERATOR NAME');
        endunless;
        if type == "prefix" then
            operator_assign(prec, op, "ident prefix_prec");
            operator_assign(rprec, op, "ident prefix_rprec");
        elseif type == "postfix" then
            operator_assign(prec, op, "ident postfix_prec");
            operator_assign(lprec, op, "ident postfix_lprec");
        else
            operator_assign(prec, op, "ident infix_prec");
            operator_assign(lprec, op, "ident infix_lprec");
            operator_assign(rprec, op, "ident infix_rprec");
        endif;
    enddefine;

    if isprologvar(prolog_deref(prec) ->> prec)
    or isprologvar(prolog_deref(type) ->> type)
    or isprologvar(prolog_deref(ops) ->> ops)
    then
        bad_goal(prec, type, ops, "op", 3);
    endif;
    if prec == "off" then
        ;;; undeclare
        NOPREC -> prec;
    elseunless isinteger(prec) and prec fi_> 0 and prec fi_< MAXPREC then
        mishap(prec, 1, 'ILLEGAL OPERATOR PRECEDENCE');
    endif;
    unless lmember(type, #_<[yfy yfx xfy xfx fy fx yf xf]>_#) then
        mishap(type, 1, 'ILLEGAL OPERATOR TYPE');
    endunless;
    ;;; convert precedence and type
    lvars (lprec, rprec);
    if isstartstring('x', type) then
        if prec == NOPREC then prec else prec fi_- 1 endif -> lprec;
    elseif isstartstring('y', type) then
        prec -> lprec;
    endif;
    if isendstring('x', type) then
        if prec == NOPREC then prec else prec fi_- 1 endif -> rprec;
    elseif isendstring('y', type) then
        prec -> rprec;
    endif;
    if isstartstring('f', type) then
        "prefix" -> type;
    elseif isendstring('f', type) then
        "postfix" -> type;
    else
        "infix" -> type;
    endif;
    while prolog_checkspec(ops, ".", 2) do
        declare_op(prec, lprec, rprec, type, prolog_arg(1, ops));
        prolog_arg(2, ops) -> ops;
    endwhile;
    unless ops == [] then
        declare_op(prec, lprec, rprec, type, ops);
    endunless;
    chain(prolog_apply_continuation);
enddefine;

;;; current_op/3
;;;     searches through all current operator declarations

define current_op\/3(Prec, Fix, Op);
    lvars Prec, Fix, Op;

    define lconstant try_prefix(op, prec);
        lvars op, prec, fix;
        SAVE;
        if prefix_rprec(op) == prec then "fy" else "fx" endif -> fix;
        if prolog_unify(op, Op) and prolog_unify(fix, Fix)
        and prolog_unify(prec, Prec)
        then
            prolog_apply_continuation();
        endif;
        RESTORE;
    enddefine;

    define lconstant try_postfix(op, prec);
        lvars op, prec, fix;
        SAVE;
        if postfix_lprec(op) == prec then "yf" else "xf" endif -> fix;
        if prolog_unify(op, Op) and prolog_unify(fix, Fix)
        and prolog_unify(prec, Prec)
        then
            prolog_apply_continuation();
        endif;
        RESTORE;
    enddefine;

    define lconstant try_infix(op, prec);
        lvars op, prec, fix;
        SAVE;
        if infix_lprec(op) == prec then
            if infix_rprec(op) == prec then "yfy" else "yfx" endif
        else
            if infix_rprec(op) == prec then "xfy" else "xfx" endif
        endif -> fix;
        if prolog_unify(op, Op) and prolog_unify(fix, Fix)
        and prolog_unify(prec, Prec)
        then
            prolog_apply_continuation();
        endif;
        RESTORE;
    enddefine;

    appproperty(prefix_prec, try_prefix);
    appproperty(postfix_prec, try_postfix);
    appproperty(infix_prec, try_infix);
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Changed to use operator_assign. Operator declarations moved out to
        various other files where the predicates themselves are defined.
        Moved in current_op/3 from defunct "statepreds.p".
--- Robert John Duncan, Jun 24 1991
        Renamed define forms.
--- Rob Duncan, Aug  8 1989
    - sectionised and added #_INCLUDEs for POPC;
    - moved out declarations of operator tables to "parse.p";
    - moved in (a simplified version of) declaration of 'op/3' from
        "statepreds.p";
    - replaced calls to -declare_op- with prolog calls to 'op/3'.
--- Rob Duncan, Apr  4 1989
    Replaced "->" by "->>" in case for "yfy" in -declare_op-: declaring an
    operator as "yfy" was giving a stack empty error!
    (See bug report tomk.60)
--- Rob Duncan, Mar 16 1988
    Renamed from plogoper.p
 */
