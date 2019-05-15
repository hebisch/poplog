/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/dynamic.p
 > Purpose:         Prolog: implementation of dynamic predicates
 > Author:          Robert John Duncan, Jul  6 1993
 > Documentation:
 > Related Files:
 */

section prolog;

constant procedure ( alias, );

;;; ========================================================================

/*
 *  Interpreter for dynamic predicates
 */

vars wascut = true;

;;; prolog_interpreter:
;;;     dynamic (i.e. assert/retract-able) predicates are closures of
;;;     this. The argument is a list of procedures, one per clause,
;;;     which are to be attempted in order.

define prolog_interpreter(/* arity, */ clauses) with_nargs 2;
    lvars   clauses, args;
    dlocal  wascut;
    SAVE;
    consvector(/* arity */) -> args;
    until (Back(clauses) ->> clauses) == [] do
        true -> wascut;
        fast_apply(destvector(args) ->, Front(clauses));
        returnif(wascut);
        RESTORE;
        ;;; skip any clauses retracted in the meantime
        until Back(clauses) do
            Front(clauses) -> clauses;
        enduntil;
    enduntil;
enddefine;

;;; is_dynamic:
;;;     true if the procedure proc is a dynamic predicate value, i.e
;;;     a closure of prolog_interpreter; returns the list of clauses
;;;     if so

define is_dynamic(proc);
    lvars proc;
    if pdpart(proc) == prolog_interpreter then
        frozval(2, proc);
    else
        false;
    endif;
enddefine;

;;; cons_dynamic_closure:
;;;     creates a new dynamic closure for the predicate fn/arity

define cons_dynamic_closure(fn, arity) -> clos;
    lvars fn, arity, clos;
    prolog_interpreter(% arity, writeable conspair([],[]) %) -> clos;
    alias(fn, arity) -> pdprops(clos);
enddefine;

/*
 *  Operations on dynamic predicates
 */

;;; insert_at_front:
;;;     adds a clause to the front of a dynamic predicate

define insert_at_front(clause, proc);
    lvars clause, proc, clauses = frozval(2, proc), first;
    if (Back(clauses) ->> first) == [] then
        ;;; no existing clauses; make -clause- the first and last
        writeable conspair(clause, []) ->> Front(clauses) -> Back(clauses);
    else
        ;;; put -clause- first
        writeable conspair(clause, first) -> Back(clauses);
    endif;
enddefine;

;;; insert_at_end:
;;;     adds a clause to the end of a dynamic predicate

define insert_at_end(clause, proc);
    lvars clause, proc, clauses = frozval(2, proc), last;
    if (Front(clauses) ->> last) == [] then
        ;;; no existing clauses; make -clause- the first and last
        writeable conspair(clause, []) ->> Back(clauses) -> Front(clauses);
    else
        ;;; put -clause- last
        writeable conspair(clause, []) ->> Back(last) -> Front(clauses);
    endif;
enddefine;

;;; insert_at_posn:
;;;     insert a clause after the existing clause number -posn- in a
;;;     dynamic predicate. Clauses are numbered from 1, so a -posn- of 0
;;;     means add to the front of a list. A -posn- < 0 or > the number
;;;     of clauses is interpreted as meaning the end of the list.

define insert_at_posn(clause, posn, proc);
    lvars clause, posn, proc;
    unless isinteger(posn) then
        mishap(posn, 1, 'INTEGER NEEDED');
    endunless;
    if posn fi_< 0 then
        insert_at_end(clause, proc);
    else
        lvars clauses = frozval(2, proc), tmp = clauses;
        until posn == 0 or Back(tmp) == [] do
            posn fi_- 1 -> posn;
            Back(tmp) -> tmp;
        enduntil;
        if Back(tmp) == [] then
            ;;; -clause- is to be the new last clause
            writeable conspair(clause, []) ->> Back(tmp) -> Front(clauses);
        else
            writeable conspair(clause, Back(tmp)) -> Back(tmp);
        endif;
    endif;
enddefine;

;;; delete_here:
;;;     deletes the first clause from -clauses-, which is a suffix of
;;;     the clauselist of -proc-. If -clauses- contains only the one
;;;     clause, then the clauselist must be updated to point to the new
;;;     last clause.

define delete_here(clauses, proc);
    lvars clauses, proc, tmp;
    unless (Back(clauses) ->> tmp) == [] then
        ;;; Remove the clause from the clause list,
        if (Back(tmp) ->> Back(clauses)) == [] then
            ;;; -clauses- is now the last pair in the list
            lvars clauselist = frozval(2, proc);
            if clauses == clauselist then [] else clauses endif
                -> Front(clauselist);
        endif;
        ;;; put a flag in the deleted clause to show that it's gone
        false -> Back(tmp);
        ;;; and leave a pointer to where the list can be picked up again
        clauses -> Front(tmp);
    endunless;
enddefine;

;;; delete_at_posn:
;;;     delete the clause number -posn- from a dynamic predicate.
;;;     A value of -posn- outside the range 0 < posn <= number of clauses
;;;     is ignored.

define delete_at_posn(posn, proc);
    lvars posn, proc;
    unless isinteger(posn) then
        mishap(posn, 1, 'INTEGER NEEDED');
    endunless;
    unless posn fi_<= 0 then
        lvars clauses = frozval(2, proc);
        until posn == 1 or Back(clauses) == [] do
            posn fi_- 1 -> posn;
            Back(clauses) -> clauses;
        enduntil;
        delete_here(clauses, proc);
    endunless;
enddefine;

endsection;     /* prolog */
