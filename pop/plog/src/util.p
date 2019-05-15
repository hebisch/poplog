/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/util.p
 > Purpose:         Prolog: utility procedures
 > Author:          Robert Duncan and Simon Nichols, 1988 (see revisions)
 */


section prolog;

/* Error Routines */

;;; bad_goal:
;;;     causes an 'INSUFFICIENT INSTANTIATION' error, and displays the
;;;     offending goal.

define bad_goal() with_props false;
    mishap(prolog_maketerm(), 1, 'INSUFFICIENT INSTANTIATION OF GOAL');
enddefine;


/* Term Manipulation */

;;; prolog_head:
;;;     returns the head of clause, or the clause itself if it's a unit
;;;     clause.

define prolog_head(clause) -> clause;
    lvars clause;
    if prolog_checkspec(clause, ":-", 2) then
        prolog_arg(1, clause) -> clause;
    endif;
enddefine;

;;; prolog_body:
;;;     returns the body of clause, or "true" if it's a unit clause

define prolog_body(clause) -> body;
    lvars clause, body = "true";
    if prolog_checkspec(clause, ":-", 2) then
        prolog_arg(2, clause) -> body;
    endif;
enddefine;

;;; isground:
;;;     <true> if -term- is ground (i.e. contains no unbound variables)

define isground(/* term */) with_nargs 1;
    lvars term = prolog_deref(/* term */);

    PROLOG_GO_ON term;

    PT_VAR:
        return(false);

    PT_PAIR:
        return(isground(Front(term)) and chain(Back(term), isground));

    PT_TERM:
        lvars i;
        For i to fast_prolog_arity(term) do
            returnunless(isground(fast_prolog_arg(i, term)))(false);
        endfor;
        return(true);

    PT_NIL:
    PT_CONST:
    PT_OTHER:
        return(true);

enddefine;

;;; prolog_generalise, prolog_instance:
;;;     "freeze" and "melt" a prolog term: unbound variables in the term
;;;     are replaced by dummy variable tokens which look the same but
;;;     which can't be instantiated.

defclass prolog_variable_tok {
    prolog_var_no : uint,
};

define prolog_generalise(/* term */) with_nargs 1;
    dlvars env = [], current_var_number = 1;

    define lconstant do_var(var);
        lvars n, var;
compile_mode:vm -pentch -bjmpch;
        if dup(prolog_deref(var)) == var then
            -> ;
            For n in env do
                returnif(Front(n) == var)(Back(n), false)
            endfor;
            consprolog_variable_tok(current_var_number) -> n;
            current_var_number fi_+ 1 -> current_var_number;
            conspair(conspair(var, n), env) -> env;
            n, false    ;;; false = changed, don't unpeel recursively
        else
            (), true    ;;; true = unpeel recursively
        endif
    enddefine;

    Prolog_unpeel((), prologvar_key, do_var)
enddefine;

define prolog_instance(/* term */) with_nargs 1;
    dlvars env = [];

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

    Prolog_unpeel((), prolog_variable_tok_key, do_var_tok)
enddefine;


/* Prolog atoms */

;;; prolog_atom:
;;;     <true> if item is a Prolog atom, i.e. either a word or nil

define prolog_atom(x);
    lvars x;
    isword(x) or x == [];
enddefine;

;;; prolog_atomic:
;;;     <true> if item is atomic, i.e. non-complex and non-variable

define prolog_atomic(/* x */) with_nargs 1;
    lvars k = datakey(/* x */);
    k /== prologvar_key and
    k /== prologterm_key and
    k /== pair_key;
enddefine;

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Moved undefined\/0 to "predicate_valof.p" and historical
        definitions to "obsolete.p". Added PROLOG_GO_ON syntax for
        switching on the prolog type of an object.
--- John Gibson, Dec  7 1990
        Replaced r*ecordclass with defclass
--- John Gibson, Aug 30 1989
        In -isground-, changed PAIR case to chain on the back of
        the pair instead of using an unnecessary recursive call.
            Replaced -prolog_generalise- and -prolog_instance- with new
        versions using new procedure -Prolog_unpeel-.
--- Rob Duncan, Aug  8 1989
    - sectionised and added #_INCLUDEs for POPC;
    - moved out syntax and macro definitions to "prologsys.ph";
    - moved in -prolog_generalise-, -prolog_instance- and the definition
        of prolog variable tokens from "gencode.p";
    - moved in various utility and error procedures from other files -
        -prolog_atom-, -prolog_atomic- etc.;
    - renamed -novarsin- to -isground-;
    - removed definitions of funny words (-nilword- etc.) in favour
        of the "'...'" syntax.
--- Rob Duncan, Jun 29 1989
    Removed macro definitions for the prolog type key field (not used
    anywhere) and the dummy definition for -prolog_type- which is
    definitely available in the system since version 13.6.
--- Simon Nichols, Mar 16 1988
    Added definition of -prolog_type- to enable V14 Prolog to be loaded with
    V13 POPLOG.
 */
