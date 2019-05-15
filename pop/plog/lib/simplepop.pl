/*  --- Copyright University of Sussex 1991. All rights reserved. ----------
 >  File:           C.all/plog/lib/simplepop.pl
 >  Purpose:        simple pop11 access from prolog
 >  Author:         Jonathan Laventhol, Jun 24 1983 (see revisions)
 >  Documentation:  HELP * SIMPLEPOP
 >  Related Files:
 */

;;; makes three prolog predicates available:
;;;     dopop/1
;;;     dopop/2
;;;     dopopreturn/2
;;;
;;; dopop/2 takes a (prolog) string or an atom which is compiled as pop11. a
;;; list of the results, if any, are unified with the second argument.
;;; a mishap causes the goal to fail.
;;; dopop/1 is like dopop/2 except any results are ignored -- so there is no
;;; results variable.  if there is a mishap, the goal fails.
;;; dopopreturn/2 is like dopop/2, except if there is a mishap, then the
;;; second argument is unified with a term:
;;;     pop_mishap(Message, Culprits, Callers)
;;; -------------------------------------------------------------------------

:- prolog_language("pop11").

compile_mode:pop11 +strict;

;;; prolog_dostring:
;;;     compiles the Prolog atom or string -s- as a Pop11 expression,
;;;     and returns any results in a list.
;;;     If a mishap occurs, returns instead a term:
;;;         pop_mishap(Msg, Culprits, Callers)

define global prolog_dostring(s) -> res;
    lvars s, res, stack;

    ;;; convert Prolog atom or string to Pop word or string
    define lconstant pop_string(s) -> s;
        lvars s, c, orig = s;
        unless isword(s) or isstring(s) then
            consstring(#|
                while ispair(s) do
                    prolog_deref(fast_front(s)) -> c;
                    quitunless(isinteger(c) and c fi_>= 0 and c fi_<= 255);
                    c, prolog_deref(fast_back(s)) -> s;
                endwhile;
                unless s == [] then
                    mishap(orig, 1, 'String or atom needed');
                endunless;
            |#) -> s;
        endunless;
    enddefine;

    ;;; package mishap information into a Prolog term
    define lconstant mishap_term(msg, culprits) -> term;
        lconstant ignore = [mishap prmishap];
        lvars p, msg, culprits, callers, term;
        if isstring(msg) then consword(msg) -> msg endif;
        [%  for p in syscallers() do
                quitif(p == prolog_dostring);
                recursive_front(pdprops(p)) -> p;
                if isword(p) and not(lmember(p, ignore)) then p endif;
            endfor;
        %] -> callers;
        prolog_maketerm(msg, culprits, callers, "pop_mishap", 3) -> term;
    enddefine;

    pop_string(s) -> s;
    consvector(stacklength()) -> stack;
    procedure;
        define dlocal prmishap(msg, culprits);
            lvars msg, culprits;
            clearstack();
            mishap_term(msg, culprits);
            exitto(prolog_dostring);
        enddefine;
        conslist(#| pop11_compile(stringin(s)) |#);
    endprocedure() -> res;
    explode(stack);
enddefine;

;;; ==========================================================================

:- prolog_language("prolog").

dopopreturn(Pop11_code, Results) :-
    prolog_eval(prolog_dostring(quote(Pop11_code)), Results).

dopop(Pop11_code, Results) :-
    dopopreturn(Pop11_code, Results),
    Results \= pop_mishap(_,_,_).

dopop(Pop11_code) :-
    dopop(Pop11_code, _).


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct 24 1991
        Changed the argument processing (-pop_string-) to ensure that Prolog
        string arguments are dereferenced fully (see bugreport isl-fr.4362),
        and so that a bad argument generates a real error rather than
        returning a 'pop_mishap' term.
        Rewrote the whole thing in a more modern style.
 */
