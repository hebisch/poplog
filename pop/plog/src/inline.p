/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/inline.p
 > Purpose:         Prolog: declare inline expansion of predicate calls
 > Author:          Robert John Duncan, May 13 1993
 > Documentation:
 > Related Files:
 */


section prolog;

constant
    procedure ( predicate_define, inline_expansion, alias, transform_body, );

weak constant procedure ( prologc_property_update, );

;;; ========================================================================

define inline\/1(clause);
    lvars clause;
    prolog_deref(clause) -> clause;
    ;;; arguments to the clause must all be prologvars
    lvars head = prolog_head(clause);
    lvars (fn, arity) = prolog_termspec(head);
    lvars i, head_vars = [];
    for i to arity do
        lvars arg = prolog_arg(i, head);
        unless isprologvar(arg)
        and not(lmember(arg, head_vars))
        then
            mishap(head, 1, 'ILLEGAL LEFT-HAND-SIDE OF INLINE CLAUSE');
        endunless;
        arg :: head_vars -> head_vars;
    endfor;
    ;;; create the base definition
    predicate_define(fn, arity, [^clause], 0, true);
    ;;; add an entry to the inline table
    lvars (goals, cut_var) = transform_body(prolog_body(clause));
    if cut_var or lmember("!", goals) then
        mishap(0, 'ILLEGAL CUT IN INLINE CLAUSE');
    endif;
    lvars wid = word_identifier(alias(fn, arity), current_section, true);
    prolog_generalise([^head ^^goals]) ->> goals -> inline_expansion(wid);
    if testdef prologc_property_update then
        weakref prologc_property_update(goals, wid, "ident inline_expansion");
    endif;
    chain(prolog_apply_continuation);
enddefine;

endsection;     /* prolog */
