/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/alias.p
 > Purpose:         Prolog: hash table for predicate names
 > Author:          Robert Duncan, Apr 26 1993
 > Documentation:
 > Related Files:
 */


section prolog;

;;; alias:
;;;     returns the true functor -- a word of the form fn/arity -- for
;;;     a given functor name and arity. Words returned are remembered in
;;;     a hash table to avoid repeated construction of the same word. The
;;;     initial table size (512) covers all the names used by the Prolog
;;;     system itself. The table hashes on the functor name, and returns
;;;     a list of alternating arities and aliases.

define lconstant alias_table =
    newanyproperty([], 512, 1, 410, false, false, "perm", [], false);
enddefine;

define alias(fn, arity) -> name;
    lvars l, arity, name, arities, fn;
    alias_table(fn) ->> arities -> l;
    until l == [] do
        returnif((Destpair(l) -> l) == arity)(Front(l) -> name);
        Back(l) -> l;
    enduntil;
    consword(#|
        if isword(fn) then destword(fn) -> else dest_characters(fn) endif,
        `/`, dest_characters(arity)
    |#) -> name;
    conspair(arity, conspair(name, arities)) -> alias_table(fn);
enddefine;

endsection;
