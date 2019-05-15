/* --- Copyright University of Sussex 1987.  All rights reserved. ---------
 > File:          C.all/plog/lib/chartype.pl
 > Purpose:       Get or set character class numbers for Prolog itemiser
 > Author:        Robert Duncan, Simon Nichols, Feb. 1987 (see revisions)
 > Documentation: PLOGHELP * CHARTYPE
 */

;;; chartype:
;;;     OldType is unified with the class number currently associated with the
;;;     character X, where X may be an integer ASCII code, a single character
;;;     atom, or a string (i.e. a list of ASCII codes), in which case each
;;;     character must belong to the same class for the predicate to succeed.
;;;     If chartype/3 is used with the 3rd. argument instantiated, the class
;;;     number of X (or each component of X) is updated to be NewType.

chartype(X, Type) :-
    chartype(X, Type, _).
chartype([Char|Rest], OldType, NewType) :- !,
    nonvar(Char),
    chartype(Char, OldType, NewType),
    chartype(Rest, OldType, NewType).
chartype([], _, _) :- !.
chartype(X, OldType, NewType) :-
    prolog_translate_char(X, Char), !,
    prolog_get_chartype(Char, OldType),
    prolog_set_chartype(Char, OldType, NewType).

prolog_translate_char(Char, Char) :-
    integer(Char), !.
prolog_translate_char(Atom, Char) :-
    name(Atom, [Char]).

prolog_get_chartype(Char, Type) :-
    prolog_eval(item_chartype(Char, valof(readitem)), Type).

prolog_set_chartype(Char, OldType, OldType) :- !.
prolog_set_chartype(Char, _, NewType) :-
    prolog_eval(apply(NewType, Char, valof(readitem), updater(valof(item_chartype)))).


/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Jan 12 1989
    Replaced references to -prolog_standard_repeater- with -readitem-.
    The standard repeater tables are fixed in the system and non-writeable;
    -readitem- uses a copy of them, and it's that copy which has to be
    changed.
 */
