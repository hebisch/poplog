/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 >  File:           C.all/plog/lib/record.pl
 >  Purpose:        DEC System-10 Prolog style 'record' predicates
 >  Author:         Unknown (see revisions)
 >  Documentation:  HELP * RECORD
 */

:- prolog_language(pop11).

;;; Recorded terms are kept in this property

vars
    prolog_record_terms,            ;;; the records for the keys
    prolog_record_counts,           ;;; addition count for each record
    prolog_record_countmax = 50,    ;;; time to purge record?
;

define new_record(size);
    lvars size;
    newproperty([], size, [], true) -> prolog_record_terms;
    newproperty([], size, 0, true) -> prolog_record_counts;
enddefine;

;;; keyof:
;;;     maps a prolog term to a key into the property.             

define keyof(term);
    lvars term;
    prolog_deref(term) -> term;
    if term.prolog_complexterm and prolog_nargs(term) > 0 then
        cons_with consword {%
            dest_characters(prolog_predword(term)), `/`,
            dest_characters(prolog_nargs(term))
        %}
    elseif term.prolog_undefvar then
       mishap(0, 'Uninstantiated variable as key')
    else
       term
    endif
enddefine;

;;; prolog_record_explode:
;;;     stacks all unmarked the entries in a list.
;;;     (added Jonathan Laventhol, Wednesday 18th January 1984)

define prolog_record_explode(records);
    lvars rec, records;
    for rec in records do
        unless rec.pdprops == "ERASED" then rec endunless
    endfor;
enddefine;

;;; add_record:
;;;     A record is a closure of PROLOG_INSTANCE.
;;;     If it is erased, its PDPROPS are given the value ERASED
;;;     existing goals moving down the list may share tails -- if they
;;;     add things then they make a copy of the different fronts.  this
;;;     makes adding at the front much faster than at the end.  the erased
;;;     entries don't get copied, and so if you add always at the front they
;;;     will stay around.  to fix this, we always copy the list periodically
;;;     (after prolog_record_countmax additions).

define add_record(key, term, position) -> refer;
    lvars key, term, position, refer, records, count;
    keyof(key) -> key;
    prolog_record_terms(key) -> records;
    prolog_record_counts(key) -> count;
    prolog_instance(% prolog_generalise(term) %) -> refer;
    if position == 0 then
        if count >= prolog_record_countmax then
            [% refer, prolog_record_explode(records) %], 0
        else
            refer :: records, count + 1
        endif
    else
        [% prolog_record_explode(records), refer %], 0
    endif -> prolog_record_counts(key) -> prolog_record_terms(key);
    "REFERENCE" -> pdprops(refer)
enddefine;

:- prolog_language(prolog).

member(X, [X|_]).
member(X, [_|Xs]) :-
    member(X, Xs).

record(Key, Term, Ref) :-
    prolog_eval(add_record(quote(Key), quote(Term), -1), Ref).

recorda(Key, Term, Ref) :-
    prolog_eval(add_record(quote(Key), quote(Term), 0), Ref).

recordz(Key, Term, Ref) :-
    prolog_eval(add_record(quote(Key), quote(Term), -1), Ref).

recorded(Key, Term, Ref) :-
    prolog_eval(prolog_record_terms(keyof(quote(Key))), List),
    member(Ref, List),
    recorded1(Term, Ref).

recorded1(Term, Ref) :-
    prolog_eval(pdprops(quote(Ref)), 'ERASED'), !,
    fail.
recorded1(Term, Ref) :-
    prolog_eval(apply(quote(Ref)), Term).

instance(Ref, Term) :-
    prolog_eval(apply(quote(Ref)), Term).

erase(Ref) :-
    prolog_eval(apply('ERASED', quote(Ref), updater(valof(pdprops)))).

;;; Initialise a new set of records with a given size.
;;; This overwrites the existing records. The size should
;;; be at least as big as the number of different keys
;;; expected.
;;;
record_init(Size) :-
    prolog_eval(new_record(quote(Size))).

;;; added Jonathan Laventhol, Wednesday 18th January 1984
;;; set the value of the additions-before-purging limit
record_purgeinit(Purgecount) :-
    integer(Purgecount),
    prolog_eval(apply(quote(Purgecount), prolog_record_countmax,
                      updater(valof(valof)))).
record_purgeinit(X) :-
    X is valof(prolog_record_countmax).

;;; Initialisation
:- record_init(500).

/*  --- Revision History ---------------------------------------------------
--- Rob Duncan, Jul 20 1989
    Added lvars declaration for -records- in -prolog_record_explode-
--- Rob Duncan, Jan 19 1988
    added the definition of record/3 (so that the file can be loaded with
    uses/1) and generally tidied up
--- Jonathan Laventhol, Jan 18 1984 - modified to ensure erased records are
    removed, by counting number of additions and removing them periodically.
    Now also shares structures where possible.
 */
