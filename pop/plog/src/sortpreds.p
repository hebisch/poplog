/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/sortpreds.p
 > Purpose:         Prolog: sorting and ordering terms
 > Author:          Rob Duncan & Simon Nichols, Nov  3 1987 (see revisions)
 > Related files:   C.all/plog/src/sort.pl
 */


section prolog;

/*  Ordering on Prolog terms  */

;;; ord:
;;;     returns a key number for a gross ordering of Prolog terms
;;;     The order is:
;;;         1 -- POP data
;;;         2 -- Prolog variables
;;;         3 -- Numbers
;;;         4 -- Atoms (i.e. terms of arity 0)
;;;         5 -- Terms of arity 1
;;;         6 -- Terms of arity 2 (including conspairs)
;;;         7+n  Terms of arity 3 + n
;;;     Also returns a possibly transformed value of -x- which accords with
;;;     the key number.

define lconstant ord(x);
    lvars x, key;
    if issimple(x) then
        3, x;
    elseif (datakey(x) ->> key) == word_key then
        4, x;
    elseif key == prologvar_key then
        2, x;
    elseif key == prologterm_key then
        if (fast_prolog_arity(x) ->> key) == 0 then
            4, fast_prolog_functor(x);
        else
            4 fi_+ key, x;
        endif;
    elseif key == pair_key then
        6, x;
    elseif x == [] then
        4, "'[]'";
    elseif isnumber(x) then
        3, x;
    else
        1, x;
    endif;
enddefine;

;;; prolog_<=:
;;;     orders two arbitrary items.
;;;     Returns <true> for less-than, 1 for equal, and <false> for
;;;     greater-than.

define prolog_<=(x, y);
    lvars x, y, ordx, ordy, i, tmp;
    repeat

        returnif(x == y)(1);

        ord(x) -> x -> ordx;
        ord(y) -> y -> ordy;
        returnunless(ordx == ordy)(ordx fi_< ordy);

        go_on ordx to
            L1 L2 L3 L4 L5 L6
        else
            L7;

        L1: ;;; non-prolog data
            returnif(x = y)(1);
            return(alphabefore(x sys_>< nullstring, y sys_>< nullstring));

        L2: ;;; prologvars
            return(prolog_var_number(x) fi_< prolog_var_number(y));

        L3: ;;; numbers
            if x = y then return(1) else return(x < y) endif;

        L4: ;;; atoms (terms of arity 0)
            return(alphabefore(x, y));

        L5: ;;; terms of arity 1
            fast_prolog_functor(x) -> ordx;
            fast_prolog_functor(y) -> ordy;
            returnunless(ordx == ordy)(alphabefore(ordx, ordy));
            prolog_arg(1, x) -> x; prolog_arg(1, y) -> y;
            nextloop;

        L6: ;;; terms of arity 2 (including pairs)
            prolog_functor(x) -> ordx;
            prolog_functor(y) -> ordy;
            returnunless(ordx == ordy)(alphabefore(ordx, ordy));
            prolog_<=(prolog_arg(1, x), prolog_arg(1, y)) -> tmp;
            returnunless(tmp == 1)(tmp);
            prolog_arg(2, x) -> x; prolog_arg(2, y) -> y;
            nextloop;

        L7: ;;; terms of arity > 2
            fast_prolog_functor(x) -> ordx;
            fast_prolog_functor(y) -> ordy;
            returnunless(ordx == ordy)(alphabefore(ordx, ordy));
            For i to fast_prolog_arity(x) fi_- 1 do
                prolog_<=(prolog_arg(i, x), prolog_arg(i, y)) -> tmp;
                returnunless(tmp == 1)(tmp);
            endfor;
            prolog_arg(i, x) -> x; prolog_arg(i, y) -> y;
            nextloop;
    endrepeat;
enddefine;

;;; prolog_compare:
;;;     order arguments as "<", "=" or ">"

define prolog_compare(/* x, y */) with_nargs 2;
    lvars tmp = prolog_<=(/* x, y */);
    if tmp == 1 then "=" elseif tmp then "<" else ">" endif;
enddefine;


/*
 *  Prolog Sorting:
 *  this is identical to LIB * SYSSORT, except that duplicate elements
 *  are removed
 */

define prolog_sort(list);
    lvars       n, list;
    dlvars      l, x, i;
    lconstant   merge_pair = writeable [[]];
    dlocal      % fast_back(merge_pair) %;

    define lconstant get_run();
        ;;; Get a run of ordered elements from l
        l ->> x;
        fast_destpair(l) -> l -> i;
        repeat
            returnif(l == []);
            unless prolog_<=(i, fast_front(l) ->> i) == true then
                [] -> fast_back(x);
                return
            endunless;
            fast_back(l ->> x) -> l
        endrepeat
    enddefine;

    define lconstant merge(l1, l2);
        lvars l1, l2;
        merge_pair -> x;
        repeat
            if prolog_<=(fast_front(l1), fast_front(l2)) ->> i then
                unless i == 1 then l1 ->> fast_back(x) -> x endunless;
                if (fast_back(l1) ->> l1) == [] then
                    l2 -> fast_back(x);
                    return(fast_back(merge_pair))
                endif
            else
                l2 ->> fast_back(x) -> x;
                if (fast_back(l2) ->> l2) == [] then
                    l1 -> fast_back(x);
                    return(fast_back(merge_pair))
                endif
            endif
        endrepeat
    enddefine;

    define lconstant get_sequence(n);
        lvars n;
        if n fi_< 2 then
            get_run()
        else
            get_sequence(n fi_- 1);
            returnif(l == []);
            get_sequence(n fi_- 1);
            merge()
        endif
    enddefine;

    [%  while ispair(prolog_deref(list) ->> list) do
            prolog_deref(fast_destpair(list) -> list);
        endwhile;
        unless list == [] then mishap(list, 1, 'LIST NEEDED') endunless;
    %] -> list;

    returnif(list == [])([]);

    list -> l;
    get_run();
    1 -> n;
    until l == [] do
        get_sequence(n);
        merge();
        n fi_+ 1 -> n
    enduntil;
enddefine;

define prolog_keysort(l);
    lvars l;

    define lconstant key_<=(x, y);
        lvars x, y;
        if prolog_arity(x) == 2 and prolog_arity(y) == 2 then
            prolog_<=(prolog_arg(1, x), prolog_arg(1, y));
        else
            mishap(x, y, 2, 'BAD ELEMENTS FOR KEYSORT');
        endif;
    enddefine;

    syssort([%
        while ispair(prolog_deref(l) ->> l) do
            prolog_deref(fast_destpair(l) -> l);
        endwhile;
        unless l == [] then mishap(l, 1, 'LIST NEEDED') endunless
    %], false, key_<=);
enddefine;

endsection;     /* prolog */

PROLOG

:- op(40, xfx, [@<, @>, @=<, @>=]).

:- module prolog.

:- inline((
        X @< Y :-
            prolog_evaltrue('prolog_<='(quote(X,Y)) == valof(true))
    )).

:- inline((
        X @=< Y :-
            prolog_evaltrue('prolog_<='(quote(X,Y)))
    )).

:- inline((
        X @> Y :-
            prolog_evaltrue('prolog_<='(quote(Y,X)) == valof(true))
    )).

:- inline((
        X @>= Y :-
            prolog_evaltrue('prolog_<='(quote(Y,X)))
    )).

:- inline((
        compare(Rel, X, Y) :-
            prolog_eval(prolog_compare(quote(X,Y)), Rel)
    )).

:- inline((
        sort(L1, L2) :-
            prolog_eval(prolog_sort(quote_nd(L1)), L2)
    )).

:- inline((
        keysort(L1, L2) :-
            prolog_eval(prolog_keysort(quote_nd(L1)), L2)
    )).

:- endmodule.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Changed to new inline strategy; findall etc. moved out to new file
        "findall.p"
--- Robert John Duncan, Nov  3 1992
        Changed -prolog_sort- in line with the definition of -syssort-.
        In particular, localising the back of -merge_pair- makes the procedure
        re-entrant (e.g. from an interrupt routine) where it wasn't before.
--- Robert John Duncan, Apr 14 1992
        Extended bagof/3 to allow existential quantification anywhere in
        the goal being evaluated.
--- Robert John Duncan, Apr 10 1992
        Moved in findall/3 from the library. Redefined fast_bagof/3 in
        terms of it, which means that findall, bagof and fast_bagof all
        now return solutions in the same (database) order.
--- Robert John Duncan, Jun 24 1991
        Renamed define forms.
--- Robert John Duncan, Jul  5 1990
        Fixed keysort to use -syssort- rather than -prolog_sort-
--- Rob Duncan, Jun 14 1990
        Replaced a missing "== true" from -get_run-
--- Rob Duncan, Aug  8 1989
    - sectionised and added #_INCLUDEs for POPC;
    - tidied up the definition of -merge_sort- and added sufficient
        dereferencing to -prolog_sort- to avoid having to call
        -prolog_full_deref- elsewhere;
    - removed the definition of -pcall- from -all_solutions-, expanding
        it out to an explicit SAVE, call, RESTORE.
--- Rob Duncan, Mar 13 1989
    Changed -bagof/3- to allow quantification over arbitrary terms by
    adding an extra call to -varsin-
--- Rob Duncan, Aug 31 1988
    Replaced -vednullstring- with -nullstring-. Tidied up -bagof/3-.
 */
