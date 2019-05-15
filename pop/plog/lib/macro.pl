/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/lib/macro.pl
 *  Purpose:        for making new macros and deleting old ones, within PROLOG
 *  Author:         Jonathan Laventhol, Jul 19 1983
 *  Documentation:  HELP * MACRO
 *  Related Files:
 */

:- prolog_language("pop11").

define prolog_setmacro(old, new);
    old -> prolog_macro(new)
enddefine;

;;; takes a prolog string
;;;
define prolog_killmac(name);
    false -> prolog_macro(cons_with consword {% explode(name) %})
enddefine;

:- prolog_language("prolog").

macro(Old, New) :-
    atom(New),
    prolog_eval(prolog_setmacro(quote(Old), quote(New))).
killmac(Namestring) :-
    name(Name, Namestring), !,
    prolog_eval(prolog_killmac(Name)).
macval(Namestring, Value) :-
    name(Name, Namestring), !,
    prolog_eval(prolog_macro(Name), Value).
