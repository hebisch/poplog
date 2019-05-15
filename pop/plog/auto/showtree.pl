/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.all/plog/auto/showtree.pl
 > Purpose:        interface to the POP11 lib SHOWTREE for parse trees in ved
 > Author:         Unknown, ??? (see revisions)
 > Documentation:  HELP * SHOWTREE
 > Related Files:  LIB * SHOWTREE.P
 */

:- prolog_language(pop11).

uses showtree;

define plog_showtree(l, messflag);
    lvars l, messflag;
    dlocal pr = prolog_write;
    prolog_full_deref(l) -> l;
    if prolog_undefvar(l) then return endif;
    if messflag == "no" then showtree(l) else showtree_mess(l) endif
enddefine;

;;; ---- prolog code ---------------------------------------------------------

:- prolog_language(prolog).

showtree(T) :-
    prolog_eval(plog_showtree(quote(T), no)).
showtreemess(T) :-
    prolog_eval(plog_showtree(quote(T), yes)).

tree_printable :-
    prolog_eval(apply(valof(showtree_printable))).

tree_graphical :-
    prolog_eval(apply(valof(showtree_graphical))).

vgap(N) :-
    var(N), !,
    N is valof(showtree_vgap).
vgap(N) :-
    integer(N), N > 3,
    prolog_setq(showtree_vgap, N).


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 24 1993
        lib new_sh*owtree renamed showtree
--- John Gibson, Nov 12 1992
        Changed to use new_sh*owtree
--- Mark Rubinstein, May 15 1986 - added vgap and tree_graphical for new
    lib showtree.p.
*/
