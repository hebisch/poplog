/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/plog/auto/showterm.pl
 >  Purpose:        Show any Prolog term as a tree in VED
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP * SHOWTERM
 >  Related Files:
 */

:- prolog_language("pop11").

uses showtree;

define showterm(term);
    lvars term;

    dlocal % class_print(prologterm_key) % = prolog_write;

    dlocal showtree_root = prolog_functor;

    define dlocal showtree_daughters(tree);
        lvars tree;
        [% prolog_args(tree) %];
    enddefine;

    define dlocal showtree_isleaf(term);
        lvars term;
        not(prolog_complexterm(term)) or prolog_checkspec(term, "'$VAR'", 1);
    enddefine;

    showtree(prolog_full_deref(term))
enddefine;

:- prolog_language("prolog").

showterm(X) :-
    numbervars(X,1,_),
    prolog_eval(showterm(quote(X))),
    fail.
showterm(X).

tree_printable :-
    prolog_eval(apply(valof(showtree_printable))).

vgap(N) :-
    var(N), !,
    N is valof(showtree_vgap).
vgap(N) :-
    integer(N),
    N > 3,
    prolog_setq(showtree_vgap, N).


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 24 1993
        lib new_sh*owtree renamed showtree
--- Robert John Duncan, Aug 19 1993
        Fixed to work with recent changes to showtree.
        Tidied up.
--- John Gibson, Nov 12 1992
        Changed to use new_sh*owtree
--- Kathryn Seifert, Oct  7 1986 'vspacing' changed to 'vgap' to be
    compatible with LIB SHOWTREE
*/
