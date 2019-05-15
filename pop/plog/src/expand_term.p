/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/expand_term.p
 > Purpose:         Prolog: pre-processing of clauses
 > Author:          Robert John Duncan, Jul  5 1993
 > Documentation:
 > Related Files:
 */

PROLOG

:- dynamic term_expansion/2.

:- module prolog.

expand_term(T1, T2) :-
    term_expansion(T1, T2).
expand_term((H --> B), (H1 :- B1)) :-
    prolog_grexpand(H, B, H1, B1).
expand_term(T, T).

:- endmodule prolog.
