/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/prolog_error.p
 > Purpose:         Prolog: redefinable error handler
 > Author:          Robert John Duncan, Jul  7 1993
 > Documentation:
 > Related Files:
 */

PROLOG

:- module prolog.

%   prolog_error(Message, Culprits) :-
%       for trapping mishaps.
%       The default definition catches 'UNDEFINED PREDICATE' errors and
%       makes them fail.
%       Its behaviour can be changed by loading LIBRARY * UNKNOWN

:- dynamic prolog_error/2.

prolog_error('UNDEFINED PREDICATE', [Goal]) :-
    !,
    fail.

:- endmodule prolog.
