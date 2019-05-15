/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/auto/unknown.pl
 > Purpose:         alter action taken when trying an undefined predicate
 > Author:          Jonathan Laventhol, Jul  1 1983 (see revisions)
 > Documentation:   PLOGHELP * UNKNOWN
 > Related Files:
 */

% facilities for altering the action taken when trying an undefined
% predicate.  approximately like in dec10, where the two-place
% predicate "unknown" allows you to find out the current action and
% set a new action.  format:
%
%   ?- unknown(Old, New).
%
% where Old and New are one of the five atoms:
%
%   fail        -- predicate fails
%   error       -- system complains and aborts
%   autoload    -- attemps to load library file of same name, or error
%   debug       -- user takes place of predicate
%   oldfail     -- predicate fails and has a dummy clause asserted for it
%
% For backward compatibility, 'mishap' is allowed as a synonym for 'error'.


:- global
    prolog_undefined_action/1,
    prolog_undefined_action/2,
    unknown/2.

:- dynamic
    prolog_undefined_action/1.


prolog_undefined_action(Term, fail) :-
    !,
    fail.
prolog_undefined_action(Term, error) :-
    !,
    prolog_syserror('UNDEFINED PREDICATE', [Term]).
prolog_undefined_action(Term, mishap) :-
    !,
    % Synonym for 'error'
    prolog_undefined_action(Term, error).
prolog_undefined_action(Term, autoload) :-
    functor(Term, F, N),
    write('Attempting to autoload: '), write(F), write(/), write(N), nl,
    prolog_error_handling(on),  % for the library load and subsequent retry
    library(F),
    predicate_info(F/N, Info),
    Info \= [undefined|_],
    !,
    Term.
prolog_undefined_action(Term, autoload) :-
    !,
    % Autoload failed
    prolog_undefined_action(Term, error).
prolog_undefined_action(Term, debug) :-
    !,
    write('Call to undefined predicate: '), write(Term), nl,
    prolog_eval(prolog_readline('Action? (<nl> = succeed) '), []).
prolog_undefined_action(Term, oldfail) :-
    !,
    functor(Term, Functor, Args),   % some args might be instantiated
    functor(Head, Functor, Args),
    assert((Head :- fail, 'UNDEFINED-PREDICATE')),
    fail.

prolog_undefined_action(fail).

prolog_error('UNDEFINED PREDICATE', [Goal]) :-
    clause(prolog_undefined_action(Action), true),  % no error if undef
    !,
    prolog_undefined_action(Goal, Action).

unknown(Old, New) :-
    clause(prolog_undefined_action(Old), true),
    (   Old = New ->
            true
    ;   clause(prolog_undefined_action(_,New), _) ->
            retract(prolog_undefined_action(Old)),
            asserta(prolog_undefined_action(New))
    ;   error('Illegal UNKNOWN setting', [New])
    ),
    % Remove any choice points from clause/2 & retract/1
    !.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 14 1993
        Replaced prolog_safer*eadline with prolog_readline
--- Robert John Duncan, Feb 11 1992
        Removed catch-all clause from prolog_error/2, now handled by default.
        Added call to re-enable the error handler for the "autoload" action
        in case the library load or the retry causes another undefined
        predicate error; also added a check that the library does define the
        predicate to prevent a recursive error.
        Rewrote definition of unknown/2 so that it always fails if Old
        doesn't unify with the current setting, where previously it would
        sometimes raise an error.
--- Simon Nichols, Oct 22 1990
        Renamed the "mishap" to "error" but kept "mishap" as a synonym.
        Tidied up.
--- Rob Duncan, Apr  9 1990
    Made all definitions global and prolog_undefined_action/1 dynamic.
--- Rob Duncan, Oct  3 1988
    Renamed the new option from "default" to "oldfail" and made "fail" the
    default action (as it was before).
--- Rob Duncan, Sep  8 1988
    Added new default option which re-establishes the old behaviour of
    asserting a dummy clause for the failing predicate. Also changed the
    "fail" option to simply fail (in line with the new system behaviour)
 */
