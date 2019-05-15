/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/plog/src/errors.p
 > Purpose:         Prolog: raising and handling errors
 > Author:          Robert Duncan and Simon Nichols, Dec 1987 (see revisions)
 > Related Files:   C.all/plog/src/prolog_exceptions.p
 */


section prolog;

constant
    procedure ( prolog_mishap, ),
;

;;; ========================================================================

;;; check_culprits:
;;;     dereferences a list of culprits to pass to mishap.

define check_culprits(culprits);
    lvars culprits;
    [%  while ispair(prolog_deref(culprits) ->> culprits) do
            prolog_deref(fast_destpair(culprits) -> culprits);
        endwhile;
        unless culprits == [] then
            culprits;
        endunless;
    %];
enddefine;

endsection;     /* prolog */

PROLOG

:- module prolog.

%   prolog_syserror/2:
%       untrappable error predicate

prolog_syserror(Message, Culprits) :-
    prolog_eval(prolog_mishap(check_culprits(quote(Message, Culprits)))).

%   error/2:
%       trappable error predicate. Calls mishap, which (in Prolog) may
%       be caught by prolog_exception_final and passed to prolog_error/2

error(Message, Culprits) :-
    prolog_eval(mishap(check_culprits(quote(Message, Culprits)))),
    % Interpret return from mishap as failure (continuation-style)
    fail.

%   bad_goal/1:
%       raises an error when a goal is insufficiently instantiated
%       (cf. Pop-11 bad_goal in "util.p")

bad_goal(Goal) :-
    error('INSUFFICIENT INSTANTIATION OF GOAL', [Goal]).

:- endmodule prolog.


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 13 1996
        Changes for new exception handling
--- Robert John Duncan, Jul 15 1993
        Moved mishap-related stuff to "prolog_prm*ishap.p". Added bad_goal/1
        and changed definitions of prolog_syserror/2 and error/2 to be in
        Prolog.
--- Simon Nichols, Jun 25 1992
        Added "wved" and "xved" to list of concealed-call prefixes.
--- Robert John Duncan, Feb 11 1992
        Changed -prolog_prm*ishap- not to require a matching clause for
        prolog_error/2: defaults to -prolog_sysprm*ishap-.
        Added explicit enabling and disabling of prolog_error/2.
        Procedure -prolog_*syserror- renamed -prolog_sysprm*ishap-.
--- Robert Duncan, Jan 27 1992
        Fix to list of names concealed in the backtrace
--- John Gibson, Dec 12 1991
        Set prm*ishap locally to sysprm*ishap inside -do_prolog_error-
        to prevent recursive mishapping.
--- Robert John Duncan, Jun 24 1991
        Renamed define forms.
--- Simon Nichols, Oct 24 1990
        Changed -prolog_*syserror- to use -prolog_printq- (a version of
        print/1) to print culprits, unless in a nested call.
--- Rob Duncan, Mar  5 1990
        Made -check_culprits- dereference the items in the list, as this
        doesn't get done if they're printed with -prolog_writeq- in
        -prolog_*syserror-.
--- Rob Duncan, Aug  8 1989
    Sectionised and added #_INCLUDEs for POPC.
--- Rob Duncan, Jun 16 1989
    Changed the printing in -prolog_prm*ishap- to insulate it from any
    localising of -pr-: it now localises -pr- to be -syspr-, but calls
    -prolog_write- explicitly on the error message.
    Also replaced calls to -prolog_full_deref- in -prolog_error/2- and
    -error/2- with -check_culprits- and -prolog_deref-: the full deref
    wasn't really needed, and could loop indefinitely on cyclic structures.
--- Rob Duncan, Jan 19 1989
    Changed -prolog_prm*ishap- yet again, to chain out of -mishap- directly:
    the last change was chaining out of too much, and losing the immediate
    mishap context
--- Rob Duncan, Sep 13 1988
    Changed the -chain- in -prolog_prm*ishap- to a -chainto- to get out of
    -mishap- as well
--- Rob Duncan, Sep  8 1988
    Rewrote to use define form -prolog_pdr-.
    Reinstated the chain in -prolog_prm*ishap- to undo the dlocal'ing of
    -prm*ishap- before 'prolog_error/2' is called.
--- Rob Duncan, Aug 31 1988
    Replaced -vednullstring- with -nullstring-
 */
