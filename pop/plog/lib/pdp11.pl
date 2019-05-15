/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/plog/lib/pdp11.pl
 >  Purpose:        PDP-11 Prolog compatible operator declarations
 >  Author:         Chris Mellish, July 1983 (see revisions)
 >  Documentation:  HELP * PDP11
 */

;;; Be careful with this file, it doesn't change any operator precedences
;;; other than the system ones.

:- abolish('DEC10', 0).     ;;; kill off flag

:- op(255, xfx, :-).
:- op(255, fx, :-).
:- op(255, xfx, -->).
:- op(255, fx, '?-').

:- op(254, xfy, ;).
:- op(254, fx, spy).
:- op(254, fx, nospy).
:- op(254, fx, system_predicate).
:- op(254, fx, user_predicate).
:- op(254, fx, dynamic).
:- op(254, fx, static).
:- op(254, fx, no_clauses).
:- op(254, fx, clauses).

:- op(253, xfy, ->).
:- op(253, fx, import).
:- op(253, fx, export).
:- op(253, fx, global).

:- op(252, xfy, ',').

:- op(249, fx, module).
:- op(249, fx, endmodule).
:- op(249, fy, \+ ).

:- op(40, xfx, is).
:- op(40, xfx, <).
:- op(40, xfx, >).
:- op(40, xfx, >=).
:- op(40, xfx, =<).
:- op(40, xfx, @<).
:- op(40, xfx, @>).
:- op(40, xfx, @>=).
:- op(40, xfx, @=<).
:- op(40, xfx, =:=).
:- op(40, xfx, =\=).
:- op(40, xfx, =..).
:- op(40, xfx, =).
:- op(40, xfx, \=).
:- op(40, xfx, ==).
:- op(40, xfx, \==).

:- op(31, yfx, -).
:- op(31, yfx, +).
:- op(31, yfx, \/).
:- op(31, yfx, /\).

:- op(21, fx, -).
:- op(21, fx, \).

:- op(21, yfx, *).
:- op(21, yfx, /).
:- op(21, yfx, div).
:- op(21, yfx, mod).
:- op(21, yfx, >>).
:- op(21, yfx, <<).

:- op(10, xfy, ^).

:- op(8, xfy, $-).
:- op(8, fy,  $-).


/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Aug 19 1993
        Quoted ?- to prevent problems caused by loading this library after
        library(ct), which defines ?- as a prolog_macro (ugh!).
--- Rob Duncan, Aug 21 1989
        Added operator declarations for modules plus some other missing
        ones: '^', '@<' etc.
--- Rob Duncan, Mar 16 1988
        Added declarations for 'dynamic', 'no_clauses' etc.
--- Simon Nichols, May 8 1987
        Added declarations for '-->' and '?-'.
*/
