/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/lib/dec10_ops.pl
 > Purpose:         DEC-10 Prolog operator precedences
 > Author:          Simon Nichols, Mar 15 1990 (see revisions)
 > Related Files:   C.all/plog/lib/dec10.pl
 */

:- op(1200, xfx, :-).
:- op(1200, xfx, -->).

:- op(1200, fx, :-).
:- op(1200, fx, '?-').

:- op(1150, fx, mode).
:- op(1150, fx, public).
:- op(1150, fx, dynamic).
:- op(1150, fx, static).
:- op(1150, fx, system_predicate).
:- op(1150, fx, user_predicate).
:- op(1150, fx, no_clauses).
:- op(1150, fx, clauses).
:- op(1150, fx, import).
:- op(1150, fx, export).
:- op(1150, fx, global).

:- op(1100, xfy, ;).

:- op(1050, xfy, ->).

:- op(1000, xfy, ',').

:- op(900, fy, \+).
:- op(900, fy, spy).
:- op(900, fy, nospy).
:- op(900, fx, module).
:- op(900, fx, endmodule).

:- op(700, xfx, =).
:- op(700, xfx, is).
:- op(700, xfx, =..).
:- op(700, xfx, ==).
:- op(700, xfx, \==).
:- op(700, xfx, @<).
:- op(700, xfx, @>).
:- op(700, xfx, @=<).
:- op(700, xfx, @>=).
:- op(700, xfx, =:=).
:- op(700, xfx, =\=).
:- op(700, xfx, <).
:- op(700, xfx, >).
:- op(700, xfx, =<).
:- op(700, xfx, >=).

:- op(500, yfx, +).
:- op(500, yfx, -).
:- op(500, yfx, /\).
:- op(500, yfx, \/).

:- op(500, fx, +).
:- op(500, fx, -).
:- op(500, fx, \).

:- op(400, yfx, *).
:- op(400, yfx, /).
:- op(400, yfx, <<).
:- op(400, yfx, >>).

:- op(300, xfx, div).
:- op(300, xfx, mod).

:- op(200, xfy, '^').

:- op(100, xfy, $-).
:- op(100, fy,  $-).

/* --- Revision History ---------------------------------------------------
--- Simon Nichols, Aug 19 1993
        Quoted ?- to prevent problems caused by loading this library after
        library(ct), which defines ?- as a prolog_macro (ugh!).
 */
