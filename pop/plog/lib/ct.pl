/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/plog/lib/ct.pl
 >  Purpose:        Extend prolog for Computers and Thought Prolog course
 >  Author:         Chris Mellish, Dec 1983 (see revisions)
 >  Documentation:  TEACH * WHY, * TRACER
 */


% THIS FILE IS USED FOR TEACHING AT SUSSEX UNIVERSITY AND IS NOT
% SUPPORTED. USE IT AT YOUR OWN RISK, IN CONNECTION WITH TRACER.PL
% See TEACH * AAreadme

% Use the command $popcom/mkct to make a saved image in $poplocalbin

:- library(prolog_trace_do).

:- prolog_language("pop11").

section;

global vars
    ctfile = 'output.pl',
;

;;; Enter Computers and Thought mode

define global ct_start();
    vedsetkey('\^[o', ved_rb);      ;;; esc-o rotates buffers
    vededitor(vedveddefaults, ctfile);
enddefine;

endsection;

section justifier;

vars prolog_just = [];

define numlen(n);
    lvars n;
    if n < 10 then 1
    elseif n fi_< 100 then 2
    elseif n fi_< 1000 then 3
    else 4
    endif
enddefine;

define ncucharout(n,c);
    lvars n, c;
    ;;; added by A. Sloman
    ;;; print the character c n times through cucharout
    repeat n times cucharout(c) endrepeat;
enddefine;

define callsign(col,n);
    lvars col, n;
    ncucharout(col, ` `);
    cucharout(`\Gbl`);
    ncucharout(2,`\G-`);
    printf(n,'(%p)');
    ncucharout(5-numlen(n), `\G-`);
    cucharout(`\Gtr`)
enddefine;

define exitsign(col);
    lvars col;
    ncucharout(col, ` `);
    cucharout(`\Gtl`);
    ncucharout(9,`\G-`);
    cucharout(`\Gbr`)
enddefine;

endsection;     /* justifier */

:- prolog_language("prolog").

:- module justifier.

:- global
        ct_start/0,
        why/0,
        prolog_static_trace_do/1.

% justification interpreter. acts just as a normal Prolog interpreter,
% except that you type '??' instead of '?-'. the special command
% '?? why.' causes a 'justification' of the last proof to be displayed.
% NB this only works inside VED
% Generalised by A. Sloman so that ?- can be used instead

ct_start :-
    trace_do,
    prolog_eval(apply(valof(ct_start))).

why :-
    prolog_static_trace_do(why).

% query interpreter
prolog_static_trace_do(why) :- !,
   prolog_val(prolog_just, Just),
   output(Just).
prolog_static_trace_do(Goal) :-
   prolog_setq(prolog_just, []),
   do(Goal, Just),
   prolog_setq(prolog_just, quote(Just)).

% Execution, keeping justifications

do(Goal,[Goal - 0]) :-
   functor(Goal,F,N),
   prolog_sys_predicate(F,N), !,
   call(Goal).
do(Goal,[Goal-N|Just]) :-
   functor(Goal,F,Ar),
   functor(Patt,F,Ar),
   findall((Patt:-Body),clause(Patt,Body),Clauses),
   Clauses \== [],
   do_clause(Goal,N,Clauses,Just).

   do_clause(Goal,N,Clauses,Just) :-
      nmember(N,(Goal:-B),Clauses),
      goals_in(B,Just,Type,Rest),
      ((Type=!,!); true),
      rest_goals(Type,Rest).

   goals_in(true,[],end,true) :- !.
   goals_in(!,[],!,true) :- !.
   goals_in((A,B),[J1|J2],Type1,Rest1) :- !,
      goals_in(A,J11,Type,Rest),
      conj_convert(J11,J1),
      conj_goals(Type,Rest,B,J2,Type1,Rest1).
   goals_in((A;B),[(A;B) - 1,Js],Type,Rest) :-
      goals_in(A,Js,Type,Rest).
   goals_in((A;B),[(A;B) - 2,Js],Type,Rest) :- !,
      goals_in(B,Js,Type,Rest).
   goals_in(Goal,[J],end,true) :-
      do(Goal,J).

   conj_convert([X],X) :- !.
   conj_convert(X,X).

   conj_goals(!,A,B,Js,!,(A,f(B,Js))) :- !.
   conj_goals(end,A,B,Js,Type,Rest) :-
      goals_in(B,Js,Type,Rest).

   rest_goals(end,_) :- !.
   rest_goals(!,Rest) :-
      rest_goals1(Rest,Type,Rest1),
      ((Type=!,!); true),
      rest_goals(Type,Rest1).

   rest_goals1(true,end,true) :- !.
   rest_goals1((A,B),Type1,Rest1) :- !,
      rest_goals1(A,Type,Rest),
      conj_rest_goals(Type,Rest,B,Type1,Rest1).
   rest_goals1(f(Goals,Just),Type,Rest) :-
      goals_in(Goals,Just,Type,Rest).

   conj_rest_goals(!,A,B,!,(A,B)) :- !.
   conj_rest_goals(end,A,B,Type,Rest) :-
      rest_goals1(B,Type,Rest).

nmember(1,X,[X|_]).
nmember(N,X,[_|L]) :-
   nmember(N1,X,L),
   N is N1 + 1.

rev(L1,L2) :- rev1(L1,[],L2).

   rev1([],L,L) :- !.
   rev1([X|Y],L,L1) :-
      rev1(Y,[X|L],L1).

prolog_sys_predicate(prolog_eval,1) :- !.
prolog_sys_predicate(prolog_eval,2) :- !.
prolog_sys_predicate(true,0) :- !.
prolog_sys_predicate(F,N) :-
   prolog_system_predicate(F,N).

% display a justification

output(Just) :-
   numbervars(Just,1,_),
   output(Just,1).

output([],_) :- !.
output([[C|Conj]],Col) :- !,
   outputlist([C|Conj],Col).
output([Goal-N|Justs],Col) :- !,
   Col1 is Col+10,
   tab(Col),
   write(Goal), nl,
   callsign(Col,N),
   outputlist(Justs,Col1),
   exitsign(Col).
output(Bluk,Col) :-
   write('******'),
   write(Bluk), nl.

outputlist([],_) :- !.
outputlist([J|Js],Col) :-
   output(J,Col), outputlist(Js,Col).

callsign(Col,N) :-
   prolog_eval(callsign(quote(Col),quote(N))),
   nl.
exitsign(Col) :-
   prolog_eval(exitsign(quote(Col))),
   nl.

% Register the tracer
:- asserta(prolog_trace_mode((static), prolog_static_trace_do)).
:- prolog_setq(prolog_trace_mode, (static)).

:- endmodule justifier.


/*  --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 20 1993
        Moved out interface code which was common to CT and TRACER into
        a new library PROLOG_TRACE_DO.
--- Robert John Duncan, Feb  8 1993
        Replaced use of fast_bag*of/3 with findall/3 which returns solutions
        in a predictable order.
--- Robert John Duncan, Mar 18 1992
        Added missing backslashes in graphics character constants.
--- John Gibson, Feb 13 1992
        Replaced use of g*raphcharsetup with new standard graphics chars
--- Robert John Duncan, Sep 10 1991
        Substantial simplification of the mixed-language stuff.
        Changed -ct_start- so that it works at all.
--- Rob Duncan, Sep  4 1989
    Removed reference to LIB * MODULES (now built in to the system). Made
    change of module explicit for each change of language. Added lots of
    missing parameter declarations and changed "vars" to "dlocal".
--- Rob Duncan, Jan 19 1988
    removed reference to LIB * POPSETQ (now built in to the system)
--- John Williams, Apr 24 1987 - changed $usepop/pop to $poplocal
--- John Williams, Aug  4 1986 removed reference to 'veddoitlimit'
--- Aaron Sloman, ??? - modified as follows:
    1. Saved image can be made by $poplocal/local/com/mkct, and invoked by
    one of
        ct
        ct <filename>           (equivalent to the following)
        ct ved <filename>
        ct help <filename>
        ct teach <filename>
        ct ref  <filename>

    2. Prolog has been altered. The definition of ?- has been changed so that
    if the POP11 variable prolog_trace_do is not false (its default) but
    has the word "prolog_trace_do" as its value, then ?- works as Chris
    defined ??, i.e. it saves a record of execution which can be invoked
    by ?- why. or ?? why.

    3. ct_start is changed so that it doesn't always go back to file 'output'
    but rather to the last VEDed file.
 */
