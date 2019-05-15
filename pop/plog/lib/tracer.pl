/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 >  File:           C.all/plog/lib/tracer.pl
 >  Purpose:        Dynamic tracer for Prolog programs
 >  Author:         Chris Mellish, Nov 1984 (see revisions)
 >  Documentation:  TEACH * TRACER, * WHY
 >  Related Files:
 */

;;; THIS FILE IS USED FOR TEACHING AT SUSSEX UNIVERSITY AND IS NOT
;;; SUPPORTED. USE IT AT YOUR OWN RISK, IN CONNECTION WITH CT.PL
;;; AND THE COMMANDS mkct (remkct) IN $usepop/pop/src
;;; See also the teach files WHY and TRACER and AAreadme

:- library(prolog_trace_do).

:- module tracer.

:- prolog_language("pop11").

section;

;;; Switch tracing on and off from Ved

lvars old_trace_do = prolog_trace_do;

define vars ved_trace;
    "dynamic" -> prolog_trace_mode;
    prolog_trace_do -> old_trace_do;
    "prolog_trace_do" -> prolog_trace_do;
enddefine;

define vars ved_notrace;
    "static" -> prolog_trace_mode;
    old_trace_do -> prolog_trace_do;
enddefine;

endsection;

;;; Generate unique numbers for goals

vars goal_num = 0;

define newgoal();
   goal_num + 1 ->> goal_num;
enddefine;

;;; Set goal number to fail back past

vars cut_goal = false;

define set_cut(num);
    lvars num;
    unless cut_goal then
        num -> cut_goal;
    endunless;
enddefine;

;;; Return "true" or "false" as follows:
;;; If there is no goal to fail past, return "false"
;;; If the number is the goal to fail past, return "true" (and reset CUT_GOAL)
;;; Otherwise return "true"

define cutting(num);
    lvars num;
    if cut_goal then
       if num == cut_goal then
          false -> cut_goal
       endif;
       "true"
    else
       "false"
    endif
enddefine;

vars realvar funny_output varwidth varno updates VAR output funny_op;
vars disp_init1 disp_init2 ann_clse1 ann_clse2 announce_g1 announce_g2;
vars announce_try1 announce_try2 announce_success1 announce_success2;
vars do_goal1 do_goal2 enter_clause1 enter_clause2;
vars announce_cut1 announce_cut2 selecttrace;

:- prolog_language("prolog").

:- global prolog_dynamic_trace_do/1.

prolog_dynamic_trace_do(Goal) :-
   Curr is consword(valof(vedcurrent)),
   Num is apply(valof(newgoal)),
   vedtotrace(Curr),
   prolog_eval(apply(valof(ved_clear))),
   mkfunny(Goal,Funn,1,Max),
;;;   goto(1,1),                    ;;; this code only works for 1 goal
;;;   disp_main_goal(Funn),
;;;   do(Funn,1,1,Max,_,_,no_parent),
   do_clause(Funn,2,1,Max,Lines,NewMax,no_parent),
   extract(Funn,Goal),
   vedfromtrace(Curr).

   disp_main_goal(Funn) :-
      funny_output(Funn,1,1,_).
   disp_main_goal(Funn) :-
      goto(1,1),
      eraseline,
      fail.

;;; satisfy a Goal which has been displayed on line Line, using column
;;; Col to specify the indentation. Max is the first integer not allocated
;;; to a funny variable when the goal is entered. NewMax is the first such
;;; integer afterwards. The 5th argument communicates back an arithmetic
;;; expression saying how many lines this goal takes up (including the line
;;; to display it).
;;; The final argument holds the number associated with the parent goal

do(Var,_,_,_,_,_,_) :- var(Var), !,
   error('Uninstantiated variable as a goal',[]).
do(!,Line,Col,Max,1+Exit,Max,Parent) :- !,
   announce_cut(Line,Col,Parent),
   Exit = 2.
do((A;B),Line,Col,Max,Lines+Exit,NewMax,Parent) :- !,
   Col1 is Col+10,
   Num is apply(valof(newgoal)),
   announce_goal(Col,Line+1),
   get_clause(N,(H:-Bod),[((X;Y):-X),((X;Y):-Y)],Num),
   announce_clause(N,Col,Line+1),
   unify((A;B),H,Max,Max1),
   announce_enter_clause(N,Line+1,Col),
   checkvars(Bod,Max1,Max2),
   do_clause(Bod,Line+2,Col1,Max2,Lines,NewMax,Parent),
   Exit=2,
   announce_goal_done(Line+Lines+1,Col).
do((A,B),Line,Col,Max,Lines+Exit,NewMax,Parent) :- !,
   Col1 is Col+10,
   Num is apply(valof(newgoal)),
   announce_goal(Col,Line+1),
   get_clause(N,(H:-Bod),[((X,Y):-X,Y)],Num),
   announce_clause(N,Col,Line+1),
   unify((A,B),H,Max,Max1),
   announce_enter_clause(N,Line+1,Col),
   checkvars(Bod,Max1,Max2),
   do_clause(Bod,Line+2,Col1,Max2,Lines,NewMax,Parent),
   Exit=2,
   announce_goal_done(Line+Lines+1,Col).
do(call(Goal),Line,Col,Max,Lines,NewMax,Parent) :- !,
   do(Goal,Line,Col,Max,Lines,NewMax,Parent).
do('$VAR'(_,_,_,Goal),Line,Col,Max,Lines,NewMax,Parent) :- !,
   do(Goal,Line,Col,Max,Lines,NewMax,Parent).
do(Goal,Line,Col,Max,1+Exit,NewMax,Parent) :-
   functor(Goal,F,N),
   prolog_sys_predicate(F,N), !,
   announce_goal(Col,Line+1),
   announce_clause(0,Col,Line+1),
   announce_enter_clause(0,Line+1,Col),
   mkunfunny(Goal,Unf),
   safe_io(Unf),
   unify(Goal,Unf,Max,NewMax),
   Exit = 2,
   announce_goal_done(Line+2,Col).
do(Goal,Line,Col,Max,Lines+Exit,NewMax,Parent) :-      ;;; Goal in funny format
   Col1 is Col+10,
   Num is apply(valof(newgoal)),
   functor(Goal,F,Ar),
   functor(Patt,F,Ar),
   findall((Patt:-Body),clause(Patt,Body),Clauses),
   Clauses \== [],
   announce_goal(Col,Line+1),
   get_clause(N,(H:-B),Clauses,Num),
   announce_clause(N,Col,Line+1),
   unify(Goal,H,Max,Max1),
   announce_enter_clause(N,Line+1,Col),
   checkvars(B,Max1,Max2),
   do_clause(B,Line+2,Col1,Max2,Lines,NewMax,par(Num,Line,Col)),
   Exit=2,
   announce_goal_done(Line+Lines+1,Col).

   ;;; invoke a goal, making sure that i/o channels are reset

   safe_io(X) :-
      X,
      prolog_eval(apply(valof(selecttrace))).
   safe_io(X) :-
      prolog_eval(apply(valof(selecttrace))), fail.

;;; Get a clause for a goal

get_clause(N,Clause,Clauses,Num) :-
   nmember(N,Clause,Clauses),
   (true;(cutfail(Num),!,fail)).

;;; What is a system predicate?
;;; For some reason, not all are given by 'prolog_system_predicate'

prolog_sys_predicate(prolog_eval,2) :- !.
prolog_sys_predicate(true,0) :- !.
prolog_sys_predicate(F,N) :-
   prolog_system_predicate(F,N).

;;; do_clause returns number of lines occupied by subgoals + 1
;;; and does this before any variables are updated
;;; The arguments are:
;;;
;;; - the goals in the clause
;;; - the line where the first goal should be displayed
;;; - the column used for indenting
;;; - the max variable number allocated on entry
;;; - the number of lines occupied + 1
;;; - the new maximum variable number
;;; - the parent - a term par(Num,Line,Col)

do_clause(true,_,_,M,1,M,_) :- !.
do_clause(B,Line,Col,Max,Lines+1,NewMax,Parent) :-
   display_goals(Line,Col,B,NewGoals,Lines),
   dogoals(NewGoals,Col,Max,NewMax,Parent).

dogoals((goal(Line,Lines,G),Gs),Col,Max,Max1,Parent) :- !,
   do(G,Line,Col,Max,Lines,Max0,Parent),
   dogoals(Gs,Col,Max0,Max1,Parent).
dogoals(goal(Line,Lines,G),Col,Max,Max1,Parent) :- !,
   do(G,Line,Col,Max,Lines,Max1,Parent).


;;; Do we fail the current goal because of a cut?

cutfail(Num) :- true is cutting(quote(Num)).

;;; Conversion to funny repn
;;; Variables are replaced by terms of the form
;;; '$VAR'(Len,Number,Places,Value)
;;; Number is a unique number
;;; Value is initially uninstantiated
;;; Len is a "column expression" indicating how much space it
;;;    takes to print out the variable's value - 2
;;; Places is a variable-terminated list of places where the variable's
;;; value is displayed. Each entry is of the form X - Y, where X is a
;;; column expression and Y is a line expression

mkfunny(Nice,Funn,Max,Max1) :-
   mkfunny(Nice,Funn,Max,Max1,_).

   mkfunny(Nice,Fvar,Max,Max1,Sofar) :-
      var(Nice), !,
      findvar(Nice,Sofar,Fvar,Max,Max1).
   mkfunny(Nice,Nice,Max,Max,Sof) :-
      atomic(Nice), !.
   mkfunny(Nice,Funn,Max,Max1,Sof) :-
      functor(Nice,F,N),
      functor(Funn,F,N),
      mkallf(N,Nice,Funn,Max,Max1,Sof).

findvar(Var,List,Fvar,Max,Max1) :-
   var(List), !,
   Max1 is Max+1,
   Fvar = '$VAR'(_,Max,_,_),
   List = [[Var|Fvar]|_].
findvar(Var,[[Var1|Fvar]|_],Fvar,Max,Max) :-
   Var == Var1, !.
findvar(Var,[_|List],Fvar,Max,Max1) :-
   findvar(Var,List,Fvar,Max,Max1).

   mkallf(0,_,_,Max,Max,_) :- !.
   mkallf(N,Nice,Funn,Max,Max2,Sof) :-
      N1 is N- 1,
      arg(N,Nice,A1), arg(N,Funn,A2),
      mkfunny(A1,A2,Max,Max1,Sof),
      mkallf(N1,Nice,Funn,Max1,Max2,Sof).

;;; Conversion from funny repn to normal representation
;;; Variables in the copy do not share with the value components
;;; in the original. however, two occurrences of the same variable
;;; term in the original give rise to sharing variables in the copy

mkunfunny(Funn,Nice) :-
   mkunfunny(Nice,Funn,_).

   mkunfunny(Nice,Fvar,Sofar) :-
      Fvar = '$VAR'(_,_,_,Val),
      var(Val), !,
      mkmember(Nice,Fvar,Sofar).
   mkunfunny(Nice,Fvar,Sofar) :-
      Fvar = '$VAR'(_,_,_,Val), !,
      mkunfunny(Nice,Val,Sofar).
   mkunfunny(Nice,Nice,Sof) :-
      atomic(Nice), !.
   mkunfunny(Nice,Funn,Sof) :-
      functor(Funn,F,N),
      functor(Nice,F,N),
      mkallunf(N,Nice,Funn,Sof).

mkmember(Var,Fvar,List) :-
   var(List), !,
   List = [[Var|Fvar]|_].
mkmember(Var,Fvar,[[Var|Fvar]|_]) :- !.
mkmember(Var,Fvar,[_|Rest]) :-
   mkmember(Var,Fvar,Rest).

   mkallunf(0,_,_,_) :- !.
   mkallunf(N,Nice,Funn,Sof) :-
      N1 is N- 1,
      arg(N,Nice,A1), arg(N,Funn,A2),
      mkunfunny(A1,A2,Sof),
      mkallunf(N1,Nice,Funn,Sof).

;;; Conversion from funny repn to ordinary repn, with variables shared
;;; between both repns

extract(V,V) :- var(V), !.
extract('$VAR'(_,_,_,V),V1) :- !, extract(V,V1).
extract(V,V) :- atomic(V), !.
extract(V1,V2) :-
   functor(V1,F,N), functor(V2,F,N),
   extractargs(N,V1,V2).

   extractargs(0,_,_) :- !.
   extractargs(N,V1,V2) :-
      N1 is N - 1,
      arg(N,V1,A1), arg(N,V2,A2),
      extract(A1,A2),
      extractargs(N1,V1,V2).

;;; Unification between a funny and a real
;;; (the Funny is a goal and the Real is the head of a clause)
;;; Any new variables introduced inside the Funny are made funny
;;; The set of variables updated is put in a list, so that the
;;; new values can be displayed and removed on backtracking

unify(Funny,Real,Max,Max1) :-
   match(Funny,Real,Updates),
   checkvars(Funny,Max,Max1),
   update(Updates).

match(X,Val,_) :- var(X), !, X=Val.
match(Val,X,_) :- var(X), !, X=Val.
match(Fvar,Real,Upd) :-
   Fvar = '$VAR'(_,N,L,Val),
   nonvar(Val), !,
   match(Val,Real,Upd).
match(Real,Fvar,Upd) :-
   Fvar = '$VAR'(_,N,L,Val),
   nonvar(Val), !,
   match(Val,Real,Upd).
match('$VAR'(_,N,_,_),'$VAR'(_,N,_,_),_) :- !.
match(Fvar,Real,Upd) :-
   Fvar = '$VAR'(_,N,L,Val), !,
   Val = Real,
   addupd(Fvar,Upd).
match(Real,Fvar,Upd) :-
   Fvar = '$VAR'(_,N,L,Val), !,
   Val = Real,
   addupd(Fvar,Upd).
match(X,Y,_) :- atomic(X), !, X=Y.
match(X,Y,Upd) :-
   functor(X,F,N), functor(Y,F,N),
   matchall(N,X,Y,Upd).

   matchall(0,_,_,_) :- !.
   matchall(N,X,Y,Upd) :-
      N1 is N - 1,
      arg(N,X,X1), arg(N,Y,Y1),
      match(X1,Y1,Upd),
      matchall(N1,X,Y,Upd).

addupd(X,List) :- var(List), !, List = [X|_].
addupd(X,[X|_]) :- !.
addupd(X,[_|List]) :- addupd(X,List).

;;; Check a funny term for new embedded variables
;;; and make such variables "funny"

checkvars(Var,Max,Max1) :- var(Var), !,
   Max1 is Max+1,
   Var = '$VAR'(_,Max,_,_).
checkvars('$VAR'(_,_,_,Val),Max,Max) :-
   (var(Val); atomic(Val)), !.
checkvars('$VAR'(_,_,_,Fvar),Max,Max1) :- !,
   checkvars(Fvar,Max,Max1).
checkvars(X,Max,Max) :- atomic(X), !.
checkvars(T,Max,Max1) :-
   functor(T,_,N), checkvargs(N,T,Max,Max1).

   checkvargs(0,_,M,M) :- !.
   checkvargs(N,T,Max,Max2) :-
      N1 is N - 1,
      arg(N,T,Arg),
      checkvars(Arg,Max,Max1),
      checkvargs(N1,T,Max1,Max2).

;;; Update variables that have matched

:- prolog_language("pop11").

;;; evaluate a column expression. in a column expression, and uninstantiated
;;; variable stands for 0

define evaluate_expr(expr);
    lvars expr;
    if expr.prolog_undefvar then 0
    elseif expr.prolog_complexterm then
       prolog_arg(1,expr).evaluate_expr +
       prolog_arg(2,expr).evaluate_expr
    else
       expr
    endif
enddefine;

define inexpr(x,expr);
    lvars x, expr;
    if x == expr then true
    elseif expr.prolog_complexterm then
       inexpr(x,prolog_arg(1,expr)) or inexpr(x,prolog_arg(2,expr))
    else
       false
    endif
enddefine;

;;; evaluate a line expression. In a line expression, an uninstantiated
;;; variable stands for 1

define eval_line(term);
    lvars term;
    if term.prolog_undefvar then 1
    elseif term.isnumber then term
    else
       eval_line(prolog_arg(1,term)) + eval_line(prolog_arg(2,term))
    endif
enddefine;

;;; Output the value of a variable at a specific coordinate.
;;; Update the variable's "length" component if necessary

define update_place(coord);
   lvars x, y, presx, presy, width, coord;
   prolog_arg(1,coord) -> x;
   prolog_arg(2,coord) -> y;
   evaluate_expr(x) -> presx;
   eval_line(y) -> presy;
   vedjumpto(presy,presx);
   vedwordrightdelete();
   funny_output(realvar,y,x) -> width;
   if varwidth.prolog_undefvar then
      prolog_assign(varwidth,prolog_maketerm(width,-2,"+",2))
   endif;
   conspair([%presx,presy,evaluate_expr(width),varno%],updates) -> updates
enddefine;

;;; update the values of the listed variables on the screen.
;;; produce a list of "updates" that can be used to reset them afterwards.
;;; An "update" has the form:
;;; [^x ^y ^wid ^num] where x and y are the coordinates, wid the width
;;; and num the number of the variable. all of these are absolute numbers
;;; (not expressions)

define update(list) -> updates;
   lvars    var, coords, heldcoords, list;
   dlocal   realvar, varno, varwidth, updates = [];
   prolog_deref(list) -> list;
   until list.prolog_undefvar do
      prolog_arg(1,list) -> var;
      prolog_arg(2,list) -> list;
      unless prolog_arg(4,var).prolog_undefvar then
         var -> realvar;
         until realvar.prolog_complexterm.not or realvar.prolog_predword /== VAR or
            realvar.prolog_nargs /== 2 or prolog_arg(4,realvar).prolog_undefvar do
            prolog_arg(4,realvar) -> realvar
         enduntil;
         prolog_arg(3,var) -> coords;
         prolog_arg(2,var) -> varno;
         prolog_arg(1,var) -> varwidth;
         [] -> heldcoords;
         unless coords.prolog_undefvar then
            while inexpr(varwidth,prolog_arg(1,prolog_arg(1,coords))) do
               conspair(prolog_arg(1,coords),heldcoords) -> heldcoords;
               prolog_arg(2,coords) -> coords
            endwhile;
            until coords.prolog_undefvar do
               update_place(prolog_arg(1,coords));
               prolog_arg(2,coords) -> coords
            enduntil;
            until heldcoords == [] do
               update_place(heldcoords.front);
               heldcoords.back -> heldcoords
            enduntil
         endunless
      endunless
   enduntil
enddefine;

;;; undo the updating of variables

define doupdates(list);
   lvars upd, x, y, wid, num, list;
   for upd in list do
      dl(upd) -> num -> wid -> y -> x;
      vedjumpto(y,x);
      repeat wid times veddotdelete() endrepeat;
      output('_'); output(num)
   endfor
enddefine;

:- prolog_language("prolog").

update(Updates) :-
   prolog_eval(update(quote(Updates)),Places),
   reset_updates(Places).

reset_updates(Places).
reset_updates(Places) :-
   prolog_eval(doupdates(quote(Places))),
   fail.

;;; Funny output that remembers where vars are mentioned by appending
;;; coordinate expressions to their "places" components.
;;; NB expects a term in the funny representation

:- prolog_language("pop11").

vars
    alhs,
    varsin,
    lineexpr,
    leftexpr,
;

define output(x);
    lvars   x;
    dlocal  cucharout = vedcharinsert;
    pr(x)
enddefine;

define add_coords(x,y,list);
    lvars x, y, list;
    until list.prolog_undefvar do
       prolog_arg(2,list) -> list
    enduntil;
    prolog_assign(list,
      prolog_maketerm(prolog_maketerm(x,y,"-",2),prolog_newvar(),".",2))
enddefine;

define funny_output(term,lineexpr,leftexpr);
   lvars    term, y;
   dlocal   alhs, varsin, lineexpr, leftexpr;
   0 -> varsin;
   vedjumpto(eval_line(lineexpr),evaluate_expr(leftexpr));
   vedcolumn -> alhs;
   vedline -> y;
   funny_op(term);
   prolog_maketerm(vedcolumn-alhs,varsin,"+",2)
enddefine;

vars VAR; consword('$VAR') -> VAR;

define funny_op(term);
   lvars term, x;
   prolog_deref(term) -> term;
 start:
   if term.prolog_complexterm then
      if term.prolog_predword == VAR and term.prolog_nargs == 4 then
         prolog_arg(4,term) -> x;
         if x.prolog_undefvar.not then
            x -> term;
            goto start
         else
            prolog_maketerm(
                prolog_maketerm(leftexpr,varsin,"+",2),
                vedcolumn-alhs,"+",2) -> x;
            output('_');
            output(prolog_arg(2,term));
            add_coords(x,lineexpr,prolog_arg(3,term));
            prolog_maketerm(prolog_arg(1,term),varsin,"+",2) -> varsin
         endif
      elseif term.ispair then
         output('[');
         funny_op(prolog_arg(1,term));
         prolog_arg(2,term) -> term;
         while term.ispair do
            output(',');
            funny_op(prolog_arg(1,term));
            prolog_arg(2,term) -> term
         endwhile;
         if term == [] then
            output(']')
         else
            output('|');
            funny_op(term);
            output(']')
         endif
      else
         output(term.prolog_predword);
         output('(');
         for x from 1 to term.prolog_nargs-1 do
            funny_op(prolog_arg(x,term));
            output(',');
         endfor;
         funny_op(prolog_arg(term.prolog_nargs,term));
         output(')')
      endif
   else
      output(term)
   endif
enddefine;

:- prolog_language("prolog").

funny_output(Term,Line,Left,Wid) :-
   prolog_eval(funny_output(quote(Term),quote(Line),quote(Left)),Wid).

;;; Bits of displaying

;;; Very first bit on entry to goal

announce_goal(Col,Line) :-
   prolog_eval(do_goal1(quote(Col),quote(Line))).
announce_goal(Col,Line) :-
   prolog_eval(do_goal2(quote(Col),quote(Line))),
   fail.

;;; Announce trying clause N

announce_clause(N,Col,Line) :-
   prolog_eval(ann_clse1(quote(N),quote(Col),quote(Line))).
announce_clause(_,Col,Line) :-
   prolog_eval(ann_clse2(quote(Col),quote(Line))),
   fail.

;;; Clause has unified with goal

announce_enter_clause(N,Line,Col) :-
   prolog_eval(enter_clause1(quote(N),quote(Line),quote(Col))).
announce_enter_clause(_,Line,Col) :-
   prolog_eval(enter_clause2(quote(Line),quote(Col))),
   fail.

;;; Goal has succeeded

announce_goal_done(Line,Col) :-
   prolog_eval(announce_g1(quote(Line),quote(Col))).
announce_goal_done(Line,Col) :-
   prolog_eval(announce_g2(quote(Line),quote(Col))),
   fail.

;;; Goal is a cut

announce_cut(Line,Col,par(_,PLine,PCol)) :-
   prolog_eval(announce_cut1(quote(Line),quote(Col),quote(PLine),quote(PCol))).
announce_cut(Line,Col,par(Num,_,_)) :- !,
   prolog_eval(announce_cut2(quote(Line),quote(Col),quote(Num))),
   fail.
announce_cut(Line,Col,no_parent) :-
   error('Cut not allowed at top level',[]).

;;; display goals for a clause. convert into a new representation, where
;;; each goal is represented by goal(Line,Lines,G):
;;;   Line - a line expression for where the goal is displayed
;;;   Lines - a line expression for how many lines its execution takes up
;;;   G  - the goal itself
;;; When the goal gets sent through 'do', it is ensured that the line length
;;; resturned shares with this Lines component. Since other goals are sharing
;;; this in their Line expressions, the place where they now are is correctly
;;; updated.

display_goals(Line,Col,B,NewGoals,Lines) :-
   disp_goals(Line,Col,B,NewGoals,Lines),
   disp1(Col,NewGoals).

   disp1(_,_).
   disp1(Col,NewGoals) :-
      undo_goals(Col,NewGoals),
      fail.

   disp_goals(Line,Col,(G,Gs),(goal(Line,Lines,G),NewGs),Lines+Rest) :- !,
      prolog_eval(eval_line(quote(Line)),L),
      goto(Col,L),
      newline,
      funny_output(G,Line,Col,_),
      disp_goals(Line+Lines,Col,Gs,NewGs,Rest).
   disp_goals(Line,Col,G,goal(Line,Lines,G),Lines) :-
      prolog_eval(eval_line(quote(Line)),L),
      goto(Col,L),
      newline,
      funny_output(G,Line,Col,_).

   undo_goals(Col,(goal(Line,_,_),Gs)) :- !,
      undo_goals(Col,Gs),
      prolog_eval(eval_line(quote(Line)),L),
      goto(Col,L),
      eraseline.
   undo_goals(Col,goal(Line,_,_)) :-
      prolog_eval(eval_line(quote(Line)),L),
      goto(Col,L),
      eraseline.

;;; Primitive VED operations

:- prolog_language("pop11").

define selectfile(word);
    lvars   word;
    vedselect(word_string(word), false)
enddefine;

define selecttrace();
    dlocal wvedalwaysraise = true;
    vedselect('trace', false);
    false ->> vedbreak -> vedwriteable
enddefine;

define screen_write(term);
    lvars   term;
    dlocal  cucharout = vedcharinsert;
    prolog_write(term);
enddefine;

define numlen(n);
    lvars n;
    if n < 10 then 1
    elseif n < 100 then 2
    elseif n < 1000 then 3
    elseif n < 10000 then 4
    else 5
    endif
enddefine;

;;; Pause and print a message, except when the goal is being
;;; cut-failed over

define pause(message);
    lvars message;
    unless cut_goal then
        if vedusewindows then wved_set_input_focus(wvedwindow) endif;
        vedputmessage(message><' Press any key to continue');
        erase(vedinascii());
        vedputmessage('');                ;;; immediate feedback
    endunless
enddefine;

vars lookahead = 3;

;;; make sure that the cursor is in the bottom half of the screen
;;; ie. there are at most LOOKAHEAD lines below the cursor

define bottom_half();
   lvars linestoscroll;
   max(vedwindowlength + vedlineoffset - lookahead - vedline,0) -> linestoscroll;
   min(linestoscroll,vedlineoffset) -> linestoscroll;
   repeat linestoscroll times
      vedscrolldown()
   endrepeat;
enddefine;

;;; same for top half
;;; check that there are at least LOOKAHEAD lines below

define top_half();
   lvars linestoscroll;
   max(vedline - vedlineoffset + lookahead - vedwindowlength,0) -> linestoscroll;
   repeat linestoscroll times
      vedscrollup();
   endrepeat;
enddefine;

;;; Calling a goal

define do_goal1(col,line);
    lvars col, line;
    eval_line(line) -> line;
    vedjumpto(line,col);
    vedcheck();
    pause('CALLING...');
    vedlineabove();
    vedjumpto(line,col);
    vedcharinsert(`\Gbl`);
    vedcharinsert(`\G-`);
    vedcharinsert(`(`);
enddefine;

define do_goal2(col,line);
    lvars col, line;
    eval_line(line) -> line;
    vedjumpto(line,col+3);
    repeat 3 times vedchardelete() endrepeat;
    bottom_half();
    pause('FAILING...');
    vedlinedelete()
enddefine;

;;; Trying clause n

define ann_clse1(n,col,line);
    lvars n, col, line;
    eval_line(line) -> line;
    vedjumpto(line,col+3);
    output(n);
    pause('TRYING CLAUSE...');
enddefine;

define ann_clse2(col,line);
    lvars col, line;
    eval_line(line) -> line;
    vedjumpto(line,1);
    vedtextright();
    until vedcurrentchar() == `(` do veddotdelete(); vedcharleft() enduntil;
    vedcharright()
enddefine;

;;; Clause matches

define enter_clause1(n,line,col);
    lvars n, col, line;
    eval_line(line) -> line;
    vedjumpto(line,1);
    vedtextright();
    vedcharinsert(`)`);
    vedcharinsert(`\G-`);
    repeat 5-numlen(n) times vedcharinsert(`\G-`) endrepeat;
    vedcharinsert(`\Gtr`);
    top_half();
    vedchardownleft()
enddefine;

define enter_clause2(line,col);
    lvars col, line;
    eval_line(line) -> line;
    vedjumpto(line,1);
    vedtextright();
    until vedcurrentchar() == `)` do veddotdelete(); vedcharleft() enduntil;
    veddotdelete();
enddefine;

define announce_g1(line,col);
    lvars col, line;
    eval_line(line) -> line;
    vedjumpto(line,col);
    vedcheck();
    pause('EXITING...');
    vedlineabove();
    vedjumpto(line,col+10);
    `\Gbr` -> vedcurrentchar();
    vedcharleft();
    repeat 9 times
       `\G-` -> vedcurrentchar();
       vedcharleft()
    endrepeat;
    `\Gtl` -> vedcurrentchar();
enddefine;

define announce_g2(line,col);
    lvars col, line;
    eval_line(line) -> line;
    vedjumpto(line,col);
    repeat 11 times ` ` -> vedcurrentchar(); vedcharright() endrepeat;
    vedcheck();
    bottom_half();
    pause('RETRYING...');
    vedlinedelete();
enddefine;

;;; Cut

define announce_cut1(line,col,pline,pcol);
    lvars line, col, pline, pcol;
    bottom_half();
    pause('CUTTING...');
    eval_line(line) -> line;
    eval_line(pline) -> pline;
    vedjumpto(line,pcol);
    until vedline == pline do
       `\G|` -> vedcurrentchar();
       vedcharup();
       vedcheck()
    enduntil;
    until vedline == line+1 do
       vedchardown();
       vedcheck()
    enduntil;
    vedlineabove();
    vedjumpto(vedline,pcol);
    vedcharinsert(`\G|`);
    vedlinebelow();
    vedjumpto(vedline,pcol);
    vedcharinsert(`\Gbl`);
    until vedcolumn == col do
       vedcharinsert(`\G-`)
    enduntil;
    vedcharinsert(`\Gtr`);
enddefine;

define announce_cut2(line,col,parent);
    lvars line, col, parent;
    eval_line(line) -> line;
    vedjumpto(line+2,col);
    vedlinedelete();
    vedcharup();
    vedlinedelete();
    bottom_half();
    pause('FAILING PARENT...');
    set_cut(parent)
enddefine;

:- prolog_language("prolog").

vedcurrent(File) :-
   File is valof(vedcurrent).

vedtotrace(Old) :-
   prolog_eval(apply(valof(selecttrace))).
vedtotrace(Old) :-
   prolog_eval(selectfile(quote(Old))), fail.

vedfromtrace(New) :-
   prolog_eval(selectfile(quote(New))).
vedfromtrace(New) :-
   prolog_eval(apply(valof(selecttrace))),
   fail.

goto(Y,X) :- prolog_eval(vedjumpto(quote(X),quote(Y))).

newline :- prolog_eval(apply(valof(vedlineabove))).

eraseline :- prolog_eval(apply(valof(vedlinedelete))).

output(T) :- prolog_eval(screen_write(quote(T))).

;;; Utilities

nmember(1,X,[X|_]).
nmember(N,X,[_|L]) :-
   nmember(N1,X,L),
   N is N1 + 1.

rev(L1,L2) :- rev1(L1,[],L2).

   rev1([],L,L) :- !.
   rev1([X|Y],L,L1) :-
      rev1(Y,[X|L],L1).


% Register the tracer
:- asserta(prolog_trace_mode((dynamic), prolog_dynamic_trace_do)).
:- prolog_setq(prolog_trace_mode, (dynamic)).

:- endmodule tracer.


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 20 1993
        Moved out interface code which was common to CT and TRACER into
        a new library PROLOG_TRACE_DO.
--- Robert John Duncan, Feb  8 1993
        Replaced use of fast_bag*of/3 with findall/3 which returns solutions
        in a predictable order.
--- John Gibson, Feb 10 1992
        Now uses new VED standard graphics chars instead of g*raphcharsetup
--- John Gibson, Jul 12 1991
        Replaced rawcharin() with vedinascii()
--- John Gibson, Mar 22 1991
        Replaced use of -ved_ved- in -selectfile- and -selecttrace- with
        -vedselect-, giving 2nd arg false to stop -vedinputfocus-
        being changed. (Re aarons.98).
--- Rob Duncan, Sep  5 1989
        Removed reference to library modules, now in the system.
        Made changes of module/section explicit with each change of
        language.
        Added lots of missing parameter declarations; replaced procedure
        "vars" declarations with "lvars" or "dlocal" as appropriate.
 */
