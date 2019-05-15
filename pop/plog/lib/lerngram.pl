/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/lib/lerngram.pl
 *  Purpose:        LEARNING PROGRAM FOR SIMPLE GRAMMAR
 *  Author:         Chris Mellish, July 1983
 *  Documentation:  HELP * LERNGRAM, TEACH * PARPAR
 *  Related Files:
 */

/* 1. LEARNING USING PARTIAL PARSE TREES.
   This is a general purpose program that only needs the predicate
   'readdata' to be defined according to the domain at hand. 'readdata'
   must produce a parse tree for a new piece of data.
   The main predicate to call is 'talk'. The program prompts the user
   for a set of examples and non-examples, in the style of Winston's
   program. For each one, it says whether it thinks it is an example of
   the concept or not, asking for conformation. Where it cannot decide,
   it changes its internal representation of the concept appropriately */

talk :-
   write('Please give me an example to start from: '),
   readdata(Data),
   talk_on(m(Data,_)).

talk_on(m(CanBe,MustBe)) :-
   agree(CanBe,MustBe), !,
   write('I think I know the concept now'), nl,
   write('My model is as follows:'), nl, nl,
   write(CanBe), nl, nl.
talk_on(Model) :-
   Model = m(CanBe,MustBe),
   nl, nl,
   write('My CAN BE tree is:'), nl,
   write(CanBe), nl, nl,
   write('My MUST BE tree is:'), nl,
   write(MustBe), nl, nl,
   write('Please tell me more'),
   readdata(Data),
   eval_data(Data,Model,NewModel),
   talk_on(NewModel).

eval_data(Data,Model,Model) :-
   Model = m(CanBe,MustBe),
   not(not(Data=CanBe)), !,
   write('That is an example. OK'),
   yesno.
eval_data(Data,Model,Model) :-
   Model = m(CanBe,MustBe),
   not(Data=MustBe), !,
   write('This is a near miss. OK'),
   yesno.
eval_data(Data,Model,NewModel) :-
   write('I don\'t know about this one'), nl,
   write('Is it an example'),
   readline(L),
   assimilate(L,Data,Model,NewModel).

assimilate([y],Data,m(CanBe,MustBe),m(NewCanBe,MustBe)) :- !,
   lub(Data,CanBe,NewCanBe).
assimilate([n],Data,m(CanBe,MustBe),m(CanBe,NewMustBe)) :- !,
   make_complement(Data,CanBe,MustBe,NewMustBe).
assimilate(_,Data,Model,NewModel) :-
   write('Please answer "y" or "n" - is it an example'),
   readline(L),
   assimilate(L,Data,Model,NewModel).

   lub(X,Y,Z) :- (var(X); var(Y)), !.
   lub(X,X,X) :- atomic(X), !.
   lub(X,Y,Z) :-
      functor(X,F,N),
      functor(Y,F,N),
      functor(Z,F,N),
      lubargs(N,X,Y,Z).
   lub(X,Y,Z).

      lubargs(0,_,_,_) :- !.
      lubargs(N,X,Y,Z) :-
         arg(N,X,AX),
         arg(N,Y,AY),
         arg(N,Z,AZ),
         lub(AX,AY,AZ),
         N1 is N - 1,
         lubargs(N1,X,Y,Z).

   make_complement(X,L,Y,Z) :- mc(X,L,Y,Z,false,true).

   mc(D,L,U,U,true,true) :- !.
   mc(D,L,U,U,F,F) :- not(not(D=L)), !.
   mc(D,L,U,NU,F1,F2) :-
      functor(D,F,N),
      functor(L,F,N), !,
      functor(U,F,N),
      functor(NU,F,N),
      mcargs(N,D,L,U,NU,F1,F2).
   mc(_,L,_,NU,_,true) :-
      functor(L,F,N),
      functor(NU,F,N).

      mcargs(0,_,_,_,_,F,F) :- !.
      mcargs(N,X,Y,Z,W,F1,F3) :-
         arg(N,X,XA),
         arg(N,Y,YA),
         arg(N,Z,ZA),
         arg(N,W,WA),
         mc(XA,YA,ZA,WA,F1,F2),
         N1 is N - 1,
         mcargs(N1,X,Y,Z,W,F2,F3).

readline(L) :-
   prolog_eval(apply(valof(readline)),L).

yesno :-
   readline(L), yesnoreply(L).

yesnoreply([y]) :- !.
yesnoreply([n]) :- !, fail.
yesnoreply(X) :- write('please answer "y" or "n"'), nl, yesno.

agree(X,Y) :- var(X), !, var(Y).
agree(X,Y) :- atom(X), !, X==Y.
agree(X,Y) :-
   functor(X,F,N),
   nonvar(Y),
   functor(Y,F,N),
   agreeargs(N,X,Y).

agreeargs(0,_,_) :- !.
agreeargs(N,X,Y) :-
   arg(N,X,X1),
   arg(N,Y,Y1),
   agree(X1,Y1),
   N1 is N - 1,
   agreeargs(N1,X,Y).

/* 2. DOMAIN SPECIFIC PART */

/* The parser. This is used to produce a parse tree from a sentence given
   as example or non-example. (The learning program deals entirely with
   parse trees) */

s(s(NP,VP)) --> np(NP), vp(VP).

np(np(D,A,N)) --> det(D), adjs(A), noun(N).

adjs(adjs(As)) --> noadjs(As).
adjs(adjs(As)) --> someadjs(As).

noadjs(noadjs) --> [].

someadjs(someadjs(A,As)) --> adj(A), adjs(As).

adj(adj(A)) --> sizeadj(A).
adj(adj(A)) --> colouradj(A).

sizeadj(sizeadj(big)) --> [big].
sizeadj(sizeadj(small)) --> [small].

colouradj(colouradj(red)) --> [red].
colouradj(colouradj(blue)) --> [blue].

det(det(a)) --> [a].
det(det(the)) --> [the].

noun(noun(man)) --> [man].
noun(noun(block)) --> [block].

vp(vp(VP)) --> transvp(VP).
vp(vp(VP)) --> verb(VP).

transvp(transvp(V,NP)) --> verb(V), np(NP).

verb(verb(sees)) --> [sees].
verb(verb(hates)) --> [hates].

/* How to read in an example / near miss */

readdata(Data) :-
   readline(L),
   s(Data,L,[]), !.
readdata(Data) :-
   write('I can\'t make sense of that'), nl,
   write('Please try again: '),
   readdata(Data).

:- write('Please type "talk." to start'), nl, nl.
