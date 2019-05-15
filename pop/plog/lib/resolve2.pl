/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/lib/resolve2.pl
 *  Purpose:        Resolution theorem prover MARK II
 *  Author:         Unknown, ???
 *  Documentation:
 *  Related Files:
 */

/* Resolution theorem prover MARK II

   Type:

        ?- prove(X).

   to try and solve a problem. X should be the single proposition to
   be proved.

   Clauses are expected to be in the format given in TEACH RESOLUTION
   Use the library file CLAUSIFY to convert expressions in Predicate
   Calculus format.

*/

;;; these operator declarations clash with library clausify ???
:- clause('DEC10', true) -> op(790, fx, [fact, #]); op(99, fx, [fact, #]).

prove(Item) :- prove(Item,0).

prove(Head,Depth) :- Depth < 6,rule(Head :- Body),provelist(Body,Depth).

provelist([],D).
provelist([Head|Rest],Depth) :- Newdepth is Depth + 1,
                              prove(Head,Newdepth),
                              provelist(Rest,Depth).

rule(Head :- Body) :- fact Clause,convert(Clause,Head,Body).

negit(#Term,Term) :- !.
negit(Term,#Term).

convert([Head|Rest],Head,Body) :- negate(Rest,Body).
convert([Item|Rest],Head,[Negitem|Body]) :-
        convert(Rest,Head,Body),
        negit(Item,Negitem).

negate([],[]).
negate([H|R],[N|S]) :- negit(H,N),negate(R,S).

/* some example facts follow */

/* fact [#parent(P,C),mother(P,C),father(P,C)].
   fact [#father(P,C),parent(P,C)].
   fact [#mother(P,C),parent(P,C)].
*/

/* Those three facts state that being a parent is equivalent to         */
/* being a mother or a father                                           */

/*
   fact [#parent(X,Y),#sibling(Y,Z),parent(X,Z)].
   fact [#sibling(X,Y),sibling(Y,X)].

   fact [father(earendil,elrond)].
   fact [sibling(elrond,elros)].
*/

/* try a question like ?-prove(parent(earendil,elros)).                 */
