/*  --- University of Sussex POPLOG file -----------------------------------
 >  File:           C.all/plog/lib/parsedemo.pl
 >  Purpose:        Demonstration program, using POP-11 and Prolog.
 >  Author:         Unknown, ???
 >  Documentation:  HELP * PARSEDEMO
 >  Related Files:
 */

;;; A POP-11 program reads a sentence and hands it over to Prolog for
;;; parsing. The possible solutions (parse trees) are produced one by one,
;;; via a POP-11 process. Each one is displayed in a VED buffer, using
;;; LIB NEW_SHOWTREE.

;;; Produce a generator of solutions to a Prolog goal
;;; This procedure is given as arguments
;;;
;;; 1) datastructures representing the arguments to the goal
;;; 2) a Prolog variable, which should appear in the above arguments as well,
;;;    and whose possible values one is interested in
;;; 3) a word denoting the name of the predicate
;;; 4) a number denoting the arity of the predicate
;;;
;;; The procedure produces a "solution repeater", which, when applied with no
;;; arguments, produces the next solution to the goal. When there are no more
;;; solutions, it returns TERMIN. If it is applied again, a mishap results.
;;;
;;; It ought to be possible to define a better interface that calls GENERATE

:- prolog_language("pop11").

define generate(res,pred,arity);
   vars process;
   consproc(arity,
      prolog_barrier_apply(%
        procedure pred arity res;
             prolog_valof(pred,arity)
                (procedure r;
                    suspend(prolog_generalise(r),1)
                 endprocedure(%res%)
                );
             ksuspend(termin,1)
        endprocedure(%pred,arity,res%)%)) -> process;
   runproc(%0,process%)
enddefine;

;;; Now here's a parser written in Prolog
;;; Note that the parse trees are represented as lists, so that various POP-11
;;; programs will understand them

:- prolog_language("prolog").

s([s,NP,VP]) --> np(NP), vp(VP).

np([np,D,A,N,P]) --> det(D), adjs(A), noun(N), postadjs(P).
np([np,P]) --> propn(P).

postadjs([]) --> [].
postadjs([padj,P,Ps]) --> postadj(P), postadjs(Ps).

postadj(X) --> pp(X).
postadj(X) --> relc(X).

vp([vp,V,NP,PPs]) --> verb(V), np(NP), pps(PPs).
vp([vp,V,PPs]) --> verb(V), pps(PPs).

det([det,D]) --> [D], {member(D,[the,a,an])} .

adjs([]) --> [].
adjs([adjs,A,As]) --> adj(A), adjs(As).

adj([a,A]) --> [A], {member(A,[big,red,fat,tall,thin,hairy,blue,green,small])} .

noun([n,N]) --> [N], {member(N,[dog,cat,tree,boy,girl,man,block,woman])} .
propn([pr,N]) --> [N], {member(N,[john,mary])} .


verb([v,V]) --> [V], {member(V,[ate,saw,sang,smiled,walked,ran,sat,jumped,chased])} .

pps([]) --> [].
pps([pps,P,PPs]) --> pp(P), pps(PPs).

pp([pp,P,NP]) --> prep(P), np(NP).

prep(P) --> [P], {member(P,[on,with,in,at,by,up,under,to])} .

relc([relc,VP]) --> [who], vp(VP).

member(X,[X|L]).
member(X,[_ |L]) :- member(X,L).

;;; Now a special purpose generator for parsing

:- prolog_language("pop11").

define parse_gen(sentence);
   vars x;
   prolog_newvar() -> x;
   generate(x,sentence,[],x,"s",3)
enddefine;

;;; Test program

uses showtree;

define go();
   vars gen, x, sent;
   repeat
      [please type your sentence]=>
      readline() -> sent;
      parse_gen(sent) -> gen;
      gen() -> x;
      while x /== termin do
         [Got another parse!]=>
         showtree(x);
         gen() -> x;
      endwhile;
      [no more parses]=>
   endrepeat
enddefine;

:- prolog_language("prolog").

;;; Now a Prolog predicate to call the system

go :- prolog_eval(apply(valof(go))).

:- write('type go. to start'), nl, nl.
