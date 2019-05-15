/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/lib/gensym.pl
 *  Purpose:        Create a new atom starting with a given root
 *  Author:         Unknown, ???
 *  Documentation:  HELP * GENSYM
 *  Related Files:
 */

/* leaves current number in database */

gensym(Root,Atom) :-
   get_num(Root,Num),
   name(Root,Name1),
   name(Num,Name2),
   append(Name1,Name2,Name),
   name(Atom,Name).

get_num(Root,Num) :-
   retract(current_num(Root,Num1)), !,
   Num is Num1+1,
   asserta(current_num(Root,Num)).
get_num(Root,1) :-
   asserta(current_num(Root,1)).

append([],X,X).
append([A|B],C,[A|D]) :- append(B,C,D).
