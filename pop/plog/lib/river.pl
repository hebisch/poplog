/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/lib/river.pl
 *  Purpose:        Riverworld library
 *  Author:         Josie Taylor, May 1983
 *  Documentation:  TEACH * RIVER
 *  Related Files:
 */

/* load ctlib unless already loaded */
:- clause(mishap(M),X); library(ctlib).

start :-
    retractall(at(_,_)),
    assert(at(left_bank,man)),
    assert(at(left_bank,fox)),
    assert(at(left_bank,chicken)),
    assert(at(left_bank,grain)),
    assert(at(left_bank,boat)),
    retractall(eats(_,_)),
    assert(eats(chicken,grain)),
    assert(eats(fox,chicken)),
    retractall(opposite(_,_)),
    assert(opposite(left_bank,right_bank)),
    assert(opposite(right_bank,left_bank)).


checkeat :- at(Place,boat),eats(X,Y),at(Place,X),at(Place,Y),
            retract(at(Place,Y)),
            mishap("YUM YUM!", [X, "has eaten", Y]).
checkeat.

putin(X):-
    (X\=boat; mishap("Can't put the boat in the boat")), !,
    (not(at(boat,man)); mishap("Man must load from the bank")),!,
    (at(Place,X); mishap("That's not anywhere!!", [X])),!,
    (at(Place,boat);
        mishap("The boat is on the other side, twit!", [Place])),!,
    (not(at(boat,_)); (at(boat, Object),
        mishap("Don't cheat, boat is already occupied", [Object]))),!,
          assert(at(boat,X)),
          retract(at(Place,X)).

takeout(X):-
    (not(at(boat,man)); mishap("Man must unload from bank")), !,
    at(Place,boat),
    assert(at(Place,X)),
    retract(at(boat,X)),
    done.

done :-
    at(right_bank, fox),
    at(right_bank, chicken),
    at(right_bank, grain),
    pr("CONGRATULATIONS you've done it!"), nl.
done.


getin(man) :-
          at(Place,man),
          (Place\=boat; mishap ("The man is already in the boat")),!,
          assert(at(boat,man)),
          retract(at(Place,man)).

getout(man) :-
          (at(boat,man); mishap ("The man isn't in the boat")),!,
          at(Place,boat),
          assert(at(Place,man)),
          retract(at(boat,man)).

crossriver(boat) :-                    
          (at(boat,man); mishap ("Boat is not self propelling!")),!,
          at(Place1,boat),
          checkeat,
          opposite(Place2,Place1),
          assert(at(Place2,boat)),
          retract(at(Place1,boat)).


thingsat(Place, []) :- not(at(Place,_)).
thingsat(Place, [Thing | Rest]) :-
    at(Place, Thing), retract(at(Place, Thing)),
    thingsat(Place, Rest),
    assert(at(Place, Thing)).

draw_boat_if_there(Things) :-
    member(boat, Things),
    write('('), thingsat(boat,BThings), drawall(BThings), pr(") ").
draw_boat_if_there(_).

drawall([]).
drawall([boat | T]) :-
    drawall(T).
drawall([H | T]) :-
    H \= boat, write(H), sp, drawall(T).

view :-
    nl,
    thingsat(left_bank, LThings),
    drawall(LThings), draw_boat_if_there(LThings),
    pr("________________ "),
    thingsat(right_bank, RThings),
    draw_boat_if_there(RThings), drawall(RThings),
    nl.


where :- listing(at).


intro :-

    nl,pr("You are a farmer crossing a river on the way to market with a"),
    nl,pr("chicken, a bag of grain and a fox. If left unattended the fox"),
    nl,pr("will eat the chicken, and the chicken will eat the grain. Your"),
    nl,pr("boat will only hold you and one of these marketables at a time."),
    nl,pr("Your task is to work out a sequence of crossings that will"),
    nl,pr("effect a safe transfer of you and all your things across the"),
    nl,pr("river. The sequence must be expressed in terms of these"),
    nl,pr("commands:"),nl,
    nl,pr("getin(man).         -- to climb into and out of the boat."),
    nl,pr("getout(man).        /"),
    nl,pr("crossriver(boat).   --to paddle the boat across."),
    nl,pr("putin(item).        --to load an item  i.e the fox,"),
    nl,pr("                      chicken or grain."),
    nl,pr("takeout(item).      --the opposite of putin."),
    nl,pr("start.              --sets up (or resets) the river scene."),
    nl,pr("where.              --shows you where everything is."),
    nl,pr("view.               --shows you the current situation pictorially."),nl,
    nl,pr("Errors may produce rude comments: don't take them personally!"),nl.


:- start, pr("Type  intro.  for an introduction to the river world."), nl.
