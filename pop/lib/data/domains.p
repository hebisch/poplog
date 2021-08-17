/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/data/domains.p
 *  Purpose:        Some domains for problem solvers
 *  Author:         Steven Hardy, January 1980
 *  Documentation:
 *  Related Files:
 */

vars operators,goalorder;
vars x, y, p;
;;;
define blocks();
    ;;;
    ;;; A blocksworld
    [
    [[take ?x off table]
        [[emptyhand] [cleartop ?x] [ontable ?x]]
        [[emptyhand] [ontable ?x] [cleartop ?x]]
        [[holding ?x]]]
     [[place ?x on table]
        [[holding ?x]]
        [[holding ?x]]
        [[cleartop ?x] [ontable ?x] [emptyhand]]]
     [[pick up ?x from ?y]
        [[emptyhand] [?x on ?y] [cleartop ?x]]
        [[emptyhand] [?x on ?y] [cleartop ?x]]
        [[holding ?x] [cleartop ?y]]]
     [[put ?x on ?y]
        [[holding ?x] [cleartop ?y]]
        [[holding ?x] [cleartop ?y]]
        [[emptyhand] [?x on ?y] [cleartop ?x]]]
    ] -> operators;
    ;;;
    [[[cleartop =]] [[holding =]] [[ontable =]] [[= on =]]]
        -> goalorder;
    ;;;
    [[ontable block1]
        [block2 on block1] [cleartop block2]
        [holding block3]
        [ontable block4] [cleartop block4]
        [ontable block5] [cleartop block5]
    ] -> database;
enddefine;
;;;
define rooms();
    ;;;
    ;;; Some rooms with a robot and a box
    ;;;
    [[[move from ?x to ?y]
        [[robot in ?x] [robot on floor] [connected ?x ?y]]
        [[robot in ?x]]
        [[robot in ?y]]]
     [[push box from ?x to ?y]
        [[robot in ?x] [box in ?x] [robot on floor] [connected ?x ?y]]
        [[robot in ?x] [box in ?x]]
        [[robot in ?y] [box in ?y]]]
     [[climb on box in ?x]
        [[robot in ?x] [box in ?x] [robot on floor]]
        [[robot on floor]]
        [[robot on box]]]
     [[climb off box]
        [[robot on box]]
        [[robot on box]]
        [[robot on floor]]]
     [[turn on light]
        [[robot in room2] [box in room2] [robot on box] [light off]]
        [[light off]]
        [[light on]]]
     [[turn off light]
        [[robot in room2] [box in room2] [robot on box] [light on]]
        [[light on]]
        [[light off]]]
    ] -> operators;
    ;;;
    [[[robot on =]] [[robot in =]] [[box in =]] [[light =]] [[connected = =]]]
        -> goalorder;
    ;;;
    [[robot in room1]
        [robot on floor]
        [box in room4]
        [light off]
        [connected room1 room2]
        [connected room2 room1]
        [connected room2 room3]
        [connected room3 room2]
        [connected room3 room4]
        [connected room4 room3]
    ] -> database;
enddefine;
;;;
;;;
define slide();
    ;;;
    ;;; Sliding blocks puzzle
    ;;;
    [[[slide ?p up from ?x to ?y]
        [[?p at ?x] [free ?y] [?y above ?x]]
        [[?p at ?x] [free ?y]]
        [[?p at ?y] [free ?x]]]
    [[slide ?p down from ?x to ?y]
        [[?p at ?x] [free ?y] [?x above ?y]]
        [[?p at ?x] [free ?y]]
        [[?p at ?y] [free ?x]]]
    [[slide ?p right from ?x to ?y]
        [[?p at ?x] [free ?y] [?y right ?x]]
        [[?p at ?x] [free ?y]]
        [[?p at ?y] [free ?x]]]
    [[slide ?p left from ?x to ?y]
        [[?p at ?x] [free ?y] [?x right ?y]]
        [[?p at ?x] [free ?y]]
        [[?p at ?y] [free ?x]]]
     ] -> operators;
    ;;;
    [[[= above =] [= right =]]] -> goalorder;
    ;;;
    [[a above d] [b above e] [c above f]
     [d above g] [e above h] [f above i]
     [b right a] [c right b]
     [e right d] [f right e]
     [h right g] [i right h]
     [2 at a] [1 at b] [4 at c]
     [6 at d] [3 at e] [7 at f]
     [5 at g] [8 at h] [free i]
    ] -> database
enddefine;
