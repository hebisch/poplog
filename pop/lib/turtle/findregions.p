/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/findregions.p
 |  Purpose:        extends lib regions to deal with regions containing inner boundaries
 |  Author:         Aaron Sloman, Dec 1982 (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:
 */

;;; This extends LIB REGIONS  to enable regions containing inner
;;; boundaries to be dealt with
;;; The ontology is extended to define an entity called a region,
;;; which is composed of cycles - exactly one outer cycle and any number
;;; of inner cycles.

;;; The procedure REGIONS, defined in LIB REGIONS is renamed FINDCYCLES.
;;; A procedure findregions is provided, which assumes that FINDCYCLES has
;;; entered information about cycles in the database. findregions then
;;; creates all the region entries, by grouping cycles into larger structures.
;;; The procedure regions is defined to call findcycles and then findregions,
;;; after which it removes the database entries concerned with cycles.

vars cyclefrom;
unless cyclefrom.isprocedure then popval([lib regions;]) endunless;

vars findcycles;
unless findcycles.isprocedure then regions -> findcycles endunless;
"findcycles" -> pdprops(findcycles);

define sumangle(pt,list);
    ;;; list is a list of points p1, p2, p3...
    ;;; add up all the angles (p1, pt, p2) + (p2, pt, p3) + ...
    ;;; If the list forms a closed ciruit and never crosses itself,
    ;;; the total should be 0 or 360 depending whether the point is inside or outside the circuit
    ;;; This version gives a total of 0 for points on the boundary
    vars ang p1 p2 ang1 ang2 tot;


    0 -> tot;
    dest(list) -> list -> p2;
    getangle(pt,p2) -> ang2;
    until   list== []
    do      p2 -> p1; ang2->ang1;
        dest(list) -> list -> p2;
        getangle(pt,p2) ->ang2;
        180+cornangle(ang2,ang1) -> ang;
        while   ang >= 180  do ang - 360 -> ang endwhile;
        if      ang < -180 then ang + 360 else ang endif + tot -> tot;
    enduntil;
    if abs(tot) < 45 then 0 else 360 endif;
enddefine;

define ptinside(pt,cycle);
    ;;; is the point in the interior of the cycle.
    ;;; Use a different criterion depending whether it is an inner or outer cycle.
    ;;; Assume boundary points of an inner cycle are in the region
    vars ang type list;
    cycle(3) -> type;
    cycle(7) -> list;
    if      member(pt,list) then    return(type=="inner")
    elseif  type=="inner" then rev(list) -> list
    endif;
    sumangle(pt, list) -> ang;
    if      type = "inner"
    then    ang == 0
    else    ang == 360
    endif
enddefine;

define subset(list1,list2);
    ;;; list1 and list2 are lists
    until list1 == [] do
        unless member(dest(list1)->list1,list2) then return(false) endunless
    enduntil;
    true
enddefine;

define cycleinside(c1,c2);
    ;;; c1 and c2 are two cycles does one enclose the other?
    vars pt list1 list2 type;
    c1(7) -> list1;
    c2(7) -> list2;
    if      subset(list1,list2) and subset(list2,list1) then return(false)
    endif;

    ;;; find a point in c1 not in c2
    until (dest(list1) -> list1 -> pt; not(member(pt,list2)))
    do      if      list1==[]
        then    return(not(cycleinside(c2,c1)))
        endif
    enduntil;

    ptinside(pt,c2)
enddefine;

define findregions();
    ;;; assumes that getcycles has already run
    vars background innersof inner outer list nextout;
    flush([region ==]);

    [] -> background;
    newassoc([]) -> innersof;

    ;;; for each inner cycle find the nearest outer cycle enclosing it
    ;;; and for each outer cycle build up a list of the associated inners
    foreach [cycle = inner ==]
    do      it-> inner;
        undef -> nextout;
        foreach [cycle = outer ==]
        do      it -> outer;
            if      cycleinside(inner,outer)
            then    if      nextout==undef
                or      cycleinside(outer,nextout)
                then    outer -> nextout
                endif
            endif
        endforeach;
        if      nextout==undef
        then    inner::background -> background
        else
            innersof(nextout) -> list;
            inner::(if not(list) then [] else list endif)
                -> innersof(nextout)
        endif
    endforeach;

    ;;; Now form the regions, made of outer cycles with their
    ;;; immediately enclosed inners, if any
    foreach [cycle = outer ==] do
        innersof(it) -> list;
        add(    if      list
            then    [region complex ^it ^list]
            else    [region simple ^it]
            endif);
    endforeach;
    add([ region background ^background]);
enddefine;

define regions();
    findcycles();
    findregions();
    flush([cycle ==])
enddefine;

define insideall(pt,list);
    ;;; is the point inside all the cycles in the list?
    until   list == []
    then    unless ptinside(pt,dest(list) -> list) then return(false) endunless;
    enduntil;
    true
enddefine;

define inregion(pt,list);
    ;;; list is a cycle or a region
    if      list(1) == "cycle"
    then    ptinside(pt,list)
    elseif  list(2) == "background"
    then    insideall(pt,list(3))
    else    ptinside(pt,list(3))
    and     (list(2) == "simple"
        or      insideall(pt,list(4)))
    endif
enddefine;
