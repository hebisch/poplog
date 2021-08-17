/*  --- Copyright University of Sussex 2009.  All rights reserved. ---------
 |  File:           C.all/lib/turtle/regions.p
 |  Purpose:        finds regions of a turtle picture
 |  Author:         Aaron Sloman, Dec 1979 (see revisions)
 |  Documentation:  TEACH * REGIONS
 |  Related Files:  LIB * TURTLE
 */


compile_mode :pop11 -defcon +defpdr +oldvar  ;

section;

;;;     The function REGIONS finds cyclic sets of junctions bounding regions,
;;;     starting from a database of the kind produced by seepicture.
;;;     Each region found is represented by a list of seven items of the form
;;;     [CYCLE <number of sides> <inner or outer boundary>
;;;             <simple or complex> <number of ends in cycle>
;;;             <list of corner angles>
;;;             [<point1> <point2> <point3> ... <point1>]]
;;; A cycle is complex if it contains repeated points.
;;; The list of corner angles corresponds to the same order as the set of points.
;;; The list of points ends with the initial point, for convenience of users.
;;; If the cycle contains a free end, then the set of points will start from one of them

;;; The points are traversed clockwise for the outer-boundary of a region
;;; and counter-clockwise for an inner boundary, e.g. the boundary of the
;;; background region.

;;; For a simple polygon, such as a square, the program produces two such cycles.
vars database it;

define raysof(junc);
    ;;; return list of points reachable from the junction.
    tl(tl(tl(junc)))
enddefine;

define pointof(junc);
    hd(tl(tl(junc)))
enddefine;

define nextpt(list, startpt) => pt;
    ;;; list is a list of points, including startpt.
    ;;; return the point following startpt, or the HD of list if startpt is last.
    unless  match([== ^startpt ?pt ==], last(list) :: list)
    then    false -> pt
    endunless
enddefine;

define cyclefound(p1, p2, cycles);
    ;;; cycles is a list of lists of points
    ;;; check whether one of the lists contains p1 followed by p2,
    until cycles == []
    do      if      match([== ^p1 ^p2 ==], hd(cycles))
        then    return(true)
        else    tl(cycles) -> cycles
        endif
    enduntil;
    false
enddefine;

define nextright(junc,startpt);
    ;;; starting at startpt, moving towards the junction junc,
    ;;; turn right. What point do you get to?
    nextpt(raysof(junc), startpt)
enddefine;


define cyclefrom(thispoint,nextpoint);
    lvars p1 p2 ;
    thispoint ->p1; nextpoint -> p2;
    [%      thispoint,
         ;;; go on adding points to the cycle by going from p1 to p2 and
         ;;; turning right to get to the next point.
         until p2 = thispoint
         do
             p2;
             lookup([junc = ^p2 ==]);
             ;;; find the next point to the right going from p1 via the junction at p2
             nextright(it, p1), p2 -> p1 -> p2;
         enduntil, thispoint %]
enddefine;


define getangle(p1,p2);
    ;;;given two points find the slope, in degrees, of the vector from
    ;;;the first to the second
    vars x2 y2;
    dl(p1);dl(p2) ->y2->x2->p2->p1;
    angleof(x2-p1,y2-p2)
enddefine;

define cornangle(ang1,ang2);
    ;;; given orientations of two adjacent segments of a polygon
    ;;; compute the angle between them, on the right
    vars ang;
    180 + ang2 - ang1 ->> ang;
    if      ang > 360 then 360 -
    elseif  ang <= 0  then 360 +
    endif
enddefine;

define cornangles(cycle);
    ;;; given a list of points, find the successive angles
    ;;; round the circuit
    ;;; use three points at a time to work out the angle.
    ;;; use the last point of the cycle to find the angle at the first
    ;;; point, and use the first point to find the angle at the last
    vars p1 p2 p3 list ang1 ang2;
    dest(dest(cycle)) -> list ->p3 ->p2;
    getangle(p2,p3) -> ang2;
    [%until list == []
         do
             p2 -> p1; p3 -> p2; ang2 -> ang1;
             dest(list) -> list -> p3;
             getangle(p2,p3) -> ang2;
             cornangle(ang1,ang2)
         enduntil%] -> list;
    ;;; Now add the angle at the first point of the list
    cornangle(ang2,getangle(p3,cycle(2)))::list
enddefine;

define turnang(cornang);
    ;;; given the angle at a corner, as found by cornangle,
    ;;; work out how much the turtle had to turn to go round the
    ;;; corner. The result is a positive or negative number,
    ;;; depending whether the turn was clockwise or counter-clockwise.
    ;;; cornang assumed to be positive.
    cornang - 180
enddefine;

define cycletype(cycle);
    ;;; type is a list of points in the cycle, the first and last being the same.
    ;;; Find the angles at the corners, and also decide whether
    ;;; the cycle is an inner or an outer boundary
    ;;; is the cycle a simple curve, i.e. with no points
    ;;; repeated, and is it closed or does it include free ends?
    vars ends pt type angs rot;
    cornangles(cycle) -> angs;
    ;;; now work out rot: total turn round circuit
    0 -> rot;
    applist(angs, procedure(ang); rot + turnang(ang) -> rot endprocedure);
    if rot == 360 then "inner" else "outer" endif-> rot;

    ;;; now find how many ends there are, and whether there are repeated points.
    "simple" -> type;       ;;; assume no repeated points.
    0 -> ends;
    tl(cycle) -> cycle;     ;;; ignore first point - repeated at end
    until   cycle == []
    do      dest(cycle) -> cycle -> pt;
        if      type == "simple" and member(pt,cycle)
        then    "complex" -> type
        elseif  present([junc end ^pt ==])
        then    ends + 1 -> ends
        endif
    enduntil;
    length(angs), rot, type, ends, angs
enddefine;

define shorter(l1,l2);
    ;;; used for ordering lists of lists according to their length.
    length(l1) <= length(l2);
enddefine;

define regions();
    vars cycles point nextpoint thisjunc thisstart
         thisfrag rays juncs;
    flush([cycle ==]);
    ;;; re-order database so that junctions occur in order of size
    syssort([%foreach [junc ==] do it endforeach %], shorter) -> juncs;

    ;;; for debugging
    ;;; [juncs ^juncs]==>

    flush([junc == ]);
    juncs <> database -> database;

    ;;; for debugging
    ;;; 'New database'=>
    ;;; database ==>

    ;;; now build up a list of cycles, starting with cycles of cycles.
    [] -> cycles;

    ;;; for each junction point start tracing paths through junctions.
    ;;; a path constantly turns right, at each junction.
    ;;; this suffices to find all cycles, since the path between
    ;;; two neighbouring points will be traversed in both directions, turning right.

    until   juncs == []
    do      dest(juncs) -> juncs -> thisjunc;
        pointof(thisjunc) ->thispoint;
        raysof(thisjunc) -> rays;

        ;;; for debugging
        ;;; [junc ^thisjunc point ^thispoint rays ^rays]==>

        until rays == [] do
            dest(rays) -> rays -> nextpoint;
            ;;; grow a cycle starting from thispoint to nextpoint
            ;;; unless it has already been found.
            unless  cyclefound(thispoint, nextpoint, cycles)
            then    cyclefrom(thispoint, nextpoint) :: cycles -> cycles
            endunless;
        enduntil
    enduntil;


    syssort(cycles, shorter) -> cycles;

    [sorted cycles ^cycles] ==>

    ;;; add the region information to the database.
    applist(cycles,
        procedure (cycle);
            [cycle %cycletype(cycle), cycle %]:: database -> database;
        endprocedure)
enddefine;


endsection;

/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 14 2009
        replaced typo (list1 should be list in cyclefrom)
        added compile_mode and a few other minor fixes
--- Aaron Sloman, May  1 1986
    replaced 'lambda... end' with 'procedure .. endoprocedure'
--- Aaron Sloman, ???  - Altered for compatibility with lib findregions -
    which will include regions made of sets of cycles - e.g. regions with
    outer and innter boundaries.  Streamlined and simplified - getangle
    introduced.
 */
