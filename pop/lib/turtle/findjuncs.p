/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 |  File:           C.all/lib/turtle/findjuncs.p
 |  Purpose:        find junctions of a turtle picture.
 |  Author:         Steven Hardy, 18 Jan 1978 (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:  LIB * TURTLE, LIB * FINDLINES, LIB * PAINTPICTURE
 */

/* ----------------------------------------------------------------
 The procedure findjuncs examines the DATABASE for entries of the
 form:
     [line ?type ?point1 ?point2 ....]
 These denote lines in some picture (probably found by FINDLINES).

 FINDJUNCS examines the relationships between these lines and adds to
 the DATABASE a record describing each. These records look like:

     [junc ?type ?point ?end1 ?end2 ...]

 where 'end1' end2, etc. are points at the other end of lines from 'point'
  - ie points which can be reached in one step from the junction.

 Possible values for "type" are:
        end ell tee arw crs kay psi peak jn4 jn5 jn6 jn7 jn8
            where a peak has four lines with max angle > 180
            and "jnN" is an unclassified N-line junction
    NOTE:  a free END is classified as a "junction"

--------------------------------------------------------- */

/*
 The procedure rayof takes two points, and returns the nearest angle in the
 list 0 45 90 135 180 225 270 315 which corresponds to the direction
 of the vector from the first to the second. A point is a two element list.
*/
define lconstant Rayof(dx, dy);
    ;;; used by rayof integer arguments.
    ;;; assume abs(dy) < or = abs(dx), i.e. angle is less than 45.
    ;;; assume both positive - see rayof
    lvars dx,dy;
    if dx == 0 then mishap(0, 'cannot do rayof(0,0)') endif;
    abs(dx) ->dx; abs(dy) ->dy;
    if  dy == 0
    or  100 fi_* dy fi_< 41 fi_* dx
            ;;; 41 = 100 * tan(22.5)
    then    0
    else    45
    endif
enddefine;

define lconstant rayof (p1,p2)->ray;
    lvars p1,p2,dx,dy, ax,ay,ray;
    fast_front(p2)  fi_-  fast_front(p1) -> dx;
    fast_front(fast_back(p2)) fi_- fast_front(fast_back(p1)) ->dy;
    abs(dx) -> ax; abs(dy) -> ay;
    if ax fi_>= ay then Rayof (ax, ay) else 90 fi_- Rayof(ay, ax) endif ->ray;
    if dx fi_> 0
    then if dy fi_< 0 and ray /== 0
        then 360 fi_- ray->ray
        endif
    else
        if dy fi_< 0
        then 180 fi_+ ray
        else 180 fi_- ray
        endif -> ray
    endif
enddefine;

vars p1, p2;    ;;; used in matcher

define findjuncs();
    lvars p,n,nn,d,l,a,type,maxang,x, passone=true;
    dlocal p1, p2;  ;;; used in matcher
    ;;; remove existing junction records, then work through line records
    ;;; to create junction records
    flush([junc ==]);
start:
    foreach [line = ?p1 ?p2] do
        if passone then p1 else p2 endif -> p;
        unless present([junc = ^p ==]) then
            ;;; find other ends of lines ending here
            [] -> l;
            foreach [line = ^p ?p2] do
                unless fast_member(p2, l) then conspair(p2, l) -> l endunless
            endforeach;
            foreach [line = ?p2 ^p] do
                unless fast_member(p2, l) then conspair(p2,l) -> l endunless
            endforeach;
            listlength(l) -> n; ;;; number of points linked to p
            if n == 1 then "end" -> type; goto classified endif;
            ;;; arrange points in cyclic order around p. First link with angles
            [%fast_for p2 in l do conspair(rayof(p,p2),p2) endfast_for %] -> l;
            syssort(l,
                procedure (x, y); lvars x,y;
                    fast_front(x) fi_< fast_front(y)
                endprocedure) ->d;

            maplist(d, fast_back) -> l;     ;;; list of points linked to p
            maplist(d, fast_front) -> d;    ;;; list of orientations of rays
            0 -> maxang;
            last(d) fi_- 360 -> a;
            1 -> x;     ;;; make list of angles, & count steps to biggest
            [%fast_for nn in d do
                    nn fi_- a -> type;  ;;; angle between rays
                    if type fi_> maxang then x -> p1; type -> maxang endif;
                    type;
                    nn -> a;
                    x fi_+ 1 -> x;
            endfast_for%] -> d;     ;;; now a list of angles
            ;;; p1 is number of steps to biggest angle, maxang is biggest
            ;;; rotate l to get node immediately after biggest angle first.
            while p1 fi_> 1 do
                back(l) <> [%front(l)%] ->l;
                p1 fi_- 1 -> p1;
            endwhile;
            if n == 2 then "ell" -> type; goto classified
            elseif n == 3 then
                if maxang > 180 then "arw" elseif maxang == 180 then "tee" else "frk" endif
                    -> type;
                goto classified;
            elseif n == 4 then
                if maxang fi_> 180 then "peak"
                elseif maxang == 180 then "kay"
                elseif d(1) == d(3) and d(2) == d(4) then
                    "crs"
                elseif d(1) fi_+ d(2) == 180 or d(2) fi_+ d(3) == 180 then
                    "psi"
                else "j4"
                endif -> type;
                goto classified;
            endif;

            consword('jn' >< n) -> type;    ;;; unclassified - jn4, jn5, etc.
        classified:
            add([junc ^type ^p ^^l]);
        endunless
    endforeach;
    if passone then
        false -> passone;
        goto start
    endif;
enddefine;


/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 17 1986
    Made substantially more efficient. New category of junction "peak".
    Comments at top extended and clarified.
    Lvars used where appriate.
 */
