/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/tdraw.p
 |  Purpose:        like draw but standardises location to centreof grid square
 |  Author:         Aaron Sloman, Jan 25 ???? (see revisions)
 |  Documentation:  HELP * TDRAW, TEACH * TURTLE
 |  Related Files:  LIB * TURTLE
 */

;;; tdraw is like draw, in the turtle package, except that it
;;; standardises the location to the centre of a grid square, to minimise
;;; "funny" lines produced by 45 degree draws. However, it will have other
;;; nasty effects in some circumstances

section;

define global procedure tdraw(x);
lvars x;
    draw(x);
    0 ->> Xdelta -> Ydelta;
enddefine;

endsection;
