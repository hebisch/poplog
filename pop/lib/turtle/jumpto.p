/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/jumpto.p
 |  Purpose:        provide TURTLE jumpto facility
 |  Author:         Unknown, (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:
 */

section;

define global procedure jumpto(newx, newy);
lvars newx newy;
    ;;; this version doesn't round, unlike that in lib turtle
    newx -> xposition;
    newy -> yposition;
    0.5 -> Xdelta;
    0.5 -> Ydelta;
enddefine;

endsection;
