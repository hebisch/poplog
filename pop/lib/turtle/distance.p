/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           $usepop/master/C.all/lib/turtle/distance.p
 |  Purpose:        find the distance between two points
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:
 |  Related Files:
 */

section;

define global procedure distance(pt1,pt2);
    pt2(1) - pt1(1);
    pt2(2) - pt1(2);
        ->pt2 ->pt1;
    sqrt(pt1*pt1 + pt2*pt2)
enddefine;

endsection;
