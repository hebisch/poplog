/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/data/overlay.p
 *  Purpose:        picture of overlapping rectangles
 *  Author:         Max Clowes (see revisions)
 *  Documentation:  TEACH * LABELLING
 *  Related Files:  LIB * OUTLINES
 */

vars rect;
if isundef(rect) then
    popval([lib outlines;])
endif;
newpicture(30,18);
rect([7 10],15,7);
rect([2 2],25,12);
