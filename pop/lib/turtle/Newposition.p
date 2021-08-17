/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/turtle/Newposition.p
 *  Purpose:        computes new position after jumping about.
 *  Author:         Unknown, ???
 *  Documentation:
 *  Related Files:
 */

section $-turtle => Newposition;

;;; This procedure computes the new position after jumping amount
;;; It alters Xdelta and Ydelta but returns the new x and y positions


define global procedure Newposition(amount) -> newy->newx;
    (xposition + Xdelta + amount * cos(heading)) -> newx;
    (yposition + Ydelta + amount * sin(heading)) -> newy;
enddefine;

endsection;
