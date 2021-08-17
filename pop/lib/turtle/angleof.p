/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           $usepop/master/C.all/lib/turtle/angleof.p
 |  Purpose:        turtle utility - find the angle at x axis of a vector
 |  Author:         Aaron Sloman, Nov 1977 (see revisions)
 |  Documentation:
 |  Related Files:
 */

;;; angleof takes two numbers, corresponding to the
;;; x and y co-ordinates of a vector, and returns the angle in degrees
;;; which the vector makes with the x axis. The angle is between 0 and
;;; 360 degrees.

section;

define global procedure angleof (dx,dy) ->angle;
    if abs(dx) >= abs(dy) then
        round(arctan(abs(dy/dx)))
    else 90 - round(arctan(abs(dx/dy)))
    endif ->angle;
    if dx > 0
    then if dy < 0 and angle /== 0
        then 360 -angle->angle
        endif
    else
        if dy <0
        then 180 + angle
        else 180 - angle
        endif -> angle
    endif
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 26 1988 - tabified
 */
