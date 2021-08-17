/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           $usepop/master/C.all/lib/turtle/drawby.p
 |  Purpose:        drawby utility for the turtle
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:
 */

section;

define global procedure drawby(dx, dy);
lvars dx dy;
    drawto(xposition + Xdelta + dx, yposition +Ydelta + dy);
enddefine;

endsection;
