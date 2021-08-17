/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/turn.p
 |  Purpose:        provide turtle TURNTO facility
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:  LIB * TURTLE
 */

section;

define global procedure turn(angle);
lvars angle;
    heading + angle -> heading;
    while heading >= 360 then heading - 360 -> heading endwhile;
    while heading < 0 then heading + 360 -> heading endwhile;
enddefine;

endsection;
