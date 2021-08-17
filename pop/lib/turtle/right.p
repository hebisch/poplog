/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/right.p
 |  Purpose:        provide turtle turn RIGHT facility.
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:  LIB * TURTLE
 */

section;

define global right(angle);
lvars angle;
    turn(-angle);
enddefine;


endsection;
