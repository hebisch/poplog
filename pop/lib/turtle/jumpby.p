/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/jumpby.p
 |  Purpose:        provide turtle JUMPBY facility
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:
 */

section;

define global procedure jumpby(dx, dy);
lvars dx dy;
    jumpto(xposition + dx, yposition + dy)
enddefine;

endsection;
