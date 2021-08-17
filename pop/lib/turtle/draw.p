/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           $usepop/master/C.all/lib/turtle/draw.p
 |  Purpose:        draw action for the turtle
 |  Author:         Aaron Sloman, circa 1982 (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:
 */

section;

define global procedure draw(amount);
lvars amount;
    drawto(Newposition(amount));
enddefine;

endsection;
