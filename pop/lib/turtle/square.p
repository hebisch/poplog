/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 |  File:           C.all/lib/turtle/square.p
 |  Purpose:        draw a square (using TURTLE)
 |  Author:         A.Sloman 1981 (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:  LIB * TURTLE
 */

section;

define global square(x);
lvars x;
    repeat 4 times
        draw(x); turn(90)
    endrepeat
enddefine;

endsection;
