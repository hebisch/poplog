/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           $usepop/master/C.all/lib/turtle/drawto.p
 |  Purpose:        drawto facility for the turtle
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:
 */

section;

define global procedure drawto(newx, newy);
lvars newx newy;
    Plotto(newx, newy);
    jumpto(newx,newy)
enddefine;

endsection;
