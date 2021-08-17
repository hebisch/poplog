/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/turnto.p
 |  Purpose:        An extension to the turtle package turnto
 |  Author:         Aaron Sloman, Oct 1977 (see revisions)
 |  Documentation:
 |  Related Files:
 */

;;; takes a pair of co-ordinates and alters the heading of the
;;; turtle. turnto(x,y) makes the turtle face towards point (x,y).

section;

define global procedure turnto (x,y);
lvars x y;
    angleof (x-xposition, y-yposition) -> heading
enddefine;

endsection;
