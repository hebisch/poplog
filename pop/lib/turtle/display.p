/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/display.p
 |  Purpose:        version of display which prints picture without grid.
 |  Author:         Aaron Sloman circal 1982 (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:
 */

;;; Version of DISPLAY (see TURTLE) which prints out picture without the
;;; Numbers on left and at bottom.

section $-turtle => display picture;
global vars picture;
define global display();
    lvars xsize, lox, ysize, loy, x, y, paint, count;
    erase(dl(pdprops(picture)) -> ysize -> loy -> xsize -> lox);
    for ysize -> y step y - 1 -> y till y < loy do
        0 -> count;
        for lox -> x step x + 1 -> x till x > xsize do
            picture(x, y) -> paint;
            if paint == space then
                count + 1 -> count
            else
                while count > 0 then
                    pr(space);
                    count - 1 -> count;
                endwhile;
                pr(paint);
            endif
        endfor;
        pr(newline);
    endfor;
enddefine;

endsection;
