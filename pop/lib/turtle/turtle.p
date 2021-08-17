/*  --- Copyright University of Sussex 2009. All rights reserved. ----------
 |  File:           C.all/lib/turtle/turtle.p
 |  Purpose:        Sussex POP11 Turtle package
 |  Author:         Steven Hardy, Aaron Sloman??? (see revisions)
 |  Documentation:  HELP * TURTLE, TEACH * TURTLE
 |  Related Files:  LIB * VTURTLE , TEACH * VTURTLE
 */



section;

;;; Variable declarations
global vars picture;   ;;; A two dimensional array representing the picture
global vars xposition; ;;; An integer representing the x - position of the turtle
global vars Xdelta;    ;;; An integer, between 0 and 99, representing the
        ;;; position of the turtle WITHIN a picture square
global vars yposition Ydelta;
global vars paint; ;;; The turtle package fills picture locations with this
global vars heading;   ;;; The angle the turtle would have to turn, in a clockwise direction,
        ;;; to be facing along the x - axis, to the right.


section $-turtle =>
    jumpto
    Newposition display
    turtle;

define global jumpto(_x, _y);
    round(_x) -> xposition;
    round(_y) -> yposition;
    _x - xposition -> Xdelta;
    _y - yposition -> Ydelta;
enddefine;


;;;define global drawto(_x, _y);
;;;    Plotto(_x, _y);
;;;    jumpto(_x,_y);
;;;enddefine;

;;;define global draw(amount);
;;;    vars _x _y;
;;;    Newposition(amount) -> _y -> _x;
;;;    Plotto(_x,_y);
;;;    jumpto(_x,_y);
;;;enddefine;

vars Headings;
    {' (E)\n' ' (ENE)\n' ' (NE)\n' ' (NNE)\n'
     ' (N)\n' ' (NNW)\n' ' (NW)\n' ' (WNW)\n'
     ' (W)\n' ' (WSW)\n' ' (SW)\n' ' (SSW)\n'
     ' (S)\n' ' (SSE)\n' ' (SE)\n' ' (ESE)\n'
     ' (E)\n'} -> Headings;

define global display();
    lvars lox x hix loy y hiy spcnt paint;
    dl(pdprops(picture)) -> hiy -> loy -> hix -> lox;erase();
    for hiy -> y step y - 1 -> y till y < loy then
        0 -> spcnt;
        pr(y rem 10);
        for lox -> x step x + 1 -> x till x > hix do
            if x == xposition and y == yposition then "T" else picture(x, y)
            endif
                 -> paint;
            if  paint == space
            then    spcnt + 1 -> spcnt
            else
                while   spcnt > 0
                do    pr(space);
                    spcnt - 1 -> spcnt
                endwhile;
                pr(paint)
            endif;
        endfor;
        pr(newline);
    endfor;
    pr(space);
    for lox -> x step x + 1 -> x till x > hix then
        pr(x rem 10)
    endfor;
    turn(0);
    pr('\n Position = (');
    prnum(xposition+Xdelta,2,2);
    pr(', ');
    prnum(yposition + Ydelta,2,2);
    pr(' ), Heading = ');
    pr(heading);
    ((heading * 2 + 67) div 45) -> x;
    pr(Headings(x));
enddefine;


;;; Initialize the system
define global turtle();
    newpicture(pdprops(picture)(3), pdprops(picture)(5));
    "*" -> paint;
    'turtle ready' =>
enddefine;
newpicture(20, 20);
"*" -> paint;

section_cancel(current_section);
endsection;
endsection;

/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 14 2009
        altered prnum commands in display to print coordinates properly.
--- Aaron Sloman, Aug 26 1988 -- tabified
--- Aaron Sloman, Nov 1982 - Revised.
--- Unknown, July 1980 - Altered so that after drawing off edge of picture
    turtle jumps back to last location at which drawing was done successfully
 */
