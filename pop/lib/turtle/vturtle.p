/*  --- Copyright University of Sussex 1994.  All rights reserved. ---------
 >  File:           C.all/lib/turtle/vturtle.p
 >  Purpose:        POP11 Turtle package using the VED mechanism
 >  Author:         Aaron Sloman, AUG 1982 (see revisions)
 >  Documentation:  HELP * VTURTLE, TEACH * TURTLE
 >  Related Files:  LIB * TURTLE
 */
compile_mode :pop11 +oldvar;

section;

;;; Variable declarations
global vars procedure picture;   ;;; A two dimensional array representing the picture: edit buffer
global vars vedcolumn; ;;; An integer representing the x - position of the turtle
global vars Xdelta;    ;;; An integer, between 0 and 99, representing the
        ;;; position of the turtle WITHIN a picture square
global vars vedline Ydelta;
global vars Xmax;  ;;; largest value for vedcolumn
global vars paint; ;;; The turtle fills picture locations with this
global vars xposition,yposition;
global vars heading;   ;;; The angle the turtle would have to turn, in a counter
        ;;;clockwise direction, to be facing along the x - axis, to the right.

global vars popturtlefile;     ;;; if its false, use the current file, otherwise the
                        ;;; file specified
'picture' -> popturtlefile;

global vars Isvturtle= true; ;;; showing that it is the VED turtle.
                             ;;; used by findlines, etc.

global vars
    vturtle_origin = 10,    ;;; line of file corresponding to y = 0
    vturtle_width = 10;     ;;; default width of picture

global vars vturtle_increment;
3 -> vturtle_increment;     ;;; amount by which to extend picture when drawing
                            ;;; off top

section $-turtle => vedsetpicturefile display Markhere Newposition
                    jumpto drawto picture newpicture turtle vturtle_max;

define constant ved_check_picture_loc(x, y);
lvars x y;
    if not(isinteger(x)) or not(isinteger(y)) then
        mishap('INTEGERS NEEDED FOR PICTURE CO-ORDINATES',[^ x ^ y])
    elseif x fi_< 1 or y fi_< 1 then
        mishap('POSITIVE COORDINATES NEEDED FOR TURTLE', [^ x ^ y])
    elseif (vturtle_origin fi_- y ->> y) == vedline then ;;; nothing to do
    else vedtrimline();
        if y fi_< 1 then
            negate(y) fi_+ 1 -> y;
            vturtle_origin fi_+ y -> vturtle_origin;
            unless vedline == 1 then 1 -> vedline;  vedsetlinesize(); endunless;
            repeat y times vedlineabove() endrepeat;
        else y -> vedline; vedsetlinesize();
        endif;
    endif;
    x -> vedcolumn;
    max(vedcolumn,Xmax) -> Xmax;
enddefine;

define global procedure vedsetpicturefile();
    vars vedediting;
    if popturtlefile and popturtlefile /= vedcurrent then
        popturtlefile -> vedvedname;
        true -> vedediting;
        chain(vedveddefaults, vedvedname,vededitor);
    endif;
enddefine;

define global procedure vturtle_max() -> ymax -> xmax;
lvars y xmax ymax;
    ;;; find upper bounds for y and x in picture file
    ;;; bottom of picture file is a row of hyphens - not part of picture
    vedsetpicturefile();
    vturtle_origin fi_- 1 -> ymax;
    0 -> xmax;
    for y from 1 to ymax
    do  max(xmax, vedusedsize(vedbuffer(y))) -> xmax;
    endfor;
enddefine;

define global display();
    ;;; provided for consistency wtih the old POP11 turtle.
    vedsetpicturefile();
enddefine;

define global procedure Markhere(x, y);
lvars x y;
    ;;; column, line on stack
    vars vedautowrite;
    vedsetpicturefile();
    false -> vedautowrite;
    ved_check_picture_loc(round(x), round(y));
    if vedediting then vedcheck() endif;
    if isinteger(paint) then
        paint + `0`
    else paint(1)
    endif  -> vedcurrentdchar()
enddefine;

;;; This procedure computes the new position after jumping amount
;;; It alters Xdelta and Ydelta but returns the new x and y positions
define global procedure Newposition(amount) -> y -> x;
lvars x y;
    vedsetpicturefile();
    (xposition + Xdelta + amount * cos(heading)) -> x;
    (yposition + Ydelta + amount * sin(heading)) -> y;
enddefine;

define global procedure jumpto(x, y);
lvars x y;
    ;;; move turtle without altering picture
    vedsetpicturefile();
    round(x) -> xposition;
    round(y) -> yposition;
    x - xposition -> Xdelta;
    y - yposition -> Ydelta;
    ved_check_picture_loc(xposition,yposition)
enddefine;

define global procedure drawto(x, y);
lvars x1 y1 inc dx dy c stepx x y;
    ;;; mark the picture from current location to location x,y
    vedsetpicturefile();
    vedchanged -> c;
    ;;; get current location
    xposition + Xdelta -> x1;
    yposition + Ydelta -> y1;
    ;;; work out increments
    x - x1 -> dx; y - y1 -> dy;
    if round(dx) == 0 and round(dy) == 0 then Markhere(x,y); return endif;
    ;;; decide whether to increment x or y
    if abs(dx) >  abs(dy) then
        true,
    else
        x1,y1 -> x1 -> y1;
        x, y  -> x -> y;
        dx, dy -> dx -> dy;
        false
    endif -> stepx;
    ;;; x1 will step by  sign(dx), i.e. 1 or -1
    ;;; set inc, the increment for the variable which changes less
    dy / abs(dx) -> inc;
    sign(dx) -> dx;
    for x1 from x1 by dx to x - (0.1 * dx) do
        Markhere(if stepx then x1, y1 else y1, x1 endif);
        y1 + inc -> y1;
    endfor;

    if c then c + 1 else 1 endif -> vedchanged;
    jumpto(if stepx then x,y else y, x endif);
    Markhere(xposition,yposition);
enddefine;

define global picture(x, y);
lvars x y;
    vars vedline vedcolumn;
    vedsetpicturefile();
    ved_check_picture_loc(x,y);
    vedcurrentdchar() -> x;
    if isnumbercode(x) then
        x - `0`
    elseif x fi_<= 255 then
        consword(x, 1)
    else
        consstring(x, 1)
    endif
enddefine;

define updaterof picture(paint, x, y);
lvars x y;
    vedsetpicturefile();
    Markhere(x, y);
enddefine;

"*" -> paint;

0 -> Xdelta; 0 -> Ydelta;
0 -> heading;

1 -> Xmax;

define global newpicture(x, y);
lvars x y;
    ;;; ignore x
    vedsetpicturefile();
    false -> vedbreak;
    false -> vedchanged;
    ved_clear();
    0 -> heading; y + 1 -> vturtle_origin; x -> vturtle_width;
    jumpto(1,1);
    1 -> Xmax;
    vedputmessage('TURTLE at (1,1), heading 0, picture clear')
enddefine;

define global turtle();
    newpicture(vturtle_width,vturtle_origin - 1);
    vedlinebelow();
    repeat vedlinemax times vedcharinsert(`-`) endrepeat;
    jumpto(1,1);
enddefine;

endsection;
endsection;


/*  --- Revision History ---------------------------------------------------
--- John Williams, Sep 30 1994
        picture now copes with dchars (and hence coloured paint).
--- Aaron Sloman, Nov 13 1988
    Fixed bug in vturtle_max. It used to adjust vturtle origin according
    to vvedbuffersize, and should not have done.

--- Aaron Sloman, September 1985 - Added vturtle_max (based on Allan
    Ramsay's modifications to findlines.
 */
