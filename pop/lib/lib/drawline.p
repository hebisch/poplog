/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/lib/drawline.p
 > Purpose:        draw a line between two given points in a ved buffer
 > Author:         Roger Evans, November 1983 (see revisions)
 > Documentation:  HELP * DRAWLINE
 > Related Files:  LIB * RUBOUT
 */
compile_mode:pop11 +strict;

/*  LIB DRAWLINE provides a single procedure -drawline- which takes the
    coordinates of the ends of a straight line and draws the line in the
    current ved buffer using line-drawing graphics. Lines must be vertical
    or horizontal. Two formats are possible:

        drawline(x1, y1, x2, y2)
        drawline([^x1 ^y1], [^x2 ^y2]);

    If the variable -rubout- is set true, calls of -drawline- rub lines out
    instead of drawing them.
*/


section;

global vars procedure drawline = veddrawline;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 10 1992
        Now just defines -drawline- as a variable procedure that defaults
        to -veddrawline-.
*/
