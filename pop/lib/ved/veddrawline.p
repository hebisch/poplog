/* --- Copyright University of Sussex 1992.  All rights reserved. ---------
 > File:           C.all/lib/ved/veddrawline.p
 > Purpose:        Draw a line between two given points in a ved buffer
 > Author:         Roger Evans, November 1983 (see revisions)
 > Documentation:  REF * VEDDRAWLINE
 */
compile_mode:pop11 +strict;

/*  -veddrawline- takes the coordinates of the ends of a straight line and
    draws the line in the current ved buffer using line-drawing graphics.
    Lines must be vertical or horizontal. Two formats are possible:

        veddrawline(x1, y1, x2, y2)
        veddrawline([^x1 ^y1], [^x2 ^y2]);

    If the variable -rubout- is set true, calls of -veddrawline- rub lines out
    instead of drawing them.
*/


section;

global vars
    rubout = false,
    ;

;;; overdraw position x,y in buffer with -gchar-
define lconstant setchar(x, y, gchar);
    lvars x, y, gchar, char;
    lconstant OTHERBITS = `\Gle` && `\Gre`;
    vedjumpto(y, x);
    vedcurrentchar() -> char;
    if `\Gle` <= char and char <= `\G+` then
        ;;; line-graphic char
        if rubout then
            (char &&~~ gchar) -> char;
            if char == 0 then `\s` else char || OTHERBITS endif
        else
            char || gchar
        endif
    elseif rubout then
        ;;; do nothing
        return
    else
        gchar
    endif -> vedcurrentchar()
enddefine;

define lconstant drawhoriz(y, x1, x2);
    lvars x, y, x1, x2;
    if x1 > x2 then x1, x2 -> (x2, x1) endif;
    setchar(x1, y, `\Gle`);
    fast_for x from x1+1 to x2-1 do
        setchar(x, y, `\G-`);
    endfor;
    setchar(x2, y, `\Gre`);
enddefine;

define lconstant drawvert(x, y1, y2);
    lvars x, y, y1, y2;
    if y1 > y2 then y1, y2 -> (y2, y1) endif;
    setchar(x, y1, `\Gte`);
    fast_for y from y1+1 to y2-1 do
        setchar(x, y, `\G|`);
    endfor;
    setchar(x, y2, `\Gbe`);
enddefine;

define lconstant posintcheck(n);
    lvars m, n;
    fast_repeat n times;
        -> m;
        unless isinteger(m) and m >= 0 then
            mishap(m, 1, 'veddrawline: INVALID ARGUMENT');
        endunless;
    endrepeat;
enddefine;

define global veddrawline(x2, y2);
    lvars x2, y2, x1, y1;

    if islist(y2) and islist(x2)
    and listlength(y2) = 2 and listlength(x2) = 2 then
        /* arguments are lists */
        dl(x2) -> (x1, y1);
        dl(y2) -> (x2, y2);
    else
        /* x1,y1 still on stack */
        -> (x1, y1);
    endif;
    posintcheck(x1, y1, x2, y2, 4);

    if x1 == x2 then
        returnif(y1 == y2);
        drawvert(x1, y1, y2)
    elseif y1 == y2 then
        drawhoriz(y1, x1, x2)
    else
        mishap(x1, x2, y1, y2, 4, 'veddrawline: HORIZONTAL OR VERTICAL LINE REQUIRED')
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 10 1992
        Renamed as -veddrawline-. LIB DRAWLINE now just defines the
        variable procedure -drawline- with default value -veddrawline-.
        Rewritten to use new VED standard graphic char set (which includes
        the 4 halfline chars, simplifying the whole thing).
        Also combined with LIB RUBOUT.
--- Ian Rogers, Jul 12 1991
    Added x1,y1 declarations to -drawline-
--- Ian Rogers, Jul  4 1991
    Changed compile mode from +strict to +varsch
--- Aaron Sloman, Jul  3 1991
    Inserted compile_mode, and lots of lvars declarations.
--- Roger Evans, Sep 18 1985 - assumption that graphics chars are > 128
    removed - this slows it down a little, but makes it more general
--- Mark Rubinstein, May 13 1986 - made public.  Made to use
    -graphcharsetuptrap- to ensure table is automatically remade when the
    graphic characters are changed.
*/
