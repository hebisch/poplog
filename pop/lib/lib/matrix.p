/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/lib/matrix.p
 *  Purpose:        fast 2d arrays for vision processing.
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:
 *  Related Files:
 */

;;; This package provides fast two-dimensional arrays appearing
;;; identical to the library arrays in every respect except that
;;; the lower bounds are assumed to be 1 overriding the values given and that
;;; the procedure for accessing the list of bounds is called -bounds_list-.
;;; The first frozen parameter is the xsize rather than the boundslist.
;;;
;;; Some timings for SOBEL filter over a 128*95 picture:
;;;   Library newsarray - 206 seconds
;;;   newsmatrix - 70 seconds
;;;   newsmatrix with boundslist as first frozen value - 126 seconds
;;;   newsmatrix with variable lower bounds but no checking - 168 seconds

section;

define mselect(x, y, xs, strip, subscr);
lvars x y xs strip subscr;
    subscr(x+(y-1)*xs, strip)
enddefine;

define updaterof mselect(x, y, xs, strip, subscr);
lvars x y xs strip subscr;
    -> subscr(x+(y-1)*xs, strip)
enddefine;

define global newanymatrix(bounds, init, subscr) -> matrix;
lvars bounds init subscr matrix strip i xs ys initialise inigiven = false;
    unless islist(bounds) do
        bounds -> initialise -> bounds;
        true -> inigiven;
    endunless;
    bounds(2) -> xs;
    bounds(4) -> ys;
    init(xs*ys) -> strip;
    if inigiven then
        for i from 1 to xs*ys do initialise -> subscr(i,strip); endfor;
    endif;
    mselect(%xs, strip, subscr%) -> matrix;
    bounds -> pdprops(matrix);
enddefine;

global vars newvmatrix = newanymatrix(%initv, subscrv%);
global vars newsmatrix = newanymatrix(%inits, subscrs%);

define global stripof(matrix);
lvars matrix;
    frozval(2, matrix);
enddefine;

define bounds_list matrix;
lvars matrix;
    pdprops(matrix);
enddefine;

define global xtop(matrix);
lvars matrix;
    bounds_list(matrix)(2);
enddefine;

define global ytop(matrix);
lvars matrix;
    bounds_list(matrix)(4);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Jun 12 1986 - sectionised, lvarsed, tidied and changed
    the name of boundslist to -bounds_list- to prevent clash with exported
    system identifier.
*/
