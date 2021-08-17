/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/newpicture.p
 |  Purpose:        makes a new array for turtle pictures.
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:
 */

;;; Redefine boundslist so that it works on turtle pictures
;;; newpicture creates an array, and wraps a checking procedure round it.
;;; Experienced users may want to do something more efficient.
section $-turtle => newpicture _oldboundslist boundslist picture
    Lastx Lasty;

global vars Lastx,Lasty;
global vars _oldboundslist;
global vars picture;

boundslist -> _oldboundslist;
sysunprotect("boundslist");

define global constant boundslist(_x);
    ;;; _x is an array or a closure or procedure
    if not(isarray(_x)) and isprocedure(_x) and islist(pdprops(_x))
    then fast_back(pdprops(_x))
    else _oldboundslist(_x)
    endif;
enddefine;


define global newpicture(xsize, ysize);
;;; This procedure makes a new picture of the given size
;;; The turtle is positioned in the bottom left corner
;;; facing to the right

    procedure(x, y, xsize, ysize, strip);
        if x > xsize or x < 1
            or y > ysize or y < 1
        then space
        else strip(x,y)
        endif
    endprocedure -> picture;
    procedure (x, y, xsize, ysize, strip);
        if x > xsize or x < 1
            or y > ysize or y < 1
        then    jumpto(Lastx,Lasty);
            mishap(x, y, 2, 'TURTLE DRAWING OFF THE PICTURE')
        else -> strip(x,y);
            x -> Lastx;
            y -> Lasty
        endif
    endprocedure -> updater(picture);
    picture(%xsize, ysize,
        newarray([1 ^xsize 1 ^ysize],space) %) -> picture;
    [picture %1, xsize, 1, ysize%] -> pdprops(picture);
    1 ->> Lastx->Lasty;
    jumpto(1, 1);
    0 -> heading;
enddefine;

endsection;
