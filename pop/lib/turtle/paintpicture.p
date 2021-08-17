/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/paintpicture.p
 |  Purpose:        load seepicture library
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  TEACH * TURTLE
 |  Related Files:  LIB * SEEPICTURE
 */

section;

global vars Isvturtle;
if Isvturtle.isundef then false -> Isvturtle endif;

define global paintpicture();
    vars t, x, y, paint;    ;;; dynamic variables for matcher
    lvars xmax, ymax;
    if Isvturtle then vturtle_max()
    else
        pdprops(picture)(3), pdprops(picture)(5)
    endif -> ymax -> xmax;
    foreach [line = [?x ?y] ==] then
        if x > xmax then x -> xmax endif;
        if y > ymax then y -> ymax endif;
    endforeach;
    foreach [line = = [?x ?y]] then
        if x > xmax then x -> xmax endif;
        if y > ymax then y -> ymax endif;
    endforeach;
    foreach [line ?t ?x ?y] then
        if t = "hrz" then "-"
        elseif t = "vrt" then "!"
        elseif t = "lft" then "\"
        elseif t = "rht" then "/"
        else consword((t >< '')(1), 1)
        endif -> paint;
        jumpto(dl(x));
        drawto(dl(y));
    endforeach;
    foreach [junc ?t [?x ?y] ==] then
        if t = "ell" then "l" else consword((t >< '')(1), 1) endif
            -> picture(x, y)
    endforeach;
    unless Isvturtle then jumpto(0, 0) endunless;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct 14 1985 - Fixed to work with VTURTLE
 */
