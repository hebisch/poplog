/*  --- Copyright University of Sussex 1988. All rights reserved. ----------
 |  File:           C.all/lib/turtle/storepicture.p
 |  Purpose:        store a picture in a file.
 |  Author:         Unknown, ??? (see revisions)
 |  Documentation:  HELP * STOREPICTURE
 |  Related Files:  LIB * TURTLE
 */

section;

define global storepicture(filename);
lvars filename x y xmax ymax;
vars cucharout = discout(filename);
    pdprops(picture)(3) -> xmax;
    pdprops(picture)(5) -> ymax;
    pr('readpicture();\n');
    for ymax -> y step y - 1 -> y till y = 0 then
        for 1 -> x step x + 1 -> x till x > xmax then
            pr(picture(x, y))
        endfor;
        cucharout(`\n);
    endfor;
    cucharout(termin)
enddefine;

endsection;
