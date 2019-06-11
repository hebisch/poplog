/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/vedchecksize.p
 > Purpose:         Check VED's idea of the screen size
 > Author:          Robert John Duncan, Oct  3 1990
 > Documentation:
 > Related Files:   LIB * VEDRESIZE
 */


section;

define global vedchecksize(nrows, ncols);
    lvars nrows, ncols;
    if vedscreenwrap then ncols - 1 -> ncols endif;
    vedscreenlength == nrows
    and vedscreenwidth == ncols;
enddefine;

endsection;
