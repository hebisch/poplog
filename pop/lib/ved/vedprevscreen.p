/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/vedprevscreen.p
 > Purpose:         Display previous screenfull of current buffer
 > Author:          John Williams, Oct  6 1989
 > Documentation:   REF * VEDPROCS
 > Related Files:   C.all/lib/ved/vednextscreen.p
 */


section;

define global vedprevscreen();
    if vedline == (vedlineoffset + 1) then
        vedscreenup()
    else
        vedscreenup();
        vedscreenup()
    endif
enddefine;

endsection;
