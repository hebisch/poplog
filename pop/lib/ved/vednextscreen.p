/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/vednextscreen.p
 > Purpose:         Display next screenfull of current file
 > Author:          John Williams, Oct  6 1989
 > Documentation:   REF * VEDPROCS
 > Related Files:   C.all/lib/ved/vedprevscreen.p
 */


section;

define global vednextscreen();
    if vedline == (vedlineoffset + vedwindowlength - 1) then
        vedscreendown()
    else
        vedscreendown();
        vedscreendown()
    endif
enddefine;

endsection;
