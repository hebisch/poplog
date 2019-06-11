/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_closeall.p
 > Purpose:         Iconify all windows
 > Author:          John Gibson, Jun  3 1993
 > Documentation:   REF * VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

define vars ved_closeall;
    if vedusewindows then
        wved_close_window("all")
    else
        vederror('NOT USING XVed')
    endif;
enddefine;

endsection;
