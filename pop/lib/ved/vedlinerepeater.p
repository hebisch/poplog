/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/vedlinerepeater.p
 > Purpose:         Return successive lines from the current VED buffer
 > Author:          John Williams, Mar 13 1991
 > Documentation:   REF * vedlinerepeater
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define vedlinerepeater();
    if vedatend() then
        termin
    else
        veddecodetabs(vedthisline());
        vednextline()
    endif
enddefine;

endsection;
