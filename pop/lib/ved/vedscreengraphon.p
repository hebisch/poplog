/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/vedscreengraphon.p
 > Purpose:         Old procedure for turning on graphic mode
 > Author:          John Gibson, Dec 20 1991
 > Documentation:   REF *OBSOLETE
 */
compile_mode :pop11 +strict;

#_INCLUDE '$usepop/pop/lib/include/vedscreendefs.ph'

section;

define global vedscreengraphon();
    vedscreencharmode fi_|| VEDCMODE_GRAPHIC -> vedscreencharmode
enddefine;

endsection;
