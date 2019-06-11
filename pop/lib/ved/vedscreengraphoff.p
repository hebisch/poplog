/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/vedscreengraphoff.p
 > Purpose:         Old procedure for turning off graphic mode
 > Author:          John Gibson, Dec 20 1991
 > Documentation:   REF *OBSOLETE
 */
compile_mode :pop11 +strict;

section;

    ;;; Rather than just turn off graphic mode, this turns offs all
    ;;; special modes
define global vedscreengraphoff();
    0 -> vedscreencharmode
enddefine;

endsection;
