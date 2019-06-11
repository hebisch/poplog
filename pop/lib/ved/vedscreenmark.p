/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/vedscreenmark.p
 > Purpose:         Old name for vedscreenrangemark/vedscreenmoremark
 > Author:          John Gibson, Jan 24 1992
 > Documentation:   REF *OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define global active vedscreenmark;
    ;;; return this one as the other may be in bold
    vedscreenmoremark
enddefine;
;;;
define updaterof active vedscreenmark char;
    lvars char;
    char ->> vedscreenmoremark -> vedscreenrangemark
enddefine;


endsection;
