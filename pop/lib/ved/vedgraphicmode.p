/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/vedgraphicmode.p
 > Purpose:         Old variable for flagging ved graphic mode
 > Author:          John Gibson, Dec 20 1991
 > Documentation:   REF *OBSOLETE
 */
compile_mode :pop11 +strict;

#_INCLUDE '$usepop/pop/lib/include/vedscreendefs.ph'

section;

    ;;; This has no updater because it was never expected to be assigned to
    ;;; (i.e. by anything other than vedscreengraphon/off)
define global active vedgraphicmode;
    vedscreencharmode &&/=_0 VEDCMODE_GRAPHIC
enddefine;

endsection;
