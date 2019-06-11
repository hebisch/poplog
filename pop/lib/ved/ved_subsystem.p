/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_subsystem.p
 > Purpose:         Show/change subsystem inside VED
 > Author:          John Gibson, Jan 12 1993
 > Documentation:   REF *SUBSYSTEM
 */
compile_mode :pop11 +strict;

include subsystem.ph;

section;

define vars ved_subsystem();
    lvars ssname;
    unless vedargument = nullstring then
        consword(vedargument) -> ssname;
        subscr_subsystem(SS_NAME, ssname, vederror) -> ;    ;;; check loaded
        ssname -> subsystem
    endunless;
    vedputmessage(word_string(subsystem))
enddefine;

endsection;
