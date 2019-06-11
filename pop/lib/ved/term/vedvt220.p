/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt220.p
 > Purpose:         Configure VED for VT220 type terminals
 > Author:          John Williams, Oct 13 1989 (see revisions)
 > Documentation:   HELP * VT220KEYS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses-by_name vedvt220screen, vedvt220keys;

section;

define vars vedvt220();
    veduseterm("vt220") -> ;
    identfn -> vedvt220;
enddefine;

if iscaller(vedsetup) then vedvt220() endif;

endsection;

/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Oct 24 1989
        Separated into "vedvt220screen.p" and "vedvt220keys.p" files.
 */
