/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt320.p
 > Purpose:         Set up VED for VT320 terminals
 > Author:          John Williams, Aug 30 1990
 > Documentation:   REF * VEDTERMINALS
 > Related Files:   LIB * VEDVT220, LIB * VEDVT320SCREEN, LIB * VEDVT320KEYS
 */
compile_mode :pop11 +strict;

section;

uses-by_name vedvt320screen, vedvt320keys;

define vars vedvt320();
    veduseterm("vt320") ->;
    identfn -> vedvt320;
enddefine;

if iscaller(vedsetup) then vedvt320() endif;

endsection;
