/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt320keys.p
 > Purpose:         Set up VED key bindings for VT320 type terminals
 > Author:          John Williams, Aug 30 1990
 > Documentation:   REF * VEDTERMINALS, HELP * VT320KEYS
 > Related Files:   LIB * VEDVT220KEYS, LIB * VEDVT320SCREEN, LIB * VEDVT320
 */
compile_mode :pop11 +strict;

section;

uses vedvt220keys;

define vedvt320keys();
    vedvt220keys()
enddefine;

endsection;
