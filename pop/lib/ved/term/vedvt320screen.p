/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt320screen.p
 > Purpose:         Set up VED for VT320 type terminals
 > Author:          John Williams, Aug 30 1990
 > Documentation:   REF * VEDTERMINALS
 > Related Files:   LIB * VEDVT220SCREEN, LIB * VEDVT320KEYS, LIB * VEDVT320
 */
compile_mode :pop11 +strict;

section;

uses vedvt220screen;

define vedvt320screen();
    vedvt220screen()
enddefine;

endsection;
