/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt300keys.p
 > Purpose:         Set up VED key bindings for VT300 type terminals
 > Author:          Robert John Duncan, Oct 22 1990
 > Documentation:
 > Related Files:   LIB * VEDVT220KEYS, LIB * VEDVT300SCREEN
 */
compile_mode :pop11 +strict;

section;

uses vedvt220keys;

define vedvt300keys();
    vedvt220keys();
enddefine;

endsection;
