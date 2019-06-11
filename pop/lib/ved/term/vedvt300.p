/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt300.p
 > Purpose:         Configure VED for VT300 series terminal
 > Author:          Robert John Duncan, Oct 22 1990
 > Documentation:
 > Related Files:   LIB * VEDVT300SCREEN, * VEDVT300KEYS
 */
compile_mode :pop11 +strict;

section;

uses-by_name vedvt300screen, vedvt300keys;

define vars vedvt300();
    veduseterm("vt300") -> ;
    identfn -> vedvt300;
enddefine;

if iscaller(vedsetup) then vedvt300() endif;

endsection;
