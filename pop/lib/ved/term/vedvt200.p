/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvt200.p
 > Purpose:         Configure VED for VT200 series terminal
 > Author:          Robert John Duncan, Oct 22 1990
 > Documentation:
 > Related Files:   LIB * VEDVT200SCREEN, * VEDVT200KEYS
 */
compile_mode :pop11 +strict;

section;

uses-by_name vedvt200screen, vedvt200keys;

define vars vedvt200();
    veduseterm("vt200") -> ;
    identfn -> vedvt200;
enddefine;

if iscaller(vedsetup) then vedvt200() endif;

endsection;
