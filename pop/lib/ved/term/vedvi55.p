/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedvi55.p
 > Purpose:         VED: Set up for Visual 55
 > Author:          Rob Duncan, Oct 19 1989
 > Documentation:
 > Related Files:   LIB * VEDVI55SCREEN, * VEDVI55KEYS
 */
compile_mode :pop11 +strict;

uses-by_name vedvi55screen, vedvi55keys;

section;

define vars vedvi55();
    veduseterm("vi55") -> ;
    identfn -> vedvi55;
enddefine;

if iscaller(vedsetup) then vedvi55() endif;

endsection;
