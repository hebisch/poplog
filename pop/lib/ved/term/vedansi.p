/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedansi.p
 > Purpose:         Configure VED for basic ANSI-compatible terminal
 > Author:          Rob Duncan, Nov  2 1989 (see revisions)
 > Documentation:   REF * VEDTERMINALS, HELP * TERMINAL, HELP * VEDNEWKEYS
 > Related Files:   LIB * VEDANSISCREEN, LIB * VEDANSIKEYS
 */
compile_mode :pop11 +strict;

uses-by_name vedansiscreen, vedansikeys;

section;

define vars vedansi();
    veduseterm("ansi") -> ;
    identfn -> vedansi;
enddefine;

if iscaller(vedsetup) then vedansi() endif;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Oct 11 1990
    Made to use vedansikeys
 */
