/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedhpterm.p
 > Purpose:         VED setup for HP VUE terminal windows
 > Author:          Julian Clinton, October 1991
 > Related Files:   LIB * VEDHPTERMSCREEN, * VEDHPTERMKEYS
 */
compile_mode :pop11 +strict;

uses-by_name vedhptermscreen, vedhptermkeys;

section;

define vars vedhpterm();
    veduseterm("hpterm") -> ;
    identfn -> vedhpterm;
enddefine;

if iscaller(vedsetup) then vedhpterm() endif;

endsection;
