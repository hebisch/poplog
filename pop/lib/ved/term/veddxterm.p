/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/veddxterm.p
 > Purpose:         VED setup for DEC windows DXterm terminal emulator
 > Author:          Rob Duncan, Apr 23 1990
 > Related Files:   LIB * VEDDXTERMSCREEN, * VEDDXTERMKEYS
 */
compile_mode :pop11 +strict;

uses-by_name veddxtermscreen, veddxtermkeys;

section;

define vars veddxterm();
    veduseterm("dxterm") -> ;
    identfn -> veddxterm;
enddefine;

if iscaller(vedsetup) then veddxterm() endif;

endsection;
