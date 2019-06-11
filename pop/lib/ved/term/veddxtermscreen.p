/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.all/lib/ved/term/veddxtermscreen.p
 > Purpose:         Screen control for DEC windows DXterm terminal emulator
 > Author:          Rob Duncan, Apr 23 1990
 > Related Files:   LIB * VEDDXTERMKEYS
 */
compile_mode :pop11 +strict;

section;

define veddxtermscreen();
    vedxtermscreen();
    "dxterm" -> vedterminalname;
enddefine;

endsection;
