/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lib/ved/vedsetscreen.p
 > Purpose:
 > Author:          John Gibson, Feb 14 1999
 > Documentation:   REF *VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

define vedsetscreen(string);
    vedalignscreen(); vedputmessage(string)
enddefine;

endsection;
