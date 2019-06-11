/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_wapp.p
 > Purpose:         Write current VED buffer, appending to named file.
 > Author:          Aaron Sloman, Mar 20 1989 (see revisions)
 > Documentation:   REF * ved_wapp
 > Related Files:   LIB * VED_WAPPR
 */
compile_mode :pop11 +strict;

define vars ved_wapp;
    dlocal vvedmarklo = 1, vvedmarkhi = vvedbuffersize;
    ved_wappr();
enddefine;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jan 14 1994
        Tidied up.
 */
