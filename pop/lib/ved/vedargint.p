/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/vedargint.p
 > Purpose:         Like vedargnum, but requires an integer
 > Author:          Aaron Sloman, Nov 11 1989
 > Documentation:
 > Related Files:   LIB * VEDARGNUM
 */
compile_mode :pop11 +strict;

/*
Used in LIB * PAGE, LIB * VED_ML, and various others.

*/

section;

;;; Given a string interpret it as an integer, defaulting to 1
;;; Complain if it is not an integer
define vedargint(arg) -> arg;
    lvars arg;
    if strnumber(arg) ->> arg then
        unless isinteger(arg) then
            vederror('INTEGER ARGUMENT NEEDED')
        endunless
    else
        1 -> arg
    endif
enddefine;

endsection;
