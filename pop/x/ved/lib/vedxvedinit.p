/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/ved/lib/vedxvedinit.p
 > Purpose:         Initialise XVed resources
 > Author:          John Gibson, Nov 21 1996
 > Documentation:   REF * XVED
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

uses vedxvedmouse;

    /*  Standard version just calls vedxvedmouse
    */
define vars vedxvedinit();
    vedxvedmouse()
enddefine;

endsection;
