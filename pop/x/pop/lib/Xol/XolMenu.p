/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/XolMenu.p
 > Purpose:         Xol Menu procedures
 > Author:          John Gibson, Apr  6 1993
 > Documentation:   HELP * OPENLOOK
 */
compile_mode :pop11 +strict;

section;

XptPopLoadProcedures XolMenu [^^XOL_EXLIBS]
    OlMenuPost(x) :void,
    OlMenuUnpost(x) :void,
;

constant XolMenu = true;

endsection;
