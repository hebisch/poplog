/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_delout.p
 >  Purpose:        delete output lines, starting with '**'
 >  Author:         Unknown, ???
 >  Documentation:
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_delout();
    dlocal vedpositionstack;
    vedpositionpush();
    1 -> vedline;
    until vedline fi_> vvedbuffersize do
        if issubstring('**',1,vedthisline()) == 1 then
            vedlinedelete()
        else
            vedchardown()
        endif
    enduntil;
    vedpositionpop();
enddefine;

endsection;
