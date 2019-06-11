/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_load.p
 > Purpose:         VED load command
 > Author:          John Gibson, Jan 12 1993
 > Documentation:   REF *VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

define vars ved_load;
    dlocal vedargument;

    define dlocal loadwarning(file) with_props false;
        lvars file;
        vedputmessage('LOADING ' sys_>< file);
    enddefine;

    ' load ' sys_>< vedargument -> vedargument;
    vedcompilevedargument()
enddefine;

endsection;
