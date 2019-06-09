/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/ved/auto/ved_newfile.p
 > Purpose:         Create a new file
 > Author:          Jonathan Meyer, Jun 28 1991
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

include subsystem.ph;

section;

define vars ved_newfile;
    vedinput( vededit(%
        gensym("unnamed") sys_>< subscr_subsystem(SS_FILE_EXTN, subsystem)
    %));
enddefine;

endsection;
