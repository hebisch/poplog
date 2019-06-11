/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.all/lib/ved/vedimshell.p
 > Purpose:        Run csh/sh/dcl from immediate mode ved file
 > Author:         John Gibson, Nov  1 1990 (see revisions)
 > Documentation:  REF *VEDPROCS
 */
compile_mode :pop11 +strict;

section;

define vedimshell(ved_file_p);
    lvars procedure (ved_file_p);
    vedsetup();
    vedinput(
        procedure();
            unless vedprocswaiting() then vedsetpop() endunless
        endprocedure);
    ved_file_p()
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 13 1993
        Removed compiler arg and redefinition of pop_setpop_compiler --
        not needed as done by subsystems now.
 */
