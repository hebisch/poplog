/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.unix/lib/ved/ved_shfile.p
 > Purpose:         run SH from within ved
 > Author:          A.Sloman (from ved_cshfile.p) Aug 1986 (see revisions)
 > Documentation:  HELP * IMSH, HELP * IMCSH, HELP * VED_CSHFILE
 > Related Files:  LIB * CSH_COMPILE, *CSH_SEND, *VED_IMSH, *VED_KILLSH
 */
compile_mode :pop11 +strict;

/*
adapted by A.Sloman from CSH versions.
Originally from ved_dclfile, roger evans, 8/83.

SEE ALSO SH_COMPILE SH_SEND VED_IMSH VED_KILLSH
*/

section;

uses sh_subsystem;

define vars vedshdefaults();
    vedveddefaults();
    false -> vedwriteable;
    false -> vedbreak;
    8 -> vedindentstep;
enddefine;

vars procedure ved_shfile;
if isundef(ved_shfile) then
    popexit <> procedure; ved_killsh() endprocedure -> popexit;
endif;

define vars ved_shfile();
    vedsetup();
    if vedargument = nullstring then 'shfile' -> vedargument endif;
    vededit([^vedargument ^false sh_subsystem], vedshdefaults)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 13 1993
        uses sh_subsystem
--- Aaron Sloman, Jan 30 1991
    Previously ignored argument to ved_shfile. now doesn't.
--- Aaron Sloman, May 29 1990 reformatted header
--- Aaron Sloman, Aug 23 1986
*/
