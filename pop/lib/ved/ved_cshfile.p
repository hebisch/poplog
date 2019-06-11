/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.unix/lib/ved/ved_cshfile.p
 > Purpose:        run CSH from within ved
 > Author:         Tom Khabaza, Sep 27 1983 (see revisions)
 > Documentation:  HELP * VED_CSHFILE
 > Related Files:  LIB * CSH_COMPILE, *CSH_SEND, *VED_IMCSH, *VED_KILLCSH
 */
compile_mode :pop11 +strict;

/* adapted without change from ved_dclfile, roger evans, 8/83. */

uses csh_subsystem;

section;

define vars vedcshdefaults();
    vedveddefaults();
    false -> vedwriteable;
    false -> vedbreak;
    8 -> vedindentstep;
enddefine;

vars procedure ved_cshfile;
if isundef(ved_cshfile) then
    popexit <> procedure; ved_killcsh() endprocedure -> popexit;
endif;

define vars ved_cshfile;
    vedsetup();
    if vedargument = nullstring then 'cshfile' -> vedargument endif;
    vededit([^vedargument ^false csh_subsystem], vedcshdefaults)
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 13 1993
        uses csh_subsystem
--- Ben Rubinstein, Dec 16 1986 - stopped popexit growing topsy-like
--- Aaron Sloman, Aug 23 1986 put into C.unix
--- Aaron Sloman, May  5 1986
        Gave ved_cshfile an optional argument.
        Made popexit respect previous version.
*/
