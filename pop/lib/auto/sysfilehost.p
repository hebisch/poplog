/*  --- Copyright University of Sussex 1990. All rights reserved. ----------
 *  File:           C.all/lib/auto/sysfilehost.p
 *  Purpose:        return the machine host part of a file spec/path
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * SYSFILEHOST
 *  Related Files:  LIB * SYSFILEPARSE, *SYSFILEFIELD
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

define global sysfilehost = sysfilefield(%1%) enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Jul  1 1989
        Added +strict etc
 */
