/*  --- Copyright University of Sussex 1990. All rights reserved. ----------
 *  File:           C.all/lib/auto/sysfiledisk.p
 *  Purpose:        return the disk part of a file spec (always '' on UNIX)
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * SYSFILEPARSE
 *  Related Files:  LIB * SYSFILEPARSE
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

define global sysfiledisk = sysfilefield(%2%) enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Jul  1 1989
        Added +strict etc.
 */
