/*  --- Copyright University of Sussex 1990. All rights reserved. ----------
 >  File:           C.all/lib/auto/sysfiledir.p
 >  Purpose:        get the directory part of a file specification.
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP * SYSFILEDIR
 >  Related Files:  LIB * SYSFILEPARSE
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

define global sysfiledir = sysfilefield(%3%) enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Jul  1 1989
        Added +strict etc
--- Aaron Sloman, May  7 1989
    changed to use define ... =
--- Mark Rubinstein, March 22 1985 - made to use sysfileparse.
--- Chris Slymon, March 1984 - sysfiledir made global.
 */
