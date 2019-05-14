/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 *  File:           C.all/lib/auto/sysfilename.p
 *  Purpose:        return the file name part of a file spec (without extension)
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * SYSFILEPARSE
 *  Related Files:  LIB * SYSFILEPARSE, *SYSFULLFILENAME
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

    ;;; same as -sys_fname_nam-
define global sysfilename = sys_fname(%4%) enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Aug  4 1989
        Now same as -sys_fname_nam-
--- John Gibson, Jul  1 1989
        Added +strict etc
--- Mark Rubinstein, March 22 1985 - altered to be compatible with vms.
--- Aaron Sloman, ??? - altered for UNIX. Produces the bit of the file name
    after the last "/"
 */
