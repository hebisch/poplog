/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 *  File:           C.all/lib/auto/sysfiletype.p
 *  Purpose:        return the file type portion of a file spec/path
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  REF *SYSUTIL
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

define global sysfiletype() with_nargs 1;
    lvars extn = sys_fname_extn();
    ;;; lose leading . if not empty
    if extn == nullstring then extn else allbutfirst(1, extn) endif
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Aug  4 1989
        Rewrote to use -sys_fname_extn-
--- John Gibson, Jul  1 1989
        Added +strict etc
--- Mark Rubinstein, Mar 22 1985 - Still didn't work.  Fixed properly.
--- Aaron Sloman, Jan 2 1984 - Didn't work for UNIX.  Fixed.
 */
