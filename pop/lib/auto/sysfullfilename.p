/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 *  File:           C.all/lib/auto/sysfullfilename.p
 *  Purpose:        return file name with extension for file spec/pathname
 *  Author:         Mark Rubinstein, Jan 1985 (see revisions)
 *  Documentation:  REF *SYSUTIL
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

    ;;; same as -sys_fname_name-
define global sysfullfilename = sys_fname(%4,5%) enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  4 1989
        Now same as -sys_fname_name-
--- John Gibson, Jul  1 1989
        Added +strict and declarations
 */
