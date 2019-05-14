/* --- Copyright University of Sussex 1991.  All rights reserved. ---------
 > File:            C.all/lib/auto/newexternal.p
 > Purpose:         General interface for -external_load- (etc)
 > Author:          Ian Rogers (see revisions)
 > Documentation:   HELP * EXTERNAL, * NEWEXTERNAL, REF * EXTERNAL
 > Related Files:   LIB * EXTERNAL, LIB * NEWC_DEC
 */

#_TERMIN_IF DEF POPC_COMPILING              ;;; needs fixing for popc

uses external;

section $-external;

"newc_dec" -> language_name_table("c");

endsection;

vars newexternal = true;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Oct 25 1991
        Removed "newfortran_dec" -> language_name_table("fortran");
--- Aaron Sloman, Sep 15 1990
        Fixed cross references
--- Ian Rogers, Aug 17 1989
        Merged, and rationalised, with LIB * EXTERNAL
 */
