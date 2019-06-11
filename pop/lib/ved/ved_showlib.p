/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_showlib.p
 >  Purpose:        for examining library files
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP *SHOWLIB, HELP *POPLIBLIST, *POPVARS/popuseslist
 >  Related Files:  LIB * LIB, * POPUSESLIST
 */
compile_mode :pop11 +strict;

section;

define vars ved_showlib();
    vedsetup(); ;;; ensure vedinit.p compiled
    vedsysfile("vedlibname", popuseslist,
                    procedure();
                        "lib" -> vedfileprops;
                        vedhelpdefaults()
                    endprocedure);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan  8 1989 called vedsetup
 */
