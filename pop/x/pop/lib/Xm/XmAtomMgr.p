/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xm/XmAtomMgr.p
 > Purpose:         Getting Atoms/Atom names
 > Author:          Jonathan Meyer, Feb  8 1991 (see revisions)
 > Documentation:   HELP *MOTIF
 */
compile_mode :pop11 +strict;

section;

constant XM_ATOM_CACHE = true;

XptPopLoadProcedures XmAtomMgr [^^XM_EXLIBS]
    XmInternAtom(x,y,z) :ulong,
    XmGetAtomName(x,y) :exptr.exacc_ntstring,
;

constant XmAtomMgr = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        Uses XptPopLoadProcedures
--- Andreas Schoter, Jul 15 1991
    Corrected spelling of global constant
 */
