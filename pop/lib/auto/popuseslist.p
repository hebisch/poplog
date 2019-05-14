/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/auto/popuseslist.p
 >  Purpose:        Defines -popuseslist-
 >  Author:         A.Sloman 1983 (see revisions)
 >  Documentation:  HELP * POPVARS/popuseslist
 >  Related Files:  LIB * LOADLIB, * SHOWLIB, * LIB
 */
compile_mode :pop11 +strict;

section;

    /* Declared as incremental in popc_declare.ph */
vars popuseslist = [%
    ident popautolist,
    '$poplocal/local/lib' dir_>< '',
    '$usepop/pop/packages/lib' dir_>< '',
    popliblibdir,
    popdatalib
%];

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Nov 27 1992
        Added note about incremental declaration.
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- John Williams, May 15 1990 - Changed -popliblist- to an ident
--- John Williams, Jan 11 1989 - Reversed last change
 */
