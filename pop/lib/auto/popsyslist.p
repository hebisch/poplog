/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/auto/popsyslist.p
 >  Purpose:        List of all src and lib dirs (mainly for POPC)
 >  Author:         John Gibson, Jun  8 1993
 >  Documentation:  REF * LIBRARY
 */
compile_mode :pop11 +strict;

section;

    /* Declared as incremental in popc_declare.ph */
vars popsyslist = [%
    '$usepop/pop/src/'      dir_>< nullstring,
    '$usepop/pop/ved/src/'  dir_>< nullstring,
    ident popuseslist
%];

endsection;
