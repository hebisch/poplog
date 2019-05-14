/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 >  File:           C.all/lib/lib/prologliblist.p
 >  Purpose:        List of library directories searched by Prolog
 >  Author:         Aaron Sloman, Dec 29 1983 (see revisions)
 >  Documentation:  HELP * PROLOGLIBLIST
 >  Related Files:  LIB * PROLOG_SUBSYSTEM
 */
compile_mode :pop11 +strict;

section;

    /* Declared incremental in liblib_declare.ph */
vars prologliblist = [%
    '$poplocal/local/plog/lib' dir_>< '',
    '$poplocal/local/plog/auto' dir_>< '',
    '$usepop/pop/plog/lib' dir_>< '',
    '$usepop/pop/plog/auto' dir_>< '',
%];

endsection;


/*  --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 14 1993
        Changed the order of directories to agree with what Prolog's been
        doing for years; removed the assignment to popuseslist, the effect
        of which is now better achieved with subsystem facilities.
--- John Gibson, Nov 27 1992
        Made it use extend_searchlist to add the ident to popuseslist
--- John Williams, Apr 24 1987 - changed $usepop/pop to $poplocal
 */
