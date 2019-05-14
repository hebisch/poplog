/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/auto/matchesoneof.p
 *  Purpose:        Match a list against a set of patterns
 *  Author:         A. Sloman, 1983
 *  Documentation:  HELP * MATCHESONEOF
 *  Related Files:  LIB * MATCHESALLOF
 */

section;

define global 9 List  matchesoneof Plist;
    ;;; Plist is a list of patterns
    lvars P List Plist;
    for P in Plist do
        if List matches P then return(true) endif
    endfor;
    false
enddefine;

endsection;
