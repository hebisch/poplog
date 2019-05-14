/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/auto/matchesallof.p
 *  Purpose:        Match a list against a set of patterns
 *  Author:         Aaron Sloman, May 1983 (see revisions)
 *  Documentation:  HELP * MATCHESALLOF
 *  Related Files:  LIB * MATHESONEOF
 */

section;

define global 9 List  matchesallof Plist;
    ;;; Plist is a list of patterns
    lvars P List PLIST;
    for P in Plist do
        unless List matches P then return(false) endunless
    endfor;
    true
enddefine;

endsection;
