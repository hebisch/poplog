/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/auto/strlowercase.p
 >  Purpose:        convert uppercase chars to lowercase
 >  Author:         Tom Khabaza, Jun 26 1985 (see revisions)
 >  Machines:       cvaxa unx1 vax2 csuna cgeca
 >  Documentation:  help * strlowercase
 >  Related Files:  help * mapdata
 */
compile_mode :pop11 +strict;

;;; Also works for non strings, due to the generality of mapdata.
section;

define global strlowercase = mapdata(%uppertolower%) enddefine;

endsection;
