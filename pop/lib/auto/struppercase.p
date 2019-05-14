/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/auto/struppercase.p
 >  Purpose:        convert lowercase chars to uppercase
 >  Author:         Tom Khabaza, Jun 26 1985 (see revisions)
 >  Documentation:  HELP * STRUPPERCASE
 >  Related Files:  HELP * MAPDATA
 */
compile_mode :pop11 +strict;


;;; Also works for non strings, due to the generality of mapdata.
section;

define global struppercase = mapdata(%lowertoupper%) enddefine;

endsection;
