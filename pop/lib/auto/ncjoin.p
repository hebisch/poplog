/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/ncjoin.p
 >  Purpose:        ncjoin replaced by nc_<>
 >  Author:         John Gibson, April 1985 (see revisions)
 */
compile_mode :pop11 +strict;

section;

define global -5 ncjoin = nonop nc_<> (%%) enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Made a closure
 */
