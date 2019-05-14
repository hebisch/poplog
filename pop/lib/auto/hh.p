/*  --- Copyright University of Sussex 1990.  All rights reserved. ---------
 >  File:           C.all/lib/auto/hh.p
 >  Purpose:        finding a help file (some spelling checking done)
 >  Author:         Jon Cunningham 1983 (see revisions)
 >  Documentation:
 >  Related Files:  LIB * VED_H
 */

section;

define global vars syntax hh = popvedcommand(%"ved_h"%) enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 31 1990
        Now just uses ved_h
 */
