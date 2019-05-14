/*  --- Copyright University of Sussex 1990.  All rights reserved. ---------
 >  File:           C.all/lib/auto/showlib.p
 >  Purpose:        for looking at library files
 >  Author:         Unknown, (see revisions)
 >  Documentation:  HELP * SHOWLIB
 >  Related Files:  LIB * VED_SHOWLIB
 */


section;

define global vars syntax showlib = popvedcommand(%"ved_showlib"%) enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 31 1990
        Changed to syntax word, using pdpart of -help-
--- John Williams, Apr 20 1986
        changed to list format for consistency with help, teach etc
*/
