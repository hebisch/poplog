/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/devname.p
 >  Purpose:        provide devname for compatibility.
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  HELP * DEVNAME
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define global devname = device_open_name(%%) enddefine;;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 10 1992
        Made a closure
 */
