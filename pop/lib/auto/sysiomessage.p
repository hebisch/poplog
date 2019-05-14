/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/sysiomessage.p
 > Purpose:         Old procedure for getting O/S error message
 > Author:          John Gibson, Apr 12 1996
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define sysiomessage = sprintf(% '\s(%M)' %) enddefine;

endsection;
