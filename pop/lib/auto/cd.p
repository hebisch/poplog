/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/auto/cd.p
 > Purpose:         Change working directory (moved out of system)
 > Author:          John Gibson, Mar  8 1988
 */

section;

define global constant syntax cd;
    sysPUSHQ(readstringline());
    sysPOP("current_directory");
    ";" :: proglist -> proglist
enddefine;

endsection;
