/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_set.p
 >  Purpose:        Alters value of variable, printing a message
 >  Author:         Aaron Sloman, Sept 1983
 >  Documentation:  HELP *VEDCOMMS/ved_set
 >  Related Files:
 */
compile_mode :pop11 +strict;

;;;; ENTER set <variable>
;;;; ENTER set <varliable> value

section;

define ved_set;
    dlocal vedargument;
    if strmember(` `,vedargument) then
        sysparse_string(vedargument) -> vedargument;
        vedargument(2) -> valof(consword(vedargument(1)))
    else
        vedtrysetvalof(vedargument,'ON','OFF')
    endif;
enddefine;

endsection;
