/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/auto/warning.p
 >  Purpose:        warning procedure - like mishap but no interrupt
 >  Author:         J.Cunningham 1983 (see revisions)
 >  Documentation:  HELP * WARNING
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars warning(count, message);
    sys_pr_message( if isstring(message) then
                        count, message
                    else
                        destlist(message), count
                    endif, nullstring, `W`)
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 17 1996
        Changed to use sys_pr_message
--- John Williams, Aug 10 1992
        Made -warning- a variable identifier (c.f. BR davidy.71)
--- Jonathan Meyer, Oct 22 1990
        Made it call sysprmessage
 */
