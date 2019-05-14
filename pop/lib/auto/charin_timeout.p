/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/auto/charin_timeout.p
 >  Purpose:        try and read a character (raw-mode) before n/100 seconds
 >  Author:         Unknown, (see revisions)
 >  Documentation:  HELP * CHARIN_TIMEOUT
 */
compile_mode :pop11 +strict;

section;

define charin_timeout(n);
    lvars n;
    dlocal pop_timeout_secs = max(1, intof((n+50) div 100));

    define dlocal pop_timeout();
        if iscaller(charin_timeout) then
            exitfrom(false, charin_timeout);
        endif;
    enddefine;

    rawcharin();
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jul 24 1995
        Tidied
 */
