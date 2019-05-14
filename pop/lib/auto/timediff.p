/*  --- Copyright University of Sussex 1989. All rights reserved. ----------
 *  File:           C.all/lib/auto/timediff.p
 *  Purpose:        record CPU time used since last called.
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  REF *TIMES
 *  Related Files:
 */
compile_mode:pop11 +strict;

section;

lvars total_time = 0;

define global timediff();
    lvars x = total_time;
    systime() -> total_time;
    (total_time - x)/100.0
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jul  4 1989
        Added +strict, made total_time be initialised to 0 rather than
        systime().
--- Mark Rubinstein, Sep 27 1985 - lvarsed and sectioned.
--- John Gibson, 3 Jan 1986 - Changed 100 to 100.0
 */
