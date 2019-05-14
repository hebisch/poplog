/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/lib/newpr.p
 >  Purpose:        for smart printing of objects
 >  Author:         Jonathan Cunningham, (see revisions)
 >  Documentation:  HELP * NEWPR
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars newpr(testpdr, prpdr);
    lvars procedure (testpdr, prpdr);
    procedure(item, testproc, prproc, oldpr);
        lvars item, testproc, prproc, oldpr;
        if testproc(item) then prproc(item) else oldpr(item) endif
    endprocedure(% testpdr, prpdr, pr %) -> pr
enddefine;

endsection;
