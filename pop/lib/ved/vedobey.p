/*  --- Copyright University of Sussex 1994.  All rights reserved. ---------
 > File:           C.all/lib/ved/vedobey.p
 > Purpose:        Runs given procedure in given file
 > Author:         Aaron Sloman, March 1982 (see revisions)
 > Documentation:
 */
compile_mode :pop11 +strict;

section;

define vedobey(file, p);
    lvars file, procedure p, oldfile = ved_current_file;

    if vedinvedprocess then
        vededit(file);
        p();
        if oldfile and vedpresent(oldfile) then vededit(oldfile) endif
    else
        vedinput(procedure();
                    p();
                    if oldfile and vedpresent(oldfile) then
                        vededit(oldfile)
                    endif;
                    vedexit(identfn)
                 endprocedure);
        vededit(file)
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1994
        Added vedpresent check on oldfile before re-editing it.
--- John Gibson, Jun 22 1992
        Rewritten so it always runs the given procedure in the proper
        environment, i.e. inside vedprocess
 */
