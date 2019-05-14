/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/auto/jumpout.p
 >  Purpose:        Create procedure to jump back to specific caller
 >  Author:         John Gibson, 1982 (see revisions)
 >  Documentation:  REF * PROCEDURE
 */
compile_mode :pop11 +strict;

section;

define jumpout(p, n);
    lvars p, n, sl = stacklength();

    define lconstant Jumpout_exit(cstacklen, target_p);
        lvars cstacklen, target_p;
        chainfrom(cstacklen, target_p, target_p,
                        procedure(cstacklen, target_p);
                            lvars cstacklen, target_p;
                            unless callstacklength(1) < cstacklen then
                                Jumpout_exit(cstacklen, target_p)
                            endunless
                        endprocedure)
    enddefine;

    define lconstant Do_jumpout(stacklen, cstacklen, n, p, target_p);
        lvars stacklen, cstacklen, n, p, target_p, vec;
        p();
        if n == 0 then
            setstacklength(stacklen)
        else
            consvector(n) -> vec;
            setstacklength(stacklen);
            explode(vec)
        endif;
        Jumpout_exit(cstacklen, target_p)
    enddefine;

    Do_jumpout(% sl, callstacklength(1), n, p, caller(1) %)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  4 1995
        The call of stacklength() inside Do_jumpout(% ... %) was
        returning the wrong value (owing to Do_jumpout being pushed on the
        stack to construct the closure).
--- John Gibson, Aug  4 1987
        Lvarsed, cleaned up, etc
 */
