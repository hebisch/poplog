/*  --- Copyright University of Sussex 1987. All rights reserved. ----------
 *  File:           C.all/lib/auto/syscallers.p
 *  Purpose:        Return a list of the callers of the caller
 *  Author:         Aaron Sloman (see revisions)
 *  Documentation:  REF *PROCEDURE
 *  Related Files:
 */

section;

define global constant procedure syscallers();
    lvars n = 2, p;
    [% while caller(n) ->> p do p, n fi_+ 1 -> n endwhile %]
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Gibson, Aug  3 1987
        Tidied up
--- Mark Rubinstein, Sep 27 1985
        lvarsed and sectioned.
 */
