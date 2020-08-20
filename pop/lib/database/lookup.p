/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 *  File:           C.all/lib/database/lookup.p
 *  Purpose:        finds a database element matching its argument
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * LOOKUP
 *  Related Files:
 */

section;

define global procedure lookup(X);
lvars X;
    unless present(X) then
        mishap(X, 1, 'LOOKUP FAILURE')
    endunless
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 27 1985 - lvarsed and sectioned.
 */
