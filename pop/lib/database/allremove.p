/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 *  File:           C.all/lib/database/allremove.p
 *  Purpose:        removes several items from the database
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:
 *  Related Files:
 */


section;

global vars them it;

define global allremove(L);
lvars L;
    [] -> them;
    until L == [] then
        remove(dest(L) -> L);
        it :: them -> them;
    enduntil
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 27 1985 - lvarsed and sectioned.
 */
