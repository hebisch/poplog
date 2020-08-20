/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 *  File:           C.all/lib/database/present.p
 *  Purpose:        returns true iff its argument is in the database
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * PRESENT
 *  Related Files:
 */

section;

global vars it ;

define global procedure present(XX);
lvars XX DB = database;
    until null(DB) do
        if fast_front(DB) matches XX then
            fast_front(DB) -> it;
            return(true);
        endif;
        fast_back(DB) -> DB
    enduntil;
    return(false)
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 27 1985 - lvarsed and sectioned.
 */
