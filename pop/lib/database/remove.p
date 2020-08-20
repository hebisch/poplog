/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 *  File:           C.all/lib/database/remove.p
 *  Purpose:        remove an item from the database
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * REMOVE
 *  Related Files:
 */

section;

define global procedure remove(X);
lvars X DB1 = database, DB2 = [];
    until null(DB1) do
        if fast_front(DB1) matches X then
            fast_front(DB1) -> it;
            fast_back(DB1) -> DB1;
            until null(DB2) do
                conspair(fast_front(DB2), DB1) -> DB1;
                fast_back(DB2) -> DB2
            enduntil;
            DB1 -> database;
            return
        endif;
        conspair(fast_front(DB1), DB2) -> DB2;
        fast_back(DB1) -> DB1;
    enduntil;
    mishap(X,1,'ATTEMPT TO REMOVE NON-EXISTENT ITEM')
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 27 1985 - lvarsed and sectioned.
 */
