/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 *  File:           C.all/lib/database/flush.p
 *  Purpose:        removes all database elements matching its argument
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * FLUSH
 *  Related Files:
 */

section;
global vars them it ;

define global flush(X);
lvars X;
    [] -> them;
    while present(X) do
        remove(it);
        it :: them -> them
    endwhile
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 27 1985 - lvarsed and sectioned.
 */
