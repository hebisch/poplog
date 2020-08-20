/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/database/alladd.p
 *  Purpose:        adds several items to the database
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * DATABASE/alladd
 *  Related Files:
 */

section;

global vars them it;

define global alladd(L);
lvars L;
    [] -> them;
    until L == [] then
        add(dest(L) -> L);
        it :: them -> them;
    enduntil
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 27 1985 - lvarsed and sectioned.
 */
