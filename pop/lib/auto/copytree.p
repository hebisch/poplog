/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/auto/copytree.p
 *  Purpose:        recursively copy a list structure
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * COPYTREE
 *  Related Files:
 */

section;

define global copytree(list);
lvars list;
    if atom(list) or list = [] then
        list
    else
        conspair(copytree(hd(list)), copytree(tl(list)))
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Feb 11 1986 - made to work with dynamic lists.
*/
