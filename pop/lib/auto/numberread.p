/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/auto/numberread.p
 *  Purpose:        read a number
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:
 *  Related Files:
 */

section;

define global numberread() -> x;
lvars x;
    itemread() -> x;
    unless isnumber(x) then
        mishap('NON-NUMBER READ', [^x])
    endunless
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 26 1985 - lvarsed and sectioned.
 */
