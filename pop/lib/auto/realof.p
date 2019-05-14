/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/auto/realof.p
 *  Purpose:        given an integer convert it to a decimal
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:  HELP * REALOF
 *  Related Files:  LIB * INTOF
 */

section;

define global realof(x);
lvars x;
    x + 0.0
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 26 1985 - sectioned and lvarsed.
 */
