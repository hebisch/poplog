/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/ved/vedthisstring.p
 *  Purpose:        vedthisstring returns a string with the tabs unchanged
 *  Author:         Aaron Sloman, Oct 1982 (see revisions)
 *  Documentation:
 *  Related Files:
 */

;;; vedthisline can return a string with extra tabs, because of the
;;; way ved represents tabs (since version 6.0)

section;

define global vedthisstring();
    veddecodetabs(vedthisline())
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
