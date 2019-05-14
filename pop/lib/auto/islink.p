/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/auto/islink.p
 *  Purpose:        check iff something is a link (not end of a dynamic list)
 *  Author:         Unknown, ???
 *  Documentation:
 *  Related Files:
 */

section;

define global constant islink(x);
lvars x;
    ispair(x) and not(null(x))
enddefine;

endsection;
