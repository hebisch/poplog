/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/ved/ved_wqved.p
 *  Purpose:        write current file, then quit current file, then edit new file.
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:
 *  Related Files:
 */

section;

define global ved_wqved();
    if vedchanged then ved_w1(); endif;
    ved_qved()
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
