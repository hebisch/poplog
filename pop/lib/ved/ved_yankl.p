/*  --- University of Sussex POPLOG file -----------------------------------
 >  File:           C.all/lib/ved/ved_yankl.p
 >  Purpose:        insert contents of vvedlinedump
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */

section;

define global ved_yankl;
    if vvedlinedump == undef then
        vederror('NOTHING TO YANK')
    else
        vedlineabove();
        vedinsertstring(vvedlinedump)
    endif;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
