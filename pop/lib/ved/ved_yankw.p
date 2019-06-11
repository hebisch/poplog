/*  --- University of Sussex POPLOG file -----------------------------------
 >  File:           C.all/lib/ved/ved_yankw.p
 >  Purpose:        insert contents of vvedworddump
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */

section;

define global ved_yankw;
    if vvedworddump == undef then
        vederror('NOTHING TO YANK')
    else
        vedinsertstring(vvedworddump)
    endif;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
