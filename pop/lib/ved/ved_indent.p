/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/ved/ved_indent.p
 >  Purpose:        Autoloadable extension to VED (change tab space)
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:
 >  Related Files:
 */

section;

define global ved_indent();
    ;;; change the indentation step
    unless vedargument == nullstring then
        strnumber(vedargument) -> vedindentstep
    endunless;
    vedindentstep
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
