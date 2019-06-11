/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 *  File:           C.all/lib/ved/ved_fp.p
 *  Purpose:        Fill current paragraph, even if its a program file.
 *  Author:         Aaron Sloman, 1982 (see revisions)
 *  Documentation:
 *  Related Files:
 */

section;

define global ved_fp();
    dlocal vedcompileable = false, vedcurrent = '';
    ;;; temporary, till ved_jp is revised
    ved_jp()
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- John Williams, May 30 1995 - dlocal instead of vars.
--- Mark Rubinstein, Oct  3 1985 - sectionised.
 */
