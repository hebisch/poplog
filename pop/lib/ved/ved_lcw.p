/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/ved/ved_lcw.p
 *  Purpose:        CONVERTS NEXT N WORDS TO LOWER CASE (default to 1)
 *  Author:         Aaron Sloman, July 1982 (see revisions)
 *  Documentation:
 *  Related Files:  LIB * VEDCONVERTWORD, * VED_UCW.
 */

section;

define global ved_lcw();
    vedconvertword( isuppercode, uppertolower, vedargument, true)
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
