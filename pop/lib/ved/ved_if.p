/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/ved/ved_if.p
 *  Purpose:        insert "if" conditional template
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:
 *  Related Files:
 */

section;

define global ved_if();
    vedlinebelow();
    vedpositionpush();
    vedinsertstring('\tif CONDITION\n\tthen ACTION\n\telseif CONDITION\n\tthen ACTION\n\telse DEFAULT\n\tendif;');
    vedpositionpop()
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
