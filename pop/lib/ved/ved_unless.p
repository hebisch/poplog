/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/ved/ved_unless.p
 *  Purpose:        insert templay for unless conditional
 *  Author:         Unknown, ??? (see revisions)
 *  Documentation:
 *  Related Files:  LIB * VED_DEFINE, *VED_IF etc.
 */

section;

define global ved_unless();
    vedlinebelow();
    vedpositionpush();
    vedinsertstring('\n\tunless CONDITION\n\tthen ACTION\n\tendunless;');
    vedpositionpop();
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Oct  4 1985 - sectionised.
 */
