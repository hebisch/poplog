/*  --- University of Sussex POPLOG file -----------------------------------
 >  File:           C.all/lib/auto/newassoc.p
 >  Purpose:        simple form of newproperty
 >  Author:         Steven Hardy, Dec 1981 (see revisions)
 >  Documentation:  HELP * NEWASSOC
 >  Related Files:
 */
compile_mode:pop11 +strict;

section;

define global newassoc(list);
lvars list;
    newproperty(list, 20, false, true)
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 26 1985 - lvarsed and sectionised.
 */
