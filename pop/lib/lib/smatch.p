/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 >  File:           C.all/lib/lib/smatch.p
 >  Purpose:        for using matches inside sections.
 >  Author:         Jonathan Cunningham, Nov 1983 (see revisions)
 >  Documentation:  HELP * SMATCH
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

define global macro !(word);
    dl([("% word_identifier(word,current_section,true) %")])
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jul 27 1995
        Added #_TERMIN_IF
--- Mark Rubinstein, Oct  3 1985 - sectionised and lvarsed.
 */
