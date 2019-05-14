/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 >  File:           C.all/lib/auto/mapdic.p
 >  Purpose:        makes a list of arg applied to all words in the dictionary
 >  Author:         Aaron Sloman, Sep 26 1985 (see revisions)
 >  Documentation:  HELP * APPDIC
 >  Related Files:
 */
compile_mode:pop11 +strict;

section;

define global mapdic(pdr);
    lvars procedure pdr;
    [% appdic(pdr)%]
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 26 1985 - lvarsed and sectionised.
 */
