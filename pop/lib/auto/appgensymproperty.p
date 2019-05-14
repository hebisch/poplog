/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 >  File:           C.all/lib/auto/appgensymproperty.p
 >  Purpose:        Apply procedure to entries in gensym_property
 >  Author:         Mark Rubinstein, Dec  2 1985 (see revisions)
 >  Documentation:  HELP * GENSYM
 >  Related Files:  LIB * GENSYM, *CLEARGENSYMPROPERTY
 */
compile_mode :pop11 +strict;

section;

define appgensymproperty(pdr);
    lvars pdr;
    appproperty(gensym_property, pdr);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 22 1993
        Made it use gensym_property
 */
