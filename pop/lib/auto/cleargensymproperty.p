/*  --- Copyright University of Sussex 1993. All rights reserved. ----------
 >  File:           C.all/lib/auto/cleargensymproperty.p
 >  Purpose:        Clear gensym_property
 >  Author:         Mark Rubinstein, Dec  2 1985 (see revisions)
 >  Documentation:  HELP * GENSYM
 >  Related Files:  LIB * GENSYM, *APPGENSYMPROPERTY
 */
compile_mode :pop11 +strict;

section;

define cleargensymproperty();
    clearproperty(gensym_property)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 22 1993
        Made it use gensym_property
 */
