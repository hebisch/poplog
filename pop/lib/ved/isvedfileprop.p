/*  --- Copyright University of Sussex 1992.  All rights reserved. ---------
 >  File:           C.all/lib/ved/isvedfileprop.p
 >  Purpose:        autoload facility from LIB * NEWVEDFILEPROP
 >  Author:         Unknown, ??? (see revisions)
 >  Documentation:  REF * VEDPROCS
 >  Related Files:  LIB * NEWVEDFILEPROP
 */
compile_mode :pop11 +strict;

section;

constant procedure isvedfileprop;

declare_incremental property isvedfileprop
                    = newproperty([], 16, false, false);

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 28 1992
        Moved in prop definition from newvedfileprop.p
 */
