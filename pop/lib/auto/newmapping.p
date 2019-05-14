/* --- Copyright University of Sussex 1989.  All rights reserved. ---------
 > File:           C.all/lib/auto/newmapping.p
 > Purpose:        Create table mapping one structure to another
 > Author:         John Williams, Oct 31 1986 (see revisions)
 > Documentation:  HELP * NEWMAPPING
 > Related Files:
 */
compile_mode:pop11 +strict;

section;

define global newmapping(list, size, default, expand);
    newanyproperty(list, size,
                   if expand then
                       1, size
                   else
                       false, false
                   endif,
                   syshash, nonop =, "perm",
                   default, false)
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jul  3 1989
        Added +strict
 */
