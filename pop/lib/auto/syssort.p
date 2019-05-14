/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/auto/syssort.p
 > Purpose:         Merge sort for lists
 > Author:          Jonathan Cunningham, May 1981 (see revisions)
 > Documentation:   HELP * SYSSORT
 > Related Files:   LIB * NC_LISTSORT, LIB * SORT
 */

compile_mode:pop11 +strict;

section;

define syssort(list, before_p);
    lvars list, procedure before_p;
    if isboolean(list) then
        if list then
            /* Copy */
            copylist()
        else
            /* Don't copy, but do expand dynamic lists */
            expandlist()
        endif
    else
        copylist(list)
    endif -> list;
    nc_listsort(list, before_p);
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jul 26 1993
        Split into this file and LIB * NC_LISTSORT
 */
