/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/database_flush.p
 > Purpose:         Removes all database elements matching its argument
 > Author:          John Gibson, Dec 27 1995
 > Documentation:   REF * DATABASE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define database_flush(item);
    lvars item, dlist = [];

    define lconstant eq_p(listitem, item) -> result;
        lvars listitem, item, result = (listitem = item);
        if result then listitem :: dlist -> dlist endif
    enddefine;

    delete(item, database, eq_p) -> database;
    ncrev(dlist) -> database_them
enddefine;

endsection;
