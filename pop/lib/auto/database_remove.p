/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/database_remove.p
 > Purpose:         Remove an item from the database
 > Author:          John Gibson, Dec 27 1995
 > Documentation:   REF * DATABASE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define database_remove(item);
    lvars item, save_db = database;

    define lconstant eq_p(listitem, item) -> result;
        lvars listitem, item, result = (listitem = item);
        if result then listitem -> database_it endif
    enddefine;

    if (delete(item, database, eq_p, 1) ->> database) == save_db then
        mishap(item, 1, 'REMOVING NON-EXISTENT ITEM FROM DATABASE')
    endif
enddefine;

endsection;
