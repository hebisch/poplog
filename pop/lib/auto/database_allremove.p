/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/database_allremove.p
 > Purpose:         Remove several items from the database
 > Author:          John Gibson, Dec 27 1995
 > Documentation:   REF * DATABASE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define database_allremove(list);
    lvars list, item;
    [%  for item in list do
            database_remove(item);
            database_it
        endfor
    %] -> database_them
enddefine;

endsection;
