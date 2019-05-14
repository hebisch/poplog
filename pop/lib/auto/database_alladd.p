/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/database_alladd.p
 > Purpose:         Add a list of items to the database
 > Author:          John Gibson, Dec 28 1995
 > Documentation:   REF * DATABASE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define database_alladd(list);
    lvars item;
    [%  for item in list do
            database_add(item);
            item
        endfor
    %] -> database_them
enddefine;

endsection;
