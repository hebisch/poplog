/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/present_in.p
 > Purpose:         Generalised operator version of database_present
 > Author:          John Gibson, Dec 29 1995
 > Documentation:   REF * DATABASE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define 5 item present_in database;
    lvars item;
    dlocal database;
    database_present(item)
enddefine;

endsection;
