/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/allpresent_in.p
 > Purpose:         Generalised operator version of database_allpresent
 > Author:          John Gibson, Dec 29 1995
 > Documentation:   REF * DATABASE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define 5 itemlist allpresent_in database;
    lvars itemlist;
    dlocal database;
    database_allpresent(itemlist)
enddefine;

endsection;
