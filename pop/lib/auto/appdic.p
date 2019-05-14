/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/auto/appdic.p
 > Purpose:         Apply procedure to dictionary words in safe mode
 > Author:          John Gibson, Jan 12 1989
 > Documentation:   REF * WORDS
 */
compile_mode:pop11 +strict;

section;

define global appdic(app_p);
    lvars app_p;
    appdata({% fast_appdic(identfn) %}, app_p)
enddefine;

endsection;
