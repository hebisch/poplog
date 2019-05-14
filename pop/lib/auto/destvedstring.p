/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/destvedstring.p
 > Purpose:         Stack vedstring characters
 > Author:          John Gibson, Nov  7 1995
 > Documentation:   REF * STRINGS
 */
compile_mode :pop11 +strict;

section;

define destvedstring(string);
    lvars n, string, len;
    check_string(string);
    datalength(string) -> len;
    fast_for n to len do fast_subscrvedstring(n,string) endfor;
    len
enddefine;

endsection;
