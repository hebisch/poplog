/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/wved_vedset_key_chars.p
 > Purpose:         Called (at run-time) by vedset keys for <expr> = ( <code> )
 >                  and redefined by XVed.
 > Author:          John Gibson, Nov 15 1992
 > Documentation:
 */
compile_mode :pop11 +strict;

section;

define vars wved_vedset_key_chars(string);
    lvars string;
    mishap('(' <> string <> ')', 1,
            'vedset keys: SEQUENCE (...) IS NOT DEFINED FOR THIS TERMINAL')
enddefine;

endsection;
