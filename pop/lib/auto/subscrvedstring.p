/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/subscrvedstring.p
 > Purpose:         Checking version of fast_subscrvedstring
 > Author:          John Gibson, Nov  7 1995
 > Documentation:   REF * STRINGS
 */
compile_mode :pop11 +strict;

section;

define subscrvedstring(subs, string);
    lvars subs, string;
    subscrs(subs,string) -> ;       ;;; check int subscript and (d)string
    fast_subscrvedstring(subs,string)
enddefine;
;;;
define updaterof subscrvedstring(vchar, subs, string);
    lvars vchar, subs, string;
    subscrs(subs,string) -> ;       ;;; check int subscript and (d)string
    vchar -> fast_subscrvedstring(subs,string)
enddefine;

endsection;
