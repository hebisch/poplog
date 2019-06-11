/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/vedtestsearch.p
 > Author:          Jonathan Meyer, Sep 28 1993
 > Documentation:   REF OBSOLETE
 > Related Files:
 */
#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;

section;

define vedtestsearch(string, bool);
    lvars string, bool;
    ved_try_search(string, bool and [] or [noembed]);
enddefine;

endsection;
