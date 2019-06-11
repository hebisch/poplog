/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/vedtestclsearch.p
 > Purpose:         backwards caseless searching
 > Author:          Jonathan Meyer, Sep 29 1993
 > Documentation:   REF OBSOLETE
 > Related Files:
 */
#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;

section;

define vedtestclsearch(str, embedded);
    lvars str, embedded;

    dlocal ved_search_state, vedline, vedcolumn;

    if ved_try_search(str, embedded and [nocase] or [nocase noembed]) then
        {% vedline, vedcolumn %}
    else
        false
    endif;
enddefine;

endsection;
