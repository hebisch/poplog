/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/vedcllocate.p
 > Purpose:         backwards caseless searching with checking
 > Author:          Jonathan Meyer, Sep 29 1993
 > Documentation:   REF OBSOLETE
 > Related Files:
 */
#_TERMIN_IF DEF POPC_COMPILING

compile_mode :pop11 +strict;

section;

define vedcllocate(flag);
    lvars flag, string;

    /* allow compatible arguments with vedlocate */
    if isboolean(flag) then
            -> string;
    elseif  isstring(flag) then
        flag -> string; true -> flag
    else
        flag sys_>< nullstring -> string; false -> flag
    endif;

    /* get string to search for if not given.  put on status line if needed. */
    if string == nullstring then
        lvars (l,,,,) = ved_query_last_search();
        if l then
            l -> string;
            vedputcommand(vedcommand <> ' ' <> l);
        else
            vederror('nothing to search for')
        endif
    endif;

    ved_check_search(string, flag and [nocase] or [noembed nocase]);
enddefine;

endsection;
