/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/subsystem_valof.p
 > Purpose:         Subsystem-sensitive valof
 > Author:          John Gibson, Dec 29 1992
 > Documentation:   REF *SUBSYSTEM
 */
compile_mode :pop11 +strict;

section;

define subsystem_valof(word, ssname);
    lvars ssname, prop, word;
    is_subsystem_sensitive(word) -> prop;
    if ssname then
        check_word(ssname);
        if prop then
            (fast_get_prop_entry(ssname, prop) ->> prop)
                and
             fast_prop_entry_value(prop)
        else
            false
        endif
    else
        ;;; Access default value
        if prop then
            property_default(prop)
        else
            valof(word)
        endif
    endif
enddefine;
;;;
define updaterof subsystem_valof(pdr, word, ssname);
    lvars ssname, pdr, prop, word;
    check_word(ssname);
    is_subsystem_sensitive(word) -> prop;
    if pdr then
        unless prop then
            true -> is_subsystem_sensitive(word);
            is_subsystem_sensitive(word) -> prop
        endunless;
        pdr -> prop(ssname)
    else
        if prop then
            fast_kill_prop_entry(ssname, prop) ->
        endif
    endif
enddefine;

endsection;
