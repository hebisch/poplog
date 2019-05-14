/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/is_subsystem_sensitive.p
 > Purpose:         Test whether valof word is subsystem-sensitive
 > Author:          John Gibson, Dec 29 1992
 > Documentation:   REF *SUBSYSTEM
 */
compile_mode :pop11 +strict;

section;

define lconstant Sensitive_apply(prop);
    lvars procedure prop;
    chain(prop(subsystem or "pop11"));
enddefine;

define is_subsystem_sensitive(word);
    lvars val, word;
    valof(word) -> val;
    if isprocedure(val)
    and pdpart(val) == Sensitive_apply then
        frozval(1, val)     ;;; Return property
    else
        false
    endif
enddefine;
;;;
define updaterof is_subsystem_sensitive(bool, word);
    lvars bool, prop, word;
    is_subsystem_sensitive(word) -> prop;
    if isproperty(bool) then
        Sensitive_apply(% bool %) -> valof(word);
        word -> pdprops(valof(word))
    elseif bool then
        unless prop then
            Sensitive_apply(% newproperty([], 16, valof(word), "perm") %)
                -> valof(word);
            word -> pdprops(valof(word))
        endunless
    else
        if prop then
            property_default(prop) -> valof(word)
        endif
    endif
enddefine;

endsection;
