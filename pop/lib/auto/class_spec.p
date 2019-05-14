/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/class_spec.p
 > Purpose:         Old version of -class_field_spec-/-class_attribute-
 > Author:          John Gibson, Jan 15 1991
 > Documentation:   REF *KEYS
 */
compile_mode:pop11 +strict;

section;

define global class_spec(key);
    lvars key, spec;
    if isword(key) then
        class_attribute(key)
    else
        if (class_field_spec(key) ->> spec) and not(islist(spec)) then
            ;;; vectorclass -- return old-style bitsize
            field_spec_info(spec) -> (spec, );
        endif;
        spec
    endif
enddefine;

endsection;
