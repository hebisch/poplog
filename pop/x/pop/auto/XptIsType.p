/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptIsType.p
 > Purpose:         Recognizer for Xpop external pointer types
 > Author:          John Gibson, Apr 13 1993
 > Documentation:   REF *XPT_TYPECHECK
 */
compile_mode :pop11 +strict;

section;

define XptIsType(item, type);
    lvars item, type;
    if class_attribute(datakey(item),"external_ptr_props")
    and fast_XptDataType(item) == type
    then
        return(item);
    else
        false
    endif;
enddefine;

endsection;
