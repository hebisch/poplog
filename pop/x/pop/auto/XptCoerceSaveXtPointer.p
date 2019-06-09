/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCoerceSaveXtPointer.p
 > Purpose:         Conversion procedure for XptPointer
 > Author:          John Gibson, Nov  1 1991
 > Documentation:   REF *XT_LIBS
 */
compile_mode:pop11 +strict;

section;

define global XptCoerceSaveXtPointer();
    XptCoerceXtPointer();
enddefine;

define updaterof XptCoerceSaveXtPointer(obj);
    lvars obj;
    if isstring(obj) then
        -> XptCoerceSaveString(obj) -> obj;
    endif;
    -> XptCoerceXtPointer(obj);
enddefine;

endsection;
