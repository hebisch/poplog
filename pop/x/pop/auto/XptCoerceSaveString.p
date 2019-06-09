/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCoerceSaveString.p
 > Purpose:         Coerce string and hold onto it
 > Author:          John Gibson, Nov  1 1991
 > Documentation:   REF *XT_LIBS
 */
compile_mode:pop11 +strict;

section;

;;; Variant of XptCoerceString which keeps hold of the
;;; coerced versions (so they don't become garbage accidentally!)

;;; property to hold on to coerced versions of string
lconstant String_cache = newproperty([],32,false,false);

define global XptCoerceSaveString();
    XptCoerceString();
enddefine;

define updaterof XptCoerceSaveString(s);
    lvars s;
    String_cache(s) or ( -> XptCoerceString(s) ->> String_cache(s));
enddefine;

endsection;
