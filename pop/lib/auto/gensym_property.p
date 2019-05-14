/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/gensym_property.p
 > Purpose:         Variable property used by gensym
 > Author:          John Gibson, Nov 25 1992
 > Documentation:   REF *gensym_property
 > Related Files:   LIB *GENSYM
 */
compile_mode:pop11 +strict;

section;

define vars gensym_property =
    newproperty([], 16, 1, true)
enddefine;

endsection;
