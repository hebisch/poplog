/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptDeferApply.p
 > Purpose:         Old name for external_defer_apply
 > Author:          John Gibson, Apr 13 1994
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

define XptDeferApply(/*p*/) with_nargs 1;
    external_defer_apply()
enddefine;

endsection;
