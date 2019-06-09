/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptTypeCheck.p
 > Purpose:         Check for Xpop external pointer types
 > Author:          John Gibson, Apr 13 1993
 > Documentation:   REF *XPT_TYPECHECK
 */
compile_mode :pop11 +strict;

section;

define XptTypeCheck(item, type);
    lvars item, type;
    XptIsType(item, type) or mishap(item, 1, type sys_>< ' NEEDED');
enddefine;

endsection;
