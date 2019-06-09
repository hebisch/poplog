/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptLiveTypeCheck.p
 > Purpose:         Check for live Xpop external pointer types
 > Author:          John Gibson, Apr 13 1993
 > Documentation:   REF *XPT_TYPECHECK
 */
compile_mode :pop11 +strict;

section;

define XptLiveTypeCheck(item, type);
    lvars item, type;
    XptIsLiveType(item, type)
            or mishap(item,1, '(LIVE) ' sys_>< type sys_>< ' NEEDED');
enddefine;

endsection;
