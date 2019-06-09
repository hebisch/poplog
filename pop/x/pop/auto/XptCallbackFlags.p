/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCallbackFlags.p
 > Purpose:         PEF flags for Xt callbacks
 > Author:          John Gibson, May 19 1993
 > Documentation:   REF * XT_CALLBACK
 */
compile_mode :pop11 +strict;

include external_flags.ph;

constant macro $-XptCallbackFlags = PEF_RETURN_ABEXIT_NEXT;
