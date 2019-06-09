/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCheckDisplayPtr.p
 > Purpose:         Check for Display Pointer
 > Author:          John Gibson, May  2 1993
 > Documentation:   REF *XPT_TYPECHECK
 */
compile_mode :pop11 +strict;

section;

define XptCheckDisplayPtr = XptLiveTypeCheck(%"DisplayPtr"%); enddefine;

endsection;
