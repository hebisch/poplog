/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptImportCacheRef.p
 > Purpose:         Conversion procedure for XptCacheRef typespec
 > Author:          John Gibson, Nov  3 1991
 > Documentation:
 > Related Files:
 */
compile_mode:pop11 +strict;

section;

include xpt_constants.ph;   ;;; Load constants in

define global XptImportCacheRef = XptImportAny(%XDT_CACHEREF%); enddefine;

endsection;
