/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/fast_XtFree.p
 > Purpose:         Free block of storage
 > Author:          John Gibson, Apr 16 1993
 > Documentation:   REF * XT_UTIL
 */
compile_mode :pop11 +strict;

section;

XptLoadProcedures fast_XtFree
lvars XtFree(x) :void;

;;; Free an allocated block of storage - 18/07/90
;;; Input - <XtPointer>
define global fast_XtFree() with_nargs 1;
    exacc raw_XtFree(-> XptCoerceXtPointer());
enddefine;

constant XtFree = fast_XtFree;

endsection;
