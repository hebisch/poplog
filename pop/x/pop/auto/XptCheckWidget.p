/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCheckWidget.p
 > Purpose:         Check for Widget
 > Author:          John Gibson, Apr 13 1993
 */
compile_mode :pop11 +strict;

section;

    ;;; Keep this separate from xpt_typecheck.p (things like XptVal
    ;;; use it, and shouldn't bring in all the procedures in xpt_typecheck)
define XptCheckWidget = XptLiveTypeCheck(%"Widget"%); enddefine;

endsection;
