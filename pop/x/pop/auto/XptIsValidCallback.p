/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptIsValidCallback.p
 > Purpose:         Check valid arg for callback procedure
 > Author:          John Gibson, Nov  1 1991
 > Documentation:   REF *XT_LIBS
 */
compile_mode :pop11 +strict;

section;

;;; checking what kinds of things can be pop callback procedures
;;; put this here rather than typecheck because its used by fast libraries
;;; which don't want all the checking stuff
define global XptIsValidCallback(proc);
    lvars proc;
    isprocedure(proc) or isident(proc) or isword(proc)
enddefine;

endsection;
