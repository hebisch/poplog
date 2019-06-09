/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCheckProcedure.p
 > Purpose:         Check for procedure
 > Author:          John Gibson, May  2 1993
 > Documentation:   REF *XPT_TYPECHECK
 */
compile_mode :pop11 +strict;

section;

;;; Checks argument is a procedure - 26/07/90
;;; allow through exfunc_closures (which are almost definitley
;;; external procedures), and any appropriately typed ptr
define XptCheckProcedure(item);
    lvars item;
    if isexfunc_closure(item) then
        item;
    else
        XptLiveTypeCheck(item, "Procedure");
    endif;
enddefine;

endsection;
