/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-guiActions.p
 > Purpose:         allow callbacks to do POP-11 and VED actions
 > Author:          Jon Meyer, Julian Clinton, May 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

section $-poplog_ui;

define call_action(action);
    lvars action;

    apply(action);
enddefine;

    ;;; run a Ved command or procedure
define call_ved(action);
    lvars action;
    vedsetup();
    if isstring(action) then
        veddo(% action, true %) -> action;
    endif;
    if vedinvedprocess or vedusewindows == "x" then
        vedinput(action);
    else
        ;;; not XVed? Just do it now, and hope for the best
        action();
    endif;
enddefine;

constant guiActions = true;
endsection;     /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Apr 27 1995
        Combined all the different Ved procedures into a single call_ved
--- John Gibson, Jun 28 1993
        Changes for POPC
--- Integral Solutions Ltd, Jun  2 1992 (Julian Clinton)
    Calls to -veddo- now put the command on the VED command line.
Julian Clinton, 11/10/91
    Replaced the calls to XptSetXtWakeup.
--- Jonathan Meyer, Sep  3 1991 stopped veddo_action applying action twice
--- Jonathan Meyer, Aug 29 1991
    vedinput does XptSetXtWakeup itself, so the calls to XptSetXtWakeup
    in veddo_action and vedcall_action were removed.
Julian Clinton, 19/6/91
    Changed to apply actions rather than put timers on them.
    Now uses vedinput rather than adding directly to ved_char_in_stream.
 */
