/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/ui/lib/pop_ui_prompttool.p
 > Purpose:         Prompt the user
 > Author:          Jonathan Meyer, Sep  5 1991 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

uses-now popxlib;

section $-poplog_ui =>  pop_ui_prompttool, pop_ui_promptsource,
                        pop_ui_promptwarpmouse;

exload_batch;

include pop_uiP.ph

vars
    pop_ui_promptwarpmouse = true,
;

lvars prompttool_switch_vec;


/* ======== User Prompts ============================================= */

define pop_ui_prompttool(/*title, type, message, aligned, labels, def*/);
    p_PROMPTTOOL()
enddefine;

define vars active pop_ui_promptsource;
    p_PROMPTSOURCE()
enddefine;
;;;
define updaterof active pop_ui_promptsource(/*widget*/);
    -> p_PROMPTSOURCE()
enddefine;

SET_GUI(prompttool_switch_vec, prompttool_xm, prompttool_xol,
                                                    'pop_ui_prompttool');

endexload_batch;
endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Apr 16 1993
        Uses Xm/xmMessageBoxWidget instead of XptW*idgetSet etc.
        prompt_dlgs no longer needs idents of procedures since they're
        now constants.
--- John Gibson, Apr  9 1993
        Uses Xol/xol*Widget and xt widgetclass libraries instead of
        XptW*idgetSet.
--- John Gibson, Sep 10 1992
        Changed to use XptVal
--- John Gibson, Aug 17 1992
        Added XptDefaultSetup() in Motif version of Create_prompt
--- Integral Solutions Ltd, Oct 22 1991 (Julian Clinton)
    Changed >< to sys_><.
Julian Clinton, 14/10/91
    Removed explicit string null terminators.
--- Jonathan Meyer, Sep 16 1991
        Made pop_ui_promptsource create prompt if necessary. Added call of
        XtCallAcceptFocus for OLIT to make window take input focus.
--- Jonathan Meyer, Sep 11 1991 Now returns result as both an index and
        an item. Replaced syshibernate with XtAppProcessEvent.
        Extra arg to allow you to specify default as an index
 */
