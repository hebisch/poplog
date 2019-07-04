/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.x/x/ui/lib/S-poplog_uiS-guiSubsystem.p
 > Purpose:         Information about subsystems used by top-level
 >                  control panel
 > Author:          Julian Clinton, August 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

section $-poplog_ui;

include pop_uiP.ph;

define subsystem_datavec = newassoc([
        [pop11  {'Pop-11'       ^false}]
        [top    {'Prolog'       '$usepop/pop/plog/src/prolog.p'}]
        [lisp   {'Common Lisp'  '$usepop/pop/lisp/src/clisp.p'}]
        [ml     {'Standard ML'  '$usepop/pop/pml/src/ml.p'}]
    ])
enddefine;

constant panel_ss_ordering = [pop11 top lisp ml];

define panel_ss_table = newassoc([]) enddefine;


;;; gen_ss_label takes the name of a subsystem and returns the
;;; title of the subsystem as a string. It also takes and optional
;;; integer which is the amount of padding to put around the string.
;;; At some point this will be changed to use the strings in lib subsystem.
define gen_ss_label(ss) -> label;
    lvars ss label padding = 0, pad_string;

    if isinteger(ss) then
        ss -> padding;
        -> ss;
    endif;

    SWITCH_SS_CHECK(ss);

    subscrv(SUBSDV_TITLE, subsystem_datavec(ss)) -> label;
    if padding > 0 then
        consstring(repeat padding times ` ` endrepeat,
                    padding) -> pad_string;
        pad_string sys_>< label sys_>< pad_string -> label;
    endif;
enddefine;

constant guiSubsystem = true;

endsection; /* $-poplog_ui */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 28 1993
        Changes for POPC
--- John Gibson, Jan 18 1993
        Added panel_ss_ordering, removed c*urrent_ss_name. Put title
        and loader in a separate property so that panel_ss_table is
        initially empty.
--- John Gibson, Jul 31 1992
        Added current_ss_name
--- Simon Nichols, Dec  3 1991
        Changed top.p to prolog.p in -panel_ss_table-.
--- Jonathan Meyer, Sep  2 1991 Added POP_UI_SUBSYSTEM
--- Integral Solutions Ltd, Aug 28 1991 (Julian Clinton)
    Added def for SWITCH_SS_CHECK.
    Added gen_ss_label for creating subsystem button labels consistently.
 */
