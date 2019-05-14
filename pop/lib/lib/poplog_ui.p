/* --- Copyright University of Sussex 1995.  All rights reserved. ---------
 > File:            C.all/lib/lib/poplog_ui.p
 > Purpose:         Poplog Graphical User Interface loader
 > Author:          Julian Clinton, July 1991 (see revisions)
 > Documentation:
 > Related Files:
*/
compile_mode :pop11 +strict;

section;

#_IF DEF popxlink_motif or DEF popxlink_openlook or DEF POPC_COMPILING

uses-now pop_ui;

#_IF DEF POPC_COMPILING
    #_INCLUDE '$usepop/pop/x/ui/lib/popc_declare.ph'
#_ELSE
    max(popmemlim, 5e5) -> popmemlim;
#_ENDIF

uses pop_ui_popcontroltool;

vars pop_ui_setup_done = false;

define vars pop_ui_setup();
    returnunless(poplog_ui_enabled);
    unless pop_ui_setup_done then
        pop_ui_popcontroltool(false, false);
        true -> pop_ui_setup_done;      ;;; prevent re-start unless flag
                                        ;;; is reset
    endunless;
enddefine;

;;; Make it come at the end for POPC
declare_incremental procedure [prec=200] sysxsetup;

sysxsetup <> pop_ui_setup -> sysxsetup;

constant poplog_ui = true;       ;;; for uses

#_IF DEF XptDefaultAppContext
    pop_ui_setup();
#_ENDIF;

#_ELSE

warning(0, 'LIB POPLOG_UI only available for OpenWindows or Motif');

#_ENDIF

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 22 1995
        Added assignment to popmemlim
--- John Gibson, Dec 15 1993
        Changed args to pop_ui_popcontroltool to be false instead of 0
--- John Williams, Jul 30 1992
        Prints warning message if not loaded with OpenWindows or Motif
--- John Gibson, Jul 23 1992
        Made pop_ui_setup do nothing unless poplog_ui_enabled is true.
--- Integral Solutions Ltd, Jun  2 1992 (Julian Clinton)
        Now only loads if the POPLOG has been linked against OPEN LOOK or
        Motif. Defines constant poplog_ui for "uses".
--- Jonathan Meyer, Sep  5 1991
        Now starts up if XptDefaultAppContext is set
--- Julian Clinton, 2/8/91
        Added pop_ui_setup_done variable signifies whether pop_ui_setup has
        been called. Renamed startup_gui to pop_ui_setup and made a global
        vars procedure.
 */
