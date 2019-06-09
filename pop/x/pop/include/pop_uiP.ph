/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/include/pop_uiP.ph
 > Purpose:         Private macro defs for PUI
 > Author:          John Gibson, Jun 28 1993 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POP_UIP_INCLUDED

include define_macexpr.ph;

lconstant macro (
    ;;; subscripts in subsystem_datavec
    SUBSDV_TITLE        = 1,
    SUBSDV_LOADER       = 2,

    ;;; subscripts in panel_ss_table entry
    SUBS_PANEL_BUTTON   = 1,            ;;; control panel button widget
    SUBS_SELECTABLE     = 2,

    ;;; Shell DeleteResponse values for Motif
    Xm_DESTROY          = 0,
    Xm_UNMAP            = 1,
    Xm_DO_NOTHING       = 2,
    );

define :inline lconstant EXIT_ACTION(action);
    dlocal 0 %, if dlocal_context fi_< 3 then
                    action
                endif%;
enddefine;

define :inline lconstant VED_SS_CHECK(n);
    if n == "top" then
        "prolog" -> n;
    elseif n == "ml" then
        "pml" -> n;
    endif
enddefine;

define :inline lconstant SWITCH_SS_CHECK(n);
    if n == "prolog" then
        "top" -> n;
    elseif n == "pml" then
        "ml" -> n;
    endif
enddefine;


weak constant $-popxlink_motif, $-popxlink_openlook;

GEN_VECSUB_MACROS controltool_switch_vec #_< [
    wc_SEPARATOR
    p_CREATE_CONTROL_AREAS
    p_ADD_MENU
    p_ADD_MENU_BUTTON
    p_ADD_SUBSYSTEM_BUTTON
] >_#

GEN_VECSUB_MACROS confirm_switch_vec     #_< [ p_CONFIRM ] >_#
GEN_VECSUB_MACROS filetool_switch_vec    #_< [ p_FILETOOL ] >_#
GEN_VECSUB_MACROS helptool_switch_vec    #_< [ p_HELPTOOL ] >_#
GEN_VECSUB_MACROS information_switch_vec #_< [ p_INFORMATION ] >_#
GEN_VECSUB_MACROS message_switch_vec     #_< [ p_MESSAGE ] >_#

GEN_VECSUB_MACROS prompttool_switch_vec #_< [
    p_PROMPTSOURCE
    p_PROMPTTOOL
] >_#

GEN_VECSUB_MACROS librarytool_switch_vec #_< [
    p_SET_LIBTOOL_FOOTER
    p_LIBRARYTOOL
] >_#

GEN_VECSUB_MACROS projecttool_switch_vec #_< [
    p_PROJECTTOOL
    p_PROJECTTOOL_OP
] >_#


lconstant macro #_THEN = newline;

define :inline lconstant SET_GUI(gui_vec, xm_vec=item, xol_vec=item,
                                                            facility=item);

    weak constant xm_vec, xol_vec;

    #_IF not(DEF POPC_COMPILING) #_THEN
        #_IF DEF popxlink_motif #_THEN
            uses $-poplog_ui$-xm_vec;
        #_ELSEIF DEF popxlink_openlook #_THEN
            uses $-poplog_ui$-xol_vec;
        #_ENDIF
    #_ENDIF

    define lconstant set_gui();
        if testdef popxlink_motif then
            weakref[popxlink_motif] xm_vec
        elseif testdef popxlink_openlook then
            weakref[popxlink_openlook] xol_vec
        else
            mishap(0, facility <> ': SYSTEM NOT LINKED WITH MOTIF OR OPENLOOK');
        endif -> gui_vec
    enddefine;

    #_IF DEF POPC_COMPILING #_THEN
        sys_runtime_apply(set_gui);
    #_ELSE
        set_gui();
    #_ENDIF
enddefine;

lconstant POP_UIP_INCLUDED = true;


/* --- Revision History ---------------------------------------------------
--- Integral Solutions Ltd, Jun 30 1995 (Julian Clinton)
        Added projecttool_switch_vec
--- Robert John Duncan, May  4 1995
        Added fields to controltool_switch_vec
 */
