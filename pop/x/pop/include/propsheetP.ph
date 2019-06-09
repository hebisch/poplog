/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/include/propsheetP.ph
 > Purpose:         Private include file for LIB * PROPSHEET
 > Author:          John Gibson, May 17 1993 (see revisions)
 > Documentation:
 */

#_TERMIN_IF DEF PROPSHEETP_INCLUDED

section;

include define_macexpr.ph;
include xpt_coretypes.ph;

GEN_VECSUB_MACROS gui_switch_vec
#_< [

    ;;; resource names
    N_sliderMax
    N_sliderMin
    N_sliderValue
    N_textFieldCallback

    ;;; procedures
    p_CREATE_LABEL
    p_LABEL_STRING
    p_LABEL_WIDTH   ;;; Motif only
    p_TEXTFIELD_VALUE
    p_UPDATE_PREVIEW_ITEM
    p_FONTSTRUCT
    p_CREATE_COMMAND
    p_CREATE_BOOLEAN
    p_CREATE_NUMERIC_RANGE
    p_CREATE_STR_OR_NUM
    p_REBUILD_MENUOF
    p_CREATE_MENUOF
    p_CREATE_LISTOF
    p_CREATE_ONEOF_OR_SOMEOF
    p_CREATE_BOX_BUTTON
    p_PROPSHEET_NEW_BOX
    p_PROPSHEET_NEW
    p_CREATE_ROW_CONTROL
    p_SET_FOCUS_ON
    p_LIST_ITEMS
    p_CURRENT_LIST_ITEM
    p_LIST_ITEMS_SET_NUM_VIS
] >_#;

iconstant macro (
        BOOLEAN             = 1,
        STRING              = 2,
        NUMBER              = 3,
        NUMERIC_RANGE       = 4,
        MENUOF              = 5,
        LISTOF              = 6,
        ONEOF               = 7,
        SOMEOF              = 8,
        MESSAGE             = 9,
        BLANK               = 10,
        COMMAND             = 11,

        COMMAND_BUTTON_PART = 1,  /* command */
        MESSAGE_LABEL_PART  = 1,  /* message */
        TEXT_FIELD_PART     = 1,  /* number, string */
        LABEL_1_PART        = 2,    /* numeric_range */
        SLIDER_PART         = 3,    /* numeric_range */
        LABEL_2_PART        = 4,    /* numeric_range */
        TOGGLE_PART         = 1,    /* boolean */
        INCREMENT_PART      = 2,    /* number */
        DECREMENT_PART      = 3,    /* number */
        UNITS_PART          = 4,    /* number */
        MENU_BUTTON_PART    = 1,  /* menuof */
        PREVIEW_PART        = 2,    /* menuof */
        LIST_PART           = 1,    /* listof, someof, oneof */
        ROWCOL_PART         = 2,    /* listof */
);

define :inline iconstant SUBPART(n, field);
    fast_subscrv(n, Pf_subparts(field))
enddefine;

    ;;; Common toolkit procedures get abbreviated names
iconstant macro (
    fXptVal = [XptVal[fast]],
    CREATE  = "fast_XtCreateManagedWidget",
    ADDCB   = "XtAddCallback",
    DESTROY = "fast_XtDestroyWidget",
);

section $-propsheet;

weak constant procedure (
    Call_formatin, Call_formatout, Make_name,
    Pf_allownone, Pf_subparts, Pf_type, Pb_allowpopdown,
);

endsection;

iconstant PROPSHEETP_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jun 25 1997
        Added p_LABEL_WIDTH (only for Motif)
 */
