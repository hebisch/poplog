/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/include/ved_do_text_action.ph
 > Purpose:         Argument definitions for ved_do_text_action
 > Author:          John Gibson, Oct 19 1995 (see revisions)
 > Documentation:   REF * VEDPROCS
 */

#_TERMIN_IF DEF VED_DO_TEXT_ACTION_INCLUDED

section;

iconstant macro (
    ;;; select argument
    VDTA_TYPE_CHARS     = 2:1e0,    ;;; allow character input (from active)
    VDTA_TYPE_PDR       = 2:1e1,    ;;; allow procedure (from active)
    VDTA_TYPE_COMM      = 2:1e2,    ;;; allow Ved command (from active)
    VDTA_TYPE_A_DOC_REF = 2:1e3,    ;;; allow doc ref with * (from active)
    VDTA_TYPE_DOC_REF   = 2:1e4,    ;;; allow doc ref with *
    VDTA_TYPE_ANY_HELP  = 2:1e5,    ;;; allow help on any word
    VDTA_TYPE_ANY       = -1,

    ;;; mode argument
    VDTA_MODE_DISPLAY   = 0,        ;;; display only
    VDTA_MODE_EXECUTE   = 1,        ;;; execute immediately
    VDTA_MODE_INPUT     = 2,        ;;; put on ved_char_in_stream to execute

    ;;; Bits in result when mode = DISPLAY
    VDTA_DISP_DID_MESS  = 2:1e0,    ;;; a message was displayed
    VDTA_DISP_INVERT    = 2:1e1,    ;;; invert foreground/background colours
    VDTA_DISP_BOX       = 2:1e2,    ;;; draw a box
    VDTA_DISP_POINTER   = 2:1e3,    ;;; change pointer
    );


iconstant VED_DO_TEXT_ACTION_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb 22 1996
        Added VDTA_TYPE_A_DOC_REF
 */
