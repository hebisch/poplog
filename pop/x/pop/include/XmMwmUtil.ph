/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XmMwmUtil.ph
 > Purpose:         General window manager utils
 > Author:          Jonathan Meyer, Feb  9 1991 (see revisions)
 > Documentation:   HELP *MOTIF
 */

#_TERMIN_IF DEF XMMWMUTIL_INCLUDED

section;

/*
 * Contents of the _MWM_HINTS property.
 */

i_typespec
    MotifWmHints {
        flags: long,
        functions: long,
        decorations: long,
        input_mode: int,
    },

    MwmHints: MotifWmHints,

    MotifWmInfo {
        flags: long,
        wm_window: ulong,  ;;;XptWindow,
    },

    MwmInfo: MotifWmInfo,

    PropMotifWmHints {
        flags: uint,
        functions: uint,
        decorations: uint,
        inputMode: int,
    },
    PropMwmHints: PropMotifWmHints,

    PropMotifWmInfo {
        flags: uint,
        wmWindow: uint,
    },
    PropMwmInfo: PropMotifWmInfo

;

iconstant macro (
    /* bit definitions for MwmHints.flags */
    MWM_HINTS_FUNCTIONS     = 2:1e0,
    MWM_HINTS_DECORATIONS   = 2:1e1,
    MWM_HINTS_INPUT_MODE    = 2:1e2,

    /* bit definitions for MwmHints.functions */
    MWM_FUNC_ALL        = 2:1e0,
    MWM_FUNC_RESIZE     = 2:1e1,
    MWM_FUNC_MOVE       = 2:1e2,
    MWM_FUNC_MINIMIZE   = 2:1e3,
    MWM_FUNC_MAXIMIZE   = 2:1e4,
    MWM_FUNC_CLOSE      = 2:1e5,

    /* bit definitions for MwmHints.decorations */
    MWM_DECOR_ALL       = 2:1e0,
    MWM_DECOR_BORDER    = 2:1e1,
    MWM_DECOR_RESIZEH   = 2:1e2,
    MWM_DECOR_TITLE     = 2:1e3,
    MWM_DECOR_MENU      = 2:1e4,
    MWM_DECOR_MINIMIZE  = 2:1e5,
    MWM_DECOR_MAXIMIZE  = 2:1e6,

    /* values for MwmHints.input_mode */
    MWM_INPUT_MODELESS                  = 0,
    MWM_INPUT_PRIMARY_APPLICATION_MODAL = 1,
    MWM_INPUT_SYSTEM_MODAL              = 2,
    MWM_INPUT_FULL_APPLICATION_MODAL    = 3,

    /*
     * The following is for compatibility only. It use is deprecated.
     */
    MWM_INPUT_APPLICATION_MODAL = MWM_INPUT_PRIMARY_APPLICATION_MODAL,


    /* bit definitions for MotifWmInfo .flags */
    MWM_INFO_STARTUP_STANDARD   = 2:1e0,
    MWM_INFO_STARTUP_CUSTOM     = 2:1e1,

    /* number of elements of size 32 in _MWM_HINTS */
    PROP_MOTIF_WM_HINTS_ELEMENTS    = 4,
    PROP_MWM_HINTS_ELEMENTS         = PROP_MOTIF_WM_HINTS_ELEMENTS,

    /* atom name for _MWM_HINTS property */
    _XA_MOTIF_WM_HINTS  = '_MOTIF_WM_HINTS',
    _XA_MWM_HINTS       = _XA_MOTIF_WM_HINTS,

    /*
     * Definitions for the _MWM_MESSAGES property.
     */

    _XA_MOTIF_WM_MESSAGES   = '_MOTIF_WM_MESSAGES',
    _XA_MWM_MESSAGES        = _XA_MOTIF_WM_MESSAGES,

    /* atom that enables client frame offset messages */
    _XA_MOTIF_WM_OFFSET =   '_MOTIF_WM_OFFSET',

    /*
     * Definitions for the _MWM_MENU property.
     */

    /* atom name for _MWM_MENU property */
    _XA_MOTIF_WM_MENU   = '_MOTIF_WM_MENU',
    _XA_MWM_MENU        = _XA_MOTIF_WM_MENU,


    /*
     * Definitions for the _MWM_INFO property.
     */


    /* number of elements of size 32 in _MWM_INFO */
    PROP_MOTIF_WM_INFO_ELEMENTS = 2,
    PROP_MWM_INFO_ELEMENTS      = PROP_MOTIF_WM_INFO_ELEMENTS,

    /* atom name for _MWM_INFO property */
    _XA_MOTIF_WM_INFO   = '_MOTIF_WM_INFO',
    _XA_MWM_INFO        = _XA_MOTIF_WM_INFO,


    /* atom for motif input bindings */
    _XA_MOTIF_BINDINGS  = '_MOTIF_BINDINGS',

);

iconstant XMMWMUTIL_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 15 1993
        Made an include file
--- Andreas Schoter, Jul 15 1991
    Added global constant XmMwmUtil for compatibility with uses
 */
