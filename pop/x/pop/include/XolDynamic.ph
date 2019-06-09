/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/XolDynamic.ph
 > Purpose:         Dynamic callback for Xol
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */

#_TERMIN_IF DEF XOLDYNAMIC_INCLUDED

include XolConstants.ph;

section;

iconstant macro (
    ;;; ButtonActions
    NOT_DETERMINED  = 0,
    MOUSE_CLICK     = 1,
    MOUSE_MOVE      = 2,
    MOUSE_MULTI_CLICK = 3,


    OL_UNKNOWN_INPUT    = 0,
    OL_UNKNOWN_BTN_INPUT= 1,
    OL_SELECT           = 2,
    OL_ADJUST           = 3,
    OL_MENU             = 4,
    OL_CONSTRAIN        = 5,
    OL_DUPLICATE        = 6,
    OL_PAN              = 7,
    OL_UNKNOWN_KEY_INPUT= 8,
    OL_CUT              = 9,
    OL_COPY             = 10,
    OL_PASTE            = 11,
    OL_HELP             = 12,
    OL_CANCEL           = 13,
    OL_PROP             = 14,
    OL_STOP             = 15,
    OL_UNDO             = 16,
    OL_NEXT_FIELD       = 17,
    OL_PREV_FIELD       = 18,
    OL_CHARFWD          = 19,
    OL_CHARBAK          = 20,
    OL_ROWDOWN          = 21,
    OL_ROWUP            = 22,
    OL_WORDFWD          = 23,
    OL_WORDBAK          = 24,
    OL_LINESTART        = 25,
    OL_LINEEND          = 26,
    OL_DOCSTART         = 27,
    OL_DOCEND           = 28,
    OL_PANESTART        = 29,
    OL_PANEEND          = 30,
    OL_DELCHARFWD       = 31,
    OL_DELCHARBAK       = 32,
    OL_DELWORDFWD       = 33,
    OL_DELWORDBAK       = 34,
    OL_DELLINEFWD       = 35,
    OL_DELLINEBAK       = 36,
    OL_DELLINE          = 37,
    OL_SELCHARFWD       = 38,
    OL_SELCHARBAK       = 39,
    OL_SELWORDFWD       = 40,
    OL_SELWORDBAK       = 41,
    OL_SELLINEFWD       = 42,
    OL_SELLINEBAK       = 43,
    OL_SELLINE          = 44,
    OL_SELFLIPENDS      = 45,
    OL_REDRAW           = 46,
    OL_RETURN           = 47,
    OL_PAGEUP           = 48,
    OL_PAGEDOWN         = 49,
    OL_HOME             = 50,
    OL_END              = 51,
    OL_SCROLLUP         = 52,
    OL_SCROLLDOWN       = 53,
    OL_SCROLLLEFT       = 54,
    OL_SCROLLRIGHT      = 55,
    OL_SCROLLLEFTEDGE   = 56,
    OL_SCROLLRIGHTEDGE  = 57,
    OL_PGM_GOTO         = 58,

);

i_typespec
    OlInputCallData {
        consumed: ShortBool,
        event: exptr,
        keysym: ulong,
        buffer: ntstring_ptr,
        length: exptr.:int,
        ol_event: uint
    },
    OlInputCallDataPointer: exptr.:OlInputCallData,
    OlDynamicResources {
        w: exptr.XptImportWidget,
        base: uint,
        dynamic: exptr,
        num_dynamic: int,
        args: exptr,
        num_args: int,
        CB: exptr.XptImportProcedure,
    },

    OlDynamicColors {
        textForeground: ulong,
        textBackground: ulong,
        inputFocus: ulong,
    },
;

iconstant XOLDYNAMIC_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Andreas Schoter, Jul 15 1991
    Added global constant XolDynamic for compatibility with uses
 */
