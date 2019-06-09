/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/XolClients.p
 > Purpose:         Client Communication with OpenWindows server
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */
compile_mode :pop11 +strict;

/* XolClients is a library that is needed for applications that want to
   communicate directly with a server that is running under Open Look.
   It declares atoms and procedures that are needed for that purpose.
*/

/* WARNING: THIS FILE LOADS UNDOCUMENTED PROCEDURES, AND THEREFORE
   CANNOT DETERMINE THE NUMBER OF ARGUMENTS THOSE PROCEDURES TAKE.
*/

section;

exload XolClients [^^XOL_EXLIBS]
    (no reloading)
        InitializeOpenLook,
        SetWMDecorationHints,
        SetWMState,
        SetWMWindowBusy,
        SendProtocolMessage,
        SetWMPushpinState,
        GetOLWinAttr,
        GetOLWinColors,
        EnqueueCharProperty,
        GetHelpKeyMessage,
        GetCharProperty,
        GetAtomList,

        ;;; the following are all unsgined long atoms, but they don't
        ;;; get initialized until the OlInitialize has been called.

        WM_DISMISS,
        WM_DECORATION_HINTS,
        WM_WINDOW_MOVED,
        WM_TAKE_FOCUS,
        WM_DELETE_WINDOW,
        BANG,
        WM_SAVE_YOURSELF,
        WM_STATE,
        WM_CHANGE_STATE,
        WM_PROTOCOLS,

        _OL_COPY,
        _OL_CUT,
        _OL_HELP_KEY,

        _OL_WIN_ATTR,
        _OL_WT_BASE,
        _OL_WT_CMD,
        _OL_WT_PROP,
        _OL_WT_HELP,
        _OL_WT_NOTICE,
        _OL_WT_OTHER,
        _OL_DECOR_ADD,
        _OL_DECOR_DEL,
        _OL_DECOR_CLOSE,
        _OL_DECOR_RESIZE,
        _OL_DECOR_HEADER,
        _OL_DECOR_PIN,
        _OL_WIN_COLORS,
        _OL_PIN_STATE,
        _OL_WIN_BUSY,
        _OL_MENU_FULL,
        _OL_MENU_LIMITED,
        _OL_NONE,

;;;     toplevelDisplay_ptr: exptr <- toplevelDisplay ;;; from OpenLook.h
endexload;

constant XolClients = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        Moved typespecs/macros to include/XolClients.ph
--- Andreas Schoter, Jul 15 1991
    Added global constant XolClients for compatibility with uses
 */
