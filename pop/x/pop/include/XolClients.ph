/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/include/XolClients.ph
 > Purpose:         Client Communication with OpenWindows server
 > Author:          Jonathan Meyer, Sep  2 1990 (see revisions)
 > Documentation:   HELP *OpenLook
 */

#_TERMIN_IF DEF XOLCLIENTS_INCLUDED

section;

i_typespec
    WMDecorationHints {
        flags: long,
        menu_type: int,
        pushpin_initial_state: long,
    },
    WMState {
        state: int,
        icon: exptr,
    },
    WMIconSize {
    min_width: int,
    min_height: int,
    max_width: int,
    max_height: int,
    width_inc: int,
    height_inc: int,
    },
    ;;; USE OF OLWinAttr IS NOT ADVISED IN POST 2.0 OLIT
    OLWinAttr {
        win_type: long,
        menu_type: long,
        pin_state: long,
    },

    OLWinColors {
        flags: ulong,
        fg_red: ulong,
        fg_green: ulong,
        fg_blue: ulong,
        bg_red: ulong,
        bg_green: ulong,
        bg_blue: ulong,
        bd_red: ulong,
        bd_green: ulong,
        bd_blue: ulong,
    },

;

iconstant macro (
        MENU_FULL           = 0,
        MENU_LIMITED        = 1,
        MENU_NONE           = 2,

        DELIMITER           = `\n,

        WMDecorationHeader      =(1<<0),        /* has title bar */
        WMDecorationPushpin     =(1<<2),        /* has push pin */
        WMDecorationCloseButton =(1<<3),        /*  has shine mark */
        WMDecorationResizeable  =(1<<5),        /*  has grow corners */

        WMWindowNotBusy     =0,
        WMWindowIsBusy      =1,

        WMPushpinIsOut      =0,
        WMPushpinIsIn       =1,

        MessageHint         = (1<<7),

        ConfigureDenied     = (1<<0),       /* WM_CONFIGURE_DENIED */
        WindowMoved         = (1<<1),       /* WM_WINDOW_MOVED */
        BangMessage         = (1<<2),       /* BANG! */
        FocusMessage        = (1<<3),       /* WM_TAKE_FOCUS */

/*
 * Following for WMState structure
 */

        WithdrawnState      = 0,
        NormalState         = 1,
        IconicState         = 3,



;;;     OLWinAttrNum32s =   (sizeof(OLWinAttr) / sizeof(Atom)),


/*
 * values for _OL_WIN_ATTR flags
 * NOTE: These should be avoided in post 2.0 OLIT
 */

        _OL_WA_WIN_TYPE     = (1<<0),
        _OL_WA_MENU_TYPE    = (1<<1),
        _OL_WA_PIN_STATE    = (1<<2),
        _OL_WA_CANCEL       = (1<<3),

/*
 * values for _OL_WIN_COLORS flags
 */

        _OL_WC_FOREGROUND   = (1<<0),
        _OL_WC_BACKGROUND   = (1<<1),
        _OL_WC_BORDER       = (1<<2),

/*
 * for compatiblity with earlier software
 */

        WM_PUSHPIN_STATE    = "_OL_PIN_STATE",
        WM_WINDOW_BUSY      = "_OL_WIN_BUSY",
        MENU_DISMISS_ONLY   = MENU_NONE,

);

/*
 * Things loaded by XolClients.p
 */

i_typespec
        ;;; don't know the number of arguments for these:
        InitializeOpenLook(...) :void,
        SetWMDecorationHints(...) :void,
        SetWMState(...) :void,
        SetWMWindowBusy(...) :void,
        SendProtocolMessage(...) :void,
        SetWMPushpinState(...) :void,
        GetOLWinAttr(...) :void,
        GetOLWinColors(...) :void,
        EnqueueCharProperty(...) :void,
        GetHelpKeyMessage(...) :int,
        GetCharProperty(...) :exptr.exacc_ntstring,
        GetAtomList(...) :exptr,

        ;;; the following are all unsgined long atoms, but they don't
        ;;; get initialized until the OlInitialize has been called.

        WM_DISMISS          :ulong,
        WM_DECORATION_HINTS :ulong,
        WM_WINDOW_MOVED     :ulong,
        WM_TAKE_FOCUS       :ulong,
        WM_DELETE_WINDOW    :ulong,
        BANG                :ulong,
        WM_SAVE_YOURSELF    :ulong,
        WM_STATE            :ulong,
        WM_CHANGE_STATE     :ulong,
        WM_PROTOCOLS        :ulong,

        _OL_COPY            :ulong,
        _OL_CUT             :ulong,
        _OL_HELP_KEY        :ulong,

        _OL_WIN_ATTR        :ulong,
        _OL_WT_BASE         :ulong,
        _OL_WT_CMD          :ulong,
        _OL_WT_PROP         :ulong,
        _OL_WT_HELP         :ulong,
        _OL_WT_NOTICE       :ulong,
        _OL_WT_OTHER        :ulong,
        _OL_DECOR_ADD       :ulong,
        _OL_DECOR_DEL       :ulong,
        _OL_DECOR_CLOSE     :ulong,
        _OL_DECOR_RESIZE    :ulong,
        _OL_DECOR_HEADER    :ulong,
        _OL_DECOR_PIN       :ulong,
        _OL_WIN_COLORS      :ulong,
        _OL_PIN_STATE       :ulong,
        _OL_WIN_BUSY        :ulong,
        _OL_MENU_FULL       :ulong,
        _OL_MENU_LIMITED    :ulong,
        _OL_NONE            :ulong,
;


iconstant XOLCLIENTS_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        Put typespec and macros in this include file
 */
