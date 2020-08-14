/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_xevent.ph
 > Purpose:         typespec definitions of X Event structures
 > Author:          Jonathan Meyer, Nov 22 1990 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF XPT_XEVENT_INCLUDED

section;

include xpt_coretypes.ph
include xpt_xtypes.ph

iconstant macro (

    /* used by MappingNotify events */
    MappingModifier     = 0,
    MappingKeyboard     = 1,
    MappingPointer      = 2,

    PointerWindow   = 0, /* destination window in SendEvent */
    InputFocus      = 1, /* destination window in SendEvent */

    PointerRoot     = 1, /* focus window in SetInputFocus */


    /*****************************************************************
     * EVENT DEFINITIONS
     *****************************************************************/

    /* Input Event Masks. Used as event-mask window attribute and as arguments
       to Grab requests.  Not to be confused with event names.  */

    NoEventMask                 = 0,
    KeyPressMask                = 2:1e0,
    KeyReleaseMask              = 2:1e1,
    ButtonPressMask             = 2:1e2,
    ButtonReleaseMask           = 2:1e3,
    EnterWindowMask             = 2:1e4,
    LeaveWindowMask             = 2:1e5,
    PointerMotionMask           = 2:1e6,
    PointerMotionHintMask       = 2:1e7,
    Button1MotionMask           = 2:1e8,
    Button2MotionMask           = 2:1e9,
    Button3MotionMask           = 2:1e10,
    Button4MotionMask           = 2:1e11,
    Button5MotionMask           = 2:1e12,
    ButtonMotionMask            = 2:1e13,
    KeymapStateMask             = 2:1e14,
    ExposureMask                = 2:1e15,
    VisibilityChangeMask        = 2:1e16,
    StructureNotifyMask         = 2:1e17,
    ResizeRedirectMask          = 2:1e18,
    SubstructureNotifyMask      = 2:1e19,
    SubstructureRedirectMask    = 2:1e20,
    FocusChangeMask             = 2:1e21,
    PropertyChangeMask          = 2:1e22,
    ColormapChangeMask          = 2:1e23,
    OwnerGrabButtonMask         = 2:1e24,

    /* Event names.  Used in "type" field in XEvent structures.  Not to be
    confused with event masks above.  They start from 2 because 0 and 1
    are reserved in the protocol for errors and replies. */

    KeyPress            = 2,
    KeyRelease          = 3,
    ButtonPress         = 4,
    ButtonRelease       = 5,
    MotionNotify        = 6,
    EnterNotify         = 7,
    LeaveNotify         = 8,
    FocusIn             = 9,
    FocusOut            = 10,
    KeymapNotify        = 11,
    Expose              = 12,
    GraphicsExpose      = 13,
    NoExpose            = 14,
    VisibilityNotify    = 15,
    CreateNotify        = 16,
    DestroyNotify       = 17,
    UnmapNotify         = 18,
    MapNotify           = 19,
    MapRequest          = 20,
    ReparentNotify      = 21,
    ConfigureNotify     = 22,
    ConfigureRequest    = 23,
    GravityNotify       = 24,
    ResizeRequest       = 25,
    CirculateNotify     = 26,
    CirculateRequest    = 27,
    PropertyNotify      = 28,
    SelectionClear      = 29,
    SelectionRequest    = 30,
    SelectionNotify     = 31,
    ColormapNotify      = 32,
    ClientMessage       = 33,
    MappingNotify       = 34,
    LASTEvent           = 35,   /* must be bigger than any event # */


    /* Notify modes */

    NotifyNormal        = 0,
    NotifyGrab          = 1,
    NotifyUngrab        = 2,
    NotifyWhileGrabbed  = 3,

    NotifyHint      = 1,   /* for MotionNotify events */

    /* Notify detail */

    NotifyAncestor          = 0,
    NotifyVirtual           = 1,
    NotifyInferior          = 2,
    NotifyNonlinear         = 3,
    NotifyNonlinearVirtual  = 4,
    NotifyPointer           = 5,
    NotifyPointerRoot       = 6,
    NotifyDetailNone        = 7,

    /* Visibility notify */

    VisibilityUnobscured        = 0,
    VisibilityPartiallyObscured = 1,
    VisibilityFullyObscured     = 2,

    /* Circulation request */

    PlaceOnTop      = 0,
    PlaceOnBottom   = 1,

    /* protocol families */

    FamilyInternet      = 0,
    FamilyDECnet        = 1,
    FamilyChaos         = 2,

    /* Property notification */

    PropertyNewValue    = 0,
    PropertyDelete      = 1,

    /* Color Map notification */

    ColormapUninstalled = 0,
    ColormapInstalled   = 1,


    /* AllowEvents modes */

    AsyncPointer        = 0,
    SyncPointer         = 1,
    ReplayPointer       = 2,
    AsyncKeyboard       = 3,
    SyncKeyboard        = 4,
    ReplayKeyboard      = 5,
    AsyncBoth           = 6,
    SyncBoth            = 7,

    /* Used in SetInputFocus, GetInputFocus */

    RevertToNone        = 0,
    RevertToPointerRoot = PointerRoot,
    RevertToParent      = 2,

    /* Modifier definitions (see X.h) */

    ShiftMask           = 2:1e0,
    LockMask            = 2:1e1,
    ControlMask         = 2:1e2,
    Mod1Mask            = 2:1e3,
    Mod2Mask            = 2:1e4,
    Mod3Mask            = 2:1e5,
    Mod4Mask            = 2:1e6,
    Mod5Mask            = 2:1e7,

    Button1Mask         = 2:1e8,
    Button2Mask         = 2:1e9,
    Button3Mask         = 2:1e10,
    Button4Mask         = 2:1e11,
    Button5Mask         = 2:1e12,

    AnyModifier         = 2:1e15,

    StandardMask        = 2:11,
);

i_typespec
    XBool: int#XptCoerceBoolean,

    XKeyEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        root: XptWindow,
        subwindow: XptWindow,
        time: long,
        x: int,
        y: int,
        x_root: int,
        y_root: int,
        state: uint,
        keycode: uint,
        same_screen: XBool,
    },
    XKeyPressedEvent: XKeyEvent,
    XKeyReleasedEvent: XKeyEvent,

    XButtonEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        root: XptWindow,
        subwindow: XptWindow,
        time: long,
        x: int,
        y: int,
        x_root: int,
        y_root: int,
        state: uint,
        button: int,
        same_screen: XBool,
    },
    XButtonPressedEvent: XButtonEvent,
    XButtonReleasedEvent: XButtonEvent,

    XMotionEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        root: XptWindow,
        subwindow: XptWindow,
        time: long,
        x: int,
        y: int,
        x_root: int,
        y_root: int,
        state: uint,
        is_hint: byte,
        same_screen: XBool,
    },
    XPointerMovedEvent: XMotionEvent,

    XCrossingEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        root: XptWindow,
        subwindow: XptWindow,
        time: long,
        x: int,
        y: int,
        x_root: int,
        y_root: int,
        mode: int,
        detail: int,
        same_screen: XBool,
        focus: XBool,
        state: uint,
    },
    XEnterWindowEvent: XCrossingEvent,
    XLeaveWindowEvent: XCrossingEvent,

    XFocusChangeEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        mode: int,
        detail: int,
    },
    XFocusInEvent: XFocusChangeEvent,
    XFocusOutEvent: XFocusChangeEvent,

    XKeymapEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        key_vector: byte[32],
    },

    XExposeEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        x: int,
        y: int,
        width: int,
        height: int,
        count: int,
    },

    XGraphicsExposeEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        x: int,
        y: int,
        width: int,
        height: int,
        count: int,
        major_code: int,
        minor_code: int,
    },

    XNoExposeEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        drawable: XptXID,
        major_code: int,
        minor_code: int,
    },

    XVisibilityEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        state: int,
    },

    XCreateWindowEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        x: int,
        y: int,
        width: int,
        height: int,
        border_width: int,
        override_redirect: XBool,
    },

    XDestroyWindowEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        event: XptWindow,
        window: XptWindow,
    },

    XUnmapEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        event: XptWindow,
        window: XptWindow,
        from_configure: XBool,
    },

    XMapEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        event: XptWindow,
        window: XptWindow,
        override_redirect: XBool,
    },

    XMapRequestEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        parent: XptWindow,
        window: XptWindow,
    },

    XReparentEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        event: XptWindow,
        parent: XptWindow,
        window: XptWindow,
        x: int,
        y: int,
        override_redirect: XBool,
    },

    XConfigureEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        event: XptWindow,
        window: XptWindow,
        x: int,
        y: int,
        width: int,
        height: int,
        border_width: int,
        above: XptWindow,
        override_redirect: XBool,
    },

    XGravityEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        event: XptWindow,
        window: XptWindow,
        x: int,
        y: int,
    },

    XResizeRequestEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        width: int,
        height: int,
    },

    XConfigureRequestEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        parent: XptWindow,
        window: XptWindow,
        x: int,
        y: int,
        width: int,
        height: int,
        border_width: int,
        above: XptWindow,
        detail: int,
        value_mask: ulong,
    },

    XCirculateEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        event: XptWindow,
        window: XptWindow,
        place: int,
    },

    XCirculateRequestEvent: XCirculateEvent,

    XPropertyEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        atom: ulong,
        time: ulong,
        state: int,
    },

    XSelectionClearEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        selection: ulong,
        time: ulong,
    },

    XSelectionRequestEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        owner: XptWindow,
        requester: XptWindow,
        selection: ulong,
        target: ulong,
        property: ulong,
        time: ulong,
    },

    XSelectionEvent: XSelectionRequestEvent,

    XColormapEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        colormap: XptXID,
        new: XBool,
        state: int,
    },

    XClientMessageEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        message_type: ulong,
        format: int,
        data: long[5],
    },

    XMappingEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
        request: int,
        first_keycode: int,
        count: int,
    },

    XErrorEvent {
        type: int,
        display: XptDisplayPtr,
        resourceid: XptXID,
        serial: ulong,
        error_code: byte,
        request_code: byte,
        minor_code: byte,
    },

    XAnyEvent {
        type: int,
        serial: ulong,
        send_event: XBool,
        display: XptDisplayPtr,
        window: XptWindow,
    },
    exptr_arr: exptr[],

    XEvent {
        type: int |
        xany: XAnyEvent |
        xkey: XKeyEvent |
        xbutton: XButtonEvent |
        xmotion: XMotionEvent |
        xcrossing: XCrossingEvent |
        xfocus: XFocusChangeEvent |
        xexpose: XExposeEvent |
        xgraphicsexpose: XGraphicsExposeEvent |
        xnoexpose: XNoExposeEvent |
        xvisibility: XVisibilityEvent |
        xcreatewindow: XCreateWindowEvent |
        xdestroywindow: XDestroyWindowEvent |
        xunmap: XUnmapEvent |
        xmap: XMapEvent |
        xmaprequest: XMapRequestEvent |
        xreparent: XReparentEvent |
        xconfigure: XConfigureEvent |
        xgravity: XGravityEvent |
        xresizerequest: XResizeRequestEvent |
        xconfigurerequest: XConfigureRequestEvent |
        xcirculate: XCirculateEvent |
        xcirculaterequest: XCirculateRequestEvent |
        xproperty: XPropertyEvent |
        xselectionclear: XSelectionClearEvent |
        xselectionrequest: XSelectionRequestEvent |
        xselection: XSelectionEvent |
        xcolormap: XColormapEvent |
        xclient: XClientMessageEvent |
        xmapping: XMappingEvent |
        xerror: XErrorEvent |
        xkeymap: XKeymapEvent |
        pad: ulong[24]
    },
;

iconstant XPT_XEVENT_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jan 17 1995
        Deleted extraneous parent field from XConfigureEvent
--- Adrian Howard, Jul  1 1992
        Corrected some bad event typespecs
--- John Gibson, Mar 28 1992
        Replaced << for bitmasks with exponent notation (compiles quicker)
--- Adrian Howard, Feb  7 1992 : Added -AnyModifier-
--- Adrian Howard, Jan 13 1992 : Added -StandardMask-
--- John Gibson, Nov  1 1991
        Replaced uses xpt_coretypes with include etc
--- Jonathan Meyer, Dec  5 1990, v13.91
        Added full XEvent union structure
--- Jonathan Meyer, Nov 23 1990
        Added uses xpt_coretypes
--- Jonathan Meyer, Nov 22 1990
        Added modifier masks
 */
