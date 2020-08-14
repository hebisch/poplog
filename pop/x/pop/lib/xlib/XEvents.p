/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XEvents.p
 > Purpose:
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XUser;


global constant macro (

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

    NoEventMask             = 0,
    KeyPressMask            = 1 << 0,
    KeyReleaseMask          = 1 << 1,
    ButtonPressMask         = 1 << 2,
    ButtonReleaseMask       = 1 << 3,
    EnterWindowMask         = 1 << 4,
    LeaveWindowMask         = 1 << 5,
    PointerMotionMask       = 1 << 6,
    PointerMotionHintMask   = 1 << 7,
    Button1MotionMask       = 1 << 8,
    Button2MotionMask       = 1 << 9,
    Button3MotionMask       = 1 << 10,
    Button4MotionMask       = 1 << 11,
    Button5MotionMask       = 1 << 12,
    ButtonMotionMask        = 1 << 13,
    KeymapStateMask         = 1 << 14,
    ExposureMask            = 1 << 15,
    VisibilityChangeMask    = 1 << 16,
    StructureNotifyMask     = 1 << 17,
    ResizeRedirectMask      = 1 << 18,
    SubstructureNotifyMask  = 1 << 19,
    SubstructureRedirectMask = 1 << 20,
    FocusChangeMask         = 1 << 21,
    PropertyChangeMask      = 1 << 22,
    ColormapChangeMask      = 1 << 23,
    OwnerGrabButtonMask     = 1 << 24,

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

    NotifyAncestor      = 0,
    NotifyVirtual       = 1,
    NotifyInferior      = 2,
    NotifyNonlinear     = 3,
    NotifyNonlinearVirtual  = 4,
    NotifyPointer       = 5,
    NotifyPointerRoot   = 6,
    NotifyDetailNone    = 7,

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

    RevertToNone        = None,
    RevertToPointerRoot = PointerRoot,
    RevertToParent      = 2,

);


external declare XEvents in c;
    (external_import_procedure XptImportProcedure)

    /*
     * A "XEvent" structure always  has type as the first entry.  This
     * uniquely identifies what  kind of event it is.  The second entry
     * is always a pointer to the display the event was read from.
     * The third entry is always a window of one type or another,
     * carefully selected to be useful to toolkit dispatchers.  (Except
     * for keymap events, which have no window.) You
     * must not change the order of the three elements or toolkits will
     * break! The pointer to the generic event must be cast before use to
     * access any other information in the structure.
     */

    /*
     * Definitions of specific events.
     */

    typedef XKeyEvent XKeyPressedEvent;
    typedef XKeyEvent XKeyReleasedEvent;

    typedef struct {
        int type;       ;;; of event */
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;          ;;; "event" window it is reported relative to */
        Window root;            ;;; root window that the event occured on */
        Window subwindow;   ;;; child window */
        Time time;      ;;; milliseconds */
        int x, y;       ;;; pointer x, y coordinates in event window */
        int x_root, y_root; ;;; coordinates relative to root */
        unsigned int state; ;;; key or button mask */
        unsigned int button;    ;;; detail */
        Bool same_screen;   ;;; same screen flag */
    } XButtonEvent;
    typedef XButtonEvent XButtonPressedEvent;
    typedef XButtonEvent XButtonReleasedEvent;

    typedef struct {
        int type;       ;;; of event */
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;          ;;; "event" window reported relative to */
        Window root;            ;;; root window that the event occured on */
        Window subwindow;   ;;; child window */
        Time time;      ;;; milliseconds */
        int x, y;       ;;; pointer x, y coordinates in event window */
        int x_root, y_root; ;;; coordinates relative to root */
        unsigned int state; ;;; key or button mask */
        char is_hint;       ;;; detail */
        Bool same_screen;   ;;; same screen flag */
    } XMotionEvent;
    typedef XMotionEvent XPointerMovedEvent;

    typedef struct {
        int type;       ;;; of event */
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;          ;;; "event" window reported relative to */
        Window root;            ;;; root window that the event occured on */
        Window subwindow;   ;;; child window */
        Time time;      ;;; milliseconds */
        int x, y;       ;;; pointer x, y coordinates in event window */
        int x_root, y_root; ;;; coordinates relative to root */
        int mode;       ;;; NotifyNormal, NotifyGrab, NotifyUngrab */
        int detail;

         ;;; NotifyAncestor, NotifyVirtual, NotifyInferior,
         ;;; NotifyNonLinear,NotifyNonLinearVirtual

        Bool same_screen;   ;;; same screen flag */
        Bool focus;     ;;; boolean focus */
        unsigned int state; ;;; key or button mask */
    } XCrossingEvent;
    typedef XCrossingEvent XEnterWindowEvent;
    typedef XCrossingEvent XLeaveWindowEvent;

    typedef struct {
        int type;       ;;; FocusIn or FocusOut */
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;      ;;; window of event */
        int mode;       ;;; NotifyNormal, NotifyGrab, NotifyUngrab */
        int detail;

         ;;; NotifyAncestor, NotifyVirtual, NotifyInferior,
         ;;; NotifyNonLinear,NotifyNonLinearVirtual, NotifyPointer,
         ;;; NotifyPointerRoot, NotifyDetailNone

    } XFocusChangeEvent;
    typedef XFocusChangeEvent XFocusInEvent;
    typedef XFocusChangeEvent XFocusOutEvent;

    /* generated on EnterWindow and FocusIn  when KeyMapState selected */
    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;
        char key_vector[32];
    } XKeymapEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;
        int x, y;
        int width, height;
        int count;      ;;; if non-zero, at least this many more */
    } XExposeEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Drawable drawable;
        int x, y;
        int width, height;
        int count;      ;;; if non-zero, at least this many more */
        int major_code;     ;;; core is CopyArea or CopyPlane */
        int minor_code;     ;;; not defined in the core */
    } XGraphicsExposeEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Drawable drawable;
        int major_code;     ;;; core is CopyArea or CopyPlane */
        int minor_code;     ;;; not defined in the core */
    } XNoExposeEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;
        int state;      ;;; either Obscured or UnObscured */
    } XVisibilityEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window parent;      ;;; parent of the window */
        Window window;      ;;; window id of window created */
        int x, y;       ;;; window location */
        int width, height;  ;;; size of window */
        int border_width;   ;;; border width */
        Bool override_redirect; ;;; creation should be overridden */
    } XCreateWindowEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window event;
        Window window;
    } XDestroyWindowEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window event;
        Window window;
        Bool from_configure;
    } XUnmapEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window event;
        Window window;
        Bool override_redirect; ;;; boolean, is override set... */
    } XMapEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window parent;
        Window window;
    } XMapRequestEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window event;
        Window window;
        Window parent;
        int x, y;
        Bool override_redirect;
    } XReparentEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window event;
        Window window;
        int x, y;
        int width, height;
        int border_width;
        Window above;
        Bool override_redirect;
    } XConfigureEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window event;
        Window window;
        int x, y;
    } XGravityEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;
        int width, height;
    } XResizeRequestEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window parent;
        Window window;
        int x, y;
        int width, height;
        int border_width;
        Window above;
        int detail;     ;;; Above, Below, TopIf, BottomIf, Opposite */
        unsigned long value_mask;
    } XConfigureRequestEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window event;
        Window window;
        int place;      ;;; PlaceOnTop, PlaceOnBottom */
    } XCirculateEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window parent;
        Window window;
        int place;      ;;; PlaceOnTop, PlaceOnBottom */
    } XCirculateRequestEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;
        Atom atom;
        Time time;
        int state;      ;;; NewValue, Deleted */
    } XPropertyEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;
        Atom selection;
        Time time;
    } XSelectionClearEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window owner;       ;;; must be next after type */
        Window requestor;
        Atom selection;
        Atom target;
        Atom property;
        Time time;
    } XSelectionRequestEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window requestor;   ;;; must be next after type */
        Atom selection;
        Atom target;
        Atom property;      ;;; ATOM or None */
        Time time;
    } XSelectionEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;
        Colormap colormap;  ;;; COLORMAP or None */
        Bool new;
        int state;      ;;; ColormapInstalled, ColormapUninstalled */
    } XColormapEvent;


    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;   ;;; Display the event was read from */
        Window window;
        Atom message_type;
        int format;
        union {
            char b[20];
            short s[10];
            long l[5];
            } data;
    } XClientMessageEvent;

    typedef struct {
        int type;
        Display *display;   ;;; Display the event was read from
        XID resourceid;     ;;; resource id */
        unsigned long serial;   ;;; serial number of failed request */
        char error_code;    ;;; error code of failed request */
        char request_code;  ;;; Major op-code of failed request */
        char minor_code;    ;;; Minor op-code of failed request */
    } XErrorEvent;

    typedef struct {
        int type;
        unsigned long serial;   ;;; # of last request processed by server */
        Bool send_event;    ;;; true if this came from a SendEvent request */
        Display *display;  ;;; Display the event was read from */
        Window window;  ;;; window on which event was requested in event mask */
    } XAnyEvent;


    /*
     * this union is defined so Xlib can always use the same sized
     * event structure internally, to avoid memory fragmentation.
     */
    typedef union _XEvent {
            int type;       ;;; must not be changed; first element */
        XAnyEvent xany;
        XKeyEvent xkey;
        XButtonEvent xbutton;
        XMotionEvent xmotion;
        XCrossingEvent xcrossing;
        XFocusChangeEvent xfocus;
        XExposeEvent xexpose;
        XGraphicsExposeEvent xgraphicsexpose;
        XNoExposeEvent xnoexpose;
        XVisibilityEvent xvisibility;
        XCreateWindowEvent xcreatewindow;
        XDestroyWindowEvent xdestroywindow;
        XUnmapEvent xunmap;
        XMapEvent xmap;
        XMapRequestEvent xmaprequest;
        XReparentEvent xreparent;
        XConfigureEvent xconfigure;
        XGravityEvent xgravity;
        XResizeRequestEvent xresizerequest;
        XConfigureRequestEvent xconfigurerequest;
        XCirculateEvent xcirculate;
        XCirculateRequestEvent xcirculaterequest;
        XPropertyEvent xproperty;
        XSelectionClearEvent xselectionclear;
        XSelectionRequestEvent xselectionrequest;
        XSelectionEvent xselection;
        XColormapEvent xcolormap;
        XClientMessageEvent xclient;
        XMappingEvent xmapping;
        XErrorEvent xerror;
        XKeymapEvent xkeymap;
        long pad[24];
    } XEvent;


    /*
     * _QEvent datatype for use in input queueing.
     */
    typedef struct _XSQEvent {
        struct _XSQEvent *next;
        XEvent event;
    } _XQEvent;


    /* Data structure for XGetMotionEvents.  */

    typedef struct {
            Time time;
        unsigned short x, y;
    } XTimeCoord;



    void XSelectInput(display, w, event_mask)
    Display *display;
    Window w;
    unsigned long event_mask;
    {}

    Status XSendEvent(display, w, propagate, event_mask, event)
    Display *display;
    Window w;
    Bool propagate;
    unsigned long event_mask;
    XEvent *event;
    {}

    void XSetInputFocus(display, focus, revert_to, time)
    Display *display;
    Window focus;
    int revert_to;
    Time time;
    {}

    void XGetInputFocus(display, focus, revert_to)
    Display *display;
    Window *focus;              ;;; RETURN
    int *revert_to;             ;;; RETURN
    {}

    void XWindowEvent(display, w, event_mask, rep)
    Display *display;
    Window w;
    long event_mask;
    XEvent *rep;            ;;; RETURN
    {}

    Bool XCheckWindowEvent(display, w, event_mask, event)
    Display *display;
    Window w;
    int event_mask;
    XEvent *event;          ;;; RETURN
    {}

    Bool XCheckTypedEvent(display, event_type, report)
    Display *display;
    int event_type;
    XEvent *report;         ;;; RETURN
    {}

    Bool XCheckTypedWindowEvent(display, w, event_type, report)
    Display *display;
    Window w;
    int event_type;
    XEvent *report;         ;;; RETURN
    {}

    void XMaskEvent(display, event_mask, rep)
    Display *display;
    unsigned long event_mask;
    XEvent *rep;                ;;;RETURN
    {}

    Bool XCheckMaskEvent(display, mask_event, event)
    Display *display;
    unsigned long mask_event;
    XEvent *event;                ;;;RETURN
    {}

    void XIfEvent(display, event, predicate, args)
    Display *display;
    XEvent *event;              ;;; RETURN
    Bool (*predicate)();
    char *args;
    {}

    Bool XCheckIfEvent(display, event, predicate, args)
    Display *display;
    XEvent *event;              ;;; RETURN
    Bool (*predicate)();
    char *args;
    {}

    void XPeekEvent(display, report)
    Display *display;
    XEvent *report;             ;;; RETURN
    {}

    void XPeekIfEvent(display, event, predicate, args)
    Display *display;
    XEvent *event;              ;;; RETURN
    Bool (*predicate)();
    char *args;
    {}

    void XAllowEvents(display, event_mode, time)
    Display *display;
    int event_mode;
    Time time;
    {}

    XTimeCoord *XGetMotionEvents(display, w, start, stop, nevents)
    Display *display;
    Window w;
    Time start, stop;
    int *nevents;               ;;; RETURN
    {}

    void XNextEvent(display, report)
    Display *display;
    XEvent *report;         ;;; RETURN
    {}

    void XPutBackEvent(display, event)
    Display *display;
    XEvent *event;
    {}

    int XEventsQueued(display, mode)
    Display *display;
    int mode;
    {}

    int XPending(display)
    Display *display;
    {}

    ;;;int (*XSynchronize(display, onoff))()
    int *XSynchronize(display, onoff)
    Display *display;
    int onoff;
    {}

endexternal;


xlib_external_require XEvents;


;;; duplicated from XlibMacros
 ;;; #define QLength(dpy)        ((dpy)->qlen)
define global QLength(dpy);
    lvars dpy;
    dpy #-> qlen
enddefine;



global vars XEvents = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
