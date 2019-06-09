/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XWindowManager.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XExtent;


global constant macro(

    /*
     * The next block of definitions are for window manager properties that
     * clients and applications use for communication.
     */

    /* flags argument in size hints */
    USPosition  = 1 << 0, /* user specified x, y */
    USSize      = 1 << 1, /* user specified width, height */

    PPosition   = 1 << 2, /* program specified position */
    PSize       = 1 << 3, /* program specified size */
    PMinSize    = 1 << 4, /* program specified minimum size */
    PMaxSize    = 1 << 5, /* program specified maximum size */
    PResizeInc  = 1 << 6, /* program specified resize increments */
    PAspect     = 1 << 7, /* program specified min and max aspect ratios */
    PAllHints   = PPosition||PSize||PMinSize||PMaxSize||PResizeInc||PAspect,


    /* definition for flags of XWMHints */

    InputHint           = 1 << 0,
    StateHint           = 1 << 1,
    IconPixmapHint      = 1 << 2,
    IconWindowHint      = 1 << 3,
    IconPositionHint    = 1 << 4,
    IconMaskHint        = 1 << 5,
    WindowGroupHint     = 1 << 6,
    AllHints = InputHint||StateHint||IconPixmapHint||IconWindowHint||
                IconPositionHint||IconMaskHint||WindowGroupHint,

    /* definitions for initial window state */

    DontCareState = 0,  /* don't know or care */
    NormalState   = 1,  /* most applications want to start this way */
    ZoomState     = 2,  /* application wants to start zoomed */
    IconicState   = 3,  /* application wants to start as an icon */
    InactiveState = 4,  /* application believes it is seldom used; some
                            wm's may put it on inactive menu */

);


external declare XWindowManager in c;
    (external_import_procedure XptImportProcedure)

    typedef struct {
        char *res_name;
        char *res_class;
    } XClassHint;


    typedef struct {
        long flags; ;;; marks which fields in this structure are defined */
        Bool input; ;;; does this application rely on the window manager to
                    ;;;   get keyboard input? */
        int initial_state;  ;;; see below */
        Pixmap icon_pixmap; ;;; pixmap to be used as icon */
        Window icon_window;     ;;; window to be used as icon */
        int icon_x, icon_y;     ;;; initial position of icon */
        Pixmap icon_mask;   ;;; icon mask bitmap */
        XID window_group;   ;;; id of related window group */
        ;;; this structure may be extended in the future */
    } XWMHints;


    typedef struct {
        int min_width, min_height;
        int max_width, max_height;
        int width_inc, height_inc;
    } XIconSize;



    Status XGetClassHint(display, w, class_hints)
    Display *display;
    Window w;
    XClassHint *class_hints;         ;;; RETURN
    {}

    void XSetClassHint(display, w, class_hints)
    Display *display;
    Window w;
    XClassHint *class_hints;
    {}

    Status XGetNormalHints(display, w, hints)
    Display *display;
    Window w;
    XSizeHints *hints;              ;;; RETURN
    {}

    void XSetNormalHints(display, w, hints)   ;;; void
    Display *display;
    Window w;
    XSizeHints *hints;
    {}

    Status XGetSizeHints(display, w, hints, property)
    Display *display;
    Window w;
    XSizeHints *hints;              ;;; RETURN
    Atom property;
    {}

    void XSetSizeHints(display, w, hints, property)
    Display *display;
    Window w;
    XSizeHints *hints;
    Atom property;
    {}

    Status XGetTransientForHint(display, w, prop_window)
    Display *display;
    Window w;
    Window *prop_window;                 ;;; RETURN
    {}

    void XSetTransientForHint(display, w, prop_window)
    Display *display;
    Window w;
    Window prop_window;
    {}

    XWMHints *XGetWMHints(display, w)
    Display *display;
    Window w;
    {}

    void XSetWMHints(display, w, wmhints)
    Display *display;
    Window w;
    XWMHints *wmhints;
    {}

    Status XGetZoomHints(display, w, zhints)
    Display *display;
    Window w;
    XSizeHints *zhints;             ;;; RETURN
    {}

    void XSetZoomHints(display, w, zhints)
    Display *display;
    Window w;
    XSizeHints *zhints;
    {}

    Status XFetchName(display, w, window_name)
    Display *display;
    Window w;
    char **window_name;             ;;; RETURN
    {}

    void XStoreName(display, w, window_name)
    Display *display;
    Window w;
    char *window_name;
    {}

    Status XGetIconName(display, w, icon_name)
    Display *display;
    Window w;
    char **icon_name;               ;;; RETURN
    {}

    void XSetIconName(display, w, icon_name)
    Display *display;
    Window w;
    char *icon_name;
    {}

    Status XGetIconSizes(display, w, size_list, count)
    Display *display;
    Window w;
    XIconSize * *size_list;              ;;; RETURN
    int *count;                         ;;; RETURN
    {}

    void XSetIconSizes(display, w, size_list, count)
    Display *display;
    Window w;
    XIconSize *size_list;
    int count;
    {}

    void XSetCommand(display, w, argv, argc)
    Display *display;
    Window w;
    char **argv;
    int argc;
    {}

endexternal;


xlib_external_require XWindowManager;


global vars XWindowManager = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
