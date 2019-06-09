/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XUser.p
 > Purpose:
 > Author:          Ian Rogers, Aug 18 1989 (see revisions)
 > Documentation:
 > Related Files:   LIB * XGrabbing XPointers XKeyboard XEvents
 */



global constant macro(

    /* Key masks. Used as modifiers to GrabButton and GrabKey, results of
    QueryPointer state in various key-, mouse-, and button-related events. */

    ShiftMask       = 1 << 0,
    LockMask        = 1 << 1,
    ControlMask     = 1 << 2,
    Mod1Mask        = 1 << 3,
    Mod2Mask        = 1 << 4,
    Mod3Mask        = 1 << 5,
    Mod4Mask        = 1 << 6,
    Mod5Mask        = 1 << 7,


    ;;; duplicated from XPointers

    MappingSuccess      = 0,
    MappingBusy         = 1,
    MappingFailed       = 2,

);

external declare xpop in c;
    (external_import_procedure XptImportProcedure)

    typedef struct {
        int type;       ;;; of event
        unsigned long serial;   ;;; # of last request processed by server
        Bool send_event;    ;;; true if this came from a SendEvent request
        Display *display;   ;;; Display the event was read from
        Window window;          ;;; "event" window it is reported relative to
        Window root;            ;;; root window that the event occured on
        Window subwindow;   ;;; child window
        Time time;      ;;; milliseconds
        int x, y;       ;;; pointer x, y coordinates in event window
        int x_root, y_root; ;;; coordinates relative to root
        unsigned int state; ;;; key or button mask
        unsigned int keycode;   ;;; detail
        Bool same_screen;   ;;; same screen flag
    } XKeyEvent;


    typedef struct {
        int type;
        unsigned long serial; ;;; # of last request processed by server
        Bool send_event;      ;;; true if this came from a SendEvent request
        Display *display;     ;;; Display the event was read from
        Window window;        ;;; unused
        int request;          ;;; one of MappingModifier, MappingKeyboard,
                              ;;;   MappingPointer
        int first_keycode;    ;;; first keycode
        int count;            ;;; defines range of change w. first_keycode
    } XMappingEvent;

endexternal;


global vars XUser = true;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
