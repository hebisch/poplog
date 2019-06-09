/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XGrabbing.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XUser;

external declare XGrabbing in c;
    (external_import_procedure XptImportProcedure)


    void XGrabKey(display, keycode, modifiers, grab_window, owner_events,
                    pointer_mode, keyboard_mode)
    Display *display;
    int keycode;
    unsigned int modifiers;
    Window grab_window;
    Bool owner_events;
    int pointer_mode, keyboard_mode;
    {}

    void XUngrabKey(display, keycode, modifiers, w)
    Display *display;
    int keycode;
    unsigned int modifiers;
    Window w;
    {}

    int XGrabKeyboard(display, grab_window, owner_events, pointer_mode,
                        keyboard_mode, time)
    Display *display;
    Window grab_window;
    Bool owner_events;
    int pointer_mode, keyboard_mode;
    Time time;
    {}

    void XUngrabKeyboard(display, time)
    Display *display;
    Time time;
    {}

    void XGrabButton(display, button, modifiers, grab_window, owner_events,
                       event_mask, pointer_mode, keyboard_mode, confine_to, cursor)
    Display *display;
    unsigned int button;
    unsigned int modifiers;
    Window grab_window;
    Bool owner_events;
    unsigned int event_mask;
    int pointer_mode, keyboard_mode;
    Window confine_to;
    Cursor cursor;
    {}

    void XUngrabButton(display, button, modifiers, w)
    Display *display;
    unsigned int button;
    unsigned int modifiers;
    Window w;
    {}

    int XGrabPointer(display, grab_window, owner_events, event_mask, pointer_mode,
                        keyboard_mode, confine_to, cursor, time)
    Display *display;
    Window grab_window;
    Bool owner_events;
    unsigned int event_mask;
    int pointer_mode, keyboard_mode;
    Window confine_to;
    Cursor cursor;
    Time time;
    {}

    void XUngrabPointer(display, time)
    Display *display;
    Time time;
    {}

    void XGrabServer(display)
    Display *display;
    {}

    void XUngrabServer(display)
    Display *display;
    {}

    ;;; duplicated from Pointers
    void XChangeActivePointerGrab(display, event_mask, cursor, time)
    Display *display;
    unsigned int event_mask;
    Cursor cursor;
    Time time;
    {}


endexternal;


xlib_external_require XGrabbing;


global vars XGrabbing = true;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Feb 10 1992 : Moved constants to INCLUDE *XConstants.ph since
        they're needed in other libraries.
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
