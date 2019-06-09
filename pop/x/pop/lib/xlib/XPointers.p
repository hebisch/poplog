/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XPointers.p
 > Purpose:
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XUser;


global constant macro (

    /* button masks.  Used in same manner as Key masks above. Not to be confused,
       with button names below. */

    Button1Mask     = 1 << 8,
    Button2Mask     = 1 << 9,
    Button3Mask     = 1 << 10,
    Button4Mask     = 1 << 11,
    Button5Mask     = 1 << 12,
);

external declare XPointers in c;
    (external_import_procedure XptImportProcedure)


    Bool XQueryPointer(display, w, root, child, root_x, root_y, win_x, win_y,
                        keys_buttons)
    Display *display;
    Window w;
    Window *root, *child;             ;;; RETURN
    int *root_x, *root_y;             ;;; RETURN
    int *win_x, *win_y;               ;;; RETURN
    unsigned int *keys_buttons;       ;;; RETURN
    {}

    void XWarpPointer(display, src_w, dest_w, src_x, src_y, src_width, src_height,
                        dest_x, dest_y)
    Display *display;
    Window src_w, dest_w;
    int src_x, src_y;
    unsigned int src_width, src_height;
    int dest_x, dest_y;
    {}

    ;;; duplicated from Grabbing
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

    ;;; duplicated from Grabbing
    void XUngrabPointer(display, time)
    Display *display;
    Time time;
    {}

    int XGetPointerMapping(display, map, nmap)
    Display *display;
    unsigned char map[];        ;;; RETURN
    int nmap;
    {}

    int XSetPointerMapping(display, map, nmap)
    Display *display;
    unsigned char map[];
    int nmap;
    {}

    void XGetPointerControl(display, accel_numerator, accel_denominator, threshold)
    Display *display;
    int *accel_numerator, *accel_denominator;       ;;; RETURN
    int *threshold;                                 ;;; RETURN
    {}

    void XChangePointerControl(display, do_accel, do_threshold, accel_numerator,
                                accel_denominator, treshold)
    Display *display;
    Bool do_accel, do_threshold;
    int accel_numerator, accel_denominator;
    int treshold;
    {}

    void XChangeActivePointerGrab(display, event_mask, cursor, time)
    Display *display;
    unsigned int event_mask;
    Cursor cursor;
    Time time;
    {}


endexternal;


xlib_external_require XPointers;


global vars XPointers = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
