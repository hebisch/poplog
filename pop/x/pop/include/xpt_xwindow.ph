/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_xwindow.ph
 > Purpose:         definitions for X Windows
 > Author:          John Gibson, 27 July 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF XPT_XWINDOW_INCLUDED

section;

include xpt_xscreen;

iconstant macro(

    /* Window attributes for CreateWindow and ChangeWindowAttributes */

    CWBackPixmap        = 2:1e0,
    CWBackPixel         = 2:1e1,
    CWBorderPixmap      = 2:1e2,
    CWBorderPixel       = 2:1e3,
    CWBitGravity        = 2:1e4,
    CWWinGravity        = 2:1e5,
    CWBackingStore      = 2:1e6,
    CWBackingPlanes     = 2:1e7,
    CWBackingPixel      = 2:1e8,
    CWOverrideRedirect  = 2:1e9,
    CWSaveUnder         = 2:1e10,
    CWEventMask         = 2:1e11,
    CWDontPropagate     = 2:1e12,
    CWColormap          = 2:1e13,
    CWCursor            = 2:1e14,


    /* Bit Gravity */

    ForgetGravity       = 0,
    NorthWestGravity    = 1,
    NorthGravity        = 2,
    NorthEastGravity    = 3,
    WestGravity         = 4,
    CenterGravity       = 5,
    EastGravity         = 6,
    SouthWestGravity    = 7,
    SouthGravity        = 8,
    SouthEastGravity    = 9,
    StaticGravity       = 10,

    /* Window gravity + bit gravity above */

    UnmapGravity        = 0,


    /* Used in CreateWindow for backing-store hint */

    NotUseful           = 0,
    WhenMapped          = 1,
    Always              = 2,

    /* Used in GetWindowAttributes reply */

    IsUnmapped          = 0,
    IsUnviewable        = 1,
    IsViewable          = 2,

       ParentRelative  = 1, /* background pixmap in CreateWindow
                               and ChangeWindowAttributes */

       CopyFromParent  = 0, /* border pixmap in CreateWindow
                               and ChangeWindowAttributes
                               special VisualID and special window
                               class passed to CreateWindow */
);


    /*
     * Data structure for getting window attributes.
     */

i_typespec XWindowAttributes {
        x :int,                     ;;; x location of window
        y :int,                     ;;; y location of window
        width :int,                 ;;; width of window
        height :int,                ;;; height of window
        border_width :int,          ;;; border width of window
        depth :int,                 ;;; depth of window
        visual :XptVisual,          ;;; the associated visual structure
        root :XptWindow,            ;;; root of screen containing window
        class :int,                 ;;; InputOutput, InputOnly
        bit_gravity :int,           ;;; one of bit gravity values
        win_gravity :int,           ;;; one of the window gravity values
        backing_store :int,         ;;; NotUseful, WhenMapped, Always
        backing_planes :ulong,      ;;; planes to be preserved if possible
        backing_pixel :XptPixel,    ;;; value to be used when restoring planes
        save_under :XptLongBoolean, ;;; boolean, should bits under be saved?
        colormap :XptColormap,      ;;; color map to be associated with window
        map_installed :XptLongBoolean, ;;; boolean, is color map currently installed
        map_state :int,             ;;; IsUnmapped, IsUnviewable, IsViewable
        all_event_masks :long,      ;;; set of events all people have interest in
        your_event_mask :long,      ;;; my event mask
        do_not_propagate_mask :long, ;;; set of events that should not propagate
        override_redirect :XptLongBoolean, ;;; boolean value for override-redirect
        screen :exptr.{:XScreen}    ;;; back pointer to correct screen
    };


    /*
     * Data structure for setting window attributes.
     */

i_typespec XSetWindowAttributes {
        background_pixmap :XptPixmap,   ;;; background or None or ParentRelative
        background_pixel :XptPixel,     ;;; background pixel
        border_pixmap :XptPixmap,       ;;; border of the window
        border_pixel :XptPixel,         ;;; border pixel value
        bit_gravity :int,               ;;; one of bit gravity values
        win_gravity :int,               ;;; one of the window gravity values
        backing_store :int,             ;;; NotUseful, WhenMapped, Always
        backing_planes :ulong,          ;;; planes to be preseved if possible
        backing_pixel :XptPixel,        ;;; value to use in restoring planes
        save_under :XptLongBoolean,     ;;; should bits under be saved? (popups)
        event_mask :long,               ;;; set of events that should be saved
        do_not_propagate_mask :long,    ;;; set of events that should not propagate
        override_redirect :XptLongBoolean, ;;; boolean value for override-redirect
        colormap :XptColormap,          ;;; color map to be associated with window
        cursor :XptCursor               ;;; cursor to be displayed (or None)
    };


iconstant macro (

        /* Attributes for ConfigureWindow */

     CWX                = 2:1e0,
     CWY                = 2:1e1,
     CWWidth            = 2:1e2,
     CWHeight           = 2:1e3,
     CWBorderWidth      = 2:1e4,
     CWSibling          = 2:1e5,
     CWStackMode        = 2:1e6,

    /* Window stacking method (in configureWindow) */

     Above              = 0,
     Below              = 1,
     TopIf              = 2,
     BottomIf           = 3,
     Opposite           = 4,

    /* Circulation direction */

     RaiseLowest        = 0,
     LowerHighest       = 1,

);


    /*
     * Data structure for XConfigureWindow
     */

i_typespec XWindowChanges {
        x :int,
        y :int,
        width :int,
        height :int,
        border_width :int,
        sibling :XptWindow,
        stack_mode :int
    };


iconstant XPT_XWINDOW_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 21 1991
        Added Configure Window stuff
 */
