/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XWindowConstants.p
 > Purpose:
 > Author:          Ian Rogers, Aug 18 1989 (see revisions)
 > Documentation:
 > Related Files:   LIB * XWindowExistence XWindowAttributes
 */



global constant macro(

    /* Window attributes for CreateWindow and ChangeWindowAttributes */

    CWBackPixmap        = (1 << 0),
    CWBackPixel         = (1 << 1),
    CWBorderPixmap      = (1 << 2),
    CWBorderPixel       = (1 << 3),
    CWBitGravity        = (1 << 4),
    CWWinGravity        = (1 << 5),
    CWBackingStore      = (1 << 6),
    CWBackingPlanes     = (1 << 7),
    CWBackingPixel      = (1 << 8),
    CWOverrideRedirect  = (1 << 9),
    CWSaveUnder         = (1 << 10),
    CWEventMask         = (1 << 11),
    CWDontPropagate     = (1 << 12),
    CWColormap          = (1 << 13),
    CWCursor            = (1 << 14),


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

    NotUseful               = 0,
    WhenMapped              = 1,
    Always                  = 2,

    /* Used in GetWindowAttributes reply */

    IsUnmapped      = 0,
    IsUnviewable    = 1,
    IsViewable      = 2,

       ParentRelative  = 1, /* background pixmap in CreateWindow
                               and ChangeWindowAttributes */

       CopyFromParent  = 0, /* border pixmap in CreateWindow
                               and ChangeWindowAttributes
                               special VisualID and special window
                               class passed to CreateWindow */
);

external declare xpop in c;
    (external_import_procedure XptImportProcedure)


    /*
     * Data structure for setting window attributes.
     */

    typedef struct {
        Pixmap background_pixmap;   ;;; background or None or ParentRelative */
        unsigned long background_pixel; ;;; background pixel */
        Pixmap border_pixmap;   ;;; border of the window */
        unsigned long border_pixel; ;;; border pixel value */
        int bit_gravity;        ;;; one of bit gravity values */
        int win_gravity;        ;;; one of the window gravity values */
        int backing_store;      ;;; NotUseful, WhenMapped, Always */
        unsigned long backing_planes; ;;; planes to be preseved if possible */
        unsigned long backing_pixel; ;;; value to use in restoring planes */
        Bool save_under;        ;;; should bits under be saved? (popups) */
        long event_mask;        ;;; set of events that should be saved */
        long do_not_propagate_mask; ;;; set of events that should not propagate */
        Bool override_redirect; ;;; boolean value for override-redirect */
        Colormap colormap;      ;;; color map to be associated with window */
        Cursor cursor;      ;;; cursor to be displayed (or None) */
    } XSetWindowAttributes;

endexternal;


global vars XWindowConstants = true;

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
