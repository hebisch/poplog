/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XWindowAttributes.p
 > Purpose:
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:
 > Related Files:
 */


uses XConstants;
uses XWindowConstants;


external declare XWindowAttributes in c;
    (external_import_procedure XptImportProcedure)


    typedef struct {
        int x, y;           ;;; location of window */
        int width, height;      ;;; width and height of window */
        int border_width;       ;;; border width of window */
        int depth;              ;;; depth of window */
        Visual *visual;     ;;; the associated visual structure */
        Window root;            ;;; root of screen containing window */
        int class;          ;;; InputOutput, InputOnly*/
        int bit_gravity;        ;;; one of bit gravity values */
        int win_gravity;        ;;; one of the window gravity values */
        int backing_store;      ;;; NotUseful, WhenMapped, Always */
        unsigned long backing_planes; ;;; planes to be preserved if possible */
        unsigned long backing_pixel; ;;; value to be used when restoring planes */
        Bool save_under;        ;;; boolean, should bits under be saved? */
        Colormap colormap;      ;;; color map to be associated with window */
        Bool map_installed;     ;;; boolean, is color map currently installed*/
        int map_state;      ;;; IsUnmapped, IsUnviewable, IsViewable */
        long all_event_masks;   ;;; set of events all people have interest in*/
        long your_event_mask;   ;;; my event mask */
        long do_not_propagate_mask; ;;; set of events that should not propagate */
        Bool override_redirect; ;;; boolean value for override-redirect */
        Screen *screen;     ;;; back pointer to correct screen */
    } XWindowAttributes;



    Status XGetWindowAttributes(display, w, window_attributes)
    Display *display;
    Window w;
    XWindowAttributes *window_attributes;       ;;; RETURN
    {}

    void XChangeWindowAttributes(display, w, valuemask, attributes)
    Display *display;
    Window w;
    unsigned long valuemask;
    XSetWindowAttributes *attributes;
    {}

    void XSetWindowBackground(display, w, background_pixel)
    Display *display;
    Window w;
    unsigned long background_pixel;
    {}

    void XSetWindowBackgroundPixmap(display, w, background_tile)
    Display *display;
    Window w;
    Pixmap background_tile;
    {}

    void XSetWindowBorder(display, w, border_pixel)
    Display *display;
    Window w;
    unsigned long border_pixel;
    {}

    void XSetWindowBorderPixmap(display, w, border_title)
    Display *display;
    Window w;
    Pixmap border_title;
    {}

    void XSetWindowColormap(display, w, cmap)
    Display *display;
    Window w;
    Colormap cmap;
    {}

    ;;; duplicated from Cursors
    void XDefineCursor(display, w, cursor)
    Display *display;
    Window w;
    Cursor cursor;
    {}

    Status XGetGeometry(display, drawable, root, x, y, width, height,
                          border_width, depth)
    Display *display;
    Drawable drawable;
    Window *root;                     ;;; RETURN
    int *x, *y;                       ;;; RETURN
    unsigned int *width, *height;     ;;; RETURN
    unsigned int *border_width;       ;;; RETURN
    unsigned int *depth;              ;;; RETURN
    {}

    ;;; duplicated from InputHandling
    void XSelectInput(display, w, event_mask)
    Display *display;
    Window w;
    unsigned long event_mask;
    {}

endexternal;


xlib_external_require XWindowAttributes;


;;; This is already be set to a structure (see above), so leave alone!
;;; global vars XWindowAttributes  = true;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
