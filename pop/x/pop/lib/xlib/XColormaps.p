/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XColormaps.p
 > Purpose:         Xlib Colormap Manipulation
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:   REF * XColormaps
 > Related Files:
 */

compile_mode: pop11 +strict;
section;

uses XConstants;

global constant macro (

    /* For CreateColormap */
    AllocNone       = 0,   /* create map with no entries */
    AllocAll        = 1,   /* allocate entire map writeable */

    /* For killid field of XStandardColormap */
    ReleaseByFreeingColormap    = 1,

);


external declare XColormaps in c;
    (external_import_procedure XptImportProcedure)

    /*
     * This defines a window manager property that clients may use to
     * share standard color maps:
     */

    typedef struct {
        Colormap colormap;
        unsigned long red_max;
        unsigned long red_mult;
        unsigned long green_max;
        unsigned long green_mult;
        unsigned long blue_max;
        unsigned long blue_mult;
        unsigned long base_pixel;
        VisualID visualid;
        XID killid;
    } XStandardColormap;


    Colormap XCopyColormapAndFree(display, cmap)
        Display *display;
        Colormap cmap;
    {}

    Colormap XCreateColormap(display, w, visual, alloc)
        Display *display;
        Window w;
        Visual *visual;
        int alloc;
    {}

    void XFreeColormap(display, cmap)
        Display *display;
        Colormap cmap;
    {}

    Status XGetStandardColormap(display, w, cmap_info, property)
        Display *display;
        Window w;
        XStandardColormap *cmap_info;       ;;; RETURN
        Atom property;
    {}

    void XSetStandardColormap(display, w, cmap, property)   ;;; void
        Display *display;
        Window w;
        XStandardColormap *cmap;
        Atom property;
    {}

    void XSetWindowColormap(display, w, cmap)
        Display *display;
        Window w;
        Colormap cmap;
    {}

    void XInstallColormap(display, cmap)
        Display *display;
        Colormap cmap;
    {}

    void XUninstallColormap(display, cmap)
        Display *display;
        Colormap cmap;
    {}

    Colormap *XListInstalledColormaps(display, w, num)
        Display *display;
        Window w;
        int *num;               ;;; RETURN
    {}

    void XSetRGBColormaps(display, w, std_colormap, count, property)
        Display *display;
        Window w;
        XStandardColormap *std_colormap;
        int count;
        Atom property;
    {}

    Status XGetRGBColormaps(display, w, std_colormap_ret, count_ret, property)
        Display *display;
        Window w;
        XStandardColormap **std_colormap_ret;
        int *count_ret;
        Atom property;
    {}



endexternal;

xlib_external_require XColormaps;

global vars XColormaps = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 22 1993
     #  Added ReleaseByFreeingColormap
     #  Added XSetRGBColormaps and XGetRGBColormaps
--- Adrian Howard, Jun 21 1993
     #  Added missing visualid and killid fields to XStandardColormap
        structure
     #  Tidied, made +strict, sectioned
     #  Made DisplayCells, DefaultVisual, DefaultColormapOfScreen, and
        DefaultColormap autoloadable to avoid code duplication in
        LIB * XlibMacros
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
