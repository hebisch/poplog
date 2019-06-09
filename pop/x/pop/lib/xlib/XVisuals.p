/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XVisuals.p
 > Purpose:         Visual manipulation
 > Author:          Gareth Palmer,  4 July 1989 (see revisions)
 > Documentation:   REF * XVisuals
 > Related Files:
 */

compile_mode: pop11 +strict;
section;


uses XConstants;


global constant macro (

    VisualNoMask            = 16:000,
    VisualIDMask            = 16:001,
    VisualScreenMask        = 16:002,
    VisualDepthMask         = 16:004,
    VisualClassMask         = 16:008,
    VisualRedMaskMask       = 16:010,
    VisualGreenMaskMask     = 16:020,
    VisualBlueMaskMask      = 16:040,
    VisualColormapSizeMask  = 16:080,
    VisualBitsPerRGBMask    = 16:100,
    VisualAllMask           = 16:1FF,

    /*  Display classes  used in opening the connection
     *  Note that the statically allocated ones are even numbered and the
     *  dynamically changeable ones are odd numbered
     */
     StaticGray      = 0,
     GrayScale       = 1,
     StaticColor     = 2,
     PseudoColor     = 3,
     TrueColor       = 4,
     DirectColor     = 5,

);


external declare XVisuals in c;
    (external_import_procedure XptImportProcedure)

    /*
     * Information used by the visual utility routines to find desired visual
     * type from the many visuals a display may support.
     */
    typedef struct {
        Visual *visual;
        VisualID visualid;
        int screen;
        int depth;
        int class;
        unsigned long red_mask;
        unsigned long green_mask;
        unsigned long blue_mask;
        int colormap_size;
        int bits_per_rgb;
    } XVisualInfo;


    XVisualInfo *XGetVisualInfo(display, vinfo_mask, vinfo_template, nitems)
        Display *display;
        long vinfo_mask;
        XVisualInfo *vinfo_template;
        int *nitems;                        ;;; RETURN
    {}

    Status XMatchVisualInfo(display, screen, depth, class, vinfo)
        Display *display;
        int screen;
        int depth;
        int class;
        XVisualInfo *vinfo;                 ;;; RETURN
    {}

    VisualID XVisualIDFromVisual(visual)
            Visual *visual;
    {}

endexternal;


xlib_external_require XVisuals;


global vars XVisuals = true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 22 1993
     #  Tidied, sectioned, made + strict
     #  Added XVisualIDFromVisual
     #  Made DefaultVisual autoloadable to avoid code duplication
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
