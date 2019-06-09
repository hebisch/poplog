/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/XColorcells.p
 > Purpose:         Colormap management
 > Author:          Gareth Palmer,  3 July 1989 (see revisions)
 > Documentation:   REF * XColorcells
 > Related Files:
 */

compile_mode: pop11 +strict;
section;

uses XConstants;
uses XColor;


global constant macro(
    /* Flags used in StoreNamedColor, StoreColors */
    DoRed       = (1 << 0),
    DoGreen     = (1 << 1),
    DoBlue      = (1 << 2),
);


external declare XColorcells in c;
    (external_import_procedure XptImportProcedure)

    Status XAllocColor(display, cmap, colorcell_def)
        Display *display;
        Colormap cmap;
        XColor *colorcell_def;       ;;; SENDs and RETURNs
    {}

    Status XAllocColorCells(display, cmap, contig, plane_masks, nplanes, pixels,
                              ncolors)
        Display *display;
        Colormap cmap;
        Bool contig;
        ;;;unsigned long plane_masks[nplanes];     ;;; RETURN
        unsigned long plane_masks[];
        unsigned int nplanes;
        ;;;unsigned long pixels[ncolors];          ;;; RETURN pixel values
        unsigned long pixels[];
        unsigned int ncolors;
    {}

    Status XAllocColorPlanes(display, cmap, contig, pixels, ncolors, nreds,
                                ngreens, nblues, rmask, gmask, bmask)
        Display *display;
        Colormap cmap;
        Bool contig;
        ;;;unsigned long pixels[ncolors];          ;;; RETURN
        unsigned long pixels[];
        int ncolors;
        int nreds, ngreens, nblues;
        unsigned long *rmask, *gmask, *bmask;      ;;; RETURN
    {}

    Status XAllocNamedColor(display, cmap, colorname, colorcell_def, rgb_db_def)
        Display *display;
        Colormap cmap;
        char *colorname;
        XColor *colorcell_def;              ;;; RETURN
        XColor *rgb_db_def;                 ;;; RETURN
    {}

    Status XLookupColor(display, cmap, colorname, rgb_db_def, hardware_def)
        Display *display;
        Colormap cmap;
        char *colorname;
        XColor *rgb_db_def, *hardware_def;    ;;; RETURN
    {}

    Status XParseColor(display, colormap, spec, rgb_db_def)
        Display *display;
        Colormap colormap;
        char *spec;
        XColor *rgb_db_def;             ;;; RETURN
    {}

    void XQueryColor(display, cmap, colorcell_def)
        Display *display;
        Colormap cmap;
        XColor *colorcell_def;          ;;; SEND and RETURN
    {}

    void XQueryColors(display, cmap, colorcell_defs, ncolors)
        Display *display;
        Colormap cmap;
        ;;;XColor *colorcell_defs[ncolors];          ;;; SEND and RETURN
        XColor *colorcell_defs[];
        int ncolors;
    {}

    void XStoreColor(display, cmap, colorcell_def)
        Display *display;
        Colormap cmap;
        XColor *colorcell_def;
    {}

    void XStoreColors(display, cmap, colorcell_defs, ncolors)
        Display *display;
        Colormap cmap;
        ;;;XColor colorcell_defs[ncolors];
        XColor colorcell_defs[];
        int ncolors;
    {}

    void XStoreNamedColor(display, cmap, colorname, pixel, flags)
        Display *display;
        Colormap cmap;
        char *colorname;
        unsigned long pixel;
        int flags;
    {}

    void XFreeColors(display, cmap, pixels, npixels, planes)
        Display *display;
        Colormap cmap;
        unsigned long pixels[];
        int npixels;
        unsigned long planes;
    {}

endexternal;

xlib_external_require XColorcells;

global vars XColorcells = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 10 1993
        o Sectioned, tided, made +strict.
        o Made WhitePixel and BlackPixel autoloadable to avoid duplication in
          LIB * XlibMacros
--- Jonathan Meyer, Jan 25 1991 Changed to use xlib_external_require
--- Ian Rogers, Dec 13 1990 Added XptImportProcedure to cope with Async events
 */
