/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_xpmtypes.ph
 > Purpose:         typespec definitions for Xpm types (V3.2g)
 > Author:          Julian Clinton, Feb 14 1996 (see revisions)
 > Documentation:
 */

#_TERMIN_IF DEF XPT_XPMTYPES_INCLUDED

section;

include xpt_xtypes.ph;

/* Return ErrorStatus codes:
 * null     if full success
 * positive if partial success
 * negative if failure
 */

iconstant macro (
    XpmColorError   = 1,
    XpmSuccess      = 0,
    XpmOpenFailed   = -1,
    XpmFileInvalid  = -2,
    XpmNoMemory     = -3,
    XpmColorFailed  = -4,
);


i_typespec

    XpmColorSymbol {
        name    :exptr,
        value   :exptr,
        pixel   :XptPixel,
    },

    XpmExtension {
        name    :exptr.exacc_ntstring, /* name of the extension */
        nlines  :uint,      /* number of lines in this extension */
        lines   :exptr.:exptr.:byte[],          /* pointer to the extension array of *strings */
    },

    XpmAttributes {
        valuemask   :ulong,     /* Specifies which attributes are defined */

        visual      :exptr, /* Specifies the visual to use */
        colormap    :XptColormap,   /* Specifies the colormap to use */
        depth       :uint,          /* Specifies the depth */
        width       :uint,          /* Returns the width of the created pixmap */
        height      :uint,      /* Returns the height of the created pixmap */
        x_hotspot   :uint,      /* Returns the x hotspot's coordinate */
        y_hotspot   :uint,      /* Returns the y hotspot's coordinate */
        cpp         :uint,          /* Specifies the number of char per pixel */
        pixels      :exptr.:XptPixel[],         /* List of used color pixels */
        npixels     :uint,      /* Number of pixels */
        colorsymbols    :exptr.:XpmColorSymbol[],   /* Array of color symbols to override */
        numsymbols  :uint,      /* Number of symbols */
        rgb_fname   :exptr.exacc_ntstring,         /* RGB text file name */
        nextensions :uint,      /* number of extensions */
        extensions  :exptr.:XpmExtension[],      /* pointer to array of extensions */

        /* Infos */
        ncolors     :uint,      /* Number of colors */
        colorTable  :exptr.:exptr.:exptr.:byte[],   /* Color table pointer */
        hints_cmt   :exptr.exacc_ntstring,         /* Comment of the hints section */
        colors_cmt  :exptr.exacc_ntstring,         /* Comment of the colors section */
        pixels_cmt  :exptr.exacc_ntstring,         /* Comment of the pixels section */
        mask_pixel  :uint,  /* Transparent pixel's color table index */

        /* Color Allocation Directives */
        exactColors :uint,      /* Only use exact colors for visual */
        closeness   :uint,      /* Allowable RGB deviation */
        red_closeness   :uint,      /* Allowable red deviation */
        green_closeness :uint,  /* Allowable green deviation */
        blue_closeness  :uint,  /* Allowable blue deviation */
        color_key   :int,           /* Use colors from this color set */

    },
;

iconstant macro (
/* Xpm attribute value masks bits */
    XpmVisual           = (1<<0),
    XpmColormap         = (1<<1),
    XpmDepth            = (1<<2),
    XpmSize             = (1<<3),   /* width & height */
    XpmHotspot          = (1<<4),   /* x_hotspot & y_hotspot */
    XpmCharsPerPixel    = (1<<5),
    XpmColorSymbols     = (1<<6),
    XpmRgbFilename      = (1<<7),
    XpmInfos            = (1<<8),   /* all infos members */
    XpmExtensions       = (1<<10),

    XpmReturnPixels     = (1<<9),
    XpmReturnInfos      = (1<<8),   /* same as XpmInfos */
    XpmReturnExtensions = (1<<10),  /* same as XpmExtensions */

    XpmExactColors      = (1<<11),
    XpmCloseness        = (1<<12),
    XpmRGBCloseness     = (1<<13),
    XpmColorKey         = (1<<14),

/*
 * color keys for visual type, they must match those defined in xpmP.h
 */
    XPM_MONO    = 2,
    XPM_GREY4   = 3,
    XPM_GRAY4   = 3,
    XPM_GREY    = 4,
    XPM_GRAY    = 4,
    XPM_COLOR   = 5,
);

iconstant XPT_XPMTYPES_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 17 1996
        Installed Julian Clinton's fix to XpmColorSymbol
 */
