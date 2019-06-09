/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_xfontstruct.ph
 > Purpose:         typespec definitions for XFontStruct structures
 > Author:          Jonathan Meyer, Jan 30 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF XPT_XFONTSTRUCT_INCLUDED

section;

include xpt_xtypes.ph;

iconstant macro (
    FontLeftToRight     = 0,
    FontRightToLeft     = 1,
);

i_typespec

    XFontProp {
        name: ulong,
        card32: ulong,
    },

    XCharStruct {
        lbearing:short, /* origin to left edge of raster */
        rbearing:short, /* origin to right edge of raster */
        width:short,        /* advance to next char's origin */
        ascent:short,       /* baseline to top edge of raster */
        descent:short,  /* baseline to bottom edge of raster */
        attributes: ushort, /* per char flags (not predefined) */
    },

    XFontStruct {
        ext_data: exptr,
        fid: XptXID,
        direction: uint,
        min_char_or_byte2: uint, /* first character */
        max_char_or_byte2: uint, /* last character */
        min_byte1: uint,    /* first row that exists */
        max_byte1: uint, /* last row that exists */
        all_chars_exist: int#XptCoerceBoolean,
        default_char: uint,
        n_properties: int,
        properties: exptr.:XFontProp[],
        min_bounds: XCharStruct,
        max_bounds: XCharStruct,
        per_char: exptr.:XCharStruct[],
        ascent: int,
        descent: int,   /* log. descent below baseline for spacing */
    },

    XRectangle {
        x       :short,
        y       :short,
        width   :ushort,
        height  :ushort,
    },

    XFontSetExtents {
        max_ink_extent      :XRectangle,
        max_logical_extent  :XRectangle,
    }
;

iconstant XPT_XFONTSTRUCT_INCLUDED = true;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1997
        Added XRectangle and XFontSetExtents.
--- Jonathan Meyer, Jan 30 1991 added iconstant
 */
