/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_ximage.ph
 > Purpose:         typespec definitions for X Images
 > Author:          Jonathan Meyer, Jan 30 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF XPT_XIMAGE_INCLUDED

section;

include xpt_xtypes.ph;

iconstant macro (
    XYBitmap    = 0,
    XYPixmap    = 1,
    ZPixmap     = 2,

    LSBFirst    = 0,
    MSBFirst    = 1,
);

i_typespec
    XImage {
        width: int,
        height: int,
        xoffset: int,
        format: int,
        data: exptr.:byte[],
        byte_order: int,
        bitmap_unit: int,
        bitmap_bit_order: int,
        bitmap_pad: int,
        depth: int,
        bytes_per_line: int,
        bits_per_pixel: int,
        red_mask: ulong,
        green_mask: ulong,
        blue_mask: ulong,
        obdata: exptr.:byte[],
        f {
            create_image: XptProcedure,
            destroy_image: XptProcedure,
            get_pixel: XptProcedure,
            put_pixel: XptProcedure,
            sub_image: XptProcedure,
            add_pixel: XptProcedure,
        }
    },

    create_image(0): exptr.:XImage,
    destroy_image(0): int,
    get_pixel(3): ulong,
    put_pixel(4): ulong,
    sub_image(5): exptr.:XImage,
    add_pixel(2): int,
;

iconstant XPT_XIMAGE_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Nov 18 1991 : Corrected definition of XImage typespec
--- Jonathan Meyer, Jan 30 1991 added iconstant's
 */
