/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_xscreen.ph
 > Purpose:         typespec definitions for X Screens
 > Author:          Jonathan Meyer, Nov 22 1990 (see revisions)
 > Documentation:   REF *XPT_SCREENINFO
 > Related Files:   LIB *XPT_SCREENINFO, LIB *XptConvertUnit
 */

#_TERMIN_IF DEF XPT_XSCREEN_INCLUDED

section;

include xpt_coretypes.ph
include xpt_xtypes.ph

;;; X Screen structure
i_typespec XScreen {
    ext_data: exptr,
    display: XptDisplayPtr,
    root: XptWindow,
    width: int,
    height: int,
    mwidth: int,
    mheight: int,
    ndepths: int,
    depths: exptr,
    root_depth: int,
    root_visual: XptXID,
    default_gc: XptXID,
    cmap: XptXID,
    white_pixel: ulong,
    black_pixel: ulong,
    min_maps: int,
    max_maps: int,
    backing_store: int,
    save_unders: int#XptCoerceBoolean,
    root_input_mask: long,
};

iconstant XPT_XSCREEN_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  1 1991
        Replaced uses xpt_coretypes with include etc
 */
