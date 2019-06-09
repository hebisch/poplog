/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_xgcvalues.ph
 > Purpose:         typespec definitions for XGCValues structures
 > Author:          Jonathan Meyer, Jan 30 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF XPT_XGCVALUES_INCLUDED

include xpt_coretypes.ph;
include xpt_xtypes.ph;

section;

iconstant macro (

/* function modes */
        GXclear          = 0,
        GXand            = 1,
        GXandReverse     = 2,
        GXcopy           = 3,
        GXandInverted    = 4,
        GXnoop           = 5,
        GXxor            = 6,
        GXor             = 7,
        GXnor            = 8,
        GXequiv          = 9,
        GXinvert         = 10,
        GXorReverse      = 11,
        GXcopyInverted   = 12,
        GXorInverted     = 13,
        GXnand           = 14,
        GXset            = 15,

/* cap style */

        CapNotLast          = 0,
        CapButt             = 1,
        CapRound            = 2,
        CapProjecting       = 3,

/* join style */

        JoinMiter           = 0,
        JoinRound           = 1,
        JoinBevel           = 2,

/* line style */

        LineSolid           = 0,
        LineOnOffDash       = 1,
        LineDoubleDash      = 2,

/* subwindow mode */

        ClipByChildren      = 0,
        IncludeInferiors    = 1,

/* drawing mode */

        CoordModeOrigin     = 0,
        CoordModePrevious   = 1,

       GCFunction           = (2:1e0),
       GCPlaneMask          = (2:1e1),
       GCForeground         = (2:1e2),
       GCBackground         = (2:1e3),
       GCLineWidth          = (2:1e4),
       GCLineStyle          = (2:1e5),
       GCCapStyle           = (2:1e6),
       GCJoinStyle          = (2:1e7),
       GCFillStyle          = (2:1e8),
       GCFillRule           = (2:1e9),
       GCTile               = (2:1e10),
       GCStipple            = (2:1e11),
       GCTileStipXOrigin    = (2:1e12),
       GCTileStipYOrigin    = (2:1e13),
       GCFont               = (2:1e14),
       GCSubwindowMode      = (2:1e15),
       GCGraphicsExposures  = (2:1e16),
       GCClipXOrigin        = (2:1e17),
       GCClipYOrigin        = (2:1e18),
       GCClipMask           = (2:1e19),
       GCDashOffset         = (2:1e20),
       GCDashList           = (2:1e21),
       GCArcMode            = (2:1e22),
       GCLastBit            = 22,
);

i_typespec

    XGCValues {
         function: int,
         plane_mask: uint,
         foreground: uint,
         background: uint,
         line_width: int,
         line_style: int,
         cap_style: int,
         join_style: int,
         fill_style: int,
         fill_rule: int,
         arc_mode: int,
         tile: XptPixmap,
         stipple: XptPixmap,
         ts_x_origin: int,
         ts_y_origin: int,
         font: XptFont,
         subwindow_mode: int,
         graphics_exposures: XptLongBoolean,
         clip_x_origin: int,
         clip_y_origin: int,
         clip_mask: XptPixmap,
         dash_offset: int,
         dashes: byte,
    },
    XGC {
        ext_data: exptr,
        gid: XptXID,
        rects: int#XptCoerceBoolean,
        dashes: int#XptCoerceBoolean,
        dirty: ulong,
        values: XGCValues
    },
;

iconstant XPT_XGCVALUES_INCLUDED = true;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  7 1992
        Fixed typo
--- Adrian Howard, Oct  1 1991 : Couple of type fixes (from Jon Meyer)
 */
