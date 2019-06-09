/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/include/xpt_xtypes.ph
 > Purpose:         typespec definitions for Xpt x types
 > Author:          John Gibson, Nov  1 1991 (see revisions)
 > Documentation:
 */

#_TERMIN_IF DEF XPT_XTYPES_INCLUDED

section;

include xpt_coretypes.ph;

i_typespec

    XptXID          :ulong,

    XptKeyCode      :byte,
    XptKeySym       :XptXID,
    XptOpaque       :ulong,
    XptAtom         :ulong,
    XptBitmap       :XptXID,
    XptPixmap       :XptXID,
    XptPixel        :XptXID,
    XptFont         :XptXID,
    XptColor        :XptXID,
    XptColormap     :XptXID,
    XptCursor       :XptXID,
    XptVisual       :XptXID,
    XptGC           :XptXID,
    XptXrmQuark     :int,

    XptWindow       :exptr#XptImportWindow,
    XptScreenPtr    :exptr#XptImportScreenPtr,
    XptXEventPtr    :exptr#XptImportXEventPtr,
    XptKeySymTable  :exptr#XptImportKeySymTable,
    XptXrmDatabase  :exptr#XptImportXrmDatabase,

    XptGCValues {
        XptGCVFunction     :int,
        XptGCVPlaneMask    :ulong,
        XptGCVForeground   :ulong,
        XptGCVBackground   :ulong,
        XptGCVLineWidth    :int,
        XptGCVLineStyle    :int,
        XptGCVCapStyle     :int,
        XptGCVJoinStyle    :int,
        XptGCVFillStyle    :int,
        XptGCVFillRule     :int,
        XptGCVArcMode      :int,
        XptGCVTile         :XptPixmap,
        XptGCVStipple      :XptPixmap,
        XptGCVTsXOrigin    :int,
        XptGCVTsYOrigin    :int,
        XptGCVFont         :XptFont,
        XptGCVSubwindowMode:int,
        XptGCVGraphicsExposures:XptLongBoolean,
        XptGCVClipXOrigin  :int,
        XptGCVClipYOrigin  :int,
        XptGCVClipMask     :XptPixmap,
        XptGCVDashOffset   :int,
        XptGCVDashes       :byte,
    },
;

iconstant XPT_XTYPES_INCLUDED = true;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 31 1995
        Corrected various types
--- Adrian Howard, Jun 30 1993
        Added XptXrmDatabase
--- Adrian Howard, Dec  2 1991 : Added -XptImportKeySymTable-
--- John Gibson, Nov 25 1991
        Moved def'n of XptXID to beginning!
--- Adrian Howard, Nov 22 1991 : Added -typespec:XptKeyCode- and
        -typespec:XptKeySym-
--- John Gibson, Nov  3 1991
        Moved in XptGCValues from XptGCValuesPtr.p
--- John Gibson, Nov  1 1991
        Moved XptWindow, XptScreenPtr and XptXEventPtr defs in from
        corresponding autoloadable conversion procedure files
 */
