/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwPixmap.h
 * Version:         Revision 5.0
 * Purpose:         Public header file for the XpwPixmap widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1981
 * Notes:
 *  this is used for all versions of the widget.
 */


#ifndef _XpwPixmap_h
#define _XpwPixmap_h

/***********************************************************************
 *
 * Poplog Widget - XpwPixmapWidget
 *
 ***********************************************************************/

/* Parameters:

 Name            Class         RepType      Default Value
 ----            -----         -------      -------------

*/

#ifdef XpwNotInstalled
#include "XpwCore.h"
#else
#include <X11/Xpw/XpwCore.h>
#endif

/* Resource names used for the poplog graphics widget */
#ifndef XtNpixmap
#define XtNpixmap "pixmap"
#define XtCPixmap "Pixmap"
#endif

#ifndef XtNfunction
#define XtNfunction "function"
#define XtCFunction "Function"
#define XtRFunction "Function"
#endif

#define XtNusersGCValues "usersGCValues"
#define XtCUsersGCValues "UsersGCValues"
#define XtCLineWidth "LineWidth"
#define XtNlineWidth "lineWidth"
#define XtDefaultFunction "xtdefaultfunction"
#define XtNsubwindowMode "subwindowMode"
#define XtCSubwindowMode "SubwindowMode"
#define XtRSubwindowMode "SubwindowMode"
#define XtDefaultSubwindow "xtdefaultsubwindow"
#define XtNlineStyle "lineStyle"
#define XtCLineStyle "LineStyle"
#define XtRLineStyle "LineStyle"
#define XtDefaultLineStyle "xtdefaultlinestyle"
#define XtNcapStyle "capStyle"
#define XtCCapStyle "CapStyle"
#define XtRCapStyle "CapStyle"
#define XtDefaultCapStyle "xtdefaultcapstyle"
#define XtNjoinStyle "joinStyle"
#define XtCJoinStyle "JoinStyle"
#define XtRJoinStyle "JoinStyle"
#define XtDefaultJoinStyle "xtdefaultjoinstyle"
#define XtNprivateGC "privateGC"
#define XtNpixmapStatus "pixmapStatus"
#define XtCPixmapStatus "PixmapStatus"
#define XtRPixmapStatus "PixmapStatus"
#define XtDefaultPixmapStatus "xtdefaultpixmapstatus"
#define XtNarcMode "arcMode"
#define XtCArcMode "ArcMode"
#define XtRArcMode "ArcMode"
#define XtNfillStyle "fillStyle"
#define XtCFillStyle "FillStyle"
#define XtRFillStyle "FillStlye"
#define XtDefaultFillStyle "xtdefaultfillstyle"
#define XtDefaultArcMode "xtdefaultarcmode"
#define XtNdashes "dashes"
#define XtCDashes "Dashes"
#define XtNdashOffset "dashOffset"
#define XtCDashOffset "DashOffset"
#define XtNtile "tile"
#define XtCTile "Tile"
#define XtNstipple "stipple"
#define XtCStipple "Stipple"
#define XtNtileStipXOrigin "tileStipXOrigin"
#define XtCTileStipXOrigin "TileStipXOrigin"
#define XtNtileStipYOrigin "tileStipYOrigin"
#define XtCTileStipYOrigin "TileStipYOrigin"
#define XtNplaneMask "planeMask"
#define XtCPlaneMask "PlaneMask"
/* added JM 29/7/91 */
#define XtNfillRule "fillRule"
#define XtCFillRule "FillRule"
#define XtRFillRule "FillRule"
#define XtDefaultFillRule "xtdefaultfillrule"
#define XtNclipMask "clipMask"
#define XtCClipMask "ClipMask"
#define XtNclipXOrigin "clipXOrigin"
#define XtCClipXOrigin "ClipXOrigin"
#define XtNclipYOrigin "clipYOrigin"
#define XtCClipYOrigin "ClipYOrigin"

#ifndef XtSpecificationRelease
#define XtUnspecifiedPixmap 0
#define XtRBitmap "Bitmap"
#endif

typedef struct _XpwPixmapRec *XpwPixmapWidget;
/* completely defined in XpwPixmapP.h */
typedef struct _XpwPixmapClassRec *XpwPixmapWidgetClass;
/* completely defined in XpwPixmapP.h */

externalref WidgetClass xpwPixmapWidgetClass;

typedef enum {
    PixmapOn,
    PixmapOff,
    PixmapHasNone,
    PixmapOnly
} PixmapStatus;

/* returns -true- if a pixmap widget's pixmap should be used */
#define HasPixmap(w) \
    (((XpwPixmapWidget)(w))->xpwpixmap.pixmap_status == PixmapOn || \
    ((XpwPixmapWidget)(w))->xpwpixmap.pixmap_status == PixmapOnly)

#endif /* _XpwPixmap_h */



/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun  1 1993
        Added HasPixmap
--- Jonathan Meyer, Jul 29 1991
    Added XtNclipMask, XtNclip?Origin, XtN fillRule
    Renamed ?ileStip?origin ?ileStip?Origin
--- Jonathan Meyer, Aug 22 1990
    Added #ifndef XtSpecificationRelease to allow compilation under R3
--- Jonathan Meyer, Aug 21 1990
    Added test for XpwNotInstalled.
--- Jonathan Meyer, Aug 20 1990
    Added arcMode, dashes, dashOffset, tile, stipple, fillStyle,
    tileStipXorigin, tileStipYorigin and planeMask definitions.
--- Jonathan Meyer, Jul 24 1990
    Added PixmapOnly to PixmapStatus enum
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
*/
