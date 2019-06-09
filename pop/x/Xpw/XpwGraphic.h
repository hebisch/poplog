/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwGraphic.h
 * Version:         Revision 5.0
 * Purpose:         Public header file for the XpwGraphic widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1981
 * Notes:
 *  this is used for all versions of the widget.
 */


#ifndef _XpwGraphic_h
#define _XpwGraphic_h

/***********************************************************************
 *
 * Poplog graphics Widget - XpwGraphicWidget
 *
 ***********************************************************************/

/* Parameters:

 Name            Class          RepType     Default Value
 ----            -----          -------     -------------
 colormap        Colormap       Pointer     NULL
 switchCmaps     Boolean        Int         FALSE
 buttonEvent     Callback       Pointer     NULL
 keyboardEvent   Callback       Pointer     NULL
 mouseEvent      Callback       Pointer     NULL
 motionEvent     Callback       Pointer     NULL
 resizeEvent     Callback       Pointer     NULL
 mouseX          MouseLocation  Int         0
 mouseY          MouseLocation  Int         0
 usePrivateGC    Boolean        Boolean     TRUE
 myGC            GC             Pointer     NULL
*/

#ifdef XpwNotInstalled
#include "XpwPixmap.h"
#else
#include <X11/Xpw/XpwPixmap.h>
#endif

/* Resource names used for the poplog graphics widget */

#ifndef XtNcolormap
#define XtNcolormap     "colormap"
#define XtCColormap     "Colormap"
#endif

#define XtNmouseEvent   "mouseEvent"
#define XtNbuttonEvent  "buttonEvent"
#define XtNkeyboardEvent "keyboardEvent"
#define XtNmotionEvent "motionEvent"
#define XtNresizeEvent "resizeEvent"
#define XtNmouseX       "mouseX"
#define XtNmouseY       "mouseY"
#define XtNusePrivateGC "usePrivateGC"
#define XtNmyGC         "myGC"
#define XtNkey          "key"
#define XtNswitchCmaps "switchCmaps"


#define XtCKey          "Key"
#define XtCMouseLocation "MouseLocation"

#ifndef XtSpecificationRelease
/* essentially means XtVersion < 11004 */
#define XtRColormap "Pointer"
#endif

#define XtColormap(widget) ((widget)->core.colormap)

#define ColorAllocNo 0
#define ColorAllocAlmost 1
#define ColorAllocReadOnly 2
#define ColorAllocYes 3

typedef struct _XpwColorList {
    XColor *colors;
    Cardinal num_colors;
    Colormap cmap;
    Boolean read_only;
    Boolean exact;
    struct _XpwColorList *next;
} XpwColorList;

/* members of flags or mode */

/* need to set mode to exact, contiguous, read or read-write */

#define AllocColorNo 0      /* RETURN */
#define AllocColorAlmost 1  /* FLAGS */
#define AllocColorExact 2   /* FLAGS | MODE */
#define AllocColorContig 4  /* FLAGS | MODE */
#define AllocColorNonContig 8   /* FLAGS */
#define AllocColorReadOnly 16   /* FLAGS | MODE */
#define AllocColorReadWrite 32  /* FLAGS | MODE */

#define XpwUnspecifiedPixel -1

typedef struct _XpwGraphicRec *XpwGraphicWidget;
typedef struct _XpwGraphicClassRec *XpwGraphicWidgetClass;
externalref WidgetClass xpwGraphicWidgetClass;


#endif /* _XpwGraphic_h */

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Dec 17 1991 Removed V*maps
--- Jonathan Meyer, Sept 9 1990 Removed some redundant external declarations
--- Jonathan Meyer, Aug 21 1990 Tested for XpwNotInstalled.
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
--- James Goodlet, May 24 1990 - XtRColormap only defined here under R3.
 */
