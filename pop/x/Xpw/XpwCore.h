/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwCore.h
 * Version:         Revision 5.0
 * Purpose:         Public header file for the XpwCore widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1989
 */
#ifndef _XpwCore_h
#define _XpwCore_h

/***********************************************************************
 *
 * Poplog Core Widget - XpwCoreWidget
 *
 ***********************************************************************/

#include <X11/Intrinsic.h>  /* for XtSpecificationRelease */
#ifdef XpwNotInstalled
#include "Methods.h"      /* methods for XpwCore widgets + descendants */
#else
#include <X11/Xpw/Methods.h>
#endif

/* Resources:

 Name            Class         RepType      Default Value
 ----            -----         -------      -------------
 width           Width         Dimension    200
 height          Height        Dimension    100
 foreground      Foreground    Pixel        XtDefaultForeground
 xpwCallback     Callback      Pointer      NULL
 font            Font          FontStruct   XtDefaultFont
 usersGC         GC            Pointer      NULL
 autoFlush       Boolean       Boolean      TRUE
 pointerShape       Cursor     Cursor       None
 modifiers       Parameter     Int          None
*/

/* define the resource names that are specific to the Poplog Core widget */

#define XtCGC "GC"
#define XtRGC "GC"
#define XtNxpwCallback  "xpwCallback"
#define XtNusersGC "usersGC"
#define XtNautoFlush "autoFlush"
#define XtCAutoFlush "AutoFlush"
#define XtNpointerShape "pointerShape"
#define XtNpointerForeground "pointerForeground"
#define XtNpointerBackground "pointerBackground"
#define XtNmodifiers "modifiers"

typedef struct _XpwCoreRec *XpwCoreWidget;
typedef struct _XpwCoreClassRec *XpwCoreWidgetClass;

externalref WidgetClass xpwCoreWidgetClass;

#endif /* _XpwCore_h */

/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug  2 1991
    Added XtNpointerForeground & XtNpointerBackground
--- Jonathan Meyer, Aug 21 1990
    Added "X11/" prefix again. Tested for XpwNotInstalled.
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Changed all occurances of Pop* variable names to Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
 */
