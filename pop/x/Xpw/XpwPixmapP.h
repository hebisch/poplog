/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 * File:        C.x/x/Xpw/XpwPixmapP.h
 * Version:     Revision 5.0
 * Purpose:     Private header file for the XpwPixmap widget.
 * Author:      Jonathan Meyer (see revisions)
 * Date:        11 February 1989
 * Documentation:   REF *XpwPixmap
 * Related Files:   XpwPixmap.h XpwPixmap.c xpwgraphics.p
 */


/*
 * Notes:
 *
 *  This contains all of the private declarations.
 *  It is organised as follows:
 *      o  new fields for instances of the XpwComposite - a structure.
 *      o  declaration of the full instance record
 *      o  new fields for the class XpwComposite - a structure.
 *      o  declaration of the full class record.
 */

#ifndef _XpwPixmapP_h
#define _XpwPixmapP_h

#ifdef XpwNotInstalled
#include "XpwPixmap.h"
#include "XpwCoreP.h"
#else
#include <X11/Xpw/XpwPixmap.h>
#include <X11/Xpw/XpwCoreP.h>
#endif

/* this is directly below the Core in widget heirarchy */

/* New fields for the poplog graphics  widget instance record */

typedef struct {
     GC private_gc;     /* private for Pixmap widgets. */
     Pixmap pixmap; /* used to save the window */
     PixmapStatus pixmap_status; /* PixmapOn, PixmapOff, PixmapHasNone */
   } XpwPixmapPart;


/* Full instance record declaration */
typedef struct _XpwPixmapRec {
   CorePart core;
   XpwCorePart xpwcore;
   XpwPixmapPart xpwpixmap;
   } XpwPixmapRec;

/* New fields for the widget class record - there are none.*/
typedef struct {int dummy;} XpwPixmapClassPart;

/* Full class record declaration. */
typedef struct _XpwPixmapClassRec {
   CoreClassPart core_class;
   XpwCoreClassPart xpwcore_class;
   XpwPixmapClassPart xpwpixmap_class;
   } XpwPixmapClassRec;

/* Class pointer. */
externalref XpwPixmapClassRec xpwPixmapClassRec;

#endif /* _XpwPixmapP_h */


/* --- Revision History ----------------------------------------------------
--- Jonathan Meyer, Aug 21 1990
    Added test for XpwNotInstalled.
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
*/

/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 31 1991
    VMS mods
 */
