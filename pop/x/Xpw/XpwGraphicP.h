/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwGraphicP.h
 * Version:         Revision 5.0
 * Purpose:         Private header file for the XpwGraphic widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1981
 * Documentation:
 * Related Files:   XpwGraphic.h XpwGraphic.c xpwgraphics.p
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

#ifndef _XpwGraphicP_h
#define _XpwGraphicP_h

#ifdef XpwNotInstalled
#include "XpwGraphic.h"
#include "XpwPixmapP.h"
#else
#include <X11/Xpw/XpwGraphic.h>
#include <X11/Xpw/XpwPixmapP.h>
#endif

/* this is directly below the Core in widget heirarchy */

#define SEG_BUFF_SIZE       128

/* New fields for the poplog graphics  widget instance record */


typedef struct {
     XtCallbackList button_event;
     XtCallbackList mouse_event;
     XtCallbackList keyboard_event;
     XtCallbackList motion_event;
     XtCallbackList resize_event;
     int use_private_gc;
     GC my_gc; /* points to either privateGC or usersGC */
     int mouse_x, mouse_y;
     String key;
     XpwColorList *allocated_colors;
     Boolean switch_cmaps;
   } XpwGraphicPart;

/* Full instance record declaration */
typedef struct _XpwGraphicRec {
   CorePart core;
   XpwCorePart xpwcore;
   XpwPixmapPart xpwpixmap;
   XpwGraphicPart xpwgraphic;
   } XpwGraphicRec;

/* New fields for the widget class record - there are none.*/
typedef struct {int dummy;} XpwGraphicClassPart;

/* Full class record declaration. */
typedef struct _XpwGraphicClassRec {
   CoreClassPart core_class;
   XpwCoreClassPart xpwcore_class;
   XpwPixmapClassPart xpwpixmap_class;
   XpwGraphicClassPart xpwgraphic_class;
   } XpwGraphicClassRec;

/* Class pointer. */
externalref XpwGraphicClassRec xpwGraphicClassRec;

#endif /* _XpwGraphicP_h */

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Dec 17 1991
        Removed V*maps
--- Jonathan Meyer, Aug 21 1990
    Added "X11" prefix again. Tested for XpwNotInstalled.
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
*/
