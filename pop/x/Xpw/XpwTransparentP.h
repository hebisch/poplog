/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwTransparentP.h
 * Version:         Revision 5.0
 * Purpose:         Private header file for the XpwTransparent widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1989
 * Documentation:   REF *XpwTransparent
 * Related Files:   XpwTransparent.h XpwTransparent.c
 */
#ifndef _XpwTransparentP_h
#define _XpwTransparentP_h

#include <X11/CoreP.h> /* this is directly below Core in widget heirarchy */
#include "XpwTransparent.h"

/* New fields for the instance record */

typedef struct {
    Cursor pointer_shape;
    Pixel pointer_foreground, pointer_background;
    unsigned int modifiers;
    int mouse_x, mouse_y;
    XtCallbackList button_event;
    XtCallbackList mouse_event;
    XtCallbackList keyboard_event;
    XtCallbackList motion_event;
    String key;
  } XpwTransparentPart;

/* Full instance record declaration */
typedef struct _XpwTransparentRec {
   CorePart core;
   XpwTransparentPart xpwtransparent;
} XpwTransparentRec;

/* Class structure */

/* New fields for the widget class record -*/
typedef struct {
    XtPointer extension;
} XpwTransparentClassPart;

/* Full class record declaration. */
typedef struct _XpwTransparentClassRec {
   CoreClassPart core_class;
   XpwTransparentClassPart xpwtransparent_class;
} XpwTransparentClassRec;

/* Class pointer. */
externalref XpwTransparentClassRec xpwTransparentClassRec;

#endif /* _XpwTransparentP_h */

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Feb  4 1992 installed
 */
