/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwTransparent.h
 * Version:         Revision 5.0
 * Purpose:         Public header file for the XpwTransparent widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1991
 */
#ifndef _XpwTransparent_h
#define _XpwTransparent_h

/***********************************************************************
 *
 * Poplog Widget - XpwTransparentWidget
 *
 ***********************************************************************/

#define XtNmouseEvent   "mouseEvent"
#define XtNbuttonEvent  "buttonEvent"
#define XtNkeyboardEvent "keyboardEvent"
#define XtNmotionEvent "motionEvent"
#define XtNmouseX       "mouseX"
#define XtNmouseY       "mouseY"
#define XtNkey          "key"
#define XtCKey          "Key"
#define XtCMouseLocation "MouseLocation"
#define XtNpointerShape "pointerShape"
#define XtNpointerForeground "pointerForeground"
#define XtNpointerBackground "pointerBackground"
#define XtNmodifiers "modifiers"

typedef struct _XpwTransparentRec *XpwTransparentWidget;
typedef struct _XpwTransparentClassRec *XpwTransparentWidgetClass;
externalref WidgetClass xpwTransparentWidgetClass;

#endif /* _XpwTransparent_h */

/* --- Revision History ---------------------------------------------------
--- Ian Rogers, Feb  4 1992  installed
 */
