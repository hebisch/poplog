/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwComposit.h
 * Version:         Revision 5.0
 * Purpose:         Public Header file for XpwComposite widget class
 * Author:          Jonathan Meyer, 15 June 1989 (see revisions)
 * Documentation:   REF *XpwComposite
 * Related Files:
 */
#ifndef _XpwComposite_h
#define _XpwComposite_h

/***********************************************************************
 *
 * XpwComposite Widget
 *
 ***********************************************************************/


typedef struct _XpwCompositeRec *XpwCompositeWidget;
typedef struct _XpwCompositeClassRec *XpwCompositeWidgetClass;

#define XtNxpwCallback "xpwCallback"
#define XtNmaxSize "maxSize"
#define XtCMaxSize "MaxSize"
#define XtNworkArea "workArea"
#define XtCWorkArea "WorkArea"

externalref WidgetClass xpwCompositeWidgetClass;

#endif /*_XpwComposite_h*/
/* DON'T ADD STUFF AFTER THIS #endif */


/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Jun 27 1991 Added WorkArea
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter, July 16 1990
    changed all occurances of Pop* variable names to Xpw*
*/
