/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwCompositP.h
 * Version:         Revision 5.0
 * Purpose:         Private header file for the XpwComposite widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1981
 * Documentation:
 * Related Files:   XpwComposit.h XpwComposit.c
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


#ifndef _XpwCompositeP_h
#define _XpwCompositeP_h

#include <X11/CompositeP.h>

#ifdef XpwNotInstalled
#include "XpwComposit.h"
#include "XpwCoreP.h"
#else
#include <X11/Xpw/XpwComposit.h>
#include <X11/Xpw/XpwCoreP.h>
#endif

/* this is directly below the Core in widget heirarchy */

/* New fields for the poplog graphics  widget instance record */

typedef struct {
    XtCallbackList xpwCallback;
    Boolean resize;
    Widget work_area;
   } XpwCompositePart;

/* Full instance record declaration */
typedef struct _XpwCompositeRec {
   CorePart core;
   CompositePart composite;
   XpwCompositePart xpwcomposite;
   } XpwCompositeRec;

/* New fields for the widget class record - there are none.*/
typedef struct {int dummy;} XpwCompositeClassPart;

/* Full class record declaration. */
typedef struct _XpwCompositeClassRec {
   CoreClassPart core_class;
   CompositeClassPart composite_class;
   XpwCompositeClassPart xpwComposite_class;
   } XpwCompositeClassRec;

/* Class pointer. */
externalref XpwCompositeClassRec xpwCompositeClassRec;

#endif /*_XpwCompositeP_h*/

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Aug 21 1990
    Added X11 prefix again, tested for XpwNotInstalled.
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Changed all occuarnces of Pop* variable names to Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
 */
