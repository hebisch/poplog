/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwCoreP.h
 * Version:         Revision 5.0
 * Purpose:         Private header file for the XpwCore widget.
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1989
 * Documentation:   REF *XpwCore
 * Related Files:   XpwCore.h XpwCore.c
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

#ifndef _XpwCoreP_h
#define _XpwCoreP_h

#include <X11/CoreP.h> /* this is directly below Core in widget heirarchy */

#ifdef XpwNotInstalled
#include "XpwCore.h"
#include "MethodsP.h" /* all XpwCore widgets use Methods */
#include "Assoc.h"   /* and Assoc Tables */
#else
#include <X11/Xpw/XpwCore.h>
#include <X11/Xpw/MethodsP.h>
#include <X11/Xpw/Assoc.h>
#endif

/* New fields for the instance record */

typedef struct {
    XFontStruct *font;
    XFontSet font_set;
    XtCallbackList xpw_callback;
    GC users_gc;
    int shared_gc; /* used to note whether Context is shared or unique */
    Boolean auto_flush; /* determines whether to flush after each method*/
    Pixel foreground_pixel;
    Cursor pointer_shape;
    unsigned int modifiers;
    Pixel pointer_foreground, pointer_background;
   } XpwCorePart;

/* Full instance record declaration */
typedef struct _XpwCoreRec {
   CorePart core;
   XpwCorePart xpwcore;
} XpwCoreRec;

/* Class structure */

/* New fields for the widget class record -*/
typedef struct {
    XpwMethodList methods;
    Cardinal num_methods;
    XpwApplyProc apply_proc;
    XpwAssocTable *methods_table;
} XpwCoreClassPart;

/* Full class record declaration. */
typedef struct _XpwCoreClassRec {
   CoreClassPart core_class;
   XpwCoreClassPart xpwcore_class;
} XpwCoreClassRec;

/* Class pointer. */
externalref XpwCoreClassRec xpwCoreClassRec;

#endif /* _XpwCoreP_h */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1997
    Added font_set field to XpwCorePart
--- Adrian Howard, Sep 11 1992
    Installed fixes from JonM for XtNautoFlush
--- John Gibson, Aug  2 1991
     Added pointer_foreground, pointer_background
--- Jonathan Meyer, Aug 21 1990
    Added "X11/" prefix again. Tested for XpwNotInstalled symbol.
--- Jonathan Meyer, Aug 15 1990 Added tile_foreground
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
 */
