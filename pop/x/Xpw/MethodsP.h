/* --- Copyright University of Sussex 2003. All rights reserved. ----------
 * File:        C.x/x/Xpw/MethodsP.h
 * Version:         Revision 5.0
 * Purpose:         Private header file for widgets that use methods.
 * Author:      Jonathan Meyer (see revisions)
 * Date:        15 January 1990
 * Notes:       Code used internally to implement Methods
 */

#ifndef _XpwMethodP_h
#define _XpwMethodP_h

#ifdef XpwNotInstalled
#include "Methods.h"
#else
#include <X11/Xpw/Methods.h>
#endif

/* Removed by A.S. (8 Nov 2003) Suggested by Andreas Eder */
/* #include <varargs.h> */
/* all XpwCore widgets previously used varargs interface */

/* replaced with this */
#include <stdarg.h>   /* all XpwCore widgets use stdarg interface */


/* structures and types for methods */

typedef caddr_t XpwMethodArg;

typedef XpwMethodRet (*XpwMethodProc)();
typedef XpwMethodRet (*XpwApplyProc)();
typedef unsigned int XpwMethodID;

typedef struct _XpwMethod {
    XpwMethodID id;
    XpwMethodProc proc;
    Cardinal num_args;
    Cardinal flags;
} XpwMethod, *XpwMethodList;

extern XpwMethodRet _XpwMakeCall();
extern void _XpwMethodWarning();

#define MAX_ARGS 10
#define METHOD_STRUCT(id, proc, num_args, flags) \
            {id, (XpwMethodProc)proc, num_args, flags}

#define XtInheritMethods ((XpwAssocTable *) _XtInherit)
#define XtInheritApplyProc ((XpwApplyProc) _XtInherit)

/* Flags for methods */

#define NoFlags 0
#define RequiresDisplay  1<<0
#define RequiresWidget   1<<1
#define RequiresDrawable 1<<2
#define RequiresFontSet  1<<3
#define RequiresGC       1<<4
#define Cut              1<<5

#endif

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, 8 Nov 2003
    Converted from varargs to stdargs, as suggested by Andreas Eder
--- John Gibson, Apr  7 1997
    Added RequiresFontSet
--- John Gibson, Dec 19 1993
    Added METHOD_STRUCT
--- Jonathan Meyer, Aug 21 1990
    Renamed MethodsP.h. Tested for XpwNotInstalled.
--- Andreas Schoter, July 16 1990
    replaced all occurances of Pop* variable names with Xpw*
--- James Goodlet, May 24 1990 - switch for X11R4, which doesn't have short
        cardinals.
 */
