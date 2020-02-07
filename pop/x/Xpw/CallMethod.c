/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 * File:            C.x/x/Xpw/CallMethod.c
 * Purpose:         Method calling interface for Xpw widgets.
 * Author:          Jonathan Meyer, Aug 23 1990 (see revisions)
 * Documentation:   REF *XpwCallMethod
 * Related Files:   see C.x/x/Xpw/ *
 */

/* using Xpw header files locally, not in <X11/Xpw/...> */
#define XpwNotInstalled

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "XpwCoreP.h"       /* need Core class structure info */


/****************************************************************
 *
 * Method warning messages
 *
 ****************************************************************/

typedef struct {
    String name;
    XpwMethodID id;
} MethodNameList[];

static MethodNameList methodNameList = {
    {"XpwMSetFont", 100},
    {"XpwMSetCursor", 101},
    {"XpwMFreeFont", 102},
    {"XpwMFreeCursor", 103},
    {"XpwMSetColor", 104},
    {"XpwMFreeColor", 105},
    {"XpwMLoadPixmap", 106},
    {"XpwMChangeUserGC", 107},
    {"XpwMDraw", 200},
    {"XpwMDrawFilled", 201},
    {"XpwMDrawPoint", 202},
    {"XpwMDrawLine", 203},
    {"XpwMDrawArc", 204},
    {"XpwMDrawRectangle", 205},
    {"XpwMDrawString", 206},
    {"XpwMDrawImageString", 207},
    {"XpwMClearArea", 208},
    {"XpwMClearWindow", 209},
    {"XpwMDrawPoints", 210},
    {"XpwMDrawLines", 211},
    {"XpwMDrawArcs", 212},
    {"XpwMDrawRectangles", 213},
    {"XpwMDrawSegments", 214},
    {"XpwMFillArc", 215},
    {"XpwMFillArcs", 216},
    {"XpwMFillPolygon", 217},
    {"XpwMFillRectangle", 218},
    {"XpwMFillRectangles", 219},
    {"XpwMCopyFrom", 220},
    {"XpwMCopyTo", 221},
    {"XpwMPutImage", 222},
    {"XpwMGetImage", 223},
    {"XpwMDrawImage", 224},
    {"XpwMCreateImage", 226},
    {"XpwMDrawRoundedRectangle", 228},
    {"XpwMFillRoundedRectangle", 229},
    {"XpwMAllocColorRange", 400},
    {"XpwMFreeColorRange", 406},
    {"XpwMAllocStoreColor", 401},
    {"XpwMSetPixelColor", 402},
    {"XpwMCreateColormap", 403},
    {"XpwMFreeColormap", 404},

    {"XpwMScroll", 600},
    {"XpwMScrollScreenUp", 601},
    {"XpwMScrollScreenDown", 602},
    {"XpwMScrollScreenLeft", 603},
    {"XpwMScrollScreenRight", 604},
    {"XpwMScrollLines", 605},
    {"XpwMScrollTail", 606},
    {"XpwMScrollTails", 607},
    {"XpwMScrollTailLeft", 608},
    {"XpwMScrollTailRight", 609},
    {"XpwMInsertLineAtCursor", 620},
    {"XpwMInsertCharAtCursor", 621},
    {"XpwMDeleteLineAtCursor", 622},
    {"XpwMDeleteCharAtCursor", 623},
    {"XpwMClear", 630},
    {"XpwMClearScreen", 631},
    {"XpwMClearLine", 632},
    {"XpwMClearLines", 633},
    {"XpwMClearTail", 634},
    {"XpwMClearTails", 635},
    {"XpwMClearChar", 636},
    {"XpwMClearLineAtCursor", 637},
    {"XpwMClearTailAtCursor", 638},
    {"XpwMClearCharAtCursor", 639},
    {"XpwMInsert", 650},
    {"XpwMInsertAtCursor", 651},
    {"XpwMWrite", 652},
    {"XpwMWriteLine", 653},
    {"XpwMWriteLines", 654},
    {"XpwMWriteSubstr", 655},
    {"XpwMWriteAtCursor", 656},
    {"XpwMCursorTo", 660},
    {"XpwMCursorUp", 661},
    {"XpwMCursorDown", 662},
    {"XpwMCursorLeft", 663},
    {"XpwMCursorRight", 664},
    {"XpwMBell", 670},
};

/* _XpwMethodWarning()

   _XpwMethodWarning(widget, id, name, type, message)
   Widget widget;
   XpwMethodID id;
   String name, type, message;

   Issues a warning - printing method name if it can be found

*/

void _XpwMethodWarning(widget, id, name, type, message)
Widget widget;
XpwMethodID id;
String name, type, message;
{
    int i;
    static char idstring[4];
    char err_message[500];
    static String params[] = {idstring, NULL};
    Cardinal num_params = 1;
    if (id == 0)
        num_params = 0;
    else {
        sprintf(idstring, "%i",id); /* turn id into string */
        for (i=0; i < XtNumber(methodNameList); i++)
            if (methodNameList[i].id == id) {
                params[1] = methodNameList[i].name;
                num_params++; break;
            };
    }
    if (num_params == 0)
        /* not printing out method ID */
        strcpy(message, err_message);
    else if (num_params == 1)
        /* could not find name of method in table */
        sprintf(err_message, "%s, method ID: %%s", message);
    else
        /* found name, so print it in message */
        sprintf(err_message, "%s, method ID: %%s (%%s)", message);

    XtAppErrorMsg(XtWidgetToApplicationContext(widget),
            name, type, XtCXpwMethodError, err_message,
            params, &num_params);

}


/* _XpwMakeCall()

   XpwMethodRet _XpwMakeCall(proc, args)
   XpwMethodProc proc;
   XpwMethodArg args[];

   Calls the procedure pointed to by proc, with the arguments specified
   in args. should be able to make this less of a cludge - but this will
   do for now.
*/

XpwMethodRet _XpwMakeCall(proc, args)
XpwMethodProc proc;
XpwMethodArg args[];
{
    /* ASSUME TEN ARGS OR LESS ! */
    return((XpwMethodRet)proc(args[0],args[1],args[2],args[3],args[4],
                args[5],args[6],args[7],args[8],args[9]));

}

/****************************************************************
 *
 * Global Method Access routines
 *
 ****************************************************************/

/* No longer needed. A.S. 8 Nov 2003, advised by Andreas Eder
XpwMethodRet XpwCallMethod(gw, method_id, va_alist)
XpwCoreWidget gw;
XpwMethodID method_id;

*/

/* procedure to call Methods of a xpw Widget */
/* Inserted A.S. 8 Nov 2003, advised by Andreas Eder */
XpwMethodRet XpwCallMethod(XpwCoreWidget gw, XpwMethodID method_id, ...)
{
    XpwCoreWidget w=(XpwCoreWidget)gw;
    XpwCoreWidgetClass class =(XpwCoreWidgetClass)XtClass(w);
    register XpwMethod *method;
    register XpwAssocTable *methods_table;
    XpwApplyProc apply_proc;
    XpwMethodRet ret_val=0;
    Boolean ret_set = FALSE;
    int num_methods;
    char str[100], *class_name;
    va_list ap;

    class_name = class->core_class.class_name;

    if (! XtIsSubclass((Widget)w, xpwCoreWidgetClass)) {
        sprintf(str,"%s widgets do not have any methods", class_name);
        _XpwMethodWarning(w, 0, "invalidClass", "xpwCallMethod",str);
        return(0);
    }

    while (class != (XpwCoreWidgetClass)widgetClass) {
        /* for each class structure */
        /* Changed. A.S. 8 Nov 2003, advised by Andreas Eder */
        /* va_start(ap); */
                va_start(ap, method_id);
        /* if there is a method list, and an apply_proc, then */
        if ((methods_table = class->xpwcore_class.methods_table) &&
            (apply_proc = class->xpwcore_class.apply_proc)) {

            /* lookup this method in the methods table */
            method = (XpwMethod *)XpwLookupAssoc( methods_table,
                          (XpwAssocID)method_id);
            if (method) {
                if (!ret_set) {
                    ret_val = apply_proc(w, method, ap);
                    ret_set = TRUE;
                } else (void)apply_proc(w,method,ap);
                if (method->flags & Cut) break;
            }
        }
        va_end(ap);
        class = (XpwCoreWidgetClass)class->core_class.superclass;
    }

    if (ret_set) {
        /* we did find the method - perform a normal return */
        if (w->xpwcore.auto_flush) XFlush(XtDisplay(w));
        return(ret_val);
    } else {
        /* no method found - issue warning */
        sprintf(str, "%s widgets don't respond to specified method",
                class_name);
        _XpwMethodWarning(w, method_id,
                "invalidMethod", "xpwCallMethod",str);
    }
}

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep 11 1992
        Installed changes from JonM to fix bug in XtNautoFlush
 */
