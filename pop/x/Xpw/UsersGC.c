/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/Xpw/UsersGC.c
 * Version:         Revision 5.0
 > Purpose:         Managing the XtNusersGC graphics context.
 > Author:          Jonathan Meyer, Aug 24 1990 (see revisions)
 > Documentation:   SYSDOC Xpw.imp
 > Related Files:   XpwCore.c XpwPixmap.c XpwGraphic.c XpwScrText.c
 */

/* using Xpw header files locally, not in <X11/Xpw/...> */
#define XpwNotInstalled

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "XpwCoreP.h"

/****************************************************************
 *
 * Global GC-related procedures - private to Xpw.
 *
 ****************************************************************/


/* a global XGCValues structure and valuemask is used to store changes to
   a widgets graphics context. These changes are then made by calling
   _XpwUpdateUsersGC.
*/

externaldef (_xpwgcvalues) XGCValues _xpwGCvalues;
externaldef (_xpwgcvaluemask) XtGCMask  _xpwGCvaluemask;

/* DEBUGGING */

#ifdef DEBUG
static void debug_msg(m)
char *m;
{
    printf("xpwCore: %s\n",m);
}
static void printgc(valuemask, values)
XtGCMask valuemask;
XGCValues *values;
{
    if (valuemask & GCBackground) printf("GC:background%i\n",values->background );
    if (valuemask & GCForeground) printf("GC:foreground%i\n",values->foreground );
    if (valuemask & GCFunction) printf("GC:function %i\n",values->function);
    if (valuemask & GCLineWidth) printf("GC:line_width%i\n",values->line_width) ;
    if (valuemask & GCLineStyle) printf("GC:line_style%i\n",values->line_style) ;
    if (valuemask & GCCapStyle) printf("GC:cap_style%i\n",values->cap_style);
    if (valuemask & GCJoinStyle) printf("GC:join_style%i\n",values->join_style) ;
    if (valuemask & GCFont) printf("GC:fid%i\n",values->font);
    if (valuemask & GCSubwindowMode) printf("GC:subwin%i\n",values->subwindow_mode);
    if (valuemask & GCFillStyle) printf("GC:fill style %i\n",values->fill_style);
    if (valuemask & GCArcMode) printf("GC: arc mode %i\n",values->arc_mode);
    if (valuemask & GCDashList) printf("GC: dashelist %i\n",values->dashes);
    if (valuemask & GCDashOffset) printf("GC: dash offset %i\n",values->dash_offset);

}
#else
#define debug_msg(a)
#define printgc(a,b)
#endif

/* _XpwUpdateUsersGC() - update XtNusersGC resource

   _XpwUpdateUsersGC(w)
   Widget w;

   Takes an Xpw widget, a new set of values and a valuemask. It looks to see
   if the widget is sharing a graphics context. If the widget doesn't have a
   graphics context, a new one is created. If it is sharing a graphics
   context, the shared graphics context is released, and a new one is created.
   Otherwise it just changes the current graphics context.

*/


void _XpwUpdateUsersGC(w, valuemask, values)
XpwCoreWidget w;
XGCValues *values;
unsigned long valuemask;
{
    register Display *dpy = XtDisplay(w);
    GC newGC;
    Screen *screen = XtScreen(w);
    Window win = XtWindow(w);
#ifdef DEBUG
    debug_msg("_XpwUpdateUsersGC start");
    printf("_XpwUpdateUsersGC: shared=");
    if (w->xpwcore.shared_gc) printf("TRUE\n");
    else printf("FALSE");
    printf("valuemask = %u\n",valuemask);
    if (w->xpwcore.users_gc) printf("_XpwUpdateUsersGC: usersGC=%i\n",w->xpwcore.users_gc);
#endif
    if (w->xpwcore.users_gc == NULL) {
      w->xpwcore.users_gc = XtGetGC((Widget)w, valuemask, values);
      w->xpwcore.shared_gc = TRUE;
    } else if (w->xpwcore.shared_gc) {
        /*  we used to have a shared GC. Now we must create a non-shared
            GC, copy accross the values from the shared GC, and
            release the shared GC. */
        if (!XtIsRealized((Widget)w)) {
          newGC = XCreateGC(dpy, RootWindowOfScreen(screen),valuemask, values);
        } else {
          newGC = XCreateGC(dpy, win, valuemask, values);
        }
        /* copy fields of GC not specified in valuemask from old GC */
        XCopyGC(dpy, w->xpwcore.users_gc, ~valuemask, newGC);
        XtReleaseGC((Widget)w, w->xpwcore.users_gc);
        w->xpwcore.shared_gc = FALSE;
        w->xpwcore.users_gc = newGC;
    } else {
        XChangeGC(dpy, w->xpwcore.users_gc, valuemask, values);
    }
    printgc(valuemask, values);
    debug_msg("not valuemask:");
    printgc(~valuemask, &(w->xpwcore.users_gc->values));
    debug_msg("updateGC end\n");

}

/* _XpwCondUpdateUsersGC - conditionally updates UsersGC

   _XpwCondUpdateUsersGC(wc, w)
   WidgetClass wc;
   Widget w;

   Calls _XpwUpdateUsersGC if the XtClass(w) == wc. Used by all subclasses
   of XpwCore so that the UserGC is only updated when the final class
   has finished performing changes to _xpwGCvalues in Initialize/SetValues.
*/


void _XpwCondUpdateUsersGC(wc,w)
Widget w;
WidgetClass wc;
{
    if (XtClass(w)==wc && _xpwGCvaluemask)
    _XpwUpdateUsersGC(w, _xpwGCvaluemask, &_xpwGCvalues);
}

/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  9 1992
        VMS externaldef mods
--- Jonathan Meyer, Sep  9 1990 Moved printgc in from XpwCore.c
 */
