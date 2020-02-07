/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwComposit.c
 * Version:         Revision 5.0
 * Purpose:         Augment the Xt Composite widget class for Xpw
 * Author:          Jonathan Meyer, 15 June 1989 (see revisions)
 * Documentation:
 * Related Files:
 */

/* using Xpw header files locally, not in <X11/Xpw/...> */
#define XpwNotInstalled

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "XpwCompositP.h"

/***************************************************************************
 *
 * Default values for resource lists
 *
 ***************************************************************************/


#define offset(field) XtOffset(XpwCompositeWidget,xpwcomposite.field)
#define goffset(field) XtOffset(Widget,core.field)

static XtResource resources[] = {
    {XtNxpwCallback, XtCCallback, XtRCallback, sizeof(caddr_t),
        offset(xpwCallback), XtRCallback, NULL},
#if XtVersion > 11002
    {XtNwidth, XtCWidth, XtRDimension, sizeof(Dimension),
        goffset(width), XtRString, "200"},
    {XtNheight, XtCHeight, XtRDimension, sizeof(Dimension),
        goffset(height), XtRString, "100"},
#else
    {XtNwidth, XtCWidth, XtRInt, sizeof(int),
        goffset(width), XtRString, "200"},
    {XtNheight, XtCHeight, XtRInt, sizeof(int),
        goffset(height), XtRString, "100"},
#endif
    {XtNmaxSize, XtCMaxSize, XtRBoolean, sizeof(Boolean),
        offset(resize), XtRImmediate, (caddr_t)TRUE},
    {XtNworkArea, XtCWorkArea, XtRWidget, sizeof(Widget),
        offset(work_area), XtRImmediate, (caddr_t)NULL},
};

static void NotifyConfigureEvent(), CompositeResize(), ChangeManaged();
static XtGeometryResult GeometryManager();
static XtGeometryResult CompositeQueryGeometry();

static char defaultTranslations[] =
    "<Configure>:   notify-configure-event()\n";

static XtActionsRec actions[] = {
{"notify-configure-event", NotifyConfigureEvent},
{NULL, NULL}};


externaldef(xpwcompositeclassrec)
    XpwCompositeClassRec xpwCompositeClassRec = {
  {
    /* superclass         */    (WidgetClass) &compositeClassRec,
    /* class_name         */    "XpwComposite",
    /* size               */    sizeof(XpwCompositeRec),
    /* Class Initializer  */    NULL,
    /* class_part_initialize*/  NULL,
    /* Class init'ed ?    */    FALSE,
    /* initialize         */    NULL,
    /* initialize_notify    */  NULL,
    /* realize            */    XtInheritRealize,
    /* actions            */    actions,
    /* num_actions        */    XtNumber(actions),
    /* resources          */    resources,
    /* resource_count     */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/    FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    CompositeResize,
    /* expose             */    NULL,
    /* set_values         */    NULL,
    /* set_values_hook      */  NULL,
    /* set_values_almost    */  XtInheritSetValuesAlmost,
    /* get_values_hook      */  NULL,
    /* accept_focus       */    NULL,
    /* intrinsics version */    XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table         */      defaultTranslations,
    /* query_geometry       */  (XtGeometryHandler)CompositeQueryGeometry,
#if XtVersion > 11002
    /* display accel    */      XtInheritDisplayAccelerator,
    /* extension        */      NULL,
#endif
  },{
    /* geometry_manager   */    (XtGeometryHandler)GeometryManager,
    /* change_managed     */    ChangeManaged,
    /* insert_child   */    XtInheritInsertChild,
    /* delete_child   */    XtInheritDeleteChild,
    /* extension        */  NULL
  },{
    /* extension        */  0
  }
};


externaldef(xpwcompositewidgetclass)
    WidgetClass xpwCompositeWidgetClass = (WidgetClass) &xpwCompositeClassRec;

#define max(a,b) ((int)(a) > (int)(b) ? (a) : (b))

static void SetToMaxSize(composite)
XpwCompositeWidget composite;
{
    Widget child, parent;
    WidgetList children = composite->composite.children;
    int num_children = composite->composite.num_children;
    int i, width = 0, height = 0;
    for (i=0; i<num_children; i++) {
        child = children[i];
        if (child->core.managed) {
            width = max(width,
                child->core.x + 2*child->core.border_width
                + child->core.width);

            height = max(height,
                child->core.y + 2*child->core.border_width
                + child->core.height);
        }
    }

    /* make sure a parent resizes itself as well */

    parent = XtParent((Widget)composite);
    if (!XtParent(parent))
        XtVaSetValues((Widget)parent,
            XtNwidth, (XtArgVal)width,
            XtNheight, (XtArgVal)height,
            NULL);
    else XtVaSetValues((Widget)composite,
            XtNwidth, (XtArgVal)width,
            XtNheight, (XtArgVal)height,
            NULL);

/*
    if (XtMakeResizeRequest ((Widget)composite, width, height,
            &width, &height) == XtGeometryAlmost)
        (void)XtMakeResizeRequest((Widget)composite, width, height,
                    NULL, NULL);

    if (!XtParent(parent))
        if (XtMakeResizeRequest ((Widget)parent, width, height,
            &width, &height) == XtGeometryAlmost)
        (void)XtMakeResizeRequest((Widget)parent, width, height,
                    NULL, NULL);
*/
}

static XtGeometryResult GeometryManager(wid, request, reply)
    Widget wid;
    XtWidgetGeometry *request;
    XtWidgetGeometry *reply;
{
    XpwCompositeWidget composite = (XpwCompositeWidget)wid->core.parent;

    *reply = *request;
    if (request->request_mode & (CWSibling | CWStackMode))
        return(XtGeometryNo);

    if (request->request_mode & CWWidth)
        wid->core.width  = request->width;
    if (request->request_mode & CWHeight)
        wid->core.height  = request->height;
    if (request->request_mode & CWBorderWidth)
        wid->core.border_width = request->border_width;

    if (request->request_mode & CWX)
        wid->core.x = request->x;
    if (request->request_mode & CWY)
        wid->core.y = request->y;

    if (composite->xpwcomposite.resize) SetToMaxSize(composite);
    return(XtGeometryYes);
}

#define RESIZE 0
static void CompositeResize(gw)
Widget gw;
  {
    XpwCompositeWidget w = (XpwCompositeWidget)gw;
    Widget work_area;
    if (work_area = w->xpwcomposite.work_area)
      {
        static XtWidgetGeometry intended, preferred;
        int bw = work_area->core.border_width;
        intended.request_mode = CWWidth | CWHeight;
        intended.width=w->core.width - 2*bw;
        intended.height=w->core.height - 2*bw;

        XtQueryGeometry(work_area, &intended, &preferred);

        XtConfigureWidget(work_area, 0,0,
            preferred.width, preferred.height,
            preferred.border_width);
      }
    if (XtIsRealized(gw))
        XtCallCallbacks(gw, XtNxpwCallback, (caddr_t)RESIZE);
  }

static XtGeometryResult CompositeQueryGeometry(w, req, reply)
XpwCompositeWidget w;
XtWidgetGeometry *req, *reply;
  { Widget work_area = w->xpwcomposite.work_area;
    XtGeometryResult result = XtGeometryYes;

    *reply = *req;

    if (work_area && (req->request_mode & (CWWidth|CWHeight)))
      { XtWidgetGeometry intended, preferred;
        int bw2 = 2*work_area->core.border_width;
        intended.request_mode = preferred.request_mode = 0;
        if (req->request_mode & CWWidth)
          { intended.request_mode |= CWWidth;
            intended.width = req->width - bw2;
          }
        if (req->request_mode & CWHeight)
          { intended.request_mode |= CWHeight;
            intended.height = req->height - bw2;
          }
        result = XtQueryGeometry(work_area, &intended, &preferred);
        reply->width = preferred.width + bw2;
        reply->height = preferred.height + bw2;
      }

    return(result);
  }

static void ChangeManaged(gw)
Widget gw;
{
    XpwCompositeWidget w = (XpwCompositeWidget)gw;
    if (w->xpwcomposite.resize) SetToMaxSize(w);
}

#define CONFIGURE 1
static void NotifyConfigureEvent(gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
{
    XpwCompositeWidget w = (XpwCompositeWidget)gw;
    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNxpwCallback, (XtPointer)CONFIGURE);
/*  if (w->xpwcomposite.resize) SetToMaxSize(w);*/
}

/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 19 1993
        Fixed ANSI warnings
--- John Gibson, Aug 17 1992
        Rewrote the QueryGeometry procedure so it takes the work_area
        borderwidth into account
--- Simon Nichols, Nov 27 1991
        Cast initializing values of query_geometry and geometry_manager
        fields of xpwCompositeClassRec to type XtGeometryHandler.
--- John Gibson, Oct 31 1991
        VMS mods
--- Jonathan Meyer, Jun 27 1991
    Totally rewrote the CompositeResize procedure
--- Jonathan Meyer, Jun 24 1991
    Made SetToMazSize called in more places
--- Jonathan Meyer, May 10 1990
    Added maxSize
--- Jonathan Meyer, Aug 21 1990
    Added X11 prefix again, and defined XpwNotInstalled. Removed some
    redundant #include's.
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter, July 16 1990
    changed all occurances of Pop* variable names to Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
--- James Goodlet, Aug  7 1989 - rewrote the geometry manager to work after
    widget is realised, and to serve reposition as well as resize requests.
    Now returns XtGeometryNo for Sibling and Stack_Mode requests, rather than
    the previous, erroneous XtGeometryYes.
 */
