/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 * File:            C.x/x/Xpw/ExposeEvent.c
 * Purpose:         Generates a synthetic exposure event in a widget
 * Author:          Jonathan Meyer, Jan 12 1992 (see revisions)
 * Documentation:
 * Related Files:
 */

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>

/* This is used to force a widget to redraw part of itself, without the
   annoying flash that results from using XClearArea with a last argument
   of -False-. See Graphics.c and XpwGraphic.c for usage.
*/

#define max(a,b) ((int)(a) > (int)(b) ? (a) : (b))

void _XpwSyntheticExposeEvent(widget, x, y, width, height)
Widget widget;
int x, y, width, height;
{
    XExposeEvent    event;

    if (x > (int)widget->core.width || y > (int)widget->core.height)
        return;

    /* zero width/height extends to right/bottom of widget */
    if (!width)  width = widget->core.width - x;
    else width = max(widget->core.width - x, width);

    if (!height) height = widget->core.height - y;
    else max(widget->core.height-y, height);

    /* build exposure event structure */

    event.type = Expose;
    event.send_event = 1;
    event.display = XtDisplay(widget);
    event.window = XtWindow(widget);
    event.x = x;
    event.y = y;
    event.width = width;
    event.height = height;
    event.count = 0; /* no more exposure events to come */

    XSendEvent(     XtDisplay(widget),      /* display */
            XtWindow(widget),       /* window to send event to */
            True,                   /* propagate */
            ExposureMask,
            (XEvent *) &event);
}


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 17 1993
        Added casts to stop ANSI warning messages
 */
