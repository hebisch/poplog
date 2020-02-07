/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwTransparent.c
 * Version:         Revision 5.0
 * Purpose:         Basic InputOnly widget
 * Author:          Jonathan Meyer, 15 June 1989 (see revisions)
 * Documentation:
 * Related Files:
 */

/* using Xpw header files locally, not in <X11/Xpw/...> */
#define XpwNotInstalled

#include <ctype.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "XpwTransparentP.h"

static void Realize();
static Boolean SetValues();

static void NotifyButtonEvent();
static void NotifyMouseEvent();
static void NotifyKeyboardEvent();
static void NotifyMotionEvent();
static void ExtractPosition();
static void RecolorPointer();

/***************************************************************************
 *
 * Default values for resource lists
 *
 ***************************************************************************/

static char XtNkey_string[32];

#define offset(field) XtOffset(XpwTransparentWidget,xpwtransparent.field)

static XtResource resources[] = {
    {XtNpointerShape, XtCCursor, XtRCursor, sizeof(Cursor),
        offset(pointer_shape), XtRImmediate, NULL},
    {XtNpointerForeground, XtCForeground, XtRPixel, sizeof(Pixel),
        offset(pointer_foreground), XtRString, XtDefaultForeground},
    {XtNpointerBackground, XtCBackground, XtRPixel, sizeof(Pixel),
        offset(pointer_background), XtRString, XtDefaultBackground},
    {XtNmodifiers, XtCParameter, XtRInt, sizeof(int),
        offset(modifiers), XtRImmediate, 0},
    {XtNmouseX, XtCMouseLocation, XtRInt, sizeof(int),
        offset(mouse_x), XtRImmediate, 0},
    {XtNmouseY, XtCMouseLocation, XtRInt, sizeof(int),
        offset(mouse_y), XtRImmediate, 0},
    {XtNbuttonEvent, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(button_event), XtRCallback, NULL},
    {XtNmouseEvent, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(mouse_event), XtRCallback, NULL},
    {XtNkeyboardEvent, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(keyboard_event), XtRCallback, NULL},
    {XtNmotionEvent, XtCCallback, XtRCallback, sizeof(XtPointer),
        offset(motion_event), XtRCallback, NULL},
    {XtNkey, XtCKey, XtRString, sizeof(String),
        offset(key), XtRString, XtNkey_string},
};

/****************************************************************
 *
 * Actions
 *
 ****************************************************************/

static XtActionsRec actions[] =
{  {"notify-button-event", NotifyButtonEvent},
   {"notify-mouse-event", NotifyMouseEvent},
   {"string", NotifyKeyboardEvent},
   {"notify-keyboard-event", NotifyKeyboardEvent},
   {"notify-motion-event", NotifyMotionEvent},
   {NULL,NULL}};

static char defaultTranslations[] =
    "<BtnDown>: notify-button-event() \n\
    <BtnUp>:    notify-button-event() \n\
     <EnterWindow>: notify-mouse-event() \n\
     <LeaveWindow>: notify-mouse-event() \n\
     <Motion>:      notify-motion-event() \n\
     <KeyPress>:    notify-keyboard-event() \n\
     <KeyRelease>:    notify-keyboard-event()";

externaldef(xpwtransparentclassrec)
    XpwTransparentClassRec xpwTransparentClassRec = {
  {
    /* superclass         */    (WidgetClass) &widgetClassRec,
    /* class_name         */    "XpwTransparent",
    /* size               */    sizeof(XpwTransparentRec),
    /* Class Initializer  */    NULL,
    /* class_part_initialize*/  NULL,
    /* Class init'ed ?    */    FALSE,
    /* initialize         */    NULL,
    /* initialize_notify    */  NULL,
    /* realize            */    Realize,
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
    /* resize             */    XtInheritResize,
    /* expose             */    NULL,
    /* set_values         */    SetValues,
    /* set_values_hook      */  NULL,
    /* set_values_almost    */  XtInheritSetValuesAlmost,
    /* get_values_hook      */  NULL,
    /* accept_focus       */    NULL,
    /* intrinsics version */    XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table         */      defaultTranslations,
    /* query_geometry       */  NULL,
#if XtVersion > 11002
    /* display accel    */      XtInheritDisplayAccelerator,
    /* extension        */      NULL,
#endif
  },{
    /* extension        */  NULL
  }
};

externaldef(xpwtransparentwidgetclass)
    WidgetClass xpwTransparentWidgetClass = (WidgetClass) &xpwTransparentClassRec;

static void Realize (gw, valueMask, attrs)
    Widget gw;
    XtValueMask *valueMask;
    XSetWindowAttributes *attrs;
{       XpwTransparentWidget w=(XpwTransparentWidget)gw;
    if (attrs->cursor = w->xpwtransparent.pointer_shape)
      { *valueMask |= CWCursor; RecolorPointer(w); }

    /* clear these fields */
    w->core.border_width = 0;
    w->core.background_pixmap = XtUnspecifiedPixmap;
    w->core.border_pixmap = XtUnspecifiedPixmap;
    w->core.colormap = 0;

    /* only ever look at these: */

    *valueMask &= (CWWinGravity | CWEventMask | CWDontPropagate |
            CWOverrideRedirect | CWCursor);

    /* give me an input only window ! */
    w->core.window = (Window)XCreateWindow(XtDisplay(w),
            XtWindow(XtParent(w)),
            (int)w->core.x, (int)w->core.y,
            (int)w->core.width, (int)w->core.height, 0, 0,
            InputOnly, NULL, *valueMask, attrs);
}

static Boolean SetValues (gcurrent, grequest, gnew)
Widget gcurrent, grequest, gnew;
{
    XpwTransparentWidget current = (XpwTransparentWidget) gcurrent;
    XpwTransparentWidget new = (XpwTransparentWidget) gnew;
    Display *dpy = XtDisplay(gcurrent);

    /* clear these fields */
    new->core.border_width = 0;
    new->core.background_pixmap = XtUnspecifiedPixmap;
    new->core.border_pixmap = XtUnspecifiedPixmap;
    new->core.colormap = 0;

    if (new->xpwtransparent.pointer_shape != current->xpwtransparent.pointer_shape)
      { if (!new->xpwtransparent.pointer_shape)
            XUndefineCursor(dpy, XtWindow(new));
        else
          {     RecolorPointer(new);
            XDefineCursor(dpy, XtWindow(new), new->xpwtransparent.pointer_shape);
          }
      }
    else if (new->xpwtransparent.pointer_shape &&
       (new->xpwtransparent.pointer_foreground != current->xpwtransparent.pointer_foreground
     || new->xpwtransparent.pointer_background != current->xpwtransparent.pointer_background)
    )
        RecolorPointer(new);

    return (FALSE);
}

static void RecolorPointer(w)
XpwTransparentWidget w;
 {
    Display *dpy = XtDisplay(w);
    XColor colordefs[2];        /* 0 is foreground, 1 is background */

    colordefs[0].pixel = w->xpwtransparent.pointer_foreground;
    colordefs[1].pixel = w->xpwtransparent.pointer_background;
    XQueryColors(dpy, DefaultColormapOfScreen(XtScreen(w)), colordefs, 2);
    XRecolorCursor(dpy, w->xpwtransparent.pointer_shape, colordefs, colordefs+1);
 }

static void ExtractPosition (event, x, y )
    XEvent *event;
    int *x, *y;         /* RETURN */
{
    switch( event->type ) {
      case MotionNotify:
        *x = event->xmotion.x;   *y = event->xmotion.y;   break;
      case ButtonPress:
      case ButtonRelease:
        *x = event->xbutton.x;   *y = event->xbutton.y;   break;
      case KeyPress:
      case KeyRelease:
        *x = event->xkey.x;      *y = event->xkey.y;      break;
      case EnterNotify:
      case LeaveNotify:
        *x = event->xcrossing.x; *y = event->xcrossing.y; break;
      default:
        *x = 0; *y = 0;
    }
}

static void NotifyButtonEvent (gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
{   long call_data;
    XpwTransparentWidget w = (XpwTransparentWidget) gw;
    call_data = event->xbutton.button;
    w->xpwtransparent.modifiers = event->xbutton.state;
    if (event->type == ButtonRelease) call_data *= -1;
    ExtractPosition( event, (int *)&(w->xpwtransparent.mouse_x), (int *)&(w->xpwtransparent.mouse_y));
    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNbuttonEvent, (XtPointer)call_data);
}

static void NotifyMouseEvent (gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
{   long call_data;
    XpwTransparentWidget w = (XpwTransparentWidget) gw;
    ExtractPosition( event, (int *)&(w->xpwtransparent.mouse_x), (int *)&(w->xpwtransparent.mouse_y));
    call_data = event->type ;
    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNmouseEvent, (XtPointer)call_data );
}

static void process_string(src, dst, len)
String src, dst;
int len;
{
    if (src[0] == 0) dst[0] = 0;
    else if (src[0] == '0' && src[1] == 'x' && src[2] != '\0') {
        /* turn 0x?? to a string containing that hex character */
        char c, *p;
        dst[0] = dst[1] = dst[2] = 0;
        for (p = src+2; (c = *p); p++) {
            dst[0] *= 16;
            if (isupper(c)) c = tolower(c);
            if (c >= '0' && c <= '9')
                dst[0] += c - '0';
            else if (c >= 'a' && c <= 'f')
                dst[0] += c - 'a' + 10;
            else break;
        }
    } else {
        /* scan string looking for \? sequences */
        char *s, *d, c = -1, *dst_lim = dst+len;
        for (s = src, d = dst; c != 0 && d < dst_lim; ) {
            c = *s++;
            if (c == '\\') {
                c = *s++;
                if (islower(c)) c ^= 32;
                if (c >= '@' && c <= '[') c -= '@';
            };
            *d++ = c;
        }
    }
}

static void NotifyKeyboardEvent (gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
{   /* converts keycode to keysym */
    KeySym key; unsigned int modifiers_return, count = 14;
    XComposeStatus compose;
    XpwTransparentWidget w = (XpwTransparentWidget) gw;
    ExtractPosition( event, (int *)&(w->xpwtransparent.mouse_x),
         (int *)&(w->xpwtransparent.mouse_y));

    count = XLookupString((XKeyEvent *)event, w->xpwtransparent.key, 15, &key, &compose);

    /* ensure it is null terminated */
    w->xpwtransparent.key[count] = 0;

    if (*num_params == 1)
        process_string(params[0], w->xpwtransparent.key, 15);

    /* get keysym */

    w->xpwtransparent.modifiers = event->xkey.state;
    if (event->type == KeyRelease) key *=(int)-1;

    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNkeyboardEvent, (XtPointer)key);
}

static void NotifyMotionEvent (gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
{   long call_data;
    XpwTransparentWidget w = (XpwTransparentWidget) gw;
    ExtractPosition( event, &(w->xpwtransparent.mouse_x), &(w->xpwtransparent.mouse_y));
    w->xpwtransparent.modifiers = call_data = event->xmotion.state;
    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNmotionEvent, (XtPointer)call_data );
}

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  2 1995
        Replaced quoted string of spaces in resources with static char array
        XtNkey_string
--- Ian Rogers, Feb  4 1992 installed
 */
