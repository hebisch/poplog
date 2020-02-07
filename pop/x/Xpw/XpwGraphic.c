/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 * File:            C.x/x/Xpw/XpwGraphic.c
 * Version:         Revision 5.0
 * Purpose:         C source code for the XpwGraphicWidget
 * Author:          Jonathan Meyer (see revisions)
 * Date:            11 February 1989, revised 10 April 1989
 */

/* using Xpw header files locally, not in <X11/Xpw/...> */
#define XpwNotInstalled

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <ctype.h>
#include "XpwGraphicP.h"

/****************************************************************
 *
 * Declarations
 *
 ****************************************************************/

/* The following procs are part of the class structure */

static void ClassInit(), Initialize(), Realize(), Resize(), Redisplay(), Destroy();
static Boolean SetValues();

/* Action procedures */

static void NotifyButtonEvent();
static void NotifyMouseEvent();
static void NotifyKeyboardEvent();
static void NotifyMotionEvent();
static void ExtractPosition();
static void NotifyResizeEvent();

/* General procedures */

static XpwMethodRet ApplyMethod();
static void ExtractPosition();

/* External declarations */
extern void _XpwCondUpdateUsersGC();
extern void _XpwRecolorPointer(), _XpwFreeColormap(), _XpwFreeColors();


/****************************************************************
 *
 * Resources
 *
 ****************************************************************/

/* Resource declaration and Initialization of defaults */

#define offset(field) XtOffset(XpwGraphicWidget,xpwgraphic.field)
#define goffset(field) XtOffset(Widget,core.field)

static char XtNkey_string[32];

static XtResource resources[] = {
    {XtNbuttonEvent, XtCCallback, XtRCallback, sizeof(caddr_t),
        offset(button_event), XtRCallback, NULL},
    {XtNmouseEvent, XtCCallback, XtRCallback, sizeof(caddr_t),
        offset(mouse_event), XtRCallback, NULL},
    {XtNkeyboardEvent, XtCCallback, XtRCallback, sizeof(caddr_t),
        offset(keyboard_event), XtRCallback, NULL},
    {XtNmotionEvent, XtCCallback, XtRCallback, sizeof(caddr_t),
        offset(motion_event), XtRCallback, NULL},
    {XtNresizeEvent, XtCCallback, XtRCallback, sizeof(caddr_t),
        offset(resize_event), XtRCallback, NULL},
    {XtNmouseX, XtCMouseLocation, XtRInt, sizeof(int),
        offset(mouse_x), XtRString, "0"},
    {XtNmouseY, XtCMouseLocation, XtRInt, sizeof(int),
        offset(mouse_y), XtRString, "0"},
    {XtNswitchCmaps, XtCBoolean, XtRBoolean, sizeof(Boolean),
        offset(switch_cmaps), XtRString, "FALSE"},
    {XtNmyGC, XtCGC, XtRPointer, sizeof(char *),
        offset(my_gc), XtRImmediate, NULL},
    {XtNkey, XtCKey, XtRString, sizeof(String),
        offset(key), XtRString, XtNkey_string},
};

#undef offset
#undef goffset

/****************************************************************
 *
 * Methods
 *
 ****************************************************************/

/* Methods for graphic widgets are inherited from pixmap widgets */

/* see XpwGraphics.c */

externalref XpwMethod _xpwGraphicMethods[];
externalref int _num_xpwGraphicMethods;


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
   {"notify-resize-event",NotifyResizeEvent},
   {NULL,NULL}};


/****************************************************************
 *
 * Translations
 *
 ****************************************************************/

static char defaultTranslations[] =
    "<BtnDown>: notify-button-event() \n\
    <BtnUp>:    notify-button-event() \n\
     <EnterWindow>: notify-mouse-event() \n\
     <LeaveWindow>: notify-mouse-event() \n\
     <Motion>:      notify-motion-event() \n\
     <ResReq>:  notify-resize-event() \n\
     <KeyPress>:    notify-keyboard-event() \n\
     <KeyRelease>:    notify-keyboard-event()";


/****************************************************************
 *
 * Class structure
 *
 ****************************************************************/

externaldef(xpwgraphicclassrec)
    XpwGraphicClassRec xpwGraphicClassRec = {
    { /* core fields */
    /* superclass       */      (WidgetClass) &xpwPixmapClassRec,
    /* class_name       */      "XpwGraphic",
    /* widget_size      */      sizeof(XpwGraphicRec),
    /* class_initialize */      ClassInit,
    /* class_part_initialize*/  NULL,
    /* class_inited     */      FALSE,
    /* initialize       */      Initialize,
    /* initialize_hook  */      NULL,
    /* realize          */      Realize,
    /* actions          */      actions,
    /* num_actions      */      XtNumber(actions),
    /* resources        */      resources,
    /* resource_count   */      XtNumber(resources),
    /* xrm_class        */      NULLQUARK,
    /* compress_motion  */      FALSE,
    /* compress_exposure*/      FALSE,
    /* compress_enterleave*/    FALSE,
    /* visible_interest */      FALSE,
    /* destroy          */      Destroy,
    /* resize           */      Resize,
    /* expose           */      Redisplay,
    /* set_values       */      SetValues,
    /* set_values_hook  */      NULL,
    /* set_values_almost*/      XtInheritSetValuesAlmost,
    /* get_values_hook  */      NULL,
    /* accept_focus     */      NULL,
    /* version          */      XtVersion,
    /* callback_private */      NULL,
    /* tm_table         */      defaultTranslations,
    /* query_geometry   */      NULL,
#if XtVersion > 11002
    /* display accel    */      XtInheritDisplayAccelerator,
    /* extension        */      NULL
#endif
    },
    { /*xpwcore fields */
    /*methods           */      _xpwGraphicMethods,
    /*num_methods       */      0, /* set at class_init */
    /*apply             */      ApplyMethod,
    /*methods_table     */      XtInheritMethods,
    }
};

externaldef(xpwgraphicwidgetclass)
    WidgetClass xpwGraphicWidgetClass = (WidgetClass) &xpwGraphicClassRec;


/****************************************************************
 *
 * private procedures
 *
 ****************************************************************/

#ifdef DEBUG
static void debug_msg(m)
char *m;
{
    printf("XpwGraphic: %s\n",m);
}
#else
#define debug_msg(a)
#endif


static void ClassInit ()
{
    xpwGraphicClassRec.xpwcore_class.num_methods = _num_xpwGraphicMethods;
}

static void Initialize (request, new)
    Widget request, new;
    {
    XpwGraphicWidget w = (XpwGraphicWidget)new;
    XpwColorList *colors;
    Screen *screen = XtScreen(w);
    w->core.mapped_when_managed = TRUE; /* important - set to false by
                           xpwpixmap */

    debug_msg("Initialize Start");
    /* initialize linked list for allocated colors */

    colors = (XpwColorList *)XtNew(XpwColorList);
    colors->colors=NULL;
    colors->next=NULL;
    w->xpwgraphic.allocated_colors = colors;

    /* set my_gc */
    _XpwCondUpdateUsersGC(xpwGraphicWidgetClass, w);
    w->xpwgraphic.my_gc = w->xpwpixmap.private_gc;

    debug_msg("Initialize End");
}

static Boolean SetValues (gcurrent, grequest, gnew)
    Widget gcurrent, grequest, gnew;
{
    XpwGraphicWidget current = (XpwGraphicWidget) gcurrent,
                     new = (XpwGraphicWidget) gnew;
    GC oldGC = current->xpwcore.users_gc, newGC = new->xpwcore.users_gc;
    Boolean redisplay = FALSE;
    register Display *dpy = XtDisplay(current);
    register Window win = XtWindow(current);
    Screen *screen = XtScreen(current);

    if (XtColormap(new) != XtColormap(current)) {
        if (XtColormap(new))
            XSetWindowColormap(dpy, win, XtColormap(new));
        else
            XSetWindowColormap(dpy, win, DefaultColormapOfScreen(screen));
        _XpwFreeColormap(current);
    }

    _XpwCondUpdateUsersGC(xpwGraphicWidgetClass, new);
    return (redisplay);
}

static void Realize (gw, valueMask, attrs)
     Widget gw;
     XtValueMask *valueMask;
     XSetWindowAttributes *attrs;
{       XpwGraphicWidget w= (XpwGraphicWidget)gw;
    debug_msg("Realize start");
     *valueMask |= CWBitGravity;
     attrs->bit_gravity = ForgetGravity;
     if ((attrs->cursor = w->xpwcore.pointer_shape) !=0)
       { *valueMask |= CWCursor; _XpwRecolorPointer(w); }
     XtCreateWindow( gw, InputOutput, (Visual *)CopyFromParent,
             *valueMask, attrs);
    debug_msg("Realize end");
}


static void Resize (gw)
    Widget gw;
{
    XpwGraphicWidget w= (XpwGraphicWidget)gw;
    /* run pixmap's resize even if unrealized */
    ((WidgetClass)xpwPixmapWidgetClass)->core_class.resize(gw);

    /* don't do this computation if window hasn't been realized yet. */
    if (XtIsRealized(gw)) {
        /* copy the pixmap onto the screen */
        if (w->xpwpixmap.pixmap_status != PixmapOn)
            XtCallCallbacks(gw,XtNxpwCallback,0);
        else {
            Display *dpy = XtDisplay(w);
            Window win = XtWindow(w);
            XCopyArea(dpy, w->xpwpixmap.pixmap, win,
                    w->xpwgraphic.my_gc, 0,0,
                    w->core.width, w->core.height, 0,0);
        }
    }
}

static void ExtractExpose (event, x, y, width, height)
XEvent *event;
int *x, *y, *width, *height;
{
    switch (event->type) {
    case GraphicsExpose:
        *x = event->xgraphicsexpose.x;
        *y = event->xgraphicsexpose.y;
        *width = event->xgraphicsexpose.width;
        *height = event->xgraphicsexpose.height;
        break;
    case Expose:
        *x = event->xexpose.x;
        *y = event->xexpose.y;
        *width = event->xexpose.width;
        *height = event->xexpose.height;
        break;
    }
}
/* ARGSUSED */
static void Redisplay (gw, event, region)
    Widget gw;
    XEvent *event;      /* unused */
    Region region;      /* unused */
{
    XpwGraphicWidget w = (XpwGraphicWidget) gw;
    register Display *dpy = XtDisplay(w);
    register Window win = XtWindow(w);
    int x,y,width,height;
    if (w->xpwpixmap.pixmap_status != PixmapOn) {
        XtCallCallbacks((Widget)w, XtNxpwCallback,0);
        return;
    }

    if (event)
        ExtractExpose(event,&x,&y,&width,&height);
    else {
        /* expose the whole window */
        x = y = 0; width = w->core.width; height = w->core.height;
    }
    XCopyArea(dpy, w->xpwpixmap.pixmap, win,  w->xpwgraphic.my_gc,
        x,y,width,height,x,y);

    debug_msg("Redisplay");

}

static void Destroy (gw)
Widget gw;
{
    XpwGraphicWidget w=(XpwGraphicWidget)gw;
    register Screen *screen = XtScreen(w);
    register Display *dpy = XtDisplay(w);
    XpwColorList *colors=w->xpwgraphic.allocated_colors;
    debug_msg("Destroy start\n");
    while (colors->next)
        _XpwFreeColors(w, colors->next);
    XtFree((char *)colors);
    if (XtColormap(w) && XtColormap(w) != DefaultColormapOfScreen(screen))
        XFreeColormap(dpy, XtColormap(w));
    debug_msg("Destroy end\n");
}

/****************************************************************
 *
 * Action procedures
 *
 ****************************************************************/

/* each of these calls the relevant callback list with the
   right parameters */

static void NotifyButtonEvent (gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
{   long call_data;
    XpwGraphicWidget w = (XpwGraphicWidget) gw;
    call_data = event->xbutton.button;
    w->xpwcore.modifiers = event->xbutton.state;
    if (event->type == ButtonRelease) call_data *= -1;
    ExtractPosition( event, (int *)&(w->xpwgraphic.mouse_x), (int *)&(w->xpwgraphic.mouse_y));
    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNbuttonEvent, (caddr_t)call_data);
}

static void NotifyResizeEvent (gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
{   long call_data;
    call_data = event->type;
    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNresizeEvent, (caddr_t)call_data);
}

static void CheckSwitchCmaps (w, type)
XpwGraphicWidget w;
int type;
{
    Display *dpy=XtDisplay(w);
    Screen *screen=XtScreen(w);
    Colormap cmap = XtColormap(w);

    if ((Boolean)w->xpwgraphic.switch_cmaps) {
        if (type == EnterNotify)
            if (XtColormap(w) != DefaultColormapOfScreen(screen))
                XInstallColormap(dpy, cmap);
        else if (type == LeaveNotify)
            XInstallColormap(dpy, DefaultColormapOfScreen(screen));
        XFlush(dpy);
    }
}

static void NotifyMouseEvent (gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
{   long call_data;
    XpwGraphicWidget w = (XpwGraphicWidget) gw;
    ExtractPosition( event, (int *)&(w->xpwgraphic.mouse_x), (int *)&(w->xpwgraphic.mouse_y));
    call_data = event->type ;
    CheckSwitchCmaps(w, event->type);
    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNmouseEvent, (caddr_t)call_data );
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
    XpwGraphicWidget w = (XpwGraphicWidget) gw;
    ExtractPosition( event, (int *)&(w->xpwgraphic.mouse_x),
         (int *)&(w->xpwgraphic.mouse_y));

    count = XLookupString((XKeyEvent*)event, w->xpwgraphic.key, 15, &key, &compose);

    /* ensure it is null terminated */
    w->xpwgraphic.key[count] = 0;

    if (*num_params == 1)
        process_string(params[0], w->xpwgraphic.key, 15);

    /* get keysym */

    w->xpwcore.modifiers = event->xkey.state;
    if (event->type == KeyRelease) key *=(int)-1;

    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNkeyboardEvent, (caddr_t)key);
}

static void NotifyMotionEvent (gw, event, params, num_params)
Widget gw;
XEvent *event;
String *params;
Cardinal *num_params;
{   long call_data;
    XpwGraphicWidget w = (XpwGraphicWidget) gw;
    ExtractPosition( event, &(w->xpwgraphic.mouse_x), &(w->xpwgraphic.mouse_y));
    w->xpwcore.modifiers = call_data = event->xmotion.state;
    /* notify the clients of the event: */
    XtCallCallbacks(gw, XtNmotionEvent, (caddr_t)call_data );
}

/****************************************************************
 *
 * General procedures
 *
 ****************************************************************/

/* takes an event, and returns the mouse location */

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


static XpwMethodRet ApplyMethod (w, method, args)
XpwGraphicWidget w;
XpwMethod *method;
va_list args;
{   int i, num_args = method->num_args;
    XpwMethodProc proc=method->proc;
    Cardinal flags = method->flags;
    XpwMethodArg arg_list[10];
    XpwMethodRet ret_val;
    int count=0;

    if (w->xpwpixmap.pixmap_status == PixmapOnly) return(0);

    if (flags & RequiresWidget) arg_list[count++]=(XpwMethodArg)w;
    else arg_list[count++]=(XpwMethodArg) XtDisplay(w);

    if (flags & RequiresDrawable) {
        if (!XtWindow(w))
            return 0; /* don't do anything if there is no window yet */
        arg_list[count++]=(XpwMethodArg)XtWindow(w);
    }

    if (flags & RequiresFontSet)
        arg_list[count++]=(XpwMethodArg)w->xpwcore.font_set;

    if (flags & RequiresGC)
        arg_list[count++]=(XpwMethodArg)w->xpwcore.users_gc;

    for (i=count; i<count+num_args; i++)
        arg_list[i] = (XpwMethodArg)va_arg(args, caddr_t);
    ret_val = (XpwMethodRet)_XpwMakeCall(proc, arg_list);
    return(ret_val);
}


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1997
        Added test for RequiresFontSet in ApplyMethod.
--- John Gibson, Jun  2 1995
        Replaced quoted string of spaces in resources with static char array
        XtNkey_string
--- Adrian Howard, Jun  1 1993
        Install fixes from Jon Meyer:
            i) ApplyMethod now checks window exists
            ii) Redisplay now exposes whole window if give -false- as the event
--- John Gibson, Mar  9 1992
        VMS externalref mods
--- Jonathan Meyer, Jan 12 1992
    Removed unnecessary setting of redisplay when the colormap is changed
--- Jonathan Meyer, Dec 17 1991
        Removed V*maps
--- John Gibson, Oct 31 1991
        VMS mods
--- Jonathan Meyer, Sep 25 1991
    Fixed process_string and NotifyKeyboardEvent to be same as
    XpwScrollText widget.
--- Jonathan Meyer, Sep 11 1991
    Removed XtNusePrivateGC
--- Jonathan Meyer, Sept 9 1990
    Fixed a few Xpw global procedure usages (added underscores to start
    of _XpwFreeColors and _XpwFreeColormap)
--- Jonathan Meyer, Aug 24 1990
    Removed CvtStringToColormap - redundant since R4 does this for you
--- Jonathan Meyer, Aug 21 1990
    Added X11 prefix again. Defined XpwNotInstalled.
    Removed some redundant #include's
    Modified NotifyKeyboardEvent to use XLookupString to find string
    associated with keysym. Added process_string and the "string"
    action (see XpwScrText.c)
--- Jonathan Meyer, Aug  8 1990
    Made Redisplay much more intelligent (only redraws parts of window
    that have been exposed). Removed (redundant) RestoreWindow proc.
    Modified Resize to use XCopyArea instead of RestoreWindow. Changed
    compress_exposures to FALSE.
--- Jonathan Meyer, Jul 24 1990
    Tidied up a bit. Added PixmapOnly to XtNpixmapStatus.
--- Andreas Schoter, Jul 23 1990 Changed all pop* identifiers to xpw*
--- Andreas Schoter 16 July 1990
    Replaced all occurances of Pop* variable names with Xpw*
--- Simon Nichols, Jun 19 1990
        Removed "X11/" prefix from #include file names.
 */
