/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 * File:            C.x/x/Xpw/ClientMessage.c
 * Version:         Revision 5.0
 * Purpose:     Handles ClientMessage events in windows
 * Author:          Jonathan Meyer, Apr  8 1991 (see revisions)
 * Documentation:
 * Related Files:
 */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/ShellP.h>
#include <X11/Xatom.h>

static void DecodeDrop(widget, event, is_load)
Widget widget;
XClientMessageEvent *event;
Bool is_load;
{
    Atom    actual_type;
    int actual_format, return_value;
    unsigned long bytes_after, nitems;
    char    *data;
    Display *display = XtDisplay(widget);

    if (is_load && event->data.l[4] && XGetWindowProperty(display, event->data.l[3],
            event->data.l[4], 0L, 0xfffffff,
            True, XA_STRING, &actual_type,
            &actual_format, &nitems, &bytes_after,
            (unsigned char **)&data) == Success) {
        /* simple Load */
        XtCallActionProc((Widget)widget, "XpwDoDragLoad",
                (XEvent*)event, (char**)&data, 1);
        XFree(data);
    } else {
        /* Not a simple transfer - probably a selection */
        if (is_load)
            XtCallActionProc((Widget)widget, "XpwDoDragLoad",
                (XEvent*)event, NULL, 0);
        else
            XtCallActionProc((Widget)widget, "XpwDoDragMove",
                (XEvent*)event, NULL, 0);
    }
}

static int dummy_handler() { return(0); }

static void TakeClientMessage(w, client, event, continue_to_dispatch)
Widget w;
Opaque client;
XEvent *event;
Boolean *continue_to_dispatch;
{
    XWindowAttributes win_attribs;
    if (event->type == ClientMessage) {
        Display *dpy = XtDisplay(w);
        Atom *ptr, protocols, message_type, atom;
        protocols = XInternAtom(dpy, "WM_PROTOCOLS", FALSE);
        message_type = event->xclient.message_type;
        *continue_to_dispatch = FALSE;

        /* WM_TAKE_FOCUS action */
        atom = XInternAtom(dpy, "WM_TAKE_FOCUS", FALSE);
        if (message_type == protocols &&
                (event->xclient.data.l[0] == atom ||
                    event->xclient.data.s[0] == atom))
          { Window win = XtWindow(w);
            XGetWindowAttributes(dpy, win, &win_attribs);
            /* Only take the focus if the window is mapped in the first
                place -- unfortunately, the WM may unmap the window before the
                request is processed, so we redefine the error handler to
                ignore errors */
            if (win_attribs.map_state == IsViewable)
              { int (*old)() = XSetErrorHandler((XErrorHandler)dummy_handler);
                if (event->xclient.data.l[0] == atom)
                    XSetInputFocus(dpy, win, RevertToParent,
                                                    event->xclient.data.l[1]);
                else
                    XSetInputFocus(dpy, win, RevertToParent, CurrentTime);
                XSync(dpy, FALSE);
                XSetErrorHandler(old);
              }
            return;
        }


        /* WM_DELETE_WINDOW action */
        atom = XInternAtom(dpy, "WM_DELETE_WINDOW", FALSE);
        if (message_type == protocols &&
                (event->xclient.data.l[0] == atom ||
                event->xclient.data.s[0] == atom)) {
            XtCallActionProc(client, "XpwDeleteWindow", event, NULL, 0);
            return;
        }


        atom = XInternAtom(dpy, "DEC_WM_TAKE_FOCUS", True);
        /* DEC_WM_TAKE_FOCUS action: ignored */
        if (atom != None && message_type == atom) return;

        /* XV_DO_DRAG_LOAD action */
        atom = XInternAtom(dpy, "XV_DO_DRAG_LOAD", True);
        if (atom != None && message_type == atom) {
            DecodeDrop(client, event, True);
            return;
        }


        /* XV_DO_DRAG_MOVE action */
        atom = XInternAtom(dpy, "XV_DO_DRAG_MOVE", True);
        if (atom != None && message_type == atom) {
            DecodeDrop(client, event, False);
            return;
        }

#ifdef DEBUG
        /* Unknown - print out a message */
        printf("Xpw warning: unknown ClientMessage: %i %i %i %i %i %i %i\n",
            event->xclient.message_type,
            event->xclient.data.l[0],
            event->xclient.data.l[1],
            event->xclient.data.l[2],
            event->xclient.data.l[3],
            event->xclient.data.s[0],
            event->xclient.data.b[0]);
#endif
    }
}

static void XpwDoDragLoadAction(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{   /* ignore */
}

static void XpwDoDragMoveAction(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{   /* ignore */
}

static void MapAction(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    TopLevelShellWidget sw = (TopLevelShellWidget)w;
    if (XtIsSubclass(w, topLevelShellWidgetClass))
            sw->topLevel.iconic = (Boolean)*num_params;
}

static Bool appcon_set = FALSE;
static XtActionsRec actions[] = {
    {"XpwDoDragLoad", XpwDoDragLoadAction},
    {"XpwDoDragMove", XpwDoDragMoveAction},
    {"XpwSetShellIconicState", MapAction},
};

static String trans = "\
    <Map>: XpwSetShellIconicState() \n\
    <Unmap>: XpwSetShellIconicState(on) \n";

static XtTranslations transtable;

void XpwSetWMProtocolActions(appcontext)
XtAppContext appcontext;
{
    XtAppAddActions(appcontext, actions, XtNumber(actions));
    transtable = XtParseTranslationTable(trans);
}


void XpwSetWMProtocols (w)
Widget w;
{
    Widget tmp, parent = XtParent(w);
    Atom atoms[2];
    Display *dpy = XtDisplay(w);
    /* only works for a single appcon */
    while (tmp=XtParent(parent)) parent = tmp;
    /* SET WM_PROTOCOLS */
    atoms[0] = XInternAtom(dpy, "WM_TAKE_FOCUS", FALSE);
    atoms[1] = XInternAtom(dpy, "WM_DELETE_WINDOW", FALSE);
    XSetWMProtocols(dpy, XtWindow(parent), atoms, 2);
    /* ADD EVENT HANDLERS */
    XtInsertEventHandler(w, NoEventMask, True,
            (XtEventHandler)TakeClientMessage, (Opaque)w, XtListHead);

    XtInsertEventHandler(parent, NoEventMask, True,
            (XtEventHandler)TakeClientMessage, (Opaque)w, XtListHead);
    XtAugmentTranslations(parent, transtable);
}

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 24 1995
        Put #ifdef DEBUG ... around printf for unknown message in
        TakeClientMessage
--- John Gibson, Nov 20 1991
        Added local redefinition of X error handler in TakeClientMessage while
        doing XSetInputFocus
--- Jonathan Meyer, Jul  4 1991
        Added checks for atoms being None to ensure they exist first
--- Jonathan Meyer, Jun  14 1991
        Added test for window map state to make sure window is not an icon.
--- Jonathan Meyer, Jun  5 1991
        Added workaround for the XtN iconic field not being set by
        window maps and unmaps
--- Jon Meyer 4 June 1991
        Changed DEC_WM_TAKE_FOCUS again - made it do nothing, but allowed
        WM_PROTOCOLS to have shorts or longs for their type argument.
 */
