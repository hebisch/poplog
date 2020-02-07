/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 * File:            C.all/extern/lib/XtPoplog.c
 * Purpose:     Poplog additions to the X toolkit library
 * Author:          Roger Evans, Dec 15 1989 (see revisions)
 * Documentation:
 * Related Files:
 */

#include "c_core.h"
#include <sys/types.h>

#ifndef VMS

#include <errno.h>
extern int errno;   /* for systems which don't have this in <errno.h> */

#endif

#include <stdio.h>
#include <locale.h>
#include <X11/IntrinsicP.h>

#ifdef __NUTC__

/* compiling in the NuTCRACKER environment */
#include <nutc.h>
#include <string.h>
#include <errno.h>
/* No signals expected */
typedef unsigned sigsave_t;
#define BLOCK_SIG_ALL(x) (x = 1)
#define RESTORE_SIG(x)   (x = 0)

#endif


/* == Hard Reference to the Vendor Shell Widget ================= */

#ifndef __NUTC__
externalref WidgetClass vendorShellWidgetClass;
static char *dummy = (char*)&vendorShellWidgetClass;
#endif



/* == Error Handling ============================================ */

#define ERRSTRINGSIZE 1024

/* X_printf - A Poplog equivalent of fprintf(stderr, ...) */

/* Stop printing of error messages via sysprmessage until things are
 * sorted out
 */
static Boolean do_pop_print = False;

#ifdef __STDC__
#include <stdarg.h>

static void
X_printf(String fmt, ...)
{
    va_list args;
    va_start(args, fmt);

#else
#include <varargs.h>

static void
X_printf(va_alist)
va_dcl
{
    String fmt;
    va_list args;
    va_start(args);
    fmt = va_arg(args, String);

#endif

    {
        POPOBJ prmessage;
        if (do_pop_print)
            prmessage = pop_get_ident("Xpt_sys_pr_message");

        if (fmt)
        {   /* get a temporary buffer to print into */
            char string[ERRSTRINGSIZE], c;
            register int i, j, slength;
            /* call the C print formatter */
            vsprintf(string, fmt, args);
            /* test and remove any newline chars from string */
            slength = strlen(string);
            for (i = 0, j = 0; i < slength; i++)
                if ((c = string[i]) != '\n') string[j++] = c;
            /* null terminate string */
            string[j] = 0;
            /* print it */
            if (do_pop_print)
                pop_call(prmessage, string);
            else
                printf(";;; %s\n", string);
        }
        else
        {   /* print a newline */
            if (do_pop_print)
                pop_call(prmessage, NULL);
            else
                printf("\n");
        }
    }

    va_end(args);
}


static char err_buff[512];

/*  Toolkit Error: mishap with the message supplied */
void PopXtError(message)
String message;
{   (void) sprintf(err_buff, "xte: X TOOLKIT ERROR (%s)", message);
    pop_mishap(err_buff);
}



/* Toolkit Warning: just print the message */
void PopXtWarning(message)
String message;
{
    (void) X_printf("WARNING - xtw: X TOOLKIT WARNING (%s)", message);
    (void) X_printf(NULL);
}


#ifndef VMS
#if defined(sun) && !(defined(SVR4) || defined(linux))
static char *_SysErrorMsg (n)
    int n;
{
    globalref char *sys_errlist[];
    globalref int sys_nerr;
    char *s = ((n >= 0 && n < sys_nerr) ? sys_errlist[n] : "unknown error");

    return (s ? s : "no such error");
}
#else
#define _SysErrorMsg strerror
#endif
#endif


/* Higher level X Toolkit error/warnings handlers
    (same as default error handlers except they also show error name/type)
*/

#define XTFORMATSTR "%s: %s -- %s"

static void Err_msg(name,type,class,defaultp,params,num_params, err_p)
    String name,type,class,defaultp;
    String* params;
    Cardinal* num_params;
    void (*err_p)();
{
    char buffer[1000], message[1000];
    XtGetErrorDatabaseText(name, type, class, defaultp, buffer, 1000);
    if (params == NULL || num_params == NULL || *num_params == 0)
    {
        sprintf(message, XTFORMATSTR, type, name, buffer);
        err_p(message);
    }
    else
    {
        String par[10];
        int i, npar = *num_params < 10 ? *num_params : 10;
        for (i = 0; i < npar; i++)
            par[i] = params[i];
        for (i = npar; i < 10; i++)
            par[i] = NULL;
        (void) sprintf(message, buffer, par[0], par[1], par[2], par[3],
            par[4], par[5], par[6], par[7], par[8], par[9]);
        (void) sprintf(buffer, XTFORMATSTR, type, name, message);
        err_p(buffer);
        if (npar != *num_params)
            XtWarning("some arguments in previous message were lost");
    }
}

void PopXtWarningMsg(name,type,class,defaultp,params,num_params)
    String name,type,class,defaultp;
    String* params;
    Cardinal* num_params;
{
    Err_msg(name, type, class, defaultp, params, num_params, XtWarning);
}

void PopXtErrorMsg(name,type,class,defaultp,params,num_params)
    String name,type,class,defaultp;
    String* params;
    Cardinal* num_params;
{
    Err_msg(name, type, class, defaultp, params, num_params, XtError);
}



#define XIOESTRING "xioe: XLIB IO ERROR -- see above"

/* Xlib 'fatal' error: mishap with a helpful message and close the display */
void PopXIOError(dpy)
    Display *dpy;
{
    POPOBJ xio_error_handler;

    (void) X_printf(NULL);

    (void) X_printf("--- XLIB IO ERROR (client killed with KillClient?)");
    (void) X_printf("--- X server: '%s'", XDisplayString (dpy));
#ifndef VMS
    (void) X_printf ("--- Error Code: %d (%s)", errno, _SysErrorMsg (errno));
#endif
    xio_error_handler = pop_get_ident("XIO_sys_error_handler");
    pop_call(xio_error_handler, dpy);
    /*  must go straight back since external code may well try and
        use the display handle... */
    _pop_external_flags &= (PEF_DOING_ABEXIT | PEF_ASYNC_CALLBACK);
    pop_mishap(XIOESTRING);
}



/* Macros used by the printing routine below */
#define XPESTRING "WARNING - xpe: X PROTOCOL ERROR -- %s"
#define FORMAT2STR "--- %s (%s)"
#define GET_TEXT(key, default_str) \
    XGetErrorDatabaseText(dpy, mtype, key, default_str, mesg, BUFSIZ);
#define PRINT_TEXT(arg) \
    sprintf(buffer, "--- %s", mesg); (void) X_printf(buffer, arg);

/* Xlib recoverable error - copied from default handler */
int PopXError(dpy, event)
    Display *dpy;
    XErrorEvent *event;
{
    char buffer[BUFSIZ];
    char mesg[BUFSIZ];
    char number[32];
    char *mtype = "XlibMessage";

    (void) X_printf(NULL);

    /* Print out Error Type */
    XGetErrorText(dpy, event->error_code, mesg, BUFSIZ);
    (void) X_printf(XPESTRING, mesg);

    /* Print out Request Number+type */

    GET_TEXT("MajorCode", "Request Major code %d");
    sprintf(buffer, mesg, event->request_code);

    sprintf(number, "%d", event->request_code);
    XGetErrorDatabaseText(dpy, "XRequest", number, "",  mesg, BUFSIZ);

    (void) X_printf(FORMAT2STR, buffer, mesg);

    /* Minor Request Code */
    GET_TEXT("MinorCode", "Request Minor code %d");
    PRINT_TEXT(event->minor_code);

    /* The Resource involved */
    GET_TEXT("ResourceID", "RequestID 0x%x");
    PRINT_TEXT(event->resourceid);

    /* Serial Number of Error */
    GET_TEXT("ErrorSerial", "Error Serial #%d");
    PRINT_TEXT(event->serial);

    /* Last processed Serial Number */
    GET_TEXT("CurrentSerial", "Current Serial #%d");
    PRINT_TEXT(NextRequest(dpy)-1);

    return(0);
}

/* Copied from default language proc */
String PopXtDefaultLanguageProc(dpy, xnl, closure)
    Display   *dpy; /* unused */
    String     xnl;
    XtPointer  closure; /* unused */
  {
    /* We assume the locale is already set
    if (! setlocale(LC_ALL, xnl))
        XtWarning("locale not supported by C library, locale unchanged");
    */

    if (! XSupportsLocale())
      { XtWarning("locale not supported by Xlib, locale set to C");
        setlocale(LC_ALL, "C");
      }
    if (! XSetLocaleModifiers(""))
        XtWarning("X locale modifiers not supported, using default");

    return setlocale(LC_ALL, NULL); /* re-query in case overwritten */
  }



/* == OLIT Scrolling List Item Wrapper Procedure ================= */

/*
   A Simple C wrapper which calls the OpenLook ScrollingList widget
   XtNapplAddItem function to add a new item to the scrolling list.

   This is needed to convert a scrolling list element from a pointer
   into an actual C structure, which is passed to the widget routine
   on the call stack.
*/

/*
From <Xol/ScrollingL.h> - nb. extra field for OLIT2.5 compatibility.
*/

typedef short       OlDefine;   /* OPEN LOOK non-bitmask #defines*/
typedef unsigned long   OlBitMask;  /* OPEN LOOK bitmask #defines   */

typedef struct _OlListItem {            /* OPEN LOOK list item */
    OlDefine      label_type;
    Opaque        label;
    XImage *      glyph;
    OlBitMask     attr;
    Opaque        tag;
    unsigned char mnemonic;
} OlListItem;

typedef struct _OlListToken *OlListToken;   /* opaque item token */

#define XtNapplAddItem "applAddItem"

OlListToken XpolAddListItem(widget, parent, reference, item)
Widget widget;
OlListToken parent, reference;
OlListItem *item;
{
    OlListToken  (*Addfn)();

    /* Get the add procedure */
    XtVaGetValues(widget, XtNapplAddItem, (XtArgVal)&Addfn, NULL);

    /* Call the add procedure, returning the new item */
    return((OlListToken)(*Addfn)(widget, parent, reference, *item));
}

#undef XtNapplAddItem


/* == WM_PROTOCOL Handler  ======================================= */

#define PROTOCOL_ACTION "XptWMProtocol"

/* Protocol message  handling procedure */
static void ProtoHandler(w, client_data, event, continue_to_dispatch)
Widget w;
Opaque client_data;
XEvent *event;
Boolean *continue_to_dispatch;
{
    Atom protocols;
    Display *dpy;
    String protocol_name;

    /* ignore all but the quit message */
    if (event->type == ClientMessage) {
        dpy = XtDisplay(w);
        protocols = XInternAtom(dpy, "WM_PROTOCOLS", FALSE);
        if (event->xclient.message_type == protocols) {

            /* Call action proc to handle message*/
            protocol_name = XGetAtomName(dpy,event->xclient.data.l[0]);
            XtCallActionProc(w,PROTOCOL_ACTION,event,
                             &protocol_name,1);
            XFree(protocol_name);

            *continue_to_dispatch = False;
        }
    }
}

/*  max number of protocols that can be set
    (currently there are only 3 protocols anyway!) */
#define MAX_WM_PROTOCOLS 32

/*  add protocol handler as event filter for specified widget
    (run on all shell widgets created by Poplog)
*/
void XptAddProtoHandler(w,pcols,pcols_len)
Widget w;
char **pcols;
int pcols_len;
{
    Atom pcol_atoms[MAX_WM_PROTOCOLS];
    Display *dpy = XtDisplay(w);
    Boolean sv = w->core.mapped_when_managed;
    int i;

    /* truncate overlong protocols list */
    if (pcols_len > MAX_WM_PROTOCOLS) pcols_len = MAX_WM_PROTOCOLS;

    /* convert protocls to atoms in buffer */
    for (i=0; i < pcols_len; i++) {
        pcol_atoms[i] = XInternAtom(dpy, *(pcols++), FALSE);
    }

    w->core.mapped_when_managed = FALSE;    /* block mapping by realize */
    XtRealizeWidget(w);
    XSetWMProtocols(dpy, XtWindow(w), pcol_atoms, pcols_len);
    if (sv) {
        /* restore mapped_when_managed */
        w->core.mapped_when_managed = sv;
        /* map real shells explicitly */
        if (XtParent(w) == (Widget)NULL) XtMapWidget(w);
    }

    XtInsertEventHandler(w, (EventMask)NoEventMask, True, ProtoHandler,
        NULL, XtListTail);
}


/* == Actions =================================================== */

/*  default action proc for protocols - respond to WM_DELETE_WINDOW
    by using XtPopdown on widgets that are Popup shells, and
    XtDestroyWidget on widgets that are application shells.
    Ignore everything else.
*/
static void XptWMProtocol(w,event,params,num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    if (*num_params == 1 && strcmp(*params,"WM_DELETE_WINDOW") == 0) {
        if (XtParent(w)) {
            XtPopdown(w);
        } else {
            XtDestroyWidget(w);
        }
    }
}

/*  default action for garbage feedback
    num-params non-zero means we're starting, and arg is what kind of
    gc
*/
XtInputMask XptAppTryEvents();

static void XptGarbageFeedback(w,event,params,num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    if (*num_params == 1)
        XtSetSensitive(w,False);
    else
        XtSetSensitive(w,True);

    /* try and make sure server updates display */
    XSync(XtDisplay(w),False);
    XptAppTryEvents(XtWidgetToApplicationContext(w));
}


/*  add standard poplog actions to an appcontext - called on every
    appcontext created by poplog
*/
static XtActionsRec XptDefaultActions[] = {
    {PROTOCOL_ACTION,   XptWMProtocol},
    {"XptGarbageFeedback",    XptGarbageFeedback}
};
#define NumXptDefaultActions 2

void XptAddAppconActions(appcon)
XtAppContext appcon;
{
    XtAppAddActions(appcon,XptDefaultActions,NumXptDefaultActions);
}


/* == Async AppContexts ================================================ */

#define INT_NBITS sizeof(int)*8

typedef struct
  { XtAppContext appcon;
    POPOBJ       clos;
  } AppEntry;

globaldef AppEntry
    appcon_tab[INT_NBITS],
    *appcon_tab_end = appcon_tab;

#define APPCON_TAB_LIM  (&appcon_tab[INT_NBITS])
#define BITOF(entp)     (1<<(entp-appcon_tab))

static unsigned int appcons_enabled;

/* pop tests this nonzero to determine if there are any appcontexts
   to be waited on */
globaldef unsigned int __pop_appcons_normal;

static int poll_appcons()
  { register AppEntry *entp = appcon_tab;
    register unsigned int mask = appcons_enabled, newmask = mask;
    POPWORD save_external_flags = _pop_external_flags;
    globalref int (*_pop_Xt_poll_deferred)();

    if (mask == 0) return(0);
    if (_pop_in_X_call)
      { _pop_stop_polling(XT_POLL_NUM);
        /*  setting this nonzero causes the external apply mechanism
         *  to call _pop_retry_Xt_poll when _pop_in_X_call reverts to zero
         */
        _pop_Xt_poll_deferred = poll_appcons;
        return(1);
      }

    /* ensure malloc won't take mem from the popsys area */
    _pop_external_flags |= PEF_DO_USER_MALLOC;

    for (; mask != 0; mask >>= 1, entp++)
        if ((mask&1) && XtAppPending(entp->appcon))
          { _pop_add_ast(AST_APP_PENDING, entp->clos);
            newmask &= ~BITOF(entp);    /* disable it */
          };

    _pop_external_flags = save_external_flags;

    mask = appcons_enabled;
    if (!(appcons_enabled = newmask)) _pop_stop_polling(XT_POLL_NUM);
    if (newmask != mask)
      { _pop_do_interrupt();
        return(2);
      }
    else
        return(0);
  }

static AppEntry *set_async_appcon(on, appcon)
Boolean on;
XtAppContext appcon;
  { register AppEntry *entp = appcon_tab, *tabend = appcon_tab_end;
    register unsigned int mask;

    for (mask = 1; entp < tabend; mask <<= 1)
        if ((entp++)->appcon == appcon)
          { register unsigned int enabled = appcons_enabled;
            if (enabled & mask)
                /* currently enabled */
              { if (!on && !(enabled &= ~mask))
                    _pop_set_poll_state(XT_POLL_NUM, NULL);
              }
            else
                /* currently disabled */
              { if (on && ((enabled |= mask) == mask))
                    _pop_set_poll_state(XT_POLL_NUM, poll_appcons);
              }

            appcons_enabled = enabled;
            return(--entp); /* entry addr in table for deleting */
          }

    return(NULL);   /* not in table */
  }

int 
_pop_set_async_appcon(XtAppContext appcon, POPOBJ clos, Boolean normaction)
{
    register AppEntry *curr;
    sigsave_t savesig;
    unsigned int m;

    BLOCK_SIG_ALL(savesig);
    curr = set_async_appcon((Boolean) clos, appcon);

    if (clos != (POPOBJ)NULL) { /* setting */
        if (curr == NULL) {
            if (appcon_tab_end < APPCON_TAB_LIM) { /* space for more */
                (appcon_tab_end++)->appcon = appcon;
                curr = set_async_appcon(TRUE, appcon);
            } else {
                RESTORE_SIG(savesig);
                return(-1);     /* no space left */
            }
        curr->clos = clos;
        m = BITOF(curr);
        if (normaction) {
            __pop_appcons_normal |= m;
        } else {
            __pop_appcons_normal &= ~m;
        }
      }
    } else {
        /* clearing */
        if (curr != NULL) {
            register AppEntry *p;
#define REMOVE_BIT(v)   v = ((v>>1) &~ m) | (v & m)
            m = BITOF(curr)-1;  /* bits below this one */
            REMOVE_BIT(appcons_enabled);
            REMOVE_BIT(__pop_appcons_normal);

            /* remove existing entry by shifting down */
            appcon_tab_end--;
            for (p = curr+1; curr < appcon_tab_end; curr++, p++)
              { curr->appcon = p->appcon;  curr->clos = p->clos; };
        }
    }

    RESTORE_SIG(savesig);
    return(0);
}

POPOBJ _pop_async_appcon_clos()
  { return(appcon_tab_end == appcon_tab ? (POPOBJ)NULL : appcon_tab[0].clos);
  }


/* == Event Handling ============================================ */

extern caddr_t (*_pop_set_Xt_poll())();

#define POP_INTERRUPT   (_pop_signals_pending)
#define POP_ABEXIT      (_pop_external_flags & PEF_DOING_ABEXIT)
#define POP_CONDITION   (POP_INTERRUPT || POP_ABEXIT)

/*  IOWaitStatus values */
#define CONTINUE       -3
#define POLL_TIMEOUT   -2
#define INTERRUPTED    -1
#define INPUT_FIRED     1

#define POLL_INIT       20      /* 1/50 sec */
#define POLL_STEP       10      /* 1/100 sec */
#define POLL_MAX        400     /* 2/5 sec */

static int
    IOWaitStatus,
    poll_time = POLL_INIT;

/* remembers appcon on which last event occurred */
static XtAppContext last_event_appcon;


/*  Private interface to XtAppPending. This version prioritises X
    events over others (to stop XtAppProcessEvent, which prioritises
    timers, sometimes exhibiting to X lockout due to repeated rapidly
    firing timers)
*/

static XtInputMask XptAppPending(appcon)
register XtAppContext appcon;
  { register XtInputMask m = XtAppPending(appcon);
    if (m & XtIMXEvent) m = XtIMXEvent;
    return(m);
  }

/* input callback for input on xt wakeup device etc - flag interrupt */
void _XptDummyInput()
  { _pop_set_xt_wakeup(FALSE);
    if (IOWaitStatus < INTERRUPTED) IOWaitStatus = INTERRUPTED;
  }

static void timeout_callback()
  { if (IOWaitStatus < POLL_TIMEOUT) IOWaitStatus = POLL_TIMEOUT;
  }

/*
 *  Wait-state polling for async appcontexts
 */
static XtAppContext try_appcons(pollingp)
  Boolean *pollingp;
  { register AppEntry *entp = appcon_tab, *tabend = appcon_tab_end;
    register XtAppContext appcon, lastevent_app = NULL, normapp;
    register unsigned int mask = 0, bit;
    Boolean last_ok = FALSE;

    _pop_external_flags |= PEF_RETURN_ABEXIT_ANY;

    for (; entp < tabend; entp++)
      { bit = BITOF(entp);
        appcon = entp->appcon;
        if (__pop_appcons_normal & bit) /* must be at least one such */
          { XtInputMask m;
            normapp = appcon;
            if (last_event_appcon == appcon) last_ok = TRUE;
            while (!POP_ABEXIT && IOWaitStatus == CONTINUE
                    && (m = XptAppPending(appcon)))
              { XtAppProcessEvent(appcon, m);
                /* only set lastevent_app for real events */
                if (IOWaitStatus == CONTINUE) lastevent_app = appcon;
              }
          }
        else if (XtAppPending(appcon))
          { _pop_add_ast(AST_APP_PENDING, entp->clos);
            continue;
          }

        if (mask) *pollingp = TRUE;     /* this is the 2nd, so more than 1 */
        mask |= bit;
      }

    appcons_enabled = mask;
    if (lastevent_app != NULL)
      { if (lastevent_app != last_event_appcon) poll_time = POLL_INIT;
        last_event_appcon = lastevent_app;
      }
    else if (!last_ok)
        last_event_appcon = normapp;

    return(lastevent_app);
  }

void XptPause()
  { XtInputMask m;
    XtIntervalId id;
    XtAppContext waitapp;
    caddr_t (*poll_other)() = _pop_set_Xt_poll(NULL);
    Boolean polling = poll_other != NULL;
#ifdef __NUTC__
    /* no asynchronous wakeup, so must always poll */
    polling = TRUE;
#endif

    __pop_xt_wakeup_struct.XWK_FLAG_ENABLED = TRUE;

    for (;;)
      { IOWaitStatus = CONTINUE;

        if (poll_other) poll_other = (caddr_t (*)()) (*poll_other)();
        if (POP_CONDITION) break;

        if (try_appcons(&polling) || POP_CONDITION || IOWaitStatus != CONTINUE
            || __pop_xt_wakeup_struct.XWK_ON)
            break;

        /* go into (possible) wait */
        waitapp = last_event_appcon;
        if (polling) id = XtAppAddTimeOut(waitapp, poll_time,
                                (XtTimerCallbackProc) timeout_callback, NULL);

        XtAppProcessEvent(waitapp, (m=XptAppPending(waitapp)) ? m : XtIMAll);

        if (polling)
          { if (IOWaitStatus == POLL_TIMEOUT)
              { if (poll_time < POLL_MAX) poll_time += POLL_STEP;
                continue;
              }
            XtRemoveTimeOut(id);
          }

        /* finish off any remaining events */
        while (!POP_CONDITION && IOWaitStatus == CONTINUE
                && (m = XptAppPending(waitapp)))
            XtAppProcessEvent(waitapp, m);

        break;
      }

    _pop_set_xt_wakeup(FALSE);
    _pop_set_Xt_poll(poll_appcons);
  }


#ifdef VMS

static void input_callback()
  { IOWaitStatus = 1; }

int XptReadWait(efnum)
int efnum;      /* event flag number */
  { XtInputMask m;
    XtIntervalId timid;
    XtInputId inid = NULL;
    XtAppContext waitapp, inidapp;
    caddr_t (*poll_other)() = _pop_set_Xt_poll(NULL);
    Boolean polling = poll_other != NULL;

    __pop_xt_wakeup_struct.XWK_FLAG_ENABLED = TRUE;

    for (;;)
      { IOWaitStatus = CONTINUE;

        if (poll_other) poll_other = (caddr_t (*)()) (*poll_other)();
        if (POP_CONDITION) break;

        try_appcons(&polling);
        if (POP_CONDITION || IOWaitStatus != CONTINUE
            || __pop_xt_wakeup_struct.XWK_ON)
            break;

        /* go into (possible) wait */
        waitapp = last_event_appcon;
        if (polling) timid = XtAppAddTimeOut(waitapp, poll_time,
                                (XtTimerCallbackProc) timeout_callback, NULL);

        if (inid && waitapp != inidapp)
            { XtRemoveInput(inid); inid = NULL; }
        if (!inid) inid = XtAppAddInput(inidapp = waitapp, efnum,
                            NULL, (XtInputCallbackProc) input_callback, NULL);

        while (!POP_ABEXIT && IOWaitStatus == CONTINUE)
            XtAppProcessEvent(waitapp, (m=XptAppPending(waitapp)) ? m : XtIMAll);

        if (polling)
          { if (IOWaitStatus == POLL_TIMEOUT)
              { if (poll_time < POLL_MAX) poll_time += POLL_STEP;
                continue;
              }
            XtRemoveTimeOut(timid);
          }

        break;
      }

    if (inid) XtRemoveInput(inid);
    _pop_set_xt_wakeup(FALSE);
    _pop_set_Xt_poll(poll_appcons);

    /* return number of devices ready (i.e. 1) or -1 if interrupted */
    return(IOWaitStatus > 0 ? IOWaitStatus : -1);
  }

#endif /* VMS */

#ifdef UNIX

/* fd_set, FD_SET, FD_ISSET, FD_ZERO... */
#include <sys/time.h>

typedef struct
  { short   nfds,       /* max fd in set + 1 (zero if empty)  */
            minfd;      /* min fd in set */
    fd_set  fdset;
  } fdesc_set;

static fdesc_set *curr_fdsets;
static XtInputId *curr_inid_array;

static void input_callback(client, fdp)
int client, *fdp;
  { int set;
    if (IOWaitStatus < 0)
      { for (set = 0; set <= 2; set++)
            if (curr_fdsets[set].nfds > 0) FD_ZERO(&curr_fdsets[set].fdset);
        IOWaitStatus = 0;
      }
    IOWaitStatus++;
    FD_SET(*fdp, &curr_fdsets[client&3].fdset);
    client >>= 2;
    XtRemoveInput(curr_inid_array[client]);
    curr_inid_array[client] = (XtInputId) 0;
  }


int XptIOWait(fdsets)
  fdesc_set *fdsets;
  { XtInputMask m;
    XtIntervalId timid;
    XtInputId inid_array[FD_SETSIZE], *inidp = NULL;
    XtAppContext waitapp, inidapp;
    caddr_t (*poll_other)() = _pop_set_Xt_poll(NULL);
    Boolean polling = poll_other != NULL;

    __pop_xt_wakeup_struct.XWK_FLAG_ENABLED = TRUE;
    curr_fdsets = fdsets;
    curr_inid_array = inid_array;

    for (;;)
      { IOWaitStatus = CONTINUE;

        if (poll_other) poll_other = (caddr_t (*)()) (*poll_other)();
        if (POP_CONDITION) break;

        try_appcons(&polling);
        if (POP_CONDITION || IOWaitStatus != CONTINUE
        || __pop_xt_wakeup_struct.XWK_ON)
            break;

        /* go into (possible) wait */
        waitapp = last_event_appcon;
        if (polling) timid = XtAppAddTimeOut(waitapp, poll_time,
                                (XtTimerCallbackProc) timeout_callback, NULL);

        if (inidp && waitapp != inidapp)
          { XtInputId *ip = inid_array, id;
            while (ip < inidp) if (id = *ip++) XtRemoveInput(id);
            inidp = NULL;
          }
        if (!inidp)
          { int set;
            inidapp = waitapp;
            inidp = inid_array;
            for (set = 0; set <= 2; set++)
              { static char masks[] = {XtInputReadMask, XtInputWriteMask,
                                                        XtInputExceptMask};
                fdesc_set *fdes = &fdsets[set];
                int fd, nfds = fdes->nfds;
                long cond = masks[set];
                fd_set *fdsp = &fdes->fdset;
                if (nfds == 0) continue;

                for (fd = fdes->minfd; fd < nfds; fd++)
                    if FD_ISSET(fd, fdsp)
                      { long client = ((inidp-inid_array)<< 2) | set;
                        *inidp++ = XtAppAddInput(inidapp, fd, (XtPointer) cond,
                                        (XtInputCallbackProc) input_callback,
                                        (XtPointer) client);
                      }
              }
          }

        while (!POP_ABEXIT && IOWaitStatus == CONTINUE)
            XtAppProcessEvent(waitapp, (m=XptAppPending(waitapp)) ? m : XtIMAll);

        if (polling)
          { if (IOWaitStatus == POLL_TIMEOUT)
              { if (poll_time < POLL_MAX) poll_time += POLL_STEP;
                continue;
              }
            XtRemoveTimeOut(timid);
          }

        if (IOWaitStatus > 0)
            /* finish off any other I/O events */
            while (!POP_ABEXIT && (XtAppPending(waitapp) & XtIMAlternateInput))
                XtAppProcessEvent(waitapp, XtIMAlternateInput);

        break;
      }

    if (inidp)
      { XtInputId *ip = inid_array, id;
        while (ip < inidp) if (id = *ip++) XtRemoveInput(id);
      }
    _pop_set_xt_wakeup(FALSE);
    _pop_set_Xt_poll(poll_appcons);

    /* return number of devices ready or -1 if interrupted */
    return(IOWaitStatus > 0 ? IOWaitStatus : -1);
  }

#endif  /* UNIX */


XtInputMask XptAppTryEvents(appcon)
XtAppContext appcon;
  { XtInputMask m;
    _pop_external_flags |= PEF_RETURN_ABEXIT_ANY;

    /* must do at least one event if available */
    while (m = XptAppPending(appcon))
      { XtAppProcessEvent(appcon, m);
        if (last_event_appcon != appcon)
          { last_event_appcon = appcon;
            poll_time = POLL_INIT;
          }
        if (POP_CONDITION) break;
      }

    /* re-enable if set for async */
    set_async_appcon(TRUE, appcon);

    return(m);
  }


/* Xpt versions of standard event routines that re-enable async processing
 * after an event is read and/or set PEF_RETURN_ABEXIT_ANY for callback, etc
 */

void XptAppProcessEvent(appcon, mask)
XtAppContext appcon;
XtInputMask mask;
  { _pop_external_flags |= PEF_RETURN_ABEXIT_ANY;
    XtAppProcessEvent(appcon, mask);
    set_async_appcon(TRUE, appcon);     /* re-enable if set for async */
  }

void XptAppNextEvent(appcon, event_return)
XtAppContext appcon;
XEvent *event_return;
  { XtAppNextEvent(appcon, event_return);
    set_async_appcon(TRUE, appcon);     /* re-enable if set for async */
  }

Boolean XptDispatchEvent(event)
XEvent *event;
  {
    _pop_external_flags |= PEF_RETURN_ABEXIT_ANY;
    return(XtDispatchEvent(event));
  }

    /* All async appcons are disabled in pop before this is called */
void XptAppMainLoop(appcon)
XtAppContext appcon;
  { /* since there's no way of returning out of MainLoop that doesn't
    screw the toolkit, might as well catch abnormal exits completely */
    _pop_external_flags |= PEF_CATCH_ABEXIT_ANY;
    XtAppMainLoop(appcon);
  }


/* == VMS Only ========================================================== */

#ifdef VMS

/*
 *  This damn procedure name is 32 chars long, 1 over the limit for VMS
    symbols, and the VMS version is called XtDisplayStringConvWarning anyway
    (instead of just being truncated to 31 chars).
    This defines the truncated name.
 */
void XtDisplayStringConversionWarnin(dpy, from_value, to_type)
Display *dpy;
String from_value, to_type;
  { XtDisplayStringConvWarning(dpy, from_value, to_type);
  }


/* == VMS Version of XtAppAddInput ====================================== */

extern int pop$get_clust0_ef();
extern void pop$free_clust0_ef();

#include <dibdef.h>
#include <dcdef.h>
#include <iodef.h>
#include <descrip.h>
#include <ssdef.h>

typedef struct _XptInputId
  { struct _XptInputId *previd,
                       *nextid;
    int                 chan;
    unsigned short      devclass,
                        ef_num;
    XtInputId           xt_id;
    XtAppContext        appcon;
    XtInputCallbackProc proc;
    Opaque              clntdata;
  } *XptInputId;


#define NUM_ID_ENTRIES 8
static struct _XptInputId
    id_entries[NUM_ID_ENTRIES],
    id_dummy,
    *free_ids;

#define curr_id_list id_dummy.nextid

static char dib1[16], dib2[16];
static $DESCRIPTOR(dib1desc, dib1);
static $DESCRIPTOR(dib2desc, dib2);

static Boolean poll_input()
  { register XptInputId id;
    register unsigned count;
    Boolean result = FALSE;

    for (id = curr_id_list; id != NULL; id = id->nextid)
      { if (id->devclass == DC$_MAILBOX)
          { /* mailbox */
            sys$getchn(id->chan, 0, &dib1desc, 0, 0);
            /* number of messages in mailbox */
            count = *((unsigned short*)
                        &((struct dibdef*) dib1)->dib$l_devdepend);
          }
        else
          { /* terminal */
            sys$qiow(0, id->chan, IO$_SENSEMODE|IO$M_TYPEAHDCNT, 0, 0, 0,
                                                    dib1, 8, 0, 0, 0, 0);
            /* number of chars in typeahead buffer */
            count = *((unsigned short*) dib1);
          };

        if (count != 0)
          { sys$setef(id->ef_num);
            result = TRUE;
          }
        else
            sys$clref(id->ef_num);
      }

    return(result);
  }

static void inpt_callback(id, efp, xtidp)
register XptInputId id;
char *efp;
XtInputId *xtidp;
  { XptInputId memid;
    /* call handler */
    memid = id;
    (*id->proc)(id->clntdata, &id->chan, (XtInputId*) &memid);
    sys$clref(id->ef_num);
  }

XptInputId XptAppAddInput(appcon, chan, condition, proc, clntdata)
XtAppContext appcon;
int chan, condition;
XtInputCallbackProc proc;
Opaque clntdata;
  { register XptInputId id, lim;
    unsigned dclass;
    int ef_num, save;
    static int setup_done;

    if (!setup_done)
      { /* set up freelist */
        free_ids = id = id_entries;
        for (lim = id+NUM_ID_ENTRIES-1; id < lim; id++) id->nextid = id+1;
        id->nextid = NULL;
        setup_done = TRUE;
      };

    /* check device */
    if (!(sys$getchn(chan, 0, &dib2desc, 0, 0)&1))
        /* invalid channel */
        return((XptInputId) NULL);
    dclass = ((struct dibdef*) dib2)->dib$b_devclass;
    if (dclass != DC$_MAILBOX && dclass != DC$_TERM)
        /* currently only mailboxes & terminals supported */
        return((XptInputId) NULL);

    /* add entry */
    if ((id = free_ids) == NULL) return((XptInputId) NULL); /* no space left */
    ef_num = pop$get_clust0_ef();
    if (ef_num == -1) return((XptInputId) NULL);    /* no event flag */

    if (curr_id_list != NULL) save = sys$setast(0);

    sys$clref(ef_num);
    free_ids = id->nextid;
    id->previd      = &id_dummy;
    id->nextid      = curr_id_list; /* current list becomes tail */
    id->chan        = chan;
    id->devclass    = dclass;
    id->ef_num      = ef_num;
    id->appcon      = appcon;
    id->proc        = proc;
    id->clntdata    = clntdata;
    id->xt_id       = XtAppAddInput(appcon, ef_num, NULL, inpt_callback, id);
    curr_id_list = id;                  /* make head of list */

    if (id->nextid == NULL)
        /* start checking */
        _pop_set_poll_state(IO_POLL_NUM, poll_input);
    else if (save == SS$_WASSET)
        sys$setast(1);

    return(id);
  }

void XptRemoveInput(id)
register XptInputId id;
  { int save = sys$setast(0);
    XtRemoveInput(id->xt_id);
    pop$free_clust0_ef(id->ef_num);

    /* unlink from list and add to freelist */
    id->previd->nextid = id->nextid;
    id->nextid = free_ids;
    free_ids = id;
    if (curr_id_list == NULL)
        /* turn off checking */
        _pop_set_poll_state(IO_POLL_NUM, NULL);

    if (save == SS$_WASSET) sys$setast(1);
  }

#endif  /* VMS */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Aug 11 1998
        Changes for NuTCRACKER 4.0
--- John Gibson, Mar 26 1998
        Added missing include for <sys/time.h>
--- John Gibson, Apr  2 1997
        Added PopXtDefaultLanguageProc
--- Robert Duncan, Oct 28 1996
        Fixed XptPause for NuTCRACKER
--- John Gibson, Sep 10 1996
        Changed poll_appcons to work better when _pop_in_X_call is true.
--- Robert Duncan, May 29 1996
        Added cases for NuTCRACKER
--- John Gibson, Aug  8 1995
        Added (XtPointer) casts on cond and client args to call of
        XtAppAddInput in XptIOWait.
--- John Gibson, Aug  4 1995
        Added cast for NULL in _pop_async_appcon_clos
--- John Gibson, Mar  7 1995
        Made some things type "int" rather than "long"
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Added extra Linux include.
--- John Gibson, Oct 24 1994
        _pop_signals_pending and _pop_external_flags are now the actual
        pop variables rather than pointers to them
--- John Gibson, Jun 10 1994
        Replaced XptReadWait with XptIOWait for Unix
--- John Gibson, May 23 1994
        Made polling time decrease to 2/5 sec over a longer period
--- John Gibson, Dec  3 1993
        Added XtDisplayStringConversionWarnin for VMS
--- Adrian Howard, Jul  5 1993
        Removed previous change and altered display structure access to
        use legal NextRequest macro.
--- Robert Duncan, May 12 1993
        Minor change to disable access to the display structure when
        compiling under HP-UX and without XLIB_ILLEGAL_ACCESS defined
        (HP have made the display structure opaque under X11R5)
--- John Gibson, May  6 1993
        Moved this file to $popexternlib
--- John Gibson, Dec  5 1992
        o Changed XptReadWait for VMS to work exactly the same as for Unix.
        o Moved VMS pop$get_clust0_ef to c_core.c
--- Simon Nichols, Nov  3 1992
        Removed definition of CC$VAXCSHR, which in VMS V5.5 leads to three
        undefined symbols (VAXC$GA_STD{ERR,IN,OUT}).
--- Robert John Duncan, Jun  9 1992
        Changed X_printf to use ANSI varargs macros from <stdarg.h>
--- Robert John Duncan, Jun  8 1992
        Removed references to b*copy and b*zero (not defined on all systems)
--- John Gibson, Dec  4 1991
        More mods to fix XptPause & XptReadWait
--- John Gibson, Dec  2 1991
        VMS mod to XptReadWait
--- Robert John Duncan, Nov 28 1991
        Added declaration for -errno-
--- John Gibson, Nov 21 1991
        Changed X_printf so it just uses printf to print messages.
        Made VMS fixes.
--- Robert John Duncan, Nov  7 1991
        Reinstated definition of _SysErrorMsg for Unix
--- John Gibson, Oct 31 1991
        Removed dummy VMS definitions for R4 X stuff, etc.
--- Jonathan Meyer, Sep 11 1991
        Changed to use mishap codes for error messages
--- Jonathan Meyer, Sep  6 1991
        Added X_printf - for printing errors nicely.
        Added erorr handlers using the X tooliit higher level interface as
        well.
--- Jonathan Meyer, Jul 31 1991
        renamed XptWm -> XptWM
--- Robert John Duncan, Jul  2 1991
        Removed unnecessary XtIsShell check in XptWMProtocol
        (wouldn't compile under OpenWindows)
--- Roger Evans, Jun 28 1991 removed callback code
--- Jonathan Meyer, Jun 21 1991
        Added hard reference to _vendorShellWidgetClass so that poplog
        can easily be linked with Motif and OLIT2.5.
--- Jonathan Meyer, Jun  5 1991
        Changed XptAddProtoHandler to add the handler to the
        tail of the list so that it will not override any handlers
        that have been added by the widget itself.
        Changed the response of XptWMProtocol to call XtPopdown on
        shell widgets created using XtAppCreatePopupShell, and
        XtDestroyWidget on other shell widgets.
--- Jonathan Meyer, May 29 1991
        added XpolAddListItem - to fix sun3 scrolling list procedure
        as in bugreport isl-fr.4319
--- Roger Evans, May 29 1991
        added better protocol handling to XptAddProtoHandler
--- Roger Evans, May 26 1991
        changed PopXIOError to call XIO_sys_error_handler and
        clear abexit flags
--- John Gibson, Mar 14 1991
        Added async appcontext processing and lots of other changes.
--- John Gibson, Feb 11 1991
        Added VMS stuff, reorganised XptReadWait and split off XptPause, etc.
--- Roger Evans, Feb  3 1991 added closdisplay to XIO error handler
--- Roger Evans, Jan 29 1991
        Added protocol message handler and action initialisation
        Added garbage action handler
--- Roger Evans, Jan 28 1991
        Added XptAppPending code to fix X lockout problems (bug report
        jonm.3)
--- John Gibson, Jan 19 1991
        Added call of _pop_set_xt_wakeup(0) in _XptDummyInput
--- Roger Evans, Nov 21 1990 fixed oneshot processing!
--- Roger Evans, Nov 21 1990 added oneshot processing to XptReadWait
--- Roger Evans, Nov 20 1990 added XptDummyInput
--- Roger Evans, Nov 20 1990 added more abnormal exit code
--- Roger Evans, Nov 19 1990 added abnormal exit code
--- John Gibson, Nov 19 1990
        Replaced pop_interrupt_pending() with *_pop_signals_pending
--- Roger Evans, Nov  9 1990 added XptReadWait
--- Roger Evans, Nov  8 1990
    added XptActionHook and revised CB_TYPE orderings
--- Roger Evans, Oct 11 1990 Much revised
    changed to use real callback
    threw away action stuff
--- Roger Evans, Jun 26 1990 added new definition for -select-
--- Roger Evans, Mar  9 1990 corrected E_PTR value in PopXtDestroyCallback
    for new data representation
 */
