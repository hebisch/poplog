/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/src/xt_error.p
 > Purpose:         X Toolkit - error handlers
 > Author:          Roger Evans, Jul  5 1990 (see revisions)
 > Documentation:   REF xt_procs
 > Related Files:   xt_*.p
 */

#_INCLUDE 'xt_declare.ph'


section $-Sys$-Xt => XIO_sys_error_handler, Xpt_sys_pr_message;

define Init_error;
    /* set sensible error handlers */
    X_apply(EXTERN_PTR(PopXError), _1, _extern XSetErrorHandler) ->;
    X_apply(EXTERN_PTR(PopXIOError), _1, _extern XSetIOErrorHandler) ->;
enddefine;

define Appcon_error(appcon);
    lvars appcon;
    /* set sensible error handlers */
    X_apply(appcon, EXTERN_PTR(PopXtError),
            _2, _extern XtAppSetErrorHandler) ->;
    X_apply(appcon, EXTERN_PTR(PopXtWarning),
            _2, _extern XtAppSetWarningHandler) ->;

    /* register handlers for the higher level interface as well */
    X_apply(appcon, EXTERN_PTR(PopXtWarningMsg),
            _2, _extern XtAppSetWarningMsgHandler) ->;
    X_apply(appcon, EXTERN_PTR(PopXtErrorMsg),
            _2, _extern XtAppSetErrorMsgHandler) ->;
enddefine;

/*  What to do when we get an IO error on a display - called from
    PopXIOError in XtPoplog.c

    Unfortunately there seems to be very little we can do - IO errors
    are assumed to be fatal in Xlib. Certainly at least the current
    appcontext is compromised (since it will try and service the dpy etc.)
    Here we optimistically assume that is all that is compromised (since
    the only alternative seems to be to exit...)

    This is a protected var, redefined by XVed
*/
protected
define vars XIO_sys_error_handler(dpy);
    lvars dpy, appcon;

    ;;; ensure these garbage hooks are temporarily turned off
    dlocal XptGarbageWidget = false, XptGarbageHook = identfn;

    fast_XtDisplayToApplicationContext(dpy) -> appcon;
    if appcon then
        /* destroy appcon (pop data structures only) */
        DestroyApplicationContext(appcon,true);
    else
        /* no appcon known - just destroy display */
        CloseDisplay(dpy,true);
    endif;
enddefine;


/*
A Pop-11 version of printf - which prints out the message to the current
error stream. Not for public use -- called from XtPoplog.c
*/

define Xpt_sys_pr_message(exptr);
    lvars exptr, string;
    exacc_ntstring(exptr) -> string;
    if string == termin then
        cucharerr(`\n`)
    else
        sys_pr_message(0, {^string ^nullstring 16:01}, nullstring, `W`)
    endif
enddefine;

;;; ensure these are always in the dictionary so XtPoplog.c can get them
uses-by_name (XIO_sys_error_handler, Xpt_sys_pr_message);

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  6 1996
        Uses sys_pr_message instead of sys*prmessage
--- Robert John Duncan, Feb  1 1995
        Removed kill_*_display: now handled in xt_display.p
--- John Gibson, Nov 15 1993
        Disabled garbage hooks inside XIO_sys_error_handler
--- John Gibson, Jun 10 1993
        Made XIO_sys_error_handler a protected var (so XVed can redefine
        it cleanly).
--- John Gibson, Mar 29 1993
        Replaced uses of _pint on _extern routines with new macro EXTERN_PTR
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- Adrian Howard, Jul  6 1992
        Moved -kill_X_display- in from sysfork.p
--- John Gibson, Nov 19 1991
        Removed (now unnecessary) #_IF not(DEF VMS) around registering
        of error handlers, etc.
--- Jonathan Meyer, Sep 11 1991
        Xpt_sys_pr_message now interprets a null string to mean do a newline
--- Jonathan Meyer, Sep  6 1991
        Added Xpt_sys_pr_message, and registered new higher level error
        handlers as well (PopXtErrorMsg, PopXtWarningMsg)
--- Roger Evans, May 26 1991
        Added XIO_sys_error_handler
--- John Gibson, Feb 11 1991
        Added VMS mods
--- Roger Evans, Oct 11 1990 Much revised
 */
