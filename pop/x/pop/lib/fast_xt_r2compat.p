/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_r2compat.p
 > Purpose:         Fast compatability routines for old Xt versions
 > Author:          Adrian Howard, Jul 18 1990 (see revisions)
 > Documentation:   REF *XT_R2COMPAT
 > Related Files:   C.x/x/pop/lib/fast_xt_r2compat.p
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

include xpt_coretypes.ph;
uses fast_xt_event;


;;; Load the external "raw" procedures
XptLoadProcedures 'xt_r2compat'
    lvars
        XtInitialize
        XtMainLoop
        XtNextEvent
        XtProcessEvent
        XtPeekEvent
        XtPending
        XtAddInput
        XtAddTimeOut
        XtAddWorkProc
        XtCreateApplicationShell
        XtDestroyGC
;

;;; Load _pop_sigmask for -fast_XtInitialize-
exload 'xt_r2compat.pop_sigmask'
    (language C)
    lvars _pop_sigmask(1) :uint
endexload;


;;; Infinite loop doing XtNextEvent followed by XtDispatchEvent - 18/07/90
define global fast_XtMainLoop();
    exacc (0) raw_XtMainLoop();
enddefine;


;;; Check for pending events - 18/07/90
;;; Output - <BOOL>
define global fast_XtPending();
    exacc (0):XptBoolean raw_XtPending();
enddefine;


;;; Create & initialize an application shell with the default application
;;; context - 18/07/90
;;; Input - <STRING> <WidgetClass> <ArgList> <Cardinal>, Output - <Widget>
define global fast_XtCreateApplicationShell(name, class, args, num);
    lvars name, class, args, num;
    exacc (4):XptWidget raw_XtCreateApplicationShell(
                    -> XptCoerceTmpString(name),
                    class,
                    args,
                    num
                );
enddefine;


;;; Initialize toolkit, create default application context, open a display
;;; and create an application shell - 13/08/90
;;; Input - <STRING> <STRING> <OptionDescList> <Cardinal> <CardinalPtr>
;;; <StringList>, Output - <Widget>
define global fast_XtInitialize(shell_string, app_string, optiondesclist,
            cardinal, cardinalptr, stringlist);
    lvars shell_string, app_string, optiondesclist, cardinal, cardinalptr,
          stringlist;

    ;;; Block signals while we're doing this -- necessary (at least) in Unix
    ;;; to stop async appcontext timers interrupting the opening of the
    ;;; display connection (XOpenDisplay is not checking for EINTR and
    ;;; retrying, so it returns an error).
    dlocal 0 %exacc _pop_sigmask(1)->, exacc _pop_sigmask(0)-> %;

    fast_XptToolkitPreInitialize();
    exacc (6) :XptWidget raw_XtInitialize(
                        -> XptCoerceTmpString(shell_string),
                        -> XptCoerceTmpString(app_string),
                        optiondesclist,
                        cardinal,
                        cardinalptr,
                        stringlist
                    );
    fast_XptSetUnprocessedArgs(cardinalptr, stringlist);
    fast_XptToolkitPostInitialize();

enddefine;


;;; Process single input event, timeout or alternate input source - 13/08/90
;;; Input - <ulong: XtInputMask>
define global fast_XtProcessEvent() with_nargs 1;
    exacc (1) raw_XtProcessEvent();
enddefine;


;;; Registar new source of events with default application context - 13/08/90
;;; Input - <INT> <XtPointer> <XtInputCallbackProc> <XtPointer>
;;; Output - <XtInputId>
define global fast_XtAddInput(source_int, condition_xtpointer,
            inputcallbackproc, client_data);
    lvars source_int, condition_xtpointer, inputcallbackproc,
          client_data;
    exacc (4):exptr raw_XtAddInput(
                    source_int,
                    -> XptCoerceXtPointer(condition_xtpointer),
                    inputcallbackproc,client_data,
                  );
enddefine;


;;; Create a timeout in the default application context - 13/08/90
;;; Input - <ulong> <XtTimerCallbackProc> <Xtpointer>, Output - <XtIntervalId>
define global fast_XtAddTimeOut(interval, timercallbackproc, xtpointer);
    lvars interval, timercallbackproc, xtpointer;
    exacc (3):exptr raw_XtAddTimeOut(
                        interval,
                        timercallbackproc,xtpointer,
                    );
enddefine;


;;; Remove a shared GC - 14/08/90
;;; Input - <Widget> <GC>
define global fast_XtDestroyGC() with_nargs 2;
    exacc (2) raw_XtDestroyGC();
enddefine;


;;; Process a single event - 14/08/90
;;; Input - <XEventPtr>
define global fast_XtNextEvent() with_nargs 1;
    exacc (1) raw_XtNextEvent();
enddefine;


;;; Look at event on queue - 14/08/90
;;; Input - <XEventPtr>, Output- <BOOL>
define global fast_XtPeekEvent() with_nargs 1;
    exacc (1):XptBoolean raw_XtPeekEvent();
enddefine;


;;; So uses works OK
global vars fast_xt_r2compat= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1991
        includes xpt_coretypes.ph
--- Adrian Howard, Oct  3 1991 : Removed -XptCoerceInput-, -XptCoerceTimeOut-
--- Adrian Howard, Oct  1 1991 :
        o -fast_XtInitialize- now sets -XptUnprocessedArgs-
        o signals blocked during display creation
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Nov 19 1990 tidied up
--- Roger Evans, Nov 19 1990 added Pre and Post initialization
--- Roger Evans, Oct 19 1990 changed to use XptLoadProcedures
--- Roger Evans, Oct 11 1990 changed to use exacc
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  6 1990 : Altered calls to XptCoerce/Import to make
        compatable with format of new external conversion/access procs
--- Adrian Howard, Sep  4 1990 : StringNullVec --> StringList
 */
