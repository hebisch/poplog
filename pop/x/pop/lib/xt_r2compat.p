/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_r2compat.p
 > Purpose:         Checking compatability routines for old Xt versions
 > Author:          Adrian Howard, Jul 18 1990 (see revisions)
 > Documentation:   REF *XT_R2COMPAT
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p,
 >                  C.x/x/pop/lib/fast_xt_r2compat.p
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

section;

uses xpt_typecheck.p;        ;;; Get the type checking routines
uses fast_xt_r2compat.p;    ;;; Get fast versions of the procedures
uses fast_xt_event.p

;;; Infinite loop doing XtNextEvent followed by XtDispatchEvent - 18/07/90
define global XtMainLoop =
    fast_XtMainLoop;
enddefine;


;;; Check for pending events - 18/07/90
;;; Output - <BOOL>
define global XtPending =
    fast_XtPending;
enddefine;


;;; Create & initialize an application shell with the default application
;;; context - 18/07/90
;;; Input - <String> <WidgetClass> <ArgList> <Cardinal>
define global XtCreateApplicationShell() with_nargs 3;
    lvars (name, class, args, num) = XptCheckArgListAndCardinal();

    fast_XtCreateApplicationShell(  XptCheckString(name),
                                    XptCheckWidgetClass(class),
                                    args, num ;;; already checked
                                 );
enddefine;


;;; Initialize toolkit, create default application context, open a display
;;; and create an application shell - 13/08/90
;;; Input - <STRING> <STRING> <OptionDescList> <Cardinal> <CardinalPtr>
;;; <StringList>, Output - <Widget>
define global XtInitialize(shell_string, app_string, optiondesclist, cardinal,
                    argc, argv);
    lvars shell_string, app_string, optiondesclist, cardinal, argc, argv;
    fast_XtInitialize(  XptCheckString(shell_string),
                        XptCheckString(app_string),
                        XptCheckOptionDescListAndLength(optiondesclist, cardinal),
                        XptCheckCardinalPtr(argc),
                        erase(XptCheckStringListAndLength(argv, XptCPValue(argc)))
                     );
enddefine;


;;; Process single input event, timeout or alternate input source - 13/08/90
;;; Input - <ulong: XtInputMask>
define global XtProcessEvent() with_nargs 1;
    fast_XtProcessEvent(XptCheckInputMask());
enddefine;


;;; Registar new source of events with default application context - 13/08/90
;;; Input - <INT> <XtPointer> <XtInputCallbackProc> <XtPointer>
;;; Output - <XtInputId>
define global XtAddInput(source_int, condition_xtpointer, proc, client);
    lvars source_int, condition_xtpointer, proc, client;
    fast_XtAddInput(    XptCheckInt(source_int),
                        condition_xtpointer,
                        if XptIsValidCallback(proc) then
                            XptExportInputCached(proc,client,true);
                        else
                            XptCheckProcedure(proc),
                            client
                        endif
                   );
enddefine;


;;; Create a timeout in the default application context - 13/08/90
;;; Input - <ulong> <XtTimerCallbackProc> <Xtpointer>, Output - <XtIntervalId>
define global XtAddTimeOut(interval, proc, client);
    lvars interval, proc, client;
    fast_XtAddTimeOut(  XptCheckUnsignedIntegral(interval),
                        if XptIsValidCallback(proc,client) then
                            XptExportTimeOutCached(proc,client, true);
                        else
                            XptCheckProcedure(proc),
                            client
                        endif
                     );
enddefine;


;;; Remove a shared GC - 14/08/90
;;; Input - <Widget> <GC>
define global XtDestroyGC(widget, gc);
    lvars widget, gc;
    fast_XtDestroyGC(XptCheckWidget(widget), XptCheckGC(gc));
enddefine;


;;; Process an event - 14/08/90
;;; Input - <XEventPtr>
define global XtNextEvent() with_nargs 1;
    fast_XtNextEvent(XptCheckXEventPtr());
enddefine;


;;; Look at event on queue - 14/08/90
;;; Input - <XEventPtr>, Output- <BOOL>
define global XtPeekEvent() with_nargs 1;
    fast_XtPeekEvent(XptCheckXEventPtr());
enddefine;


;;; So uses works OK
global vars xt_r2compat = true;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Mar 16 1995
        fast_XtAddTimeOut renamed to XtAddTimeOut (cf. BR adrianh.92).
--- Adrian Howard, Oct  3 1991 :
        - Removed -XptCoerceInput-, -XptCoerceTimeOut-
        - Replaced with -XptExportInputCached-, -XptExportTimeOutCached-
    (NOTE: the procs should use the widget dependency list rather than
     -XptExportFOOCached- with a -true- hold arg, needs fixing later).
--- Adrian Howard, Sep 11 1991 : Added checks for list lengths
--- Roger Evans, Jun 28 1991 changed to use new callback stuff
--- Roger Evans, Jun 18 1991 added XPtCheckArgListAndCardinal code
--- Roger Evans, Nov 19 1990 tidied up
--- Roger Evans, Nov 11 1990 XptCheckProc -> XptCheckProcedure
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
--- Adrian Howard, Sep  4 1990 : StringNullVec -> StringList
 */
