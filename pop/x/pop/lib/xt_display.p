/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_display.p
 > Purpose:         Display initialisation, opening and closing
 > Author:          Adrian Howard, Aug  7 1990 (see revisions)
 > Documentation:   REF *XT_DISPLAY
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p,
 >                  C.x/x/pop/lib/fast_xt_display.p
 */
compile_mode:pop11 +strict;

section;
exload_batch;

include xpt_generaltypes.ph;

uses
    xpt_typecheck,      ;;; Get the type checking routines
    xpt_general,
    fast_xt_display,    ;;; Get the fast versions of the procedures
;


;;; Initialize a display and add it to an application context - 08/08/90
;;; Input - <AppContext> <DisplayPtr> <String> <String> <OptionsDescList>
;;;         <Cardinal> <CardinalPtr> <StringList>
define XtDisplayInitialize(appcontext, displayptr, app_name, app_class,
                           optiondesclist, cardinal, argc, argv);
    lvars appcontext, displayptr, app_name, app_class, optiondesclist,
          cardinal, argc, argv;
    fast_XtDisplayInitialize(   XptCheckAppContext(appcontext),
                                XptCheckDisplayPtr(displayptr),
                                XptCheckString(app_name),
                                XptCheckString(app_class),
                                XptCheckOptionDescListAndLength(optiondesclist, cardinal),
                                XptCheckCardinalPtr(argc),
                                erase(XptCheckStringListAndLength(argv, XptCPValue(argc)))
                            );
enddefine;


;;; Open, initialize and add a display to an application context - 08/08/90
;;; Input - <AppContext> <String> <String> <String> <OptionDescList>
;;;         <Cardinal> <CardinalPtr> <StringList>, Output - <DisplayPtr>
define XtOpenDisplay(appcontext, display_string, app_name, app_class,
                     optiondesclist, cardinal, argc, argv);
    lvars appcontext, display_string, app_name, app_class, optiondesclist,
          cardinal, argc, argv;
    fast_XtOpenDisplay( XptCheckAppContext(appcontext),
                        if display_string then
                            XptCheckString(display_string)
                        else
                            false
                        endif,
                        if app_name then
                            XptCheckString(app_name)
                        else
                            false
                        endif,
                        XptCheckString(app_class),
                        XptCheckOptionDescListAndLength(optiondesclist, cardinal),
                        XptCheckCardinalPtr(argc),
                        erase(XptCheckStringListAndLength(argv, XptCPValue(argc)))
                      );
enddefine;


;;; Close a display and remove it from an application context - 08/08/90
;;; Input - <DisplayPtr>
define XtCloseDisplay() with_nargs 1;
    fast_XtCloseDisplay(XptCheckDisplayPtr());
enddefine;


;;; Get the appcontext from a display
;;; Input - <DisplayPtr>
define XtDisplayToApplicationContext() with_nargs 1;
    fast_XtDisplayToApplicationContext(XptCheckDisplayPtr());
enddefine;


;;; So uses works OK
constant xt_display= true;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        Made XtDisplay autoloadable
--- Adrian Howard, Sep 25 1991 : -XtOpenDisplay- now allows -false- to
        indicate a null display string and/or application name.
--- Adrian Howard, Sep 11 1991 : Added tests for list lengths
--- Roger Evans, Dec  7 1990 added XtDisplayToApplicationContext
--- Roger Evans, Nov 18 1990 tidied up
--- Adrian Howard, Sep 13 1990 : Moved -XtDisplay- into library from
        LIB *XT_WIDGETINFO
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
--- Adrian Howard, Sep  4 1990 : StringNullVec --> StringList
 */
