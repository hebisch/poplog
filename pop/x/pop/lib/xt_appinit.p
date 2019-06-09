/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_appinit.p
 > Purpose:         Checking convenience procedures to initialise an
 >                  application
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XT_APPINIT
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p,
 >                  C.x/x/pop/lib/fast_xt_appinit.p
 */


compile_mode:pop11 +strict;


section;

uses xpt_typecheck.p;        ;;; Get the type checking routines
uses fast_xt_appinit.p;     ;;; Get the fast versions of the procedures

include xpt_constants.ph;
include xpt_generaltypes.ph;

shadowclass XptAppContextPtr #_< [props ^XDT_APPCONTEXTPTR] >_# {
    XptACPValue     :XptAppContext,
};


;;; Initialize the X Toolkit Internals, create an application context,
;;; initialize a display and create initial application shell - 27/07/90
;;; Input - <AppContextPtr> <String> <XrmOptionDescList> <Cardinal>
;;;         <CardinalPtr> <StringList> <StringList> <ArgList> <Cardinal>
;;; Output - <Widget>
define global XtAppInitialize();

    lvars ( app_context_ptr, app_class,
            options, num_options,
            argc_io, argv_io,
            fallback_resources,
            args, num_args ) = XptCheckArgListAndCardinal();

    fast_XtAppInitialize(   if app_context_ptr then
                                XptCheckAppContextPtr(app_context_ptr)
                            else
                                false
                            endif,
                            XptCheckString(app_class),
                            XptCheckOptionDescListAndLength(options, num_options),
                            XptCheckCardinalPtr(argc_io),
                            erase(XptCheckStringListAndLength(argv_io, XptCPValue(argc_io))),
                            if fallback_resources then
                                XptCheckNTStringList(fallback_resources)
                            else
                                false
                            endif,
                            args, num_args ;;; already checked
                        );
enddefine;


;;; Initialize the X Toolkit Internals, create an application context,
;;; initialize a display and create initial application shell - 27/07/90
;;; Input - <AppContextPtr> <String> <XrmOptionDescList> <Cardinal> <CardinalPtr>
;;;         <StringList> <StringList> <ArgVarargs>, Output - <Widget>
define global XtVaAppInitialize(count);
    lvars count, cardinal;
    XptCheckVarargs(count);
    if subscr_stack(count fi_+ 7) then
        XptCheckAppContextPtr(subscr_stack(count fi_+ 7)) ->;
    endif;
    XptCheckString(subscr_stack(count fi_+ 6)) ->;
    XptCheckCardinal(subscr_stack(count fi_+ 4)) -> cardinal;
    XptCheckOptionDescListAndLength(subscr_stack(count fi_+ 5), cardinal) -> ->;
    XptCPValue(XptCheckCardinalPtr(subscr_stack(count fi_+ 3))) -> cardinal;
    XptCheckStringListAndLength(subscr_stack(count fi_+ 2), cardinal) -> ->;
    if subscr_stack(count fi_+1) then
        XptCheckNTStringList(subscr_stack(count fi_+ 1)) ->;
    endif;
    fast_XtVaAppInitialize(count);
enddefine;


;;; So uses works OK
global vars xt_appinit= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep 30 1991 : Fixed -XtVaAppInitialize-
--- Adrian Howard, Sep 10 1991 : Added checks for null-terminated/oversized
        lists
--- Adrian Howard, Aug 21 1991 : Bug fixes in Xt(Va)AppInitialize,
        o allows -false- AppContextPtr arguments
        o allows -false- fallback resources arguments
--- Roger Evans, Jun 18 1991 changed to use XptCheckArgListAndCardinal
--- Roger Evans, Jan 26 1991 changed to new shadowclass props format
--- Roger Evans, Nov 18 1990 tidied a little
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
--- Adrian Howard, Sep  4 1990 : StringNullVec --> StringList
 */
