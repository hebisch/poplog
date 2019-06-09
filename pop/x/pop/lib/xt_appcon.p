/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_appcon.p
 > Purpose:         Application context handling
 > Author:          Adrian Howard, Aug  7 1990 (see revisions)
 > Documentation:   REF *XT_APPCON
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p
 >                  C.x/x/pop/lib/fast_xt_appcon.p
 */


compile_mode:pop11 +strict;


section;

include xpt_generaltypes.ph;        ;;; Get typspecs (for StringList type)

uses xpt_typecheck.p;       ;;; Get the type checking routines
uses fast_xt_appcon.p;      ;;; Get the fast versions of the procedures


;;; Create an application context - 07/08/90
;;; Output - <AppContext>
define global XtCreateApplicationContext;
    fast_chain(fast_XtCreateApplicationContext);
enddefine;


;;; Create an application context with a given POP identifier and sets
;;; XptCurrentAppContext - 07/08/90
;;; Input - <ITEM>, Output - <AppContext>
define global XptCreateApplicationContext with_nargs 1;
    fast_chain(fast_XptCreateApplicationContext);
enddefine;


;;; Destroy an application context - 07/08/90
;;; Input - <AppContext>
define global XtDestroyApplicationContext() with_nargs 1;
    fast_XtDestroyApplicationContext(XptCheckAppContext());
enddefine;


;;; Gets the application context of a given widget - 07/08/90
;;; Input - <Widget>, Output - <AppContext>
define global XtWidgetToApplicationContext() with_nargs 1;
    fast_XtWidgetToApplicationContext(XptCheckWidget());
enddefine;


;;; Specification of a default set of resource values - 07/08/90
;;; Input - <AppContext> <StringList>
define global XtAppSetFallbackResources(appcontext, stringlist);
    lvars appcontext stringlist;
    fast_XtAppSetFallbackResources( XptCheckAppContext(appcontext),
                                    if stringlist then
                                        XptCheckNTStringList(stringlist);
                                    else
                                        false
                                    endif
                                  );
enddefine;


;;; So uses works OK
global vars xt_appcon= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1991 
        includes xpt_generaltypes.ph
--- Adrian Howard, Sep 10 1991 : Added check for null terminated string list
--- Adrian Howard, Aug 23 1991 : -XtAppSetFallbackResources- now allows
        -false- as an indication of a null stringlist
--- Roger Evans, Nov 16 1990 tidied up
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
--- Adrian Howard, Sep  4 1990 : StringNullVec --> StringList
 */
