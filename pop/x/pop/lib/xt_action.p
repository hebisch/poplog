/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_action.p
 > Purpose:         Action table management
 > Author:          Adrian Howard, Aug  9 1990 (see revisions)
 > Documentation:   REF *XT_ACTION
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p
 >                  C.x/x/pop/lib/fast_xt_action.p
 */

compile_mode:pop11 +strict;

section;

include xpt_constants.ph;
include xpt_generaltypes.ph;    ;;; Get dataype definitions

uses fast_xt_action.p;      ;;; Get the fast versions of the procedures
uses xpt_typecheck.p;       ;;; Get the type checking routines


shadowclass XptActionsPtr {:XptActionsRec};
shadowclass XptActionList #_< [props ^XDT_ACTIONLIST] >_# :XptActionsRec[];

;;; Declare and registar an action table - 09/08/90
;;; Input - <AppContext> <XtActionList> <Cardinal>
define global XtAppAddActions(appcontext, actionlist, cardinal);
    lvars appcontext, actionlist, cardinal;
    fast_XtAppAddActions(   XptCheckAppContext(appcontext),
                            XptCheckActionListAndLength(
                                actionlist,
                                cardinal
                            )
                        );
enddefine;

;;; pop list variant of above
define global XptAppAddActionList(appcontext, actionlist);
    lvars appcontext, actionlist, item proc;
    unless islist(actionlist) then mishap(actionlist,1,'LIST NEEDED') endunless;
    [%  for item in actionlist do
            if (#| explode(item) |#) == 2 then
                -> (item, proc);
                [%XptCheckString(item);
                if XptIsValidCallback(proc) then
                    XptExportAction(proc, false)
                else
                    XptCheckProcedure(proc);
                endif;%]
            else
                mishap(item,1,'[STRING ACTION] PAIR NEEDED');
            endif;
        endfor;
    %] -> actionlist;
    fast_XptAppAddActionList(XptCheckAppContext(appcontext), actionlist);
    sys_grbg_list(actionlist);
enddefine;


;;; Add an "action hook" procedure - 09/08/90
;;; Input - <AppContext> <ActionHookProc> <XtPointer>, Output - <ActionHookId>
define global XtAppAddActionHook(appcontext, proc, client);
    lvars appcontext, proc, client;
    fast_XtAppAddActionHook(    XptCheckAppContext(appcontext),
                                if XptIsValidCallback(proc) then
                                    XptExportActionHook(proc,client,false)
                                else
                                    XptCheckProcedure(proc),
                                    client
                                endif
                           );
enddefine;


;;; Remove an "action hook" procedure - 09/08/90
;;; Input - <XtActionHookId>
define global XtRemoveActionHook() with_nargs 1;
    fast_XtRemoveActionHook(XptCheckActionHookId());
enddefine;


;;; Call an action procedure directly - 09/08/90
;;; Input - <Widget> <STRING> <XEventPtr> <StringList> <Cardinal>
define global XtCallActionProc(widget, name_string, xeventptr, stringlist,
                        cardinal);
    lvars widget, name_string, xeventptr, stringlist, cardinal;
    fast_XtCallActionProc(  XptCheckWidget(widget),
                            XptCheckString(name_string),
                            if xeventptr then
                                XptCheckXEventPtr(xeventptr)
                            else
                                false
                            endif,
                            XptCheckStringListAndLength(stringlist, cardinal),
                         );
enddefine;

;;; So uses works OK
constant xt_action= true;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1991
        Moved typespecs to xpt_generaltypes.ph
--- Adrian Howard, Oct 31 1991 :
        o -XptActionsRec- changed to a typespec
        o -XptActionsRec- shadowclass renamed -XptActionsPtr-
--- Adrian Howard, Sep 10 1991 : Added checks for the length of list
        structures
--- Adrian Howard, Sep  4 1991 : Allowed null XEvent argument in
        -XtCallActionProc-
--- Adrian Howard, Aug 30 1991 : Changed to use -XptExportAction(Hook)(Cached)-
        procedures.
--- Jonathan Meyer, Aug  2 1991 removed 'uses XptXEventPtr'
--- Roger Evans, Jun 28 1991
        added new callback coercion code
--- Jonathan Meyer, Jun 24 1991
        Fixed bug in XptAppAddActionList - also allowed action lists
        to contain vectors as well as lists as their subelements.
--- Roger Evans, Jun 24 1991  fixed for new XptCoerceAction code
--- Jonathan Meyer, Feb 10 1991
        changed XtAppAddActionList to XptAppAddActionList
--- Roger Evans, Feb  7 1991 changed XptPopString to XptString
--- Roger Evans, Jan 26 1991 changed to new shadowclass props format
--- Roger Evans, Nov 16 1990 added 'uses XptXEventPtr'
--- Roger Evans, Nov 15 1990 added datatypes and XptAppAddActionList
--- Roger Evans, Nov 11 1990 XptCheckProc -> XptCheckProcedure
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
--- Adrian Howard, Sep  4 1990 : StringNullVec --> StringList
 */
