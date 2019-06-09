/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/src/xt_action.p
 > Purpose:         support for x toolkit actions
 > Author:          Roger Evans, Oct 29 1990 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'


section $-Sys$-Xt =>
                        fast_XtAppAddActions
                        fast_XptAppAddActionList
                        fast_XtAppAddActionHook
                        fast_XtRemoveActionHook
                        fast_XtCallActionProc
                        XptGarbageWidget
                        XptGarbageHook
                    ;

define fast_XptAppAddActionList(appcon,list);
    lvars item count appcon list vec ac alist;
    Get_Descriptor(XDT_APPCONTEXT,appcon) -> ac;
    if ac then ac!XD_DEPENDENTS!XD_AC_ACTIONS -> alist endif;
    (#| for item in list do
            XTC_str(item!P_FRONT);
#_IF DEFV HPUX >= 8.0
            ;;; must guarentee that the string is fixed (nb. XTC_str doesn't)
            unless is_fixed(dup()) then copy_fixed() endunless;
#_ENDIF
            item!P_BACK!P_FRONT -> item;
            if  isexternal_ptr_class(item) then item!XP_PTR
            else item;  ;;; probably an exfunc_closure
            endif;
            if ac then dup() :: alist -> alist; endif;
        endfor;
    |#) -> count;
#_IF DEFV HPUX >= 8.0
    ;;; HPMotif1.1 doesn't seem to copy this vector, so we hold onto it.
    cons_fixed(count, vector_key, true) -> vec;
#_ELSE
    consvector(count) -> vec;
#_ENDIF
    if ac then alist -> ac!XD_DEPENDENTS!XD_AC_ACTIONS endif;
    X_apply(appcon,vec,count div 2, _3, _extern XtAppAddActions) ->;
enddefine;

define fast_XtAppAddActions with_nargs 3;
    ;;; no coercion or gc handling possible here
    X_apply(_3, _extern XtAppAddActions) -> ;
enddefine;

;;; -- ACTION HOOKS ---------------------------------------------------------

;;; actionhook record constructor
define lconstant Cons_ActionHookId_rec(_ptr,appcon,proc,arg) -> id;
    lvars _ptr appcon proc arg id dep;
    Cons_XptDescriptor(_ptr,XDT_ACTIONHOOKID,false,XD_AH_DEP_LEN) -> id;
    id!XD_DEPENDENTS -> dep;

    proc -> dep!XD_AH_PROC;
    arg -> dep!XD_AH_CLIENT;
    false -> dep!XD_AH_APPCON;

    if (Get_Descriptor(XDT_APPCONTEXT,appcon) ->> appcon) then
        appcon -> dep!XD_AH_APPCON;
        appcon!XD_DEPENDENTS -> appcon;
        id :: appcon!XD_AC_ACTIONHOOKS -> appcon!XD_AC_ACTIONHOOKS;
    endif;
enddefine;

define fast_XtAppAddActionHook(appcon,proc,arg) -> id;
    lvars appcon proc arg id _ptr;
    X_apply(appcon,proc,arg,_3,_extern XtAppAddActionHook) -> _ptr;
    Cons_ActionHookId_rec(_ptr,appcon,proc,arg) -> id;
enddefine;

define fast_XtRemoveActionHook(id);
    lvars id dep;
    unless id!XP_PTR == _NULL then
        X_apply(id,_1,_extern XtRemoveActionHook) ->;
        if Descriptor(XDT_ACTIONHOOKID,id!XP_PTR) ->> id then
            id!XD_DEPENDENTS -> dep;
            if dep!XD_AH_APPCON then
                Del_item(id,dep!XD_AH_APPCON!XD_DEPENDENTS@XD_AC_ACTIONHOOKS);
            endif;
            Kill_XptDescriptor(id);
        endif;
    endunless;
enddefine;

define fast_XtCallActionProc(widget, string, xeventptr, stringlist, cardinal);
    lvars widget, string, xeventptr, stringlist, cardinal;
    X_apply(
        widget,
        XTC_str(string),
        if xeventptr then
            xeventptr
        else
            null_external_ptr
        endif,
        stringlist,
        cardinal,
        _5, _extern XtCallActionProc
    ) ->;
enddefine;

;;; --- garbage collector feedback interface ------------------------

;;; variable holding the widget that indicates garbage collection
vars XptGarbageWidget = false;

;;; (private) notify procedure to call during a garbage collection
protected vars procedure XptGarbageHook;
declare_incremental procedure (XptGarbageHook);

lconstant GARBAGE_ACTION = 'XptGarbageFeedback';

;;; NB: if -why- is non-false, the following routine is called when
;;;     there's no space - it MUST not allocate memory (eg through
;;;     callbacks etc.)
define XptGarbageHandler(why);
    lvars why, w = XptGarbageWidget;
    lconstant param = writeable inits(12); ;;; big enough for 'why' arg
    lconstant params = {^param};
    dlocal _disable = _DISABLE_ALL;
    ;;; call action if it is there
    if w and iscompound(w)
    and w!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR_PROPS
    and fast_XptDataType(w) == XDT_WIDGET
    and _nonzero(w!XP_PTR)
    then
        ;;; call garbage action on garbage widget
        X_apply(w, GARBAGE_ACTION, 0,
            if why then
                _0 -> _bmove(@@(b)[why!V_LENGTH],why@V_BYTES,param@V_BYTES)!(b);
                params, 1;
            else
                0, 0;
            endif, _5, _extern XtCallActionProc) -> ;
    endif;
    ;;; call procedure if it is there
    if XptGarbageHook /== identfn then
        XptGarbageHook(why)->;
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Feb  1 1995
        Changed XptGarbageHandler to check that XptGarbageWidget is live
--- John Gibson, Mar 16 1993
        Added incremental (procedure) declaration for XptGarbageHook
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- Simon Nichols, Dec  3 1991
        Changes to -fast_XptAppAddActionList- for HP-UX 8.0.
--- Adrian Howard, Sep 30 1991 : Passed -null_external_ptr- instead of 0 in
        -fast_XtCallActionProc-
--- Adrian Howard, Sep  4 1991 : Allowed a null XEventPtr in
        -fast_XtCallActionProc-
--- Jonathan Meyer, Aug 30 1991
        Added XptGarbageHook. (Should old mechanism be removed?)
--- Roger Evans, Jul  1 1991 removed destroy action from actionhook records
--- Roger Evans, Jun 28 1991 removed Init_action, altered for freelists
--- Roger Evans, Jun 24 1991 moved XptCoerceAction code to library
--- Roger Evans, Jan 29 1991 fixed bug in XptAppAddActionList - didn't
        dereference pointer to coerced proc inside vector properly
        Changed garbage handler to be action-baed
--- Roger Evans, Nov 19 1990 moved gc handler code here
--- Roger Evans, Nov 16 1990
        fixed XptCoerceAction to return an Procedure descriptor
 */
