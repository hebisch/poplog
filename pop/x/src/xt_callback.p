/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/src/xt_callback.p
 > Purpose:         X toolkit - callbacks
 > Author:          Roger Evans, Jul  5 1988 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'


section $-Sys$-Xt  =>
                        fast_XtAddCallback,
                        fast_XtAddCallbacks,
                        fast_XptAddCallbackList,
                        fast_XtRemoveCallback,
                        fast_XtRemoveCallbacks,
                        fast_XptRemoveCallbackList,
                        fast_XtRemoveAllCallbacks,
                        fast_XtCallCallbacks,
                        fast_XtHasCallbacks,
;

define fast_XtAddCallback(w,name,proc,arg);
    lvars w name proc arg wd;
    XTC_str(name) -> name;

    ;;; save args in widget dependencies if possible
    if (Get_Descriptor(XDT_WIDGET,w) ->> wd) then
        wd!XD_DEPENDENTS -> wd;
        consvector(name,proc,arg,3) :: wd!XD_W_CALLBACKS -> wd!XD_W_CALLBACKS;
    endif;
    ;;; add the callback
    X_apply(w, name, proc, arg, _4, _extern XtAddCallback) -> ;
enddefine;

define fast_XptAddCallbackList(w,name,list);
    lvars w name list item vec wd clist proc arg;
    Get_Descriptor(XDT_WIDGET,w) -> wd;
    if wd then wd!XD_DEPENDENTS!XD_W_CALLBACKS -> clist endif;
    (#| for item in list do
            fast_front(item), fast_front(fast_back(item));
            if wd then
                -> proc -> arg;
                consvector(name,proc,arg,3) :: clist -> clist;
                arg; proc;
            endif;
        endfor;
        _NULL; _NULL
    |#);
    consvector() -> vec;
    if wd then clist -> wd!XD_DEPENDENTS!XD_W_CALLBACKS; endif;
    X_apply(w,XTC_str(name),vec, _3, _extern XtAddCallbacks) ->;
enddefine;

define fast_XtAddCallbacks(w,name,list);
    lvars w name list;
    ;;; no coercion or dependency handling for callbacks - user must do it
    X_apply(w,XTC_str(name),list,_3, _extern XtAddCallbacks) ->;
enddefine;


;;; look for matching saved record, remove and return it if found
define lconstant RemoveCallback(w, name, proc, arg);
    lvars l, last, rec, w;
    dlvars name, proc, arg;

    ;;; does this record match the spec ?
    define lconstant Matches(rec);
        lvars rec;
        fast_subscrv(1,rec) = name and          ;;; same name
        (   not(proc)                           ;;; no proc check needed
         or
            fast_subscrv(2,rec) = proc and      ;;; same (external) proc
            fast_subscrv(3,rec) = arg           ;;; same arg
        );
    enddefine;

    returnunless(w and (Descriptor(XDT_WIDGET,w!XP_PTR) ->> w)) (false);

    w!XD_DEPENDENTS -> w;
    w!XD_W_CALLBACKS -> l;
    false -> last;

    until l == [] do
        if Matches(fast_front(l) ->> rec) then
            sys_grbg_destpair(l) -> (, l);
            l -> if last then fast_back(last) else w!XD_W_CALLBACKS endif;
            returnif(proc) (rec);   ;;; only looking for one record
        else
            l -> last;
            fast_back(l) -> l
        endif
    enduntil;
    false
enddefine;


define fast_XtRemoveCallback(w,name,proc,arg);
    lvars w name proc arg;

    XTC_str(name) -> name;

    ;;; recover external form (and remove from dependencies)
    RemoveCallback(w,name,proc,arg) -> proc;

    if proc then
        X_apply(w, name, fast_subscrv(2,proc), fast_subscrv(3,proc),
                _4, _extern XtRemoveCallback) -> ;
    endif;
enddefine;

define fast_XtRemoveCallbacks(w,name,list);
    lvars w name list;
    ;;; no coercion or dependency handling for callbacks - user must do it
    X_apply(w,XTC_str(name),list,_3, _extern XtRemoveCallbacks) ->;
enddefine;

define fast_XptRemoveCallbackList(w,name,list);
    lvars w name list item vec;

    XTC_str(name)-> name;

    (#| for item in list do
            RemoveCallback(w, name, fast_front(item),
                                    fast_front(fast_back(item))) -> item;
            if item then
                fast_subscrv(2,item); fast_subscrv(3,item);
            endif;
        endfor;
        _NULL; _NULL
    |#);
    consvector() -> vec;

    X_apply(w,name,vec, _3, _extern XtRemoveCallbacks) ->;
enddefine;


define fast_XtRemoveAllCallbacks(w,name);
    lvars w name;

    XTC_str(name) -> name;
    RemoveCallback(w, name, false, false) -> ;
    X_apply(w,name, _2, _extern XtRemoveAllCallbacks) ->;
enddefine;

define fast_XtCallCallbacks(w,name,data);
    lvars w name data;
    X_apply(w,XTC_str(name), data, _3, _extern XtCallCallbacks) ->;
enddefine;

define fast_XtHasCallbacks(widget,name);
    lvars widget name;
    _pint(X_apply(widget,XTC_str(name), _2, _extern XtHasCallbacks));
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 25 1993
        Rewrote RemoveCallback so it works.
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- Adrian Howard, Mar 30 1992
        Stopped -RemoveCallback- mishapping when an attempt is made to remove
        a callback which is not known to POPLOG.
--- Roger Evans, Jul 31 1991 fixed bug ([2] -> [_2]) in RemoveCallback
        causing XtRemoveCallback to behave erratically!
--- Roger Evans, Jun 24 1991 moved pop procedure coercion to libraries
--- Roger Evans, Nov 18 1990 added removecallbacks and removecallbacklist
--- Roger Evans, Nov  8 1990 bug fix in XptCoerceCallback
--- Roger Evans, Nov  4 1990 added XptAddCallbackList
--- Roger Evans, Oct 11 1990 Much revised
--- Roger Evans, Jul  4 1990 changed to use X_apply
--- James Goodlet, Mar 23 1990 - merged changes to Call_callbacks in local
        development xpop with those in master version.
--- Roger Evans, Feb  2 1990
    added disabling of sigio in Call_callbacks
--- Roger Evans,
    added flag to External_coerce_rec
 */
