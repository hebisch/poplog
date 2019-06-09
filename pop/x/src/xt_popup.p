/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/src/xt_popup.p
 > Purpose:         X Toolkit - popup widgets
 > Author:          Roger Evans, Jul  5 1988 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'


section $-Sys$-Xt  =>
                        fast_XtCreatePopupShell,
                        fast_XtVaCreatePopupShell,
                        fast_XtPopup,
                        fast_XtPopupSpringLoaded,
                        fast_XtCallbackNone,
                        fast_XtCallbackNonexclusive,
                        fast_XtCallbackExclusive,
                        fast_XtCallbackPopdown,
                        fast_XtPopdown,
                        raw_XtCallbackNone,
                        raw_XtCallbackNonexclusive,
                        raw_XtCallbackExclusive,
                        raw_XtCallbackPopdown,
;

define fast_XtCreatePopupShell(name,class,parent,args,num_args);
    lvars _widget, name, class, parent, args, num_args;
    X_apply(name,class,parent,args,num_args,
                _5, _extern XtCreatePopupShell) -> _widget;
    Cons_Widget(_widget,class,parent,"popup",name);
enddefine;

define fast_XtVaCreatePopupShell(/*name,class,parent,VARARGS,num_args */);
    lvars num_args, class, parent, _widget, name;

    ;;; handle stack in situ to avoid saving the varargs somewhere

    ;;; total arg count
    XTC_VARARGS(3) -> num_args;

    ;;; fetch (copy) parent and class from under varargs
    subscr_stack(num_args fi_- 2) -> parent;
    subscr_stack(num_args fi_- 1) -> class;
    subscr_stack(num_args) -> name;

    ;;; pass all args out to toolkit routine
    X_apply(_int(num_args), _extern XtVaCreatePopupShell) -> _widget;
    Cons_Widget(_widget,class,parent,"popup", name);
enddefine;

define fast_XtPopup with_nargs 2;
    X_apply(_2, _extern XtPopup) -> ;
enddefine;

define fast_XtPopupSpringLoaded with_nargs 1;
    X_apply(_1, _extern XtPopupSpringLoaded) -> ;
enddefine;

define fast_XtPopdown with_nargs 1;
    X_apply(_1, _extern XtPopdown) -> ;
enddefine;

define fast_XtCallbackNone with_nargs 3;
    X_apply(_3, _extern XtCallbackNone) -> ;
enddefine;

define fast_XtCallbackNonexclusive with_nargs 3;
    X_apply(_3, _extern XtCallbackNonexclusive) -> ;
enddefine;

define fast_XtCallbackExclusive with_nargs 3;
    X_apply(_3, _extern XtCallbackExclusive) -> ;
enddefine;

define fast_XtCallbackPopdown with_nargs 3;
    X_apply(_3, _extern XtCallbackPopdown) -> ;
enddefine;

vars
    raw_XtCallbackNone,
    raw_XtCallbackNonexclusive,
    raw_XtCallbackExclusive,
    raw_XtCallbackPopdown,
;

;;; initialization routine called fro xt_init.p
define Init_popup;
    Cons_Procedure_rec( $-Sys$-Cons_extern_ptr(_extern XtCallbackNone),
                        'XtCallbackNone')
        -> raw_XtCallbackNone;

    Cons_Procedure_rec( $-Sys$-Cons_extern_ptr(_extern XtCallbackNonexclusive),
                        'XtCallbackNonexclusive')
        -> raw_XtCallbackNonexclusive;

    Cons_Procedure_rec( $-Sys$-Cons_extern_ptr(_extern XtCallbackExclusive),
                        'XtCallbackExclusive')
        -> raw_XtCallbackExclusive;

    _extern XtCallbackPopdown -> $-Sys$-Sys_external_ptr!XP_PTR;
    Cons_Procedure_rec( $-Sys$-Cons_extern_ptr(_extern XtCallbackPopdown),
                        'XtCallbackPopdown')
        -> raw_XtCallbackPopdown;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jun 18 1993
        Added "name" argument to Cons_Widget calls
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- John Gibson, Sep  6 1992
        Uses XTC_VARARGS instead of X*TC_varargs (arg conversion done by
        latter is no longer necessary)
        Removed unnecessary calls of X*TC_str
--- Roger Evans, May 29 1991 added shell arg to Cons_Widget calls
--- Roger Evans, Nov 19 1990 added fast_XtCallback... routines
--- Roger Evans, Oct 20 1990 added X*TC_varargs
--- Roger Evans, Oct 11 1990 Much revised
--- Roger Evans, Jul  4 1990 changed to use X_apply
 */
