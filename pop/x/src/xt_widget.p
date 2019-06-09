/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/src/xt_widget.p
 > Purpose:         X Toolkit - widgets
 > Author:          Roger Evans, Jul  5 1990 (see revisions)
 > Documentation:   REF xt_procs
 > Related Files:   xt_*.p
 */

#_INCLUDE 'xt_declare.ph'


section $-Sys$-Xt  =>
                        fast_XtCreateWidget,
                        fast_XtVaCreateWidget,
                        fast_XtAppCreateShell,
                        fast_XtVaAppCreateShell,
                        fast_XtRealizeWidget,
                        fast_XtUnrealizeWidget,
                        fast_XtDestroyWidget,
                        fast_XtCreateManagedWidget,
                        fast_XtVaCreateManagedWidget,
                        fast_XtMapWidget,
                        fast_XtUnmapWidget,
                        XptImportWidget,
                        XptWMProtocols,
;

;;; default protocols to set on a shell window when realized
global vars XptWMProtocols = {'WM_DELETE_WINDOW\(0)'};

/* a variant of list delete ... */
define Del_item(item, _list);
    lvars temp next item _list;
    _list!(w) -> temp;
    returnif(temp == [] or not(temp));
    if item == temp!P_FRONT then
        temp!P_BACK -> _list!(w);
        return;
    endif;
    repeat;
        returnif((temp!P_BACK ->> next) == []);
        if item == next!P_FRONT then
            next!P_BACK -> temp!P_BACK;
            return;
        else
            next -> temp;
        endif;
    endrepeat;
enddefine;


/*  Destroys the POP-11 side of a widget (see -DestroyCallback-). This extra
    level of procedure is provided so that the callback is done via
    XptCallbackHandler (so tracing etc. works properly.)

    This procedure also called in xt_display.p to destroy the POP-11 widget
    info after a display is closed due to an IO error.
*/
define DestroyPopWidget(widget,client,call)
        with_props XptDestroyCallback with_nargs 3;
    lvars dep parent widget client call;
    returnunless(widget);   ;;; Get_Descriptor failed to find widget
    widget!XD_DEPENDENTS -> dep;
    if (dep!XD_W_PARENT ->> parent) then
        ;;; break links between widget and parent
        false -> dep!XD_W_PARENT;
        unless isref(parent) then
            Del_item(widget, parent!XD_DEPENDENTS@XD_W_CHILDREN);
        endunless;
    endif;
    Kill_XptDescriptor(widget);
enddefine;


/*  Destroy callback
    all widget descriptors created by Pop have this procedure
    attached to their destroy callback, so that if external routines
    destroy them, Pop finds out and tidies the widget record

    It is run as an exfunc given a pointer to its arg buffer (see below)
*/
define lconstant DestroyCallback(extdata);
    lvars extdata;

    ;;; actual widget is first arg, so dereference pointer given and
    ;;; turn it into a widget. Fake other callback args so trace behaves
    extdata!XP_PTR!(Widget) -> extdata!XP_PTR;
    XptCallbackHandler(Get_Descriptor(XDT_WIDGET,extdata),false,false,
                       DestroyPopWidget,"callback");
enddefine;


/*  ext_DestroyCallback holds the exfunc version of DestroyCallback,
    and is initialized by Init_widget at toolkit initialization time
*/
lvars ext_DestroyCallback;
define Init_widget();
    exfunc_export(DestroyCallback,16:0100,false) -> ext_DestroyCallback;
enddefine;

lconstant DESTROY_CALLBACK_NAME = 'destroyCallback\(0)';

;;; Basic widget record builder
define lconstant Cons_Widget_rec(_ptr,class,parent,shell, name) -> w;
    lvars w _ptr class parent dep p shell name;

    Cons_XptDescriptor(
        _ptr,
        XDT_WIDGET,
        if name then
            name
        else
            _extern XrmQuarkToString(_ptr!XRW_xrm_name)
                -> $-Sys$-Sys_external_ptr!XP_PTR;
            exacc_ntstring($-Sys$-Sys_external_ptr)
        endif,
        XD_W_DEP_LEN
    ) -> w;

    w!XD_DEPENDENTS -> dep;
    false -> dep!XD_W_PARENT;
    [] -> dep!XD_W_CHILDREN;
    [] -> dep!XD_W_CALLBACKS;
    [] -> dep!XD_W_EVENTHANDLERS;
    shell -> dep!XD_W_SHELL;
    Get_Descriptor(XDT_WIDGETCLASS,class) ->> class -> dep!XD_W_CLASS;

    if parent and (Get_Descriptor(XDT_WIDGET,parent) ->> p) then
        ;;; parent is a widget - add to appropriate siblings list
        p -> dep!XD_W_PARENT;
        p!XD_DEPENDENTS -> dep;
        w :: dep!XD_W_CHILDREN -> dep!XD_W_CHILDREN;
    else
        ;;; use appcon for parent field
        ImportApplicationContextDesc(
            X_apply(w,_1,_extern XtWidgetToApplicationContext)) -> p;
        ;;; and create a destroy-dependent link to it
        erase -> $-Sys$-Sys_destroy_action(
                        consref(p) ->> dep!XD_W_PARENT,
                        true);

        ;;; if -parent- is a display update it with appcon
        if parent and (Get_Descriptor(XDT_DISPLAYPTR,parent) ->> parent)
                  and not(parent!XD_DEPENDENTS!XD_D_APPCON) then
            p -> parent!XD_DEPENDENTS!XD_D_APPCON;
        endif;
    endif;

    ;;; add destroy callback in case widget killed externally
    fast_XtAddCallback(w,DESTROY_CALLBACK_NAME,ext_DestroyCallback,false);
enddefine;

define Cons_Widget(_ptr,class,parent,shell, name) -> w;
    lvars w _ptr class parent shell name;

    Cons_Widget_rec(_ptr,class,parent,shell,name) -> w;

    ;;; MAKE SURE SHELL WIDGETS ARE DESTROYED IF THEY BECOME GARBAGE
    ;;; (CHILDREN WILL BE DESTROYED WHEN THEIR SHELL IS.)
    if fast_XtIsShell(w) then
        fast_XtDestroyWidget -> $-Sys$-Sys_process_destroy_action(w,false);
    endif;
enddefine;

define fast_XtCreateWidget(name,class,parent,args,num_args);
    lvars _widget name class parent args num_args;

    ;;; in situ stack handling only saves a couple of pops so we don't bother
    X_apply(name,class,parent,args,num_args,
                _5, _extern XtCreateWidget) -> _widget;
    Cons_Widget(_widget,class,parent,false,name);
enddefine;

define fast_XtVaCreateWidget(/* name,class,parent,VARARGS, num_args */);
    lvars num_args, class, parent, _widget, name;

    ;;; handle stack in situ to avoid saving the varargs somewhere

    ;;; total number of args
    XTC_VARARGS(3) -> num_args;

    ;;; fetch (copy) parent and class from under varargs
    subscr_stack(num_args fi_- 2) -> parent;
    subscr_stack(num_args fi_- 1) -> class;
    subscr_stack(num_args) -> name;

    ;;; pass all args out to toolkit routine
    X_apply(_int(num_args), _extern XtVaCreateWidget) -> _widget;

    ;;; create a widget record from result
    Cons_Widget(_widget,class,parent,false,name);
enddefine;

define fast_XtAppCreateShell(appname,appclass,class,display,args,num_args)
                                                            -> _widget;
    lvars _widget appname appclass class display args num_args;

    X_apply(appname,appclass,class,display,args,num_args,
            _6, _extern XtAppCreateShell) -> _widget;

    Cons_Widget(_widget,class,display,true,appname) -> _widget;
enddefine;

define fast_XtVaAppCreateShell(/* appname,appclass,class,display,VARARGS,
                                        num_args */) -> _widget;
    lvars _widget, class, display, num_args, appname;

    ;;; handle stack in situ to avoid saving the varargs somewhere

    ;;; total number of args
    XTC_VARARGS(4) -> num_args;

    ;;; copy display and class from under varargs
    subscr_stack(num_args fi_- 3) -> display;
    subscr_stack(num_args fi_- 2) -> class;
    subscr_stack(num_args) -> appname;

    X_apply(_int(num_args), _extern XtVaAppCreateShell) -> _widget;

    Cons_Widget(_widget,class,display,true,appname) -> _widget;
enddefine;

define fast_XtRealizeWidget(w);
    lvars w wd pcols = XptWMProtocols;

    if pcols and (Get_Descriptor(XDT_WIDGET,w) ->> wd)
    and wd!XD_DEPENDENTS!XD_W_SHELL then
        ;;; shell (or popup) widget - do protocol handler stuff (in XtPoplog.c)
        X_apply(w, pcols, datalength(pcols), _3, _extern XptAddProtoHandler) -> ;
    else
        ;;; just realize
        X_apply(w, _1, _extern XtRealizeWidget) -> ;
    endif;
enddefine;

define fast_XtUnrealizeWidget(widget);
    lvars widget;
#_IF _XtVersion == 11004
    /* call this first to set display at front of _XtPerDisplay list */
    X_apply(widget, _1, _extern XtWidgetToApplicationContext)->;
#_ENDIF
    X_apply(widget, _1, _extern XtUnrealizeWidget) -> ;
enddefine;

/*  destroy a widget, unless its dead already */
define fast_XtDestroyWidget(widget);
    lvars widget;
    unless widget!XP_PTR == _NULL then
#_IF _XtVersion == 11004
        /* call this first to set display at front of _XtPerDisplay list */
        X_apply(widget, _1, _extern XtWidgetToApplicationContext)->;
#_ENDIF
        X_apply(widget,_1, _extern XtDestroyWidget) -> ;
        ;;; callback does all the tidying of pop records
    endunless;
enddefine;

define fast_XtCreateManagedWidget(name,class,parent,args,num_args);
    lvars _widget name class parent args num_args;

    ;;; in situ stack handling only saves a couple of pops so we don't bother
    X_apply(name,class,parent,args,num_args,
            _5, _extern XtCreateManagedWidget) -> _widget;
    Cons_Widget(_widget,class,parent,false,name);
enddefine;


define fast_XtVaCreateManagedWidget(/* name,class,parent,VARARGS, num_args */);
    lvars num_args, class, parent, _widget, name;

    ;;; handle stack in situ to avoid saving the varargs somewhere

    ;;; total number of args
    XTC_VARARGS(3) -> num_args;

    ;;; fetch (copy) parent and class from under varargs
    subscr_stack(num_args fi_- 2) -> parent;
    subscr_stack(num_args fi_- 1) -> class;
    subscr_stack(num_args) -> name;

    ;;; pass all args out to toolkit routine
    X_apply(_int(num_args), _extern XtVaCreateManagedWidget) -> _widget;

    ;;; create a widget record from result
    Cons_Widget(_widget,class,parent,false,name);
enddefine;


define fast_XtMapWidget with_nargs 1;
    X_apply(_1, _extern XtMapWidget) -> ;
enddefine;

define fast_XtUnmapWidget with_nargs 1;
    X_apply(_1, _extern XtUnmapWidget) -> ;
enddefine;

define lconstant ImportWidgetDesc(_ptr);
    lvars _ptr;
    Descriptor(XDT_WIDGET,_ptr) or
    (   Cons_Widget_rec(_ptr,
                    ImportWidgetClassDesc(_ptr!XRW_widget_class),
                    if _ptr!XRW_parent == _NULL then
                        false;  ;;; Cons_Widget_rec will use appcon
                        true;   ;;; its a shell
                    else
                        ImportWidgetDesc(_ptr!XRW_parent);
                        false;  ;;; not a shell
                    endif,
                    false);
    );
enddefine;

;;; system import routine
define ImportWidget with_nargs 1;
    Register(ImportWidgetDesc());
enddefine;

;;; public implicit access routine
define XptImportWidget(_ptr);
    lvars _ptr;
    Checkr_exptrclass_ptr(_ptr) -> _ptr;
    if _ptr == _NULL then
        false
    else
        ImportWidget(_ptr);
    endif;
enddefine;

define updaterof XptImportWidget(_ptr) -> _ptr;
    lvars _ptr;
    if _ptr then
        unless XptDataType(_ptr) == XDT_WIDGET then
            mishap(_ptr,1,'Widget NEEDED');
        endunless;
    else
        null_external_ptr -> _ptr;
    endif;
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Nov 10 1997
        Changed test for shell in Cons_Widget
--- Adrian Howard, Jun 18 1993
         # Gadget names now accessed via XrmQuarkToString
         #  Widget names now taken from Xt(Va)Create... procedures if possible
         #  Now only shell widgets have associated destroy actions since child
            widgets can never become garbage before their parents
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- Adrian Howard, Nov 27 1992
        Made callback to destroy the POP-11 side of a widget instance
        available in the Xt section so it can be using in xt_display.p
--- John Gibson, Sep  6 1992
        Removed unnecessary calls of X*TC_str
--- John Gibson, Sep  6 1992
        Uses XTC_VARARGS instead of X*TC_varargs (arg conversion done by
        latter is no longer necessary)
--- Adrian Howard, Jun 12 1992
        Widgets destroy action is now in -sys_process_destroy_action- to
        prevent problems with forks.
--- Adrian Howard, Oct  2 1991 : Added initialisation of event handler widget
        dependents.
--- Jonathan Meyer, Aug 19 1991
        Added calls to _extern XtWidgetToApplicationContext in DestroyWidget
        and UnrealizeWidget - this is a workaround for the bug in XtDestroyGC,
        which the widget might call during its destroy action.
--- Jonathan Meyer, Jul 31 1991 Renamed XptWm -> XptWM
--- Roger Evans, Jun 28 1991 altered for freelists
--- Roger Evans, Jun 27 1991 removed numargs arg from exfunc_export and
        XptCallbackApply
--- Roger Evans, Jun 25 1991
        fixed DestroyCallback to be an exfunc procedure
--- Jonathan Meyer, Jun 17 1991
        Changed XtIsWidget back to using _extern (X_Apply checks its
        argument looking for a key field which doesn't exist).
--- Roger Evans, Jun 19 1991
        changed call to XTIsWidget to use X_Apply, and props for
        non-widgets to <false>
--- Jonathan Meyer, Jun 17 1991
        Added call to XtIsWidget to make objects/gadgets get the label
        <unnamed object> rather than <termin> or some other strange thing.
--- Roger Evans, May 29 1991 added shell arg to Cons_Widget calls
        and better wm protocol handling in XtRealize
--- Roger Evans, Apr  3 1991
        Fixed previous change to cope with non-existent descriptor
--- John Gibson, Mar 30 1991
        Added call of -Get_Descriptor- in -DestroyCallback-.
--- Roger Evans, Jan 29 1991 added protocol handler to XtRealizeWidget
--- Roger Evans, Jan 27 1991 fixed bug in Cons_widget_rec causing problem
        reported as jonm.5 (mishap when importing shell widgets)
--- Roger Evans, Dec  6 1990 removed buggy display code from ImportWidget
--- Jonathan Meyer, Nov 26 1990
        Fixed XtVaCreate* to call extern XtVaCreate* procedures
        rather than XtCreate* procs.
--- Roger Evans, Nov  4 1990 added ImportWidget
--- Roger Evans, Oct 20 1990 added X*TC_varargs
--- Roger Evans, Oct 11 1990 Much revised
 */
