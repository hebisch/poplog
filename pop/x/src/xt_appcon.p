/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/src/xt_appcon.p
 > Purpose:         X Toolkit - application contexts
 > Author:          Roger Evans, Jul  5 1990 (see revisions)
 > Documentation:   REF xt_procs
 > Related Files:   xt_*.p
 */

#_INCLUDE 'xt_declare.ph'
#_INCLUDE '../../src/io.ph'


section $-Sys$-Xt =>
                        fast_XtCreateApplicationContext,
                        fast_XptCreateApplicationContext,
                        fast_XtDestroyApplicationContext,
                        XptImportApplicationContext,
                        fast_XtWidgetToApplicationContext,
                        fast_XtAppSetFallbackResources,
                        XptCurrentAppContext,
;

global vars XptCurrentAppContext = false;


    /* See pop_set_xt_wakeup in c_core.c */
struct XTWAKE
  { byte    XWK_ON,             ;;; interrupt has occurred
            XWK_FLAG_ENABLED,   ;;; flag mechanism enabled
            XWK_FLAG_ON,        ;;; flag is on
            XWK_FD_IN,          ;;; pipe input fd/event flag number
            XWK_FD_OUT,         ;;; output fd
            XWK_SPARE[3];
  };

define Init_appcon();
    lvars _xwk = _extern __pop_xt_wakeup_struct:data;
#_IF DEF VMS
    ;;; Put a (nonzero) event flag number in XWK_FD_IN -- added as alternate
    ;;; input for every appcon
    _extern pop\$get_clust0_ef() -> _xwk!XWK_FD_IN;
#_ELSEIF DEF UNIX
    ;;; create a pipe whose input descriptor is added as alternate input
    ;;; for every appcon. _extern pop_set_xt_wakeup writes a single char to
    ;;; the other end
    $-Sys$-Io$-New_pipe() -> (_xwk!XWK_FD_OUT, _xwk!XWK_FD_IN);
#_ELSEIF DEF WIN32
    ;;; no wakeup mechanism enabled yet
#_ELSE_ERROR
#_ENDIF
enddefine;


;;; make a basic appcon record
define lconstant Cons_AppCon_rec(_ptr) -> appcon;
    lvars _ptr, appcon, dep;

    Cons_XptDescriptor(_ptr,XDT_APPCONTEXT,false,XD_AC_DEP_LEN) -> appcon;
    appcon!XD_DEPENDENTS -> dep;
    [] -> dep!XD_AC_DISPLAYS;
    [] -> dep!XD_AC_INPUTS;
    [] -> dep!XD_AC_TIMEOUTS;
    [] -> dep!XD_AC_ACTIONHOOKS;
    [] -> dep!XD_AC_ACTIONS;
    false -> dep!XD_AC_ASYNC;

    ;;; set up error handlers (defined in xt_error.p)
    Appcon_error(appcon);
    ;;; set up default actions (defined in XtPoplog.c)
    X_apply(appcon, _1, _extern XptAddAppconActions) -> ;

#_IF DEF VMS or DEF UNIX
    ;;; add wakeup callback for pipe input fd/event flag
    X_apply(appcon, _pint(_extern __pop_xt_wakeup_struct:data!XWK_FD_IN),
            #_IF DEF VMS 0  #_ELSE XtInputReadMask  #_ENDIF,
            EXTERN_PTR(_XptDummyInput), 0, _5, _extern XtAppAddInput) -> ;
#_ENDIF
enddefine;

;;; make an appcon record for a new appcon
define fast_XtCreateApplicationContext() -> appcon;
    lvars appcon;
    Cons_AppCon_rec(X_apply(_0, _extern XtCreateApplicationContext)) -> appcon;
    ;;; add destroy action
    fast_XtDestroyApplicationContext
                        -> $-Sys$-Sys_process_destroy_action(appcon,false);
enddefine;

define fast_XptCreateApplicationContext(name) -> appcon;
    lvars name, appcon = fast_XtCreateApplicationContext();
    if name then
        conspair(appcon!XP_PROPS,name) -> appcon!XP_PROPS;
    endif;
    appcon -> XptCurrentAppContext;
enddefine;

/*  io_err flag true means destroying due to an IO error - mustn't
    actually call toolkit destroy routines in this case (so we leave
    C structs floating around, but hopefully completely harmless...)
*/
define DestroyApplicationContext(appcon, io_err);
    lvars appcon, desc, io_err, dpy;

    returnif(appcon!XP_PTR == _NULL);

    if appcon == XptCurrentAppContext then
        false -> XptCurrentAppContext;
    endif;
    Get_Descriptor(XDT_APPCONTEXT,appcon) -> desc;
    if desc then
        false -> XptAsyncAppContext(desc);
        fast_for dpy in desc!XD_DEPENDENTS!XD_AC_DISPLAYS do
            CloseDisplay(dpy, io_err)
        endfor
    endif;
    unless io_err then
        X_apply(appcon,_1,_extern XtDestroyApplicationContext) -> ;
    endunless;
    if desc then
        Kill_XptDescriptor(desc);
    endif;
enddefine;

define fast_XtDestroyApplicationContext with_nargs 1;
    DestroyApplicationContext(false);
enddefine;

define ImportApplicationContextDesc(_ptr);
    lvars _ptr;
    Descriptor(XDT_APPCONTEXT, _ptr) or Cons_AppCon_rec(_ptr);
enddefine;

define XptImportApplicationContext(_ptr);
    lvars _ptr;
    Checkr_exptrclass_ptr(_ptr) -> _ptr;
    if _ptr == _NULL then
        false
    else
        Register(ImportApplicationContextDesc(_ptr));
    endif;
enddefine;

define updaterof XptImportApplicationContext(_ptr) -> _ptr;
    lvars _ptr;
    if _ptr then
        unless XptDataType(_ptr) == XDT_APPCONTEXT then
            mishap(_ptr,1,'AppContext NEEDED');
        endunless;
    else
        null_external_ptr -> _ptr;
    endif;
enddefine;

define fast_XtWidgetToApplicationContext with_nargs 1;
    Register(ImportApplicationContextDesc(
                X_apply((),_1,_extern XtWidgetToApplicationContext)));
enddefine;

define fast_XtAppSetFallbackResources(appcon, spec) with_nargs 2;
    lvars appcon, spec;
    /* no sensible coercion for spec possible, false == null */
    X_apply(
        appcon,
        if spec then
            spec
        else
            null_external_ptr
        endif,
        _2, _extern XtAppSetFallbackResources
    ) -> ;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, May 29 1996
        Added Win32 case (for compilation in the NuTCRACKER environment)
--- John Gibson, Nov 14 1993
        Stopped DestroyApplicationContext creating garbage closures by
        using a for loop
--- John Gibson, Mar 29 1993
        Replaced use of _pint on extern address in Cons_AppCon_rec with new
        macro EXTERN_PTR
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- John Gibson, Dec  5 1992
        VMS changes
--- Adrian Howard, Jun 12 1992
        Application context destroy action now in -sys_process_destroy_action-
--- John Gibson, Dec  2 1991
        VMS mod to Cons_AppCon_rec to add event flag 3 as alt input
--- Adrian Howard, Sep 30 1991 : Changed to pass -null_external_ptr- instead
        of 0 in -fast_XtAppSetFallbackResources-.
--- Adrian Howard, Aug 23 1991 : -fast_XtAppSetFallbackResources- now accepts
        -false- StringList as null.
--- Roger Evans, Jun 28 1991 altered for freelists
--- Roger Evans, May 26 1991
        Added IO error handling code to XtDestroyApplicationContext
--- John Gibson, Mar 13 1991
        Added false initialisation for XD_AC_ASYNC in -Cons_AppCon_rec-,
        and unsetting async in -fast_XtDestroyApplicationContext-. Also
        final VMS mods.
--- John Gibson, Feb 11 1991
        Added VMS mods
--- Roger Evans, Jan 30 1991 moved stuff from Cons_Appcon to Cons_Appcon_rec
--- Roger Evans, Jan 29 1991 added action initialisation
--- John Gibson, Jan 24 1991
        Replaced condition mask 1 with XtInputReadMask in call to
        _extern XtAppAddInput.
--- John Gibson, Jan 19 1991
        Changed -Init_appcon- to create a pipe and assign file descriptors
        into _extern __pop_xt_wakeup_struct; changed -Cons*_AppCon- to
        add callback for input descriptor.
--- Roger Evans, Nov 21 1990 added Init_appcon and dummy input code
--- Roger Evans, Nov  4 1990
    added actionhooks and actions
--- Roger Evans, Oct 11 1990 Much revised
 */
