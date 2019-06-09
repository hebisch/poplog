/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.x/x/src/xt_display.p
 > Purpose:         X Toolkit - displays
 > Author:          Roger Evans, Jul  16 1990 (see revisions)
 > Documentation:   REF *XT_DISPLAY
 > Related Files:   xt_*.p
 */

#_INCLUDE 'xt_declare.ph'
#_INCLUDE '../../src/io.ph'
#_INCLUDE '../../lib/include/sigdefs.ph'

section $-Sys$-Xt  =>
                        fast_XtDisplayInitialize,
                        fast_XtOpenDisplay,
                        fast_XtCloseDisplay,
                        XptImportDisplayPtr,
                        XptImportUnregisteredDisplayPtr,
                        fast_XtDisplay,
                        XptDisplayDevice,
                        fast_XtDisplayToApplicationContext,
                        XptUnprocessedArgs,
                        fast_XptSetUnprocessedArgs
;


    /*  Keep a record of all display connections: assigned by
        Cons_DisplayPtr_rec, entries disappear when they become garbage
    */
define lconstant DisplayProp =
    newproperty([], 16, false, "tmparg");
enddefine;

define lconstant DisplayAppContext(display);
    lvars display;
    ImportApplicationContextDesc(
        X_apply(display,_1, _extern XtDisplayToApplicationContext)
    );
enddefine;

define lconstant Cons_DisplayPtr_rec(_ptr, appcon, display, registered)
                                                                -> display;
    lvars _ptr, appcon, display, dep, name, ptr, ref, registered;

    unless display then
        /* fetch device name */
        Uint_->_pint(_ptr) -> ptr;
        X_apply(ptr, _1, _extern XDisplayString)
                        -> $-Sys$-Sys_external_ptr!XP_PTR;
        exacc_ntstring($-Sys$-Sys_external_ptr) -> name;

        /* make descriptor */
        Cons_XptDescriptor(_ptr,XDT_DISPLAYPTR,name,XD_D_DEP_LEN) -> display;

        ;;; ADD TO SET OF ACTIVE DISPLAYS
        true -> DisplayProp(display);
    endunless;

    if registered then
        if appcon then
            Get_Descriptor(XDT_APPCONTEXT,appcon)
        else
            DisplayAppContext(display)
        endif;
    else
        false
    endif -> appcon;

    ;;; make links between display and appcon
    display!XD_DEPENDENTS -> dep;
    appcon -> dep!XD_D_APPCON;
    if appcon then
        appcon!XD_DEPENDENTS -> dep;
        display :: dep!XD_AC_DISPLAYS -> dep!XD_AC_DISPLAYS;
    endif;
enddefine;

;;; make a DisplayPtr record for a new display
define lconstant Cons_DisplayPtr(_ptr,appcon,display) -> display;
    lvars _ptr, appcon, display;
    Cons_DisplayPtr_rec(_ptr,appcon,display, true) -> display;
    fast_XtCloseDisplay -> $-Sys$-Sys_process_destroy_action(display,false);
enddefine;

define lconstant Check_display_unused(display);
    lvars display, appcon;
    unless (((display!XD_DEPENDENTS)!XD_D_APPCON) ->> appcon) == false do
        mishap(display, 1, 'DISPLAY ALREADY USED IN ' sys_>< appcon);
    endunless;
enddefine;

/* the following list is set as a side-effect of calling OpenDisplay. It
   holds any remaining arguments passed in argv/argc - these arguments
   aren't recognized by the OpenDisplay.
*/
protected vars XptUnprocessedArgs = [];

/* public because it is also used in LIB XT_APPINIT */

define fast_XptSetUnprocessedArgs(argc, argv);
    lvars argc, argv, _argc, _argv;
    if XptUnprocessedArgs /== [] then
        ;;; always garbage last list
        sys_grbg_list(XptUnprocessedArgs);
    endif;

    [%  argc -> _argc;
        if argc!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR then
            argc!((int)) -> _argc
        endif;
        _argc!(int) -> _argc;

        argv -> _argv;
        if (argv!KEY)!K_FLAGS _bitst _:M_K_EXTERN_PTR then
            argv!(((b))) -> _argv
        endif;
        _argv@((b))[_1] -> _argv;

        fast_repeat _pint(_argc _sub _1) times
            Consstring_bptr(_argv!((b))++ -> _argv, _-1, CSB_FIXED)
        endrepeat;

    %] -> XptUnprocessedArgs;
enddefine;

define fast_XtDisplayInitialize(appcon,display,appname,appclass,
                                options,numoptions,argc,argv);
    lvars appcon display appname appclass options numoptions argc argv;

    Check_display_unused(display);

    X_apply(appcon, display, appname, appclass,
            options, numoptions, argc, argv,
            _8, _extern XtDisplayInitialize) ->;

    fast_XptSetUnprocessedArgs(argc, argv);

    ;;; make a descriptor for display and put it in appcon list
    ;;; (but don't return it to the user!);
    Cons_DisplayPtr(display!XP_PTR,
                    appcon,
                    Get_Descriptor(XDT_DISPLAYPTR,display)) -> ;
enddefine;

define fast_XtOpenDisplay(appcon,display_string,appname,appclass,
                        options, numoptions, argc,argv);
    lvars _display appcon, display_string, appname, appclass,
          options, numoptions argc, argv;

    ;;; Block signals while we're doing this -- necessary (at least) in Unix
    ;;; to stop async appcontext timers interrupting the opening of the
    ;;; display connection (XOpenDisplay is not checking for EINTR and
    ;;; retrying, so it returns an error).

    dlocal 0 %_extern _pop_sigmask(_1)->, _extern _pop_sigmask(_0)-> %;

#_IF DEF NCR
    ;;; XtOpenDisplay can scribble on the handler for SIGALRM (when
    ;;; opening a local display)
    lconstant macro SIG_IGN = 1;
    lvars _save_sigalrm;
    dlocal 0 %
        _extern _pop_sigaction(_:SIG_ALRM, _:SIG_IGN) -> _save_sigalrm,
        _extern _pop_sigaction(_:SIG_ALRM, _save_sigalrm) ->
    %;
#_ENDIF

    X_apply(
        appcon,
        display_string,
        appname,
        appclass,
        options, numoptions,
        argc,argv,
    _8, _extern XtOpenDisplay) -> _display;

    fast_XptSetUnprocessedArgs(argc, argv);

    if _display == _NULL then
        mishap(display_string,1,
               ('INVALID DISPLAY SPECIFICATION';
                unless display_string then
                    sys_>< ' ($DISPLAY NOT SET?)';
                endunless)
              );
    endif;
    Cons_DisplayPtr(_display,appcon,false);
enddefine;

/*  io_err flag true means called from IO error - don't actually attempt
    to close the toolkit display structure in this case */
define CloseDisplay(display, io_err);
    lvars display, io_err;

        ;;; return all the widgets associated with a display, plus a
        ;;; count of their number
    define lconstant widgets_of_display(display);
        lvars display;
        dlvars _display_ptr = display!XP_PTR;
        (#| fast_appproperty(DescriptorProp,
                procedure(_ptr, desc);
                    lvars _ptr, desc;
                    if fast_XptDataType(desc) == XDT_WIDGET
                    ;;; CHECK WIDGET USES THE DISPLAY THAT IS BEING
                    ;;; CLOSED: SINCE ALL INFORMATION TO GET THE DISPLAY
                    ;;; OF A WIDGET EXISTS ON THE CLIENT SIDE IT'S SAFE
                    ;;; TO CALL THIS PROCEDURE EVEN IF THE DISPLAY
                    ;;; CONNECTION IS DEAD DUE TO AN IO ERROR
                    and X_apply(desc, _1, _extern XtDisplayOfObject) == _display_ptr
                    then
                        desc;
                    endif;
                endprocedure)
        |#);
    enddefine;

    unless display!XP_PTR == _NULL then

        ;;; DESTROY WIDGETS ASSOCIATED WITH THE DISPLAY
        fast_repeat widgets_of_display(display) times
            lvars widget = ();
            ;;; IF THE DISPLAY CONNECTION WAS CLOSED BY AN IO ERROR JUST
            ;;; KILL THE POP-11 SIDE OF THE WIDGET
            if io_err then
                DestroyPopWidget(widget, false, false);
            ;;; OTHERWISE KILL IT PROPERLY (IF ITS A SHELL --- CHILDREN
            ;;; SHOULD GET DESTROYED BY THEIR PARENT)
            elseif fast_XtIsShell(widget) then
                fast_XtDestroyWidget(widget);
            endif;
        endrepeat;

        ;;; DON'T CLOSE DISPLAY IF CALLED FROM IO ERROR
        unless io_err then
            X_apply(display, _1, _extern XtCloseDisplay) -> ;
        endunless;

        if Get_Descriptor(XDT_DISPLAYPTR, display) ->> display then
            lvars appcon = display!XD_DEPENDENTS!XD_D_APPCON;
            if appcon then
                Del_item(display, appcon!XD_DEPENDENTS@XD_AC_DISPLAYS);
            endif;
            Kill_XptDescriptor(display);
        endif;

    endunless;
enddefine;

#_IF DEF UNIX

    /* destroy all displays after a fork */
define Kill_displays();
    ;;; terminate a display with extreme prejudice: must not change
    ;;; DisplayProp
    define lconstant Kill_display(display, flag);
        lvars display, flag;
        returnif(_zero(display!XP_PTR));
        ;;; close the file descriptor
        _extern close(X_apply(display, _1, _extern XConnectionNumber)) -> ;
        ;;; destroy Pop-11 display structure: passing true as the io_err
        ;;; parameter indicates that the display pointer is unusable
        CloseDisplay(display, true);
    enddefine;
    fast_appproperty(DisplayProp, Kill_display);
    clear_prop_entries(DisplayProp);
enddefine;

#_ENDIF

define fast_XtCloseDisplay() with_nargs 1;
    CloseDisplay(false);
enddefine;

define lconstant ImportDisplayPtrDesc(_ptr);
    lvars _ptr;
    Descriptor(XDT_DISPLAYPTR, _ptr) or
        Cons_DisplayPtr_rec(_ptr,false,false, true);
enddefine;

define lconstant ImportUnregisteredDisplayPtrDesc(_ptr);
    lvars _ptr;
    Descriptor(XDT_DISPLAYPTR, _ptr) or
        Cons_DisplayPtr_rec(_ptr,false,false, false);
enddefine;

define XptImportDisplayPtr(_ptr);
    lvars _ptr;
    Checkr_exptrclass_ptr(_ptr) -> _ptr;
    if _ptr == _NULL then
        false
    else
        Register(ImportDisplayPtrDesc(_ptr));
    endif;
enddefine;

define updaterof XptImportDisplayPtr(_ptr) -> _ptr;
    lvars _ptr;
    if _ptr then
        unless XptDataType(_ptr) == XDT_DISPLAYPTR then
            mishap(_ptr,1,'DisplayPtr NEEDED');
        endunless;
    else
        null_external_ptr -> _ptr;
    endif;
enddefine;

define XptImportUnregisteredDisplayPtr(_ptr);
    lvars _ptr;
    Checkr_exptrclass_ptr(_ptr) -> _ptr;
    if _ptr == _NULL then
        false
    else
        Register(ImportUnregisteredDisplayPtrDesc(_ptr));
    endif;
enddefine;

define updaterof XptImportUnregisteredDisplayPtr(_ptr) -> _ptr;
    lvars _ptr;
    if _ptr then
        unless XptDataType(_ptr) == XDT_DISPLAYPTR then
            mishap(_ptr,1,'DisplayPtr NEEDED');
        endunless;
    else
        null_external_ptr -> _ptr;
    endif;
enddefine;

define fast_XtDisplay(w);
    lvars w;
    Register(ImportDisplayPtrDesc(X_apply(w, _1, _extern XtDisplay)));
enddefine;

define fast_XtDisplayToApplicationContext() with_nargs 1;
    Register(DisplayAppContext());
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Nov 10 1997
        Changed test for shell in CloseDisplay
--- John Gibson, Jun 26 1997
        Fixed incorrect last arg to Consstring_bptr
--- Robert Duncan, Sep 24 1996
        Added special case to fast_XtOpenDisplay for NCR to restore the
        signal handler for SIGALRM, otherwise it gets reset to the default
        (SIG_DFL -- aborts with 'Alarm call') when the display is local.
--- John Gibson, Mar 31 1995
        Fixed use of types in fast_XptSetUnprocessedArgs
--- Robert John Duncan, Feb  1 1995
        Added Kill_displays, called from sys_fork to close all active
        displays. Uses a temporary property DisplayProp instead of the old
        X*_display_list so that displays can be garbage-collected properly.
        Also simplified the method of killing a display to do the minimum.
--- Robert John Duncan, Feb  1 1995
        Changed the way CloseDisplay destroys its widgets: destroying a
        widget updates DescriptorProp and so is unsafe inside a call of
        fast_appproperty (a simple appproperty would copy the whole of the
        descriptor table, which is a bit of overkill).
--- Adrian Howard, Jul  5 1993
        CloseDisplay now only calls fast_XtDestroyWidget on shells, letting
        the children be destroyed by Xt.
--- Adrian Howard, Mar 18 1993
        Now actually removes the displays from the per-process display list.
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- Adrian Howard, Nov 27 1992
        Closing a display now destroys the widgets associated with that
        display first.
--- John Gibson, Sep  6 1992
        Removed unnecessary calls of X*TC_str
--- John Gibson, Jun 29 1992
        Removed XptD*isplayDevice and got rid of device in XD_D_D*EVICE field.
--- Adrian Howard, Jun 17 1992
        Added -X_display_list- to store all the open Xdisplays in POPLOG. Cleared
        by -sysfork-.
--- Adrian Howard, Jun 12 1992
        Display pointers destroy action is now in -sys_process_destroy_action-
--- Adrian Howard, Sep 30 1991 : Changed instances of 0 to -null_external_ptr-
--- John Gibson, Sep 28 1991
        Amplified comment in fast_XtOpenDisplay about why signal blocking
        is necessary.
--- Adrian Howard, Sep 27 1991
        o Removed checks for a null return from _XtDisplayToApplicationContext
          since it does not check that the display is registered with the
          toolkit
        o -XtDisplayInitialize- no longer leaves rubbish on the stack and also
          checks that the display is not used in another application context
        o rewrote -XptSetUnprocessedArgs- so it would work in general.
        o renamed -XptSetUnprocessedArgs- to -fast_XptSetUnprocessedArgs-
        o added -XptImportUnregisteredDisplayPtr- for display pointers not
          registered with the toolkit
--- Adrian Howard, Sep 25 1991 : -fast_XtOpenDisplay- now allows -false-
        (null) display and application name strings.
--- Jonathan Meyer, Aug 30 1991
        Added XptUnprocessedArgs and XptSetUnprocessedArgs
--- John Gibson, Jul  9 1991
        Blocked signals during fast_XtOpenDisplay
--- Roger Evans, Jun 28 1991 altered for freelists
--- Roger Evans, May 26 1991
        Added io error code to XtCloseDisplay
--- John Gibson, Feb 11 1991
        Added VMS mods. Moved Unix no-fd device close procedure for display
        to devio.p where it belongs!
--- Roger Evans, Feb  3 1991 added Display_close
--- Roger Evans, Dec  6 1990
        changed XptDisplayAppContext to fast_XtDisplayToApplicationContext
        and fixed up importing to get the appcontext right
--- ROger Evans, Nov 12 1990 added check for failed display connection
--- Roger Evans, Oct 11 1990 Much revised
 */
