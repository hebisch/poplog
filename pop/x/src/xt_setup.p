/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/src/xt_setup.p
 > Purpose:         Default setup procedure for X interface
 > Author:          Roger Evans, Oct  9 1990 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'


section $-Sys$-Xt  =>   sysxsetup,
                        XptSetArgv,
                        XptDefaultSetup,
                        XptDefaultShutdown,
                        XptDefaultAppContext,
                        XptDefaultDisplay,
                        XptDefaultFallbackResources
;

global vars
    XptDefaultAppContext = false,
    XptDefaultDisplay = false,
    XptDefaultFallbackResources = false
;

define XptSetArgv(list);
    lvars argc list;
    unless islist(list) then [] -> list; endunless;

    ;;; length of args - given args plus arg0
    listlength(list) fi_+ 1 -> argc;

    ;;; pointer to count
    consintvec(argc,1);

    ;;; args fixed vector - add a null
    cons_fixed(poparg0, applist(list,XTC_str), _NULL, argc fi_+ 1, vector_key)
enddefine;

protected
define vars XptDefaultSetup();
    lvars name, appcontext;
    unless  isXptDescriptor(XptDefaultAppContext)
        and isXptDescriptor(XptDefaultDisplay)
        and fast_XptDataType(XptDefaultAppContext) == XDT_APPCONTEXT
        and fast_XptDataType(XptDefaultDisplay) == XDT_DISPLAYPTR
    then
        sys_fname_nam(poparg0) -> name;
        fast_XtToolkitInitialize();
        fast_XptCreateApplicationContext(name) -> appcontext;

        ;;; SET FALLBACK RESOURCES IF APPROPRIATE
        if XptDefaultFallbackResources.islist then
            ;;; CHECK IT'S A LIST OF STRINGS
            applist(
                XptDefaultFallbackResources,
                procedure(string);
                    lvars string;
                    unless isstring(string) do;
                        mishap(
                            string, XptDefaultFallbackResources, 2,
                            'NON-STRING IN XptDefaultFallbackResources'
                        );
                    endunless;
                endprocedure
            );
            ;;; HAS TO BE FIXED UNTIL THE DISPLAY OPENED
            lvars fallback_string_list =
                cons_fixed(
                    #| applist(XptDefaultFallbackResources, copy_fixed), _NULL |#,
                    vector_key
                );
            fast_XtAppSetFallbackResources(appcontext, fallback_string_list);
        endif;

        fast_XtOpenDisplay(
            appcontext,             ;;; application just created
            isstring(XptDefaultDisplay) and XptDefaultDisplay,
                                    ;;; display selection from $DISPLAY etc.
            name,                   ;;; application name
            XT_POPLOG_CLASSNAME,    ;;; application class (from xpt_constants.ph)
            0,0,                    ;;; no startup options
            XptSetArgv(popunderx)   ;;; derive argv from popunderx
        ) -> XptDefaultDisplay;

        ;;; We do the assignment here in case -fast_XtOpenDisplay- mishaps
        appcontext -> XptDefaultAppContext;

        true -> XptAsyncAppContext(XptDefaultAppContext);
    endunless;
enddefine;

/* This doesn't do what we want so leave it out for now
define XptDefaultShutdown;
    lvars appcon = XptDefaultAppContext;
    if appcon and (Get_Descriptor(XDT_APPCONTEXT,appcon) ->> appcon) then
        fast_XtDestroyApplicationContext(appcon);
    endif;
    false ->> XptDefaultAppContext ->> XptDefaultDisplay;
enddefine;
*/

;;; set XptDefaultSetup as the (default) value of sysxsetup - called by
;;; syssetup (see poplog_main.p)
global vars procedure sysxsetup = procedure; XptDefaultSetup(); endprocedure;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 11 1992
        Removed declarations for things from src
--- Adrian Howard, Nov 11 1992
        Added XptDefaultFallbackResources
--- Adrian Howard, Jul 31 1992
        Fixed bug which caused multiple displays/appcontexts to be created when
        -XptDefaultDisplay- was called more than once
--- Adrian Howard, Apr 29 1992
      o -XptDefaultSetup- now runs if -XptDefaultDisplay- is not a display and
        -XptDefaultAppContext- is not an application context
      o If -XptDefaultDisplay- is a string before -XptDefaultSetup- is called
        it is used as the display name
--- Jonathan Meyer, Sep 12 1991
        sysxsetup now a procedure which calls XptDefaultSetup
--- Jonathan Meyer, Aug 19 1991
        XptDefaultSetup is now a protected vars procedure.
--- John Gibson, Mar 13 1991
        Made -XptDefaultSetup- set async on for -XptDefaultAppContext-.
--- Roger Evans, Feb  6 1991 commented out XptDefaultShutdown
--- Roger Evans, Feb  5 1991 exported XptSetArgv
--- Roger Evans, Feb  3 1991 added XptDefaultShutdown
--- Roger Evans, Oct 11 1990 Much revised
 */
