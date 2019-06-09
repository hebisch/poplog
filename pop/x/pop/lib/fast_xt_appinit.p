/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_appinit.p
 > Purpose:         Fast convenience procedures to initialise an application
 > Author:          Adrian Howard, Jul 17 1990 (see revisions)
 > Documentation:   REF *XT_APPINIT
 > Related Files:   C.x/x/pop/lib/xt_appinit.p
 */

compile_mode:pop11 +strict;

section;

include xpt_coretypes.ph;

;;; Load the external "raw" procedures
XptLoadProcedures fast_xt_appinit
    lvars
        XtAppInitialize
        XtVaAppInitialize
;

;;; Load _pop_sigmask for -fast_Xt(Va)AppInitialize-
exload 'xt_appinit.pop_sigmask'
    (language C)
    lvars _pop_sigmask(1) :uint
endexload;

;;; Initialize the X Toolkit Internals, create an application context,
;;; initialize a display and create initial application shell - 27/07/90
;;; Input - <AppContextPtr> <String> <XrmOptionDescList> <Cardinal> <CardinalPtr>
;;;         <StringList> <StringList> <ArgList> <Cardinal>,
;;; Output - <Widget>
define global fast_XtAppInitialize(app_context_ptr, app_class, options,
        num_options, argc_io, argv_io, fallback_resources, args, num_args);
    lvars app_context_ptr, app_class, options, num_options, argc_io, argv_io,
          fallback_resources, args, num_args;

    ;;; Block signals while we're doing this -- necessary (at least) in Unix
    ;;; to stop async appcontext timers interrupting the opening of the
    ;;; display connection (XOpenDisplay is not checking for EINTR and
    ;;; retrying, so it returns an error).
    dlocal 0 %exacc _pop_sigmask(1)->, exacc _pop_sigmask(0)-> %;

    exacc (9):XptWidget raw_XtAppInitialize(
                            if app_context_ptr then
                                app_context_ptr
                            else
                                null_external_ptr   ;;; -false- to null
                            endif,
                            -> XptCoerceTmpString(app_class),
                            options, num_options,
                            argc_io, argv_io,
                            if fallback_resources then
                                fallback_resources
                            else
                                null_external_ptr   ;;; -false- to null
                            endif,
                            args, num_args);
    fast_XptSetUnprocessedArgs(argc_io, argv_io);
    ;;; WE DO THIS TO MAKE SURE THE DISPLAY IS IMPORTED PROPERLY INTO POPLOG
    ;;; CAUSING IT TO BE CLOSED PROPERLY ON A -SYSFORK-
    fast_XtDisplayOfObject(dup()) ->;
enddefine;


;;; Initialize the X Toolkit Internals, create an application context,
;;; initialize a display and create initial application shell - 27/07/90
;;; Input - <AppContextPtr> <String> <XrmOptionDescList> <Cardinal> <CardinalPtr>
;;;         <StringList> <StringList> <ArgVarargs>, Output - <Widget>
define global fast_XtVaAppInitialize(count);
    lvars count, argc, argv;

    ;;; Block signals while we're doing this -- necessary (at least) in Unix
    ;;; to stop async appcontext timers interrupting the opening of the
    ;;; display connection (XOpenDisplay is not checking for EINTR and
    ;;; retrying, so it returns an error).
    dlocal 0 %exacc _pop_sigmask(1)->, exacc _pop_sigmask(0)-> %;

    -> XptCoerceString(subscr_stack(count fi_+ 6)) -> subscr_stack(count fi_+ 6);

    ;;; -false- AppContextPtr goes to null
    unless subscr_stack(count fi_+ 7) do;
        null_external_ptr -> subscr_stack(count fi_+ 7);
    endunless;

    ;;; -false- fallback resources StringList goes to null
    unless subscr_stack(count fi_+ 1) do;
        null_external_ptr -> subscr_stack(count fi_+ 1);
    endunless;

    subscr_stack(count fi_+2) -> argv;
    subscr_stack(count fi_+3) -> argc;

    -> XptCoerceVarargs(count) -> count;

    exacc (N):XptWidget raw_XtVaAppInitialize(count fi_+ 7);
    fast_XptSetUnprocessedArgs(argc, argv);

    ;;; WE DO THIS TO MAKE SURE THE DISPLAY IS IMPORTED PROPERLY INTO POPLOG
    ;;; CAUSING IT TO BE CLOSED PROPERLY ON A -SYSFORK-
    fast_XtDisplayOfObject(dup()) ->;

enddefine;


;;; So uses works OK
constant fast_xt_appinit= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Aug 25 1992
        Made sure displays were imported into POPLOG
--- John Gibson, Nov  3 1991
        includes xpt_coretypes.ph;
--- Adrian Howard, Oct  1 1991 : blocked signals during display creation
--- Adrian Howard, Sep 30 1991 :
        o Fixed -fast_XtVaAppInitialize-
        o Added calls to -fast_XptSetUnprocessedArgs-
--- Adrian Howard, Aug 21 1991 : Bug fixes on -Xt(Va)AppInitialize-
        o coerces -false- AppContextPtr to null.
        o coerces -false- fallback resources to null.
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Oct 19 1990 now uses XptLoadProcedures
--- Roger Evans, Oct 16 1990 removed XptExternalPtr and fixed typos
--- Roger Evans, Oct 11 1990 changed to use exacc
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  6 1990 : Altered calls to XptCoerce/Import to make
        compatable with format of new external conversion/access procs
--- Adrian Howard, Sep  4 1990 : StringNullVec --> StringList
 */
