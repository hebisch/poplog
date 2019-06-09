/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_converter.p
 > Purpose:         Fast resource converter routines
 > Author:          Adrian Howard, Jul 18 1990 (see revisions)
 > Documentation:   REF *XT_CONVERTER
 > Related Files:   C.x/x/pop/lib/xt_converter.p
 */

compile_mode:pop11 +strict;

section;

include xpt_coretypes.ph;


define global XptTypeConverterWrapper(ptr, proc);
    lvars ptr, proc, stacked, result = false;
    l_typespec extdata {
        dpy     :XptDisplayPtr,
        args    :exptr,
        numargs :exptr.:XptCardinal,
        from    :exptr,
        to      :exptr,
        cdata   :XptPointer
    };

    #|
        exacc [fast] :extdata ptr.dpy;
        exacc [fast] :extdata ptr.args;
        exacc [fast] :extdata ptr.numargs;
        exacc [fast] :extdata ptr.from;
        exacc [fast] :extdata ptr.to;
        exacc [fast] :extdata ptr.cdata;
        XptCallbackHandler(proc,"type_converter");
    |# -> stacked;

    if stacked == 0 then
        warning(
            proc, 1,
            'TYPE CONVERTER DID NOT RETURN RESULT - FALSE BEING RETURNED'
        );
    else
         -> result;   ;;; TOP ITEM ON STACK IS THE RESULT
        if stacked > 1 do;
            warning(
                proc, stacked,
                'EXCESS RESULTS AFTER TYPE CONVERTER BEING IGNORED'
            );
        endif;
    endif;

    result -> exacc :XptLongBoolean ptr; ;;; STACK RESULT FOR INTRINSICS

enddefine;


define global XptDestructorWrapper(extdata, proc);
    lvars extdata proc stacked;
    l_typespec extdata {
        app     :XptAppContext,
        to      :exptr,
        cdata   :XptPointer,
        args    :exptr,
        numargs :exptr.:XptCardinal,
    };

    #|
        exacc [fast] :extdata extdata.app;
        exacc [fast] :extdata extdata.to;
        exacc [fast] :extdata extdata.cdata;
        exacc [fast] :extdata extdata.args;
        exacc [fast] :extdata extdata.numargs;
        XptCallbackHandler(proc,"destructor");
    |# -> stacked;

    unless stacked == 0 do;
        warning(
            proc, stacked+1,
            'ITEMS ON STACK AFTER DESTRUCTOR BEING IGNORED'
        );
    endunless;

enddefine;


define global XptConvertArgProcWrapper(ptr, proc);
    lvars ptr, proc, stacked;
    l_typespec extdata {
        object  :XptWidget,
        size    :exptr.:XptCardinal,
        value   :exptr
    };

    #|
        exacc [fast] :extdata ptr.object;
        exacc [fast] :extdata ptr.size;
        exacc [fast] :extdata ptr.value;
        XptCallbackHandler(proc, "convert_arg_proc");
    |# -> stacked;

    unless stacked == 0 do;
        warning(
            proc, stacked+1,
            'ITEMS ON STACK AFTER CONVERT ARG PROC BEING IGNORED'
        );
    endunless;
enddefine;


;;; Cache, indexing closures of -XptTypeConverterWrapper- and
;;; -XptDestructorWrapper- onto external procedures
lconstant callback_cache =
    newanyproperty( [], 50, false, false,
                    syshash, sys_=, "tmpval",
                    false, false
    );


;;; Instances of the closures used to index -callback_cache-
lconstant XptTypeConverterWrapperInstance =
    writeable XptTypeConverterWrapper(%false%);
lconstant XptDestructorWrapperInstance =
    writeable XptDestructorWrapper(%false%);
lconstant XptConvertArgProcWrapperInstance =
    writeable XptConvertArgProcWrapper(%false%);


lconstant do_cached_export=
    procedure(proc, hold, wrapper_instance) -> efc;
        lvars proc, hold, wrapper_instance, efc;
        lvars callback_wrapper;
        proc -> explode(wrapper_instance);
        unless (callback_cache(wrapper_instance) ->> efc) do;
            if XptIsValidCallback(proc) then
                copy(wrapper_instance) -> callback_wrapper;
                exfunc_export(callback_wrapper, XptCallbackFlags, hold)
                    ->> callback_cache(callback_wrapper) -> efc;
            else
                mishap(proc, 1, 'PROCEDURE, IDENT, OR WORD NEEDED');
            endif;
        endunless;
    endprocedure;
;;;
define XptExportTypeConverterCached() with_nargs 2;
    do_cached_export(XptTypeConverterWrapperInstance);
enddefine;
;;;
define XptExportDestructorCached() with_nargs 2;
    do_cached_export(XptDestructorWrapperInstance);
enddefine;
;;;
define XptExportConvertArgProcCached with_nargs 2;
    do_cached_export(XptConvertArgProcWrapperInstance);
enddefine;


lconstant do_export =
    procedure(proc, hold, wrapper); /* -> EFC */
        lvars proc, hold, wrapper;
        if XptIsValidCallback(proc) then
            exfunc_export(wrapper(%proc%), XptCallbackFlags,hold);
        else
            mishap(proc, 1, 'WORD, IDENT, OR PROCEDURE NEEDED');
        endif;
    endprocedure;
;;;
define global XptExportTypeConverter() with_nargs 2;
    do_export(XptTypeConverterWrapper);
enddefine;
;;;
define global XptExportDestructor() with_nargs 2;
    do_export(XptDestructorWrapper);
enddefine;
;;;
define global XptExportConvertArgProc() with_nargs 2;
    do_export(XptConvertArgProcWrapper);
enddefine;


;;; Load the external "raw" procedures
XptLoadProcedures 'xt_converter'
    lvars
        XtSetTypeConverter
        XtAppSetTypeConverter
        XtCallConverter
        XtAppReleaseCacheRefs
        XtCallbackReleaseCacheRef
        XtCallbackReleaseCacheRefList
        XtConvertAndStore
        XtDisplayStringConversionWarning
;


;;; Lookup & call a resource converter, copy resulting value and free cached
;;; resource when widget destroyed - 14/08/90
;;; Input - <Widget> <STRING> <XrmValuePtr> <STRING> <XrmValuePtr>
;;; Output - <BOOL>
define global fast_XtConvertAndStore(widget, source_type, source, target_type,
                              target);
    lvars widget, source_type, source, target_type, target;
    exacc (5) :XptBoolean raw_XtConvertAndStore(
                                    widget,
                                    -> XptCoerceTmpString(source_type),
                                    source,
                                    -> XptCoerceTmpString(target_type),
                                    target
                                );
enddefine;


;;; Register a global type converter - 15/08/90
;;; Input - <STRING> <STRING> <XtTypeConverter> <XtConvertArgList> <Cardinal>
;;;         <XtCacheType> <XtDestructor>
define global fast_XtSetTypeConverter(src, target, typeconverter,
        convertarglist, cardinal, cachetype, destructor);
    lvars src, target, typeconverter, convertarglist, cardinal, cachetype,
          destructor;
    exacc (7) raw_XtSetTypeConverter(
                            -> XptCoerceTmpString(src),
                            -> XptCoerceTmpString(target),
                            typeconverter,
                            if convertarglist then convertarglist else 0 endif,
                            cardinal,
                            cachetype,
                            if destructor then destructor else 0 endif
                          );
enddefine;


;;; Register a type converter in a single application context - 15/08/90
;;; Input - <AppContext> <STRING> <STRING> <XtConverter> <XtConvertArgList>
;;;         <Cardinal> <XtCacheType> <XtDestructor>
define global fast_XtAppSetTypeConverter(appcontext, src, target,
        typeconverter, convertarglist, cardinal, cachetype, destructor);
    lvars appcontext, src, target, typeconverter, convertarglist, cardinal,
          cachetype, destructor;
    exacc (8) raw_XtAppSetTypeConverter(
        appcontext,
        -> XptCoerceTmpString(src),
        -> XptCoerceTmpString(target),
        typeconverter,
        if convertarglist then convertarglist else 0 endif,
        cardinal,
        cachetype,
        if destructor then destructor else 0 endif
    );
enddefine;


;;; Decrement the reference count of resources - 15/08/90
;;; Input - <AppContext> <XtCacheRefList>
define global fast_XtAppReleaseCacheRefs() with_nargs 2;
    exacc (2) raw_XtAppReleaseCacheRefs();
enddefine;


;;; Make explicit resource conversions - 15/08/90
;;; Input - <DisplayPtr> <XtTypeConverter> <XrmValuePtr> <Cardinal>
;;; <XrmValuePtr> <XrmValuePtr> <XtCacheRefPtr>, Output - <BOOL>
define global fast_XtCallConverter(displayptr, typeconverter, args, num_args,
            src, target, cacherefptr);
    lvars displayptr, typeconverter, args, num_args, src, target, cacherefptr;
    exacc (7):XptBoolean raw_XtCallConverter(
        displayptr,
        typeconverter,
        if args then args else 0 endif,
        num_args,
        src, target,
        if cacherefptr then cacherefptr else 0 endif
    );
enddefine;


;;; Release a previously returned XtCacheRef value - 18/07/90
;;; Input - <Widget> <XptCacheRef> <XtPointer>
define global fast_XtCallbackReleaseCacheRef(object, client_data, call_data);
    lvars object, client_data, call_data;
    exacc (3) raw_XtCallbackReleaseCacheRef(
                                    object,
                                    client_data,
                                    0   ;;; Call data ignored
                                );
enddefine;


;;; Release a list of previously returned XtCacheRef values - 18/07/90
;;; Input - <Widget> <XptCacheRefList> <XtPointer>
define global fast_XtCallbackReleaseCacheRefList(object, client_data,
        call_data);
    lvars object, client_data, call_data;
    exacc (3) raw_XtCallbackReleaseCacheRefList(
                                    object,
                                    client_data,
                                    0   ;;; Call data ignored
                                );
enddefine;


;;; Convenience routine for resource converter warnings - 10/09/91
;;; Input - <DISPLAY> <STRING> <STRING>
define global fast_XtDisplayStringConversionWarning(display, s, d);
    lvars display, s, d;
    exacc (3) raw_XtDisplayStringConversionWarning(
        display,
        s -> XptCoerceTmpString(),
        d -> XptCoerceTmpString()
    );
enddefine;


;;; So uses works OK
constant fast_xt_converter = true;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1991
        includes xpt_coretypes.ph
--- Adrian Howard, Sep 23 1991
        o Added -XptConvertArgProcWrapper-, -XptExportTypeConverter(Cached)-,
          -XptExportDestructor(Cached)-, -XptExportConvertArgProc(Cached)-,
          -fast_XtDisplayStringConversionWarning-
        o -fast_XtCallbackReleaseCacheRef(List) now accept
          -XptCacheRef(List)- structures rather than -XtPointers-.
        o Removed -XptCoerceTypeConverter- and -XptCoerceDestructor-
        o Loads of fixes on the argument types to various procs
        o Altered wrappers so they catch the user stacking too many/few
          items during external callback
        o -XptTypeConverterWrapper- now coerces boolean result for the
          intrinsics
--- Roger Evans, Jul  2 1991 added callback handlers
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Nov 19 1990 added temporary coercion procedures
--- Roger Evans, Oct 19 1990 changed to use XptloadProcedures
--- Roger Evans, Oct 11 1990 changed to use exacc
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  6 1990 : Altered calls to XptCoerce/Import to make
        compatable with format of new external conversion/access procs
--- Adrian Howard, Sep  5 1990 : Seperated out references to CachRefPtr
        structures in references to CacheRefPtr's and CacheRefList's
 */
