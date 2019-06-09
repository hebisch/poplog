/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_converter.p
 > Purpose:         Checking resource converter routines
 > Author:          Adrian Howard, Jul 18 1990 (see revisions)
 > Documentation:   REF *XT_CONVERTER
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p
 >                  C.x/x/pop/lib/fast_xt_converter.p
 */


compile_mode:pop11 +strict;
section;
exload_batch;


include xpt_constants;
include xpt_generaltypes;

uses xpt_typecheck;
uses fast_xt_converter;
uses xpt_general;


/*- DATA STRUCTURE SUPPORT ----------------------------------------------*/


shadowclass XptConvertArgPtr {:XptConvertArgRec};
shadowclass XptConvertArgList #_< [props ^XDT_CONVERTARGLIST] >_# {
    :XptConvertArgRec[]
};

shadowclass XptCacheRefPtr #_< [props ^XDT_CACHEREFPTR] >_# {:XptCacheRef};
shadowclass XptCacheRefList #_< [props ^XDT_CACHEREFLIST] >_#
    :XptCacheRef[];



/*- TOOLKIT PROCEDURES --------------------------------------------------*/

;;; Convenience routine for resource converter warnings - 10/09/91
;;; Input - <DISPLAY> <STRING> <STRING>
define XtDisplayStringConversionWarning(display, f, t);
    lvars display, f, t;
    fast_XtDisplayStringConversionWarning(
        XptCheckDisplayPtr(display),
        XptCheckString(f),
        XptCheckString(t)
    );
enddefine;


;;; Register a type converter - 15/08/90
;;; Input - <STRING> <STRING> <XtTypeConverter> <XtConvertArgList> <Cardinal>
;;;         <XtCacheType> <XtDestructor>
define XtSetTypeConverter(src, target, typeconverter, convertarglist,
                               cardinal, cachetype, destructor);
    lvars src, target, typeconverter, convertarglist, cardinal, cachetype,
          destructor;
    fast_XtSetTypeConverter(XptCheckString(src),
                            XptCheckString(target),
                            if XptIsValidCallback(typeconverter) then
                                XptExportTypeConverterCached(
                                    typeconverter, true
                                )
                            else
                                XptCheckProcedure(typeconverter),
                            endif,
                            if convertarglist then
                                XptCheckConvertArgListAndLength(
                                    convertarglist, cardinal
                                )
                            else
                                false; fi_check(cardinal, 0, 0);
                            endif,
                            XptCheckCacheType(cachetype),
                            if XptIsValidCallback(destructor) then
                                XptExportDestructorCached(destructor, true)
                            elseif destructor then
                                XptCheckProcedure(destructor)
                            else
                                false,
                            endif
                           );
enddefine;


;;; Register a type converter in a single application context - 15/08/90
;;; Input - <AppContext> <STRING> <STRING> <XtConverter> <XtConvertArgList>
;;;         <Cardinal> <XtCacheType> <XtDestructor>
define XtAppSetTypeConverter(appcontext, src, target, typeconverter,
                             convertarglist, cardinal, cachetype, destructor);
    lvars appcontext, src, target, typeconverter, convertarglist, cardinal,
          cachetype, destructor;
    fast_XtAppSetTypeConverter(
        XptCheckAppContext(appcontext),
        XptCheckString(src),
        XptCheckString(target),
        if XptIsValidCallback(typeconverter) then
            XptExportTypeConverterCached(typeconverter, true)
        else
            XptCheckProcedure(typeconverter),
        endif,
        if convertarglist then
            XptCheckConvertArgListAndLength(convertarglist, cardinal);
        else
            false; fi_check(cardinal, 0, 0);
        endif,
        XptCheckCacheType(cachetype),
        if XptIsValidCallback(destructor) then
            XptExportDestructorCached(destructor, true)
        elseif destructor then
            XptCheckProcedure(destructor)
        else
            false,
        endif
    );
enddefine;


;;; Make explicit resource conversions - 15/08/90
;;; Input - <DisplayPtr> <XtTypeConverter> <XrmValuePtr> <Cardinal>
;;; <XrmValuePtr> <XrmValuePtr> <XtCacheRefPtr>, Output - <BOOL>
define XtCallConverter(displayptr, typeconverter, args, num_args, src,
                       target, cacherefptr);
    lvars displayptr, typeconverter, args, num_args, src, target, cacherefptr;
    fast_XtCallConverter(   XptCheckDisplayPtr(displayptr),
                            if XptIsValidCallback(typeconverter) then
                                XptExportTypeConverterCached(typeconverter, true)
                            else
                                XptCheckProcedure(typeconverter),
                            endif,
                            if args then
                                XptCheckValueListAndLength(args, num_args)
                            else
                                false;
                                fi_check(num_args, 0, 0);
                            endif,
                            XptCheckValidValuePtr(src),
                            XptCheckValuePtr(target),
                            if cacherefptr then
                                XptCheckCacheRefPtr(cacherefptr)
                            else
                                false
                            endif
                        );
enddefine;


;;; Lookup & call a resource converter, copy resulting value and free cached
;;; resource when widget destroyed - 14/08/90
;;; Input - <Widget> <STRING> <XrmValuePtr> <STRING> <XrmValuePtr>
;;; Output - <BOOL>
define XtConvertAndStore(widget, source_type, source, target_type,
            target);
    lvars widget, source_type, source, target_type, target;
    fast_XtConvertAndStore( XptCheckWidget(widget),
                            XptCheckString(source_type),
                            XptCheckValidValuePtr(source),
                            XptCheckString(target_type),
                            XptCheckValuePtr(target)
                          );
enddefine;


;;; Decrement the reference count of resources - 15/08/90
;;; Input - <AppContext> <XtCacheRefList>
define XtAppReleaseCacheRefs(appcontext, cachereflist);
    lvars appcontext, cachereflist;
    fast_XtAppReleaseCacheRefs( XptCheckAppContext(appcontext),
                                XptCheckNTCacheRefList(cachereflist)
                              );
enddefine;


;;; Release a previously returned XtCacheRef value - 18/07/90
;;; Input - <Widget> <XtCacheRef> <XtPointer>
define XtCallbackReleaseCacheRef(object, client_data, call_data);
    lvars object, client_data, call_data;
    fast_XtCallbackReleaseCacheRef(
        XptCheckWidget(object),
        XptTypeCheck(client_data, XDT_CACHEREF),
        0   ;;; Call data ignored
    );
enddefine;


;;; Release a list of previously returned XtCacheRef values - 18/07/90
;;; Input - <Widget> <XptCacheRefList> <XtPointer>
define XtCallbackReleaseCacheRefList(object, client_data, call_data);
    lvars object, client_data, call_data;
    fast_XtCallbackReleaseCacheRefList( XptCheckWidget(object),
                                        XptCheckNTCacheRefList(client_data),
                                        0   ;;; Call data ignored
                                      );
enddefine;


constant xt_converter= true;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  8 1993
     #  XptXrmValuePtr|List shadowclasses moved to LIB * XPT_GENERAL
     #  check_valid_valueptr renamed XptCheckValidValuePtr, exported, and moved
        into LIB * XPT_TYPECHECK
--- John Gibson, Nov  3 1991
        Moved typespecs to xpt_generaltypes.ph.
        Conversion procedure XptImportCacheRef made autoloadable
--- Adrian Howard, Oct 31 1991 :
        o -XptCacheRef- changed to be an XptDescriptor.
        o Removed -XptExaccCacheRef-, -isXptCacheRef-, & -XptCacheRef_key-
        o Added -XptImportCacheRef-
        o -XptConvertArgRec- & -XptXrmValue- now typespecs
        o -XptConvertArgRec- & -XptXrmValue- shadowclasses now named
          -XptConvertArgPtr- & -XptXrmValuePtr-
--- Adrian Howard, Sep 23 1991
        o Added test for null-terminated/sized list structures
        o Added -check_valid_valueptr- & -XtDisplayStringConversionWarning-
        o Removed -XptCoerceTypeConverter- and -XptCoerceDestructor-
        o Changed to use -XptExportTypeConverterCached- and
          -XptExportDestructorCached-
        o Loads of fixes on the argument types to various procs
        o -XptCacheRef-, -XptXrmValueList- & -XptCacheRefPtr- support added
--- Roger Evans, Jul  2 1991 added explicit callback coercion
--- Roger Evans, Feb 10 1991 changed XptPopObj to XptPointer
--- Roger Evans, Jan 26 1991 changed to new shadowclass props format
--- Roger Evans, Nov 19 1990 added shadowclass defs
--- Roger Evans, Nov 11 1990 XptCheckProc -> XptCheckProcedure
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : Seperated occurances of the CacheRefPtr
        structure into CacheRefPtr's and CacheRefList's
--- Adrian Howard, Sep  5 1990 : Added reference to REF *XT_CONVERTER
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
