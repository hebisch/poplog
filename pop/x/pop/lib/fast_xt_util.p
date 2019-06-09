/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_util.p
 > Purpose:         Fast general Xt utilities
 > Author:          Adrian Howard, Jul 18 1990 (see revisions)
 > Documentation:   REF *XT_UTIL
 > Related Files:   C.x/x/pop/lib/xt_util.p
 >                  C.x/x/pop/lib/xpt_external.p
 >                  C.x/x/pop/lib/xpt_coerce.p
 */


compile_mode:pop11 +strict;


section;
exload_batch;

include xpt_coretypes.ph;

define XptFilePredicateWrapper(extdata,proc);
    lvars extdata proc;
    exacc [fast] :exptr.:XptString extdata;

    XptCallbackHandler(proc,"file_predicate");
enddefine;

define global XptCoerceFilePredicate(proc);
    lvars proc;

    if XptIsValidCallback(proc) then
        exfunc_export(XptFilePredicateWrapper(%proc%),
                      XptCallbackFlags,false);
    else
        proc;
    endif;
enddefine;


;;; Load the external "raw" procedures
XptLoadProcedures xt_util
    lvars
        XtNameToWidget
        XtGetGC
        XtReleaseGC
        XtTranslateCoords
        XtWindowToWidget
        XtSetWMColormapWindows
        XtFindFile
        XtResolvePathname
        XtAppWarningMsg
;


;;; Translate a widget name to a widget instance - 18/07/90
;;; Input - <Widget> <String>, Output - <Widget|FALSE>
define global fast_XtNameToWidget() with_nargs 2;
    exacc (2):XptWidget raw_XtNameToWidget(
                            -> XptCoerceTmpString()
                        );
enddefine;


;;; Translate window and display pointer to a widget instance - 13/08/90
;;; Input - <DisplayPtr> <Window>, Output - <Widget>
define global fast_XtWindowToWidget() with_nargs 2;
    exacc (2):XptWidget raw_XtWindowToWidget();
enddefine;


;;; Set value of widgets WM_COLORMAP_WINDOWS property - 13/08/90
;;; Input - <WIDGET> <WidgetList> <Cardinal>
define global fast_XtSetWMColormapWindows() with_nargs 3;
    exacc (3) raw_XtSetWMColormapWindows();
enddefine;


;;; Translate widget coordinates to root coordinates - 13/08/90
;;; Input - <Widget> <Position> <Position> <PositionPtr> <PositionPtr>
define global fast_XtTranslateCoords(widget, x_position, y_position,
                          rootx_positionptr_return, rooty_positionptr_return);
    lvars widget, x_position, y_position, rootx_positionptr_return,
          rooty_positionptr_return;
    exacc (5) raw_XtTranslateCoords(
                            widget,
                            x_position,
                            y_position,
                            rootx_positionptr_return,
                            rooty_positionptr_return
                         );
enddefine;


;;; Search for a file in a path list - 14/08/90
;;; Input - <STRING> <Substitution> <Cardinal> <XtFilePredicate>
;;; Output - <STRING>
define global fast_XtFindFile(string, substitution, cardinal, filepredicate);
    lvars string, substitution, cardinal, filepredicate eptr;
    exacc (4):exptr raw_XtFindFile(
                    -> XptCoerceTmpString(string),
                    substitution,
                    cardinal,
                    filepredicate
                  ) -> eptr;
    exacc_ntstring(eptr);
    fast_XtFree(eptr);
enddefine;


;;; Search for a file using standard substitutions in a path list - 14/08/90
;;; Input - <DisplayPtr> <STRING> <STRING> <STRING> <STRING> <Substitution>
;;; <Cardinal> <XtFilePredicate>, Output - <String>
define global fast_XtResolvePathname(displayptr, type, filename, suffix, path,
                              substitution, cardinal, filepredicate);
    lvars displayptr, type, filename, suffix, path, substitution, cardinal,
          filepredicate eptr;
    exacc (8):exptr raw_XtResolvePathname(
                            displayptr,
                            -> XptCoerceTmpString(type),
                            -> XptCoerceTmpString(filename),
                            -> XptCoerceTmpString(suffix),
                            -> XptCoerceTmpString(path),
                            substitution,
                            cardinal,
                            filepredicate
                         ) -> eptr;
    exacc_ntstring(eptr);
    fast_XtFree(eptr);
enddefine;


;;; Obtain a read-only, sharable GC - 14/08/90
;;; Input - <Widget> <XtGCMask> <XGCValuesPtr>, Output - <GC>
define global fast_XtGetGC() with_nargs 3;
    exacc (3):int raw_XtGetGC();
enddefine;


;;; Release a shared GC - 14/08/90
;;; Input - <Widget> <GC>
define global fast_XtReleaseGC() with_nargs 2;
    exacc (2) raw_XtReleaseGC();
enddefine;


;;; So uses works OK
constant fast_xt_util= true;

endexload_batch;
endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        Made fast_XtFree autoloadable
--- John Gibson, Nov  3 1991
        includes xpt_coretype.ph
--- Roger Evans, Jul  2 1991 added callback code
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Nov 19 1990 added temporary fielpred coercion routine
--- Roger Evans, Nov 19 1990 tidied up
--- Roger Evans, Oct 19 1990 changed to use XptLoadProcedures
--- Roger Evans, Oct 11 1990 changed to use exacc
--- Adrian Howard, Sep 13 1990 : The Sep.4th bug-fix on -fast_XtNameToWidget-
        removed since alterations to -XptImportWidget- remove the problem it
        fixed.
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  6 1990 : Altered calls to XptCoerce/Import to make
        compatable with format of new external conversion/access procs
--- Adrian Howard, Sep  4 1990 : Bug fix on -fast_XtNameToWidget-
 */
