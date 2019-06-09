/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_util.p
 > Purpose:         Checking general Xt utilities
 > Author:          Adrian Howard, Jul 18 1990 (see revisions)
 > Documentation:   REF *XT_UTIL
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p,
 >                  C.x/x/pop/lib/fast_xt_util.p
 */


compile_mode:pop11 +strict;


section;

include xpt_constants.ph;
include xpt_generaltypes.ph;

uses xpt_typecheck.p;        ;;; Get the type checking routines
uses fast_xt_util.p;         ;;; Get the fast versions of the procedures
uses XptGCValuesPtr

shadowclass XptPositionPtr  #_< [props ^XDT_POSITIONPTR] >_# {
    XptPPValue  :uint,
};

shadowclass XptSubstitutionPtr {:XptSubstitutionRec};
shadowclass XptSubstitution #_< [props ^XDT_SUBSTITUTION] >_#
    :XptSubstitutionRec[];

;;; Translate a widget name to a widget instance - 18/07/90
;;; Input - <Widget> <String>, Output - <Widget>
define global XtNameToWidget(reference, names);
    lvars reference, names;
    fast_XtNameToWidget(XptCheckWidget(reference), XptCheckString(names));
enddefine;


;;; Translate window and display pointer to a widget instance - 13/08/90
;;; Input - <DisplayPtr> <Window>, Output - <Widget>
define global XtWindowToWidget(displayptr, window);
    lvars displayptr, window;
    fast_XtWindowToWidget(  XptCheckDisplayPtr(displayptr),
                            XptCheckWindow(window)
                         );
enddefine;


;;; Set value of widgets WM_COLORMAP_WINDOWS property - 13/08/90
;;; Input - <WIDGET> <WidgetList> <Cardinal>
define global XtSetWMColormapWindows(widget, widgetlist, cardinal);
    lvars widget, widgetlist, cardinal;
    fast_XtSetWMColormapWindows(    XptCheckWidget(widget),
                                    XptCheckWidgetListAndLength(widgetlist, cardinal)
                               );
enddefine;


;;; Translate widget coordinates to root coordinates - 13/08/90
;;; Input - <Widget> <Position> <Position> <PositionPtr> <PositionPtr>
define global XtTranslateCoords(widget, x_position, y_position,
                         rootx_positionptr_return, rooty_positionptr_return);
    lvars widget, x_position, y_position, rootx_positionptr_return,
          rooty_positionptr_return;
    fast_XtTranslateCoords( XptCheckWidget(widget),
                            XptCheckPosition(x_position),
                            XptCheckPosition(y_position),
                            XptCheckPositionPtr(rootx_positionptr_return),
                            XptCheckPositionPtr(rooty_positionptr_return)
                          );
enddefine;


;;; Search for a file in a path list - 14/08/90
;;; Input - <STRING> <Substitution> <Cardinal> <XtFilePredicate>
;;; Output - <STRING>
define global XtFindFile(string, substitution, cardinal, filepredicate);
    lvars string, substitution, cardinal, filepredicate;
    fast_XtFindFile(    XptCheckString(string),
                        XptCheckSubstitution(substitution),
                        XptCheckCardinal(cardinal),
                        if XptIsValidCallback(filepredicate) then
                            XptCoerceFilePredicate(filepredicate);
                        else
                            XptCheckFilePredicate(filepredicate)
                        endif
                   );
enddefine;


;;; Search for a file using standard substitutions in a path list - 14/08/90
;;; Input - <DisplayPtr> <STRING> <STRING> <STRING> <STRING> <Substitution>
;;; <Cardinal> <XtFilePredicate>, Output - <String>
define global XtResolvePathname(displayptr, type, filename, suffix, path,
                         substitution, cardinal, filepredicate);
    lvars displayptr, type, filename, suffix, path, substitution, cardinal,
          filepredicate;
    fast_XtResolvePathname( XptCheckDisplayPtr(displayptr),
                            XptCheckString(type),
                            XptCheckString(filename),
                            XptCheckString(suffix),
                            XptCheckString(path),
                            XptCheckSubstitution(substitution),
                            XptCheckCardinal(cardinal),
                            if XptIsValidCallback(filepredicate) then
                                XptCoerceFilePredicate(filepredicate);
                            else
                                XptCheckFilePredicate(filepredicate)
                            endif
                          );
enddefine;


;;; Obtain a read-only, sharable GC - 14/08/90
;;; Input - <Widget> <XtGCMask> <XGCValuesPtr>, Output - <GC>
define global XtGetGC(widget, gcmask, gcvaluesptr);
    lvars widget, gcmask, gcvaluesptr;
    fast_XtGetGC(   XptCheckWidget(widget),
                    XptCheckGCMask(gcmask),
                    XptCheckGCValuesPtr(gcvaluesptr)
                );
enddefine;


;;; Release a shared GC - 14/08/90
;;; Input - <Widget> <GC>
define global XtReleaseGC(widget, gc);
    lvars widget, gc;
    fast_XtReleaseGC(XptCheckWidget(widget), XptCheckGC(gc));
enddefine;


;;; So uses works OK
global vars xt_util= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 16 1993
        Made XtFree autoloadable
--- John Gibson, Nov  3 1991
        typespecs moved to xpt_generaltypes.ph
--- Adrian Howard, Oct 31 1991 :
    o -XptSubstitutionRec- now a typespec.
    o -XptSubstitutionRec- renamed -XptSubstitutionPtr-.
--- Adrian Howard, Sep 11 1991 : Added checks for list length
--- Adrian Howard, Sep 10 1991 : Fixed typo's that stopped library compiling
--- Roger Evans, Jul  2 1991 added coercion code for filepredicates
--- Roger Evans, Feb  7 1991 changed XptPopString to XptString
--- Roger Evans, Jan 26 1991 changed to new shadowclass props format
--- Roger Evans, Nov 19 1990 added shadowclass defs etc.
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
