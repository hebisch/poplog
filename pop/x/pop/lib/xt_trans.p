/* --- Copyright University of Sussex 1990. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xt_trans.p
 > Purpose:         Checking routines that control mapping of user events to
 >                  widget behaviour
 > Author:          Adrian Howard, Jul 18 1990 (see revisions)
 > Documentation:   REF *XT_TRANS
 > Related Files:   C.x/x/pop/lib/xpt_typecheck.p,
 >                  C.x/x/pop/lib/fast_xt_trans.p
 */

compile_mode:pop11 +strict;

section;

uses xpt_typecheck.p;        ;;; Get the type checking routines
uses fast_xt_trans.p;       ;;; Get the fast versions of the procedures


;;; Read the multi-click time of a display - 18/07/90
;;; Input - <DidplayPtr>, Output - <INT>
define global XtGetMultiClickTime() with_nargs 1;
    fast_XtGetMultiClickTime(XptCheckDisplayPtr());
enddefine;


;;; Remove existing translations from a widget - 18/07/90
;;; Input - <Widget>
define global XtUninstallTranslations() with_nargs 1;
    fast_XtUninstallTranslations(XptCheckWidget());
enddefine;


;;; Install one widgets accelerators on another - 18/07/90
;;; Input - <Widget> <Widget>
define global XtInstallAccelerators(destination, source);
    lvars destination, source;
    fast_XtInstallAccelerators( XptCheckWidget(destination),
                                XptCheckWidget(source)
                              );
enddefine;


;;; Install the accelerators of a widget, and all its descendants, on another
;;; widget - 18/07/90
;;; Input - <Widget> <Widget>
define global XtInstallAllAccelerators(destination, source);
    lvars destination, source;
    fast_XtInstallAllAccelerators( XptCheckWidget(destination),
                                   XptCheckWidget(source)
                                 );
enddefine;


;;; Set the multi-click time - 13/08/90
;;; Input - <DisplayPtr> <INT>
define global XtSetMultiClickTime(displayptr, int);
    lvars displayptr, int;
    fast_XtSetMultiClickTime(   XptCheckDisplayPtr(displayptr),
                                XptCheckInt(int)
                            );
enddefine;


;;; Parse an accelerator table - 14/08/90
;;; Input - <String>, Output - <XtAccelerators>
define global XtParseAcceleratorTable() with_nargs 1;
    fast_XtParseAcceleratorTable(XptCheckString());
enddefine;


;;; Compile a translation table - 14/08/90
;;; Input - <STRING>, Output - <XtTranslations>
define global XtParseTranslationTable() with_nargs 1;
    fast_XtParseTranslationTable(XptCheckString());
enddefine;


;;; Merge translations with a translation table - 14/08/90
;;; Input - <Widget> <XtTranslations>
define global XtAugmentTranslations(widget, translations);
    lvars widget, translations;
    fast_XtAugmentTranslations( XptCheckWidget(widget),
                                translations,
                              );
enddefine;


;;; Overwrite existing translations with new ones - 14/08/90
;;; Input - <Widget> <XtTranslations>
define global XtOverrideTranslations(widget, translations);
    lvars widget, translations;
    fast_XtOverrideTranslations(    XptCheckWidget(widget),
                                    translations,
                               );
enddefine;


;;; So uses works OK
constant xt_trans= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- Roger Evans, Nov 19 1990 tidied up - removed XptCheckTranslations
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  5 1990 : xt_typecheck --> xpt_typecheck
 */
