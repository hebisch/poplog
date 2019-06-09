/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/fast_xt_trans.p
 > Purpose:         Fast routines to control mapping of user events to
 >                  widget behaviour
 > Author:          Adrian Howard, Jul 18 1990 (see revisions)
 > Documentation:   REF *XT_TRANS
 > Related Files:   C.x/x/pop/lib/xt_trans.p
 */
compile_mode:pop11 +strict;

section;

include xpt_generaltypes.ph;

;;; Load the external "raw" procedures
XptLoadProcedures fast_xt_trans
    lvars
        XtSetMultiClickTime
        XtGetMultiClickTime
        XtParseTranslationTable
        XtAugmentTranslations
        XtOverrideTranslations
        XtUninstallTranslations
        XtParseAcceleratorTable
        XtInstallAccelerators
        XtInstallAllAccelerators
;


;;; Read the multi-click time of a display - 18/07/90
;;; Input - <DidplayPtr>, Output - <INT>
define global fast_XtGetMultiClickTime() with_nargs 1;
    exacc (1):int raw_XtGetMultiClickTime();
enddefine;


;;; Remove existing translations from a widget - 18/07/90
;;; Input - <Widget>
define global fast_XtUninstallTranslations() with_nargs 1;
    exacc (1) raw_XtUninstallTranslations();
enddefine;


;;; Install one widgets accelerators on another - 18/07/90
;;; Input - <Widget> <Widget>
define global fast_XtInstallAccelerators() with_nargs 2;
    exacc (2) raw_XtInstallAccelerators();
enddefine;


;;; Install the accelerators of a widget, and all its descendants, on another
;;; widget - 18/07/90
;;; Input - <Widget> <Widget>
define global fast_XtInstallAllAccelerators() with_nargs 2;
    exacc (2) raw_XtInstallAllAccelerators();
enddefine;


;;; Set the multi-click time - 13/08/90
;;; Input - <DisplayPtr> <INT>
define global fast_XtSetMultiClickTime() with_nargs 2;
    exacc (2) raw_XtSetMultiClickTime();
enddefine;


;;; Parse an accelerator table - 14/08/90
;;; Input - <String>, Output - <XtAccelerators>
define global fast_XtParseAcceleratorTable() with_nargs 1;
    exacc (1):XptAccelerators raw_XtParseAcceleratorTable();
enddefine;


;;; Compile a translation table - 14/08/90
;;; Input - <STRING>, Output - <XtTranslations>
define global fast_XtParseTranslationTable with_nargs 1;
    exacc (1):XptTranslations raw_XtParseTranslationTable();
enddefine;


;;; Merge translations with a translation table - 14/08/90
;;; Input - <Widget> <XtTranslations>
define global fast_XtAugmentTranslations() with_nargs 2;
    exacc (2) raw_XtAugmentTranslations();
enddefine;


;;; Overwrite existing translations with new ones - 14/08/90
;;; Input - <Widget> <XtTranslations>
define global fast_XtOverrideTranslations() with_nargs 2;
    exacc (2) raw_XtOverrideTranslations();
enddefine;


;;; So uses works OK
constant fast_xt_trans= true;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  3 1991
        Moved typespec to xpt_generaltypes.ph
--- Adrian Howard, Jun 25 1991 : Removed reference to fast_xt_procs
--- Roger Evans, Nov 19 1990 added typespecs
--- Roger Evans, Oct 19 1990 changed to use XptLoadProcedures
--- Roger Evans, Oct 11 1990 changed to use exacc
--- Adrian Howard, Sep 13 1990 : Made procs global
--- Adrian Howard, Sep  7 1990 : Added var so uses works
--- Adrian Howard, Sep  6 1990 : Altered calls to XptCoerce/Import to make
        compatable with format of new external conversion/access procs
 */
