/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/fast_XtDisplayOfObject.p
 > Purpose:         Returns the display of an Xt object
 > Author:          Adrian Howard, Aug 25 1992
 > Documentation:   REF *XT_DISPLAY
 > Related Files:   LIB *FAST_XT_DISPLAY
 */

compile_mode: pop11 +strict;
section;

include xpt_coretypes.ph;

XptLoadProcedures 'fast_XtDisplayOfObject'
    lvars XtDisplayOfObject;

;;; Returns the display pointer for the object if it's a widget, otherwise the
;;; display pointer of its nearest widget ancestor - 17/07/90
;;; Input - <Widget>, Output - <DisplayPtr>
define global fast_XtDisplayOfObject() with_nargs 1;
    exacc (1):XptDisplayPtr raw_XtDisplayOfObject();
enddefine;

endsection;
