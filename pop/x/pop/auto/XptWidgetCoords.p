/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptWidgetCoords.p
 > Purpose:         Returns/updates a widgets X, Y, WIDTH and HEIGHT
 > Author:          Jonathan Meyer, Aug 17 1991 (see revisions)
 > Documentation:   REF *XT_LIBS/XptWidgetCoords
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

include xpt_coretypes.ph;

lconstant macro VALS = [
    XptVal (/*widget*/)(XtN x:XptPosition <OPT>,
                        XtN y:XptPosition <OPT>,
                        XtN width:XptDimension <OPT>,
                        XtN height:XptDimension <OPT>)
    ];

define global XptWidgetCoords(/*w*/) /* -> (x, y, width, height) */ with_nargs 1;
    VALS
enddefine;
;;;
define updaterof XptWidgetCoords(/*x, y, width, height, w*/) with_nargs 5;
    () -> VALS
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep  6 1992
        Now uses XptVal
--- Adrian Howard, Nov  4 1991 : xt_general --> xpt_general
--- John Gibson, Nov  3 1991
        Uses xt_general and include xpt_coretypes
--- John Gibson, Sep 26 1991
        Corrected to use XptPosition (signed short) for x, y
--- John Gibson, Sep 21 1991
        Changed to use EXPTRINITSTR
 */
