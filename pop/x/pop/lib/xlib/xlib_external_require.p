/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib/xlib_external_require.p
 > Purpose:         external require utility for Xlib libraries
 > Author:          Jonathan Meyer, Jan 25 1991 (see revisions)
 > Documentation:
 > Related Files:
 */


section;

;;; this saves us loading xdefs.ph for each library
include xdefs.ph;

define constant macro xlib_external_require;
    lvars tag = readitem();
    lconstant expr =
        [external require tag ; % explode(XLINK_EXLIBS) % endexternal];
    dl(tag -> expr(3), expr);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 10 1993
        Replaced XTB*ASELIBS with XLINK_EXLIBS
--- Ian Rogers, Feb  6 1991
    -tag- is now explicitly read using -readitem- as oppposed to letting the
    macro procedure read it as an argument (stopped * XLIB files from loading)
 */
