/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptDoesBackingStore.p
 > Purpose:         User friendly version of XDoesBackingStore
 > Author:          Adrian Howard, Jun 30 1993
 > Documentation:   * XptDoesBackingStore
 > Related Files:   LIB * FAST_XPT_SCREENINFO
 */

compile_mode: pop11 +strict;
section;

uses fast_xpt_screeninfo;
uses xpt_typecheck;
include xpt_xwindow;

define XptDoesBackingStore() with_nargs 1;
    lvars type = fast_XDoesBackingStore(XptCheckScreenPtr());
    type == NotUseful and "notuseful" or type == WhenMapped and "whenmapped"
        or "always";
enddefine;

endsection;
