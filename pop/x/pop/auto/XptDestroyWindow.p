/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptDestroyWindow.p
 > Purpose:         Destroy parent of widget, see -XptNewWindow-
 > Author:          Adrian Howard, Feb 17 1992 (see revisions)
 > Documentation:   REF *XptDestroyWindow, *XptNewWindow
 > Related Files:   C.x/x/pop/auto/XptNewWindow.p
 */

compile_mode: pop11 +strict;
section;

uses fast_xt_widget;

define global XptDestroyWindow();
    fast_XtDestroyWidget(XptShellOfObject());
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Feb 17 1992 : Uses -fast_*- procedure
 */
