/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptWidgetOfObject.p
 > Purpose:         Returns the widget whose window is associated with the given gadget
 > Author:          Adrian Howard, Sep 23 1992
 > Documentation:   REF *XptWidgetOfObject
 > Related Files:   fast_XptWidgetOfObject.p
 */
compile_mode: pop11 +strict;

section;

define XptWidgetOfObject() with_nargs 1;
    fast_XptWidgetOfObject(XptCheckWidget());
enddefine;

endsection;
