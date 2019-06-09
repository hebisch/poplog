/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/xtCoreWidget.p
 > Purpose:         Define Toolkit widgetclass
 > Author:          John Gibson, Mar 31 1993
 > Documentation:   REF * XT_WIDGETCLASS
 > Related Files:   x/pop/auto/xt*Widget.p
 */
compile_mode :pop11 +strict;

section;

XptLoadWidgetClass xtCoreWidget
    xtCoreWidget <- coreWidgetClass
;

constant xtWidget = xtCoreWidget;

endsection;
