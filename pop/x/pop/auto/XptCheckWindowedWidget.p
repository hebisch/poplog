/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptCheckWindowedWidget.p
 > Purpose:         Check for Subclass of Core
 > Author:          John Gibson, Apr 13 1993
 */
compile_mode :pop11 +strict;

section;

;;; Check widget is a subclass of Core - 28/05/92
define XptCheckWindowedWidget(item);
    lvars item;
    if fast_XtIsWidget(XptCheckWidget(item)) then
        return(item);
    else
        mishap(item, 1, 'SUBCLASS OF Core NEEDED');
    endif;
enddefine;

endsection;
