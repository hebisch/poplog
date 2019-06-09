/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xpw.p
 > Purpose:         Make the Poplog widgetset directory available to uses
 > Author:          John Gibson, Apr 23 1993
 > Documentation:   HELP * Xpw
 > Related Files:   x/pop/auto/XPW_EXLIBS.p
 */
compile_mode :pop11 +strict;

section;

include xdefs.ph;

;;; For POPC, make the addition go at the end (after XPOPLIB = 100)
declare_incremental list[prec=250] popuseslist;

extend_searchlist(XPOPLIB dir_>< 'Xpw/', popuseslist, true) -> popuseslist;

constant Xpw = true;

endsection;
