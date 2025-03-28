/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol.p
 > Purpose:         Make the OpenLook widgetset directory available to uses
 > Author:          John Gibson, Apr 23 1993
 > Documentation:   HELP * OPENLOOK
 > Related Files:   x/pop/auto/XOL_EXLIBS.p
 */
compile_mode :pop11 +strict;

section;

include xdefs.ph;

;;; For POPC, make the addition go at the end (after XPOPLIB = 100)
declare_incremental list[prec=200] popuseslist;

extend_searchlist(XPOPLIB dir_>< 'Xol/', popuseslist, true) -> popuseslist;

constant Xol = true;

endsection;
