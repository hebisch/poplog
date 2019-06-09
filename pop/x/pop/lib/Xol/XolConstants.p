/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/lib/Xol/XolConstants.p
 > Purpose:         Old library :WILL BE MOVED TO POPOBSOLETELIB AFTER V14.5
 > Author:          Adrian Howard, Jun 18 1993
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
 * Added for backwards compatability (cf bugreport ianr.40) LIB * XolConstants
 * was previously documented in REF * OPENLOOK and is now obsolete
 */

printf(';;; NOTE: LIB *XolConstants HAS BEEN SUPERCEEDED BY INCLUDE * XolConstants\n');
printf(';;;       See REF * OPENLOOK, REF * OBSOLETE\n');

compile_mode:pop11 +strict;
section;

loadinclude XolConstants.ph;
global constant XolConstants = true;

endsection;
