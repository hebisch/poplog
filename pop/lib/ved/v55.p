/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/v55.p
 > Purpose:         VED setup for Visual 55 terminal
 > Author:          Rob Duncan, Oct 19 1989
 > Documentation:
 > Related Files:   LIB * VEDVI55
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
    This file has now been superceded by LIB * VEDVI55
    and is kept only for backward compatability.
 */

if iscaller(vedsetup) then vedvi55() endif;
