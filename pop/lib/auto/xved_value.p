/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/auto/xved_value.p
 > Purpose:         Dummy entry for xved_value which mishaps
 > Author:          Jonathan Meyer, Jul  3 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

/* This version of xved_value will be overriden when XVed is loaded */

section;

define global vars procedure xved_value() with_nargs 2;
    mishap(0, 'XVed not loaded');
enddefine;

define updaterof xved_value with_nargs 3;
    mishap(0, 'XVed not loaded');
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 11 1992
        Added #_TERMIN_IF guard for POPC
 */
