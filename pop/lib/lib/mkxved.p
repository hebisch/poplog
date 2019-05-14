/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/mkxved.p
 > Purpose:         Run by the "mkxved" script; compiles XVed if necessary
 > Author:          John Williams, Jul 29 1992 (see revisions)
 > Documentation:
 > Related Files:   C.unix/com/mkxved, C.vms/com/mkxved.com
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses xved, xved_standalone_setup;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun  7 1993
        Added uses for xved_standalone_setup (now a separate XVed library)
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF for popc
 */
