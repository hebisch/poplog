/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/lib/xved.p
 > Purpose:         Loads xved
 > Author:          Jonathan Meyer, Apr  1 1991 (see revisions)
 > Documentation:
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

section;

uses popxved;

include xveddefs.ph;

pop11_compile(XVEDSRC dir_>< 'xved.p');

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF for popc
--- Julian Clinton, Jul 17 1992
        Removed previous change (Motif widgets now used by default)
--- Robert John Duncan, Mar 16 1992
        DECstation hack to force use of Motif widgets and prevent bug with
        keyboard focus
--- Robert John Duncan, Oct 21 1991
        Made default configuration "vanilla"
--- Jonathan Meyer, Jul  7 1991
        Removed XVED_LOADED constant.
 */
