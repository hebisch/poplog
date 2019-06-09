/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.x/x/pop/lib/xlib.p
 > Purpose:         Makes available xlib interface facilities
 > Author:          Ian Rogers, Aug 18 1989 (see revisions)
 > Documentation:   HELP * XLIB, HELP * X
 > Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

uses extend_searchlist;

section;

#_IF not(DEF XtVersion)
    mishap(0, 'Can\'t load LIB * XLIB: Poplog not linked with X facilities');
#_ENDIF

uses newexternal;

include xdefs.ph
global constant xlib = XPOPXLIB;

extend_searchlist(xlib, popautolist) -> popautolist;
extend_searchlist(xlib, popuseslist) -> popuseslist;

if isinteger(popmemlim) then
    200000 + popmemlim -> popmemlim
endif;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Apr 27 1995
        Now copes with popmemlim being false (c.f. BR isl-fr.4529).
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- Adrian Howard, Aug 20 1991 : HELP *XPOP --> HELP *X
--- Ian Rogers, Jan 29 1991
        Added popmemlim increment
--- Jonathan Meyer, Jan 25 1991
        Removed xlib from popincludelist. Changed mishap message to
        make it more up to date.
--- Aaron Sloman, Jul 28 1990
        replaced extendsearchlist with extend_searchlist
--- Roger Evans, Jun  1 1990
        changed to use extendsearchlist and removed sysfileok
--- Roger Evans, May 31 1990
        Removed stuff already done by lib popxpop, added xdefs.ph
--- Ian Rogers, May 24 1990
        Added xt_check_exlinkbase.
        Corrected -member- tests on popincludelist.
--- Aaron Sloman, May 23 1990
Added popincludelist
 */
