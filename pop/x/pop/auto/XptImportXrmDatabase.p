/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XptImportXrmDatabase.p
 > Purpose:         Import XrmDatabase structure
 > Author:          Adrian Howard, Jun 30 1993 (see revisions)
 > Documentation:   * XptImportXrmDatabase
 > Related Files:   LIB * FAST_XT_RESOURCE
 */

compile_mode:pop11 +strict;
section;

include xpt_constants.ph;

define global XptImportXrmDatabase
    = XptImportAny(%XDT_DATABASE%);
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Jul  8 1993
        XDT_XRMDATABASE renamed XDT_DATABASE
 */
