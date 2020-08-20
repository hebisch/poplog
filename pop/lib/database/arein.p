/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/database/arein.p
 > Purpose:         Generalised version of -allpresent-
 > Author:          Aaron Sloman, July 1982 (see revisions)
 > Documentation:   HELP * AREIN
 > Related Files:   LIB * DATABASE
 */

section;

uses database;

define global 5 items arein database;
    lvars items;
    dlocal database;
    allpresent(items)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 31 1991 - moved from lib/auto to lib/database.
--- John Williams, Sep 14 1990 - tidied up.
 */
