/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/database/isin.p
 > Purpose:         Generalised version of -present-
 > Author:          Aaron Sloman, July 1982 (see revisions)
 > Documentation:   HELP * ISIN
 > Related Files:   LIB * DATABASE
 */

section;

uses database;

define global 5 item isin database;
    lvars item;
    dlocal database;
    present(item)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan 31 1991 - moved from lib/auto to lib/database
--- John Williams, Sep 14 1990 - tidied up.
 */
