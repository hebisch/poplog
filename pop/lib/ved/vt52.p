/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/vt52.p
 >  Purpose:        set ved for a 'DUMB' vt52 compatible terminal.
 >  Author:         Aaron Sloman, Jan 1983 (see revisions)
 >  Documentation:  HELP * VT52
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

uses vedvt52

if iscaller(vedsetup) then vedvt52() endif;

/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Nov  3 1989
        Renamed to "vedvt52.p", keeping this file as an alias
--- Ben Rubinstein, Oct 12 1986  - vedenter, vedredo indirected through ..key
*/
