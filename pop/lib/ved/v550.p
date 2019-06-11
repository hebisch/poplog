/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/v550.p
 >  Purpose:        lib v500 actually for Visual 550.- liable to change
 >  Author:         Aaron Sloman, Nov 1983 (see revisions)
 >  Documentation:  HELP * V550
 >  Related Files:
 */

#_TERMIN_IF DEF POPC_COMPILING

uses vedvi550

section;

define global v550();
    vedvi550();
enddefine;

if iscaller(vedsetup) then v550() endif;

endsection;

/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Oct 27 1989
        Renamed as "vedvi550.p", keeping this file as an alias
--- Ben Rubinstein, Oct 12 1986 - vedenter, vedredo indirected through ..key
*/
