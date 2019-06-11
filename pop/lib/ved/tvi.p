/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/tvi.p
 >  Purpose:        For TELEVIDEO MODEL TVI 920C Emulating VT52
 >  Author:         A Sloman, 1982 (see revisions)
 >  Documentation:  HELP * TVI
 >  Related Files:  LIB * TV925
 */

#_TERMIN_IF DEF POPC_COMPILING

uses vedtvi

section;

define global tvi();
    vedtvi();
enddefine;

if iscaller(vedsetup) then tvi() endif;

endsection;

/* --- Revision History ---------------------------------------------------
--- Rob Duncan, Oct 27 1989
        Renamed as "vedtvi.p", keeping this file as an alias
 */
