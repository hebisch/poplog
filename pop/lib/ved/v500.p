/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/v500.p
 >  Purpose:        Conversion of VED for Visual 500 with VT52 emulation.
 >  Author:         A Sloman, Jan 1984 (see revisions)
 >  Documentation:  HELP * V500
 >  Related Files:
 */
#_TERMIN_IF DEF POPC_COMPILING

uses vedvi500

section;

define global setv500screen();
    setvi500screen();
enddefine;

if iscaller(vedsetup) then vedvi500() endif;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Rob Duncan, Oct 27 1989
        Renamed as "vedvi500.p", keeping this file as an alias
--- Poplog System, Nov 29 1988 - Added "uses graphic" (cf ALPHA 8)
--- Ben Rubinstein, Oct 12 1986 - vedenter, vedredo indirected through ..key
*/
