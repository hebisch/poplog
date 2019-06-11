/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/ved/tvi925.p
 >  Purpose:        FOR TELEVIDEO VDU MODEL TVI 925 (Non emulating)
 >  Author:         Jim Hunter, April 1983 (see revisions)
 >  Documentation:  HELP * VED, VEDKEYS
 >  Related Files:  LIB * TVI
 */

#_TERMIN_IF DEF POPC_COMPILING

uses vedtvi925

section;

define global tvi925();
    vedtvi925();
enddefine;

if iscaller(vedsetup) then tvi925() endif;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Rob Duncan, Oct 27 1989
        Renamed as "vedtvi925.p", keeping this file as an alias
--- Ben Rubinstein, Oct 12 1986 - vedenter, vedredo indirected through ..key
--- Aaron Sloman, Sep  5 1986 replaced cancel with sysunprotect
--- Roger Sinnhuber, June 1984 - customised.
 */
