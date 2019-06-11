/* --- Copyright University of Sussex 2000. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedlinux.p
 > Purpose:         Set up VED for linux terminals
 > Author:          Andrew Sayers (Birmingham), 16 Feb 2000
 > Documentation:   REF * VEDTERMINALS
 > Related Files:   LIB * VEDXTERM, LIB * VEDLINUXSCREEN, LIB * VEDLINUXKEYS
 */
compile_mode :pop11 +strict;

section;

uses-by_name vedlinuxscreen, vedlinuxkeys;

define vars vedlinux();
    veduseterm("linux") ->;
    identfn -> vedlinux;
enddefine;

if iscaller(vedsetup) then vedlinux() endif;

endsection;
