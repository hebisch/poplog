/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedcifer.p
 > Purpose:         Initialise VED for use with CIFER terminal
 > Author:          Andreas Schoter, Jul 31 1989
 > Related Files:   LIB * VEDCIFERSCREEN, * VEDCIFERKEYS
 */
compile_mode :pop11 +strict;

uses-by_name vedciferscreen, vedciferkeys;

section;

define vars vedcifer();
    veduseterm("cifer") -> ;
    identfn -> vedcifer;
enddefine;

if iscaller(vedsetup) then vedcifer() endif;

endsection;
