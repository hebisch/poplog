/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedsun_cmd.p
 > Purpose:         VED: Setup for Sun CMDTOOL window
 > Author:          Rob Duncan, Oct 27 1989
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

uses-by_name vedsun_cmdscreen, vedsun_cmdkeys;

section;

define vars vedsun_cmd();
    veduseterm("'sun-cmd'") -> ;
    identfn -> vedsun_cmd;
enddefine;

if iscaller(vedsetup) then vedsun_cmd() endif;

endsection;
