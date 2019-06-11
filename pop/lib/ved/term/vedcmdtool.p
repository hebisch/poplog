/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/ved/term/vedcmdtool.p
 > Purpose:         Configure VED for Sun command tool window
 > Author:          Rob Duncan, Nov  2 1989
 > Documentation:
 > Related Files:   LIB * VEDSUN_CMDSCREEN, * VEDSUN_CMDKEYS
 */
compile_mode :pop11 +strict;

uses-by_name vedsun_cmdscreen, vedsun_cmdkeys;

section;

define vars vedcmdtool();
    veduseterm("'sun-cmd'") -> ;
    identfn -> vedcmdtool;
enddefine;

if iscaller(vedsetup) then vedcmdtool() endif;

endsection;
