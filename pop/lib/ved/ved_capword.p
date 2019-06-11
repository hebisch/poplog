/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_capword.p
 > Purpose:         Capitalise only first character of <vedargument> words
 > Author:          Aaron Sloman, Jun  4 1988
 > Documentation:   <ENTER> ?? ved_capword
 > Related Files:   LIB * VEDCAPWORD, LIB * VEDEMACS
 */
compile_mode :pop11 +strict;

section;

define ved_capword;
    lvars count = strnumber(vedargument);
    unless count then 1 -> count endunless;

    until count == 0 do vedcapword(); count fi_- 1 -> count enduntil
enddefine;

endsection;
