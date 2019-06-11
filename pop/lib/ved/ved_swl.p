/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_swl.p
 >  Purpose:        swap current string with string on left, (space delimited)
 >  Author:         S.Hardy 1982
 >  Documentation:  HELP * VEDCOMMS/ved_swl
 >  Related Files:  LIB * VED_SWR
 */
compile_mode :pop11 +strict;

section;

define lconstant swleft();
    while vedcurrentchar() == `\s` do vedcharleft() endwhile;
    until vedcolumn == 1 or vedcurrentchar() == `\s` do
        vedcharleft();
    enduntil;
    if vedcurrentchar() == `\s` then vedcharright() endif;
enddefine;

define vars ved_swl;
    lvars string;
    swleft();
    if vedline == 1 and vedcolumn == 1 then vederror('TOP OF FILE') endif;
    consstring(#|
        until vedcurrentchar() == `\s` do
            vedcurrentchar(),
            veddotdelete();
        enduntil
    |#) -> string;
    veddotdelete();
    if vedcolumn = 1 then
        vedcharleft();
        unless vedcolumn == 1 then vedcharinsert(`\s`) endunless;
        vedinsertstring(string);
    else
        vedcharleft();
        swleft();
        vedinsertstring(string)
    endif;
    vedcharinsert(`\s`);
    vedcharleft();
    swleft();
enddefine;

define vars ved_swr();
    lvars string;
    while vedcurrentchar() == `\s` do
        if vedatend() then vederror('END OF FILE') endif;
        if vedcolumn > vvedlinesize then
            vednextline()
        else
            vedcharright()
        endif;
    endwhile;
    swleft();
    consstring(#|
        until vedcurrentchar() == `\s` do
            vedcurrentchar();
            veddotdelete();
        enduntil
    |#) -> string;
    veddotdelete();
    if vedcolumn > vvedlinesize then
        vednextline();
        vedinsertstring(string);
        vedcharinsert(` `);
        vedcharleft();
        swleft();
    else
        while vedcurrentchar() == `\s` do vedcharright() endwhile;
        until vedcurrentchar() == `\s` do vedcharright() enduntil;
        vedcharinsert(`\s`);
        vedinsertstring(string);
        swleft()
    endif
enddefine;

endsection;
