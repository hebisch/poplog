/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_mp.p
 > Purpose:         Move cursor to matching parenthesis
 > Author:          John Williams, Oct 21 1988
 > Documentation:
 > Related Files:   C.all/lib/ved/vedfindbracket.p
 */
compile_mode :pop11 +strict;

section;

global vars vedbratable;

if isundef(vedbratable) then
    '()[]{}<>' -> vedbratable
endif;

define vars ved_mp();
    lvars c, i;
    vedcurrentchar() -> c;
    if (locchar(c, 1, vedbratable) ->> i) then
        if testbit(i, 0) then
            fast_subscrs(i fi_+ 1, vedbratable), c, vedatend, vedcharnext
        else
            fast_subscrs(i fi_- 1, vedbratable), c, vedatstart, vedcharleft
        endif;
        vedfindbracket()
    else
        vederror('Unrecognised bracket: "' <> consstring(c, 1) <> '"')
    endif
enddefine;

endsection;
