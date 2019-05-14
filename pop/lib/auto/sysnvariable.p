/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/auto/sysnvariable.p
 > Purpose:         Defunct -- Moved out of system
 > Author:          John Gibson, Mar  6 1988
 */

section;

lvars nextvar = 1;

define global constant procedure sysnvariable() -> word;
    lvars word;
    if nextvar >= 16:10000 then
        1 -> nextvar
    endif;
    consword(0, nextvar >> 8, nextvar && 255, 3) -> word;
    nextvar fi_+ 1 -> nextvar;
    if identprops(word) == "undef" then
        sysSYNTAX(word, 0, false)
    endif
enddefine;

endsection;
