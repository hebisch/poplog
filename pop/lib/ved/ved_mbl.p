/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_mbl.p
 > Purpose:         Mark beginning of current list  (breaks line if needed)
 > Author:          John Williams, Oct 21 1988
 > Documentation:
 > Related Files:   C.all/lib/ved/ved_gbl.p
 */
compile_mode :pop11 +strict;

section;

define vars ved_mbl();

    define lconstant Clear_to_left();
        lvars c;
        dlocal vedcolumn;
        until vedcolumn == 1 do
            vedcharleft();
            vedcurrentchar() -> c;
            returnunless(c == `\s` or c == `\t`)(false)
        enduntil;
        true
    enddefine;

    ved_gbl();
    unless Clear_to_left() do
        veddocr()
    endunless;
    vedmarklo()
enddefine;

endsection;
