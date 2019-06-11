/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_mel.p
 > Purpose:         Mark end of current list (breaks line if needed)
 > Author:          John Williams, Oct 21 1988
 > Documentation:
 > Related Files:   C.all/lib/ved/ved_mbl.p
 */
compile_mode :pop11 +strict;

section;

define vars ved_mel();

    define lconstant Clear_to_right();
        vedtrimline();
        vedcolumn fi_>= vvedlinesize
    enddefine;

    ved_gel();
    unless Clear_to_right() do
        vedcharright();
        veddocr();
        vedcharup();
    endunless;
    vedmarkhi()
enddefine;

endsection;
