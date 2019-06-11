/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/wved_get_one_input.p
 > Purpose:         Old oddity for backward compatibility
 > Author:          John Gibson, Jan 17 1996
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define wved_get_one_input();
    consstring(`A`, vedinascii(), 2)
enddefine;

endsection;
