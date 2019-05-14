/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/lib/auto/sysisprogfile.p
 > Purpose:         Test if file has extension -pop_default_type-
 > Author:          John Gibson, Aug  3 1989
 > Documentation:   REF *CHARIO
 */
compile_mode:pop11 +strict;

section;

define global sysisprogfile(string);
    lvars string;
    sys_fname_extn(string) = pop_default_type
enddefine;

endsection;
