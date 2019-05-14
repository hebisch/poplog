/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/exptr_init_fixed.p
 > Purpose:         Init an object and return an external pointer to it
 > Author:          Jonathan Meyer, Aug  1 1991
 > Documentation:   REF *EXTERNAL_DATA
 > Related Files:   LIB *exptr_cons_fixed, *exptr_copy_fixed
 */
compile_mode :pop11 +strict;

section;

define global exptr_init_fixed() with_nargs 2;
    fill_external_ptr(init_fixed(), consexternal_ptr());
enddefine;

endsection;
