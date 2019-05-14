/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/exptr_cons_fixed.p
 > Purpose:         Init an object and return an external pointer to it
 > Author:          Jonathan Meyer, Aug  1 1991
 > Documentation:   REF *EXTERNAL_DATA
 > Related Files:   LIB *exptr_init_fixed, *exptr_copy_fixed
 */
compile_mode :pop11 +strict;

section;

define global exptr_cons_fixed() with_nargs 1;
    fill_external_ptr(cons_fixed(), consexternal_ptr());
enddefine;

endsection;
