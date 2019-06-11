/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_lcf.p
 > Purpose:        loads current flavour
 > Author:         Mark Rubinstein, Apr 1986
 > Documentation:  TEACH * FLAVOURS, HELP * FLAVOUR_LIBRARY
 > Related Files:  LIB * VED_LCF
 */
compile_mode :pop11 +strict;

section;

define vars ved_lcf;
    vedmarkpush();
    false -> vvedmarkprops;
    ved_mcf();
    ved_lmr();
    vedmarkpop();
enddefine;

endsection;
