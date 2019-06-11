/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_mcf.p
 > Purpose:        marks a flavour record
 > Author:         Mark Rubinstein, Apr 18 1986 (see revisions)
 > Documentation:  TEACH * FLAVOURS, HELP * FLAVOUR_LIBRARY
 > Related Files:  LIB * VED_LCF
 */
compile_mode :pop11 +strict;

section;

define vars ved_mcf;
    dlocal ved_search_state, pop_pr_quotes=false;
    vedpositionpush();
    until vedlinestart('flavour ') do vedcharup() enduntil;
    vedmarklo();
    vedlocate("endflavour");
    vedmarkhi();
    vedpositionpop();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Jonathan Meyer, Sep 29 1993
        Changed vvedsr*ch vars to ved_search_state
--- James Goodlet, Jun  8 1989 - dlocalised vvedsrchstring and vvedsrchsize,
        and introduced dlocal pop_pr_quotes=false.
 */
