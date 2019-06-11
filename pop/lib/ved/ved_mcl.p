/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_mcl.p
 > Purpose:         Mark current list (breaking lines if needed)
 > Author:          John Williams, Oct 21 1988
 > Documentation:
 > Related Files:   C.all/lib/ved/ved_mbl.p     C.all/lib/ved/ved_mel.p
 */
compile_mode :pop11 +strict;

section;

define vars ved_mcl();
    ved_mbl();
    ved_mel();
    vedmarkfind();
enddefine;

endsection;
