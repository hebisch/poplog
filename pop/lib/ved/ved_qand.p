/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_qand.p
 > Purpose:         Quit file & do a command
 > Author:          Adrian Howard, Jan  9 1992 (see revisions)
 > Documentation:   REF *VED_QAND
 > Related Files:
 */

compile_mode: pop11 +strict;
section;

define global ved_qand();
    if vedargument = nullstring then
        vedqget(identfn);
    else
        vedqget(veddo(%vedargument%));
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  9 1992
        Installed in masters
 */
