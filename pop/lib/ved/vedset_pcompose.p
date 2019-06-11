/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/vedset_pcompose.p
 > Purpose:         Compose procedures/procedure valofs -- used by vedset
 > Author:          John Gibson, Nov 15 1992 (see revisions)
 > Documentation:
 */
compile_mode :pop11 +strict;

section;

define vedset_pcompose(p, q);
    lvars p, q;
    if isprocedure(p) and isprocedure(q) then
        p <> q
    else
        procedure(p, q);
            lvars p, q;
            repeat
                if isword(p) then
                    valof(p) -> p
                elseif isident(p) then
                    idval(p) -> p
                endif;
                p();
                quitunless(q ->> p);
                false -> q
            endrepeat
        endprocedure(% p, q %)
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  5 1993
        Allowed to take idents as well as words
 */
