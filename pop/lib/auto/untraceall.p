/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/auto/untraceall.p
 >  Purpose:        Untrace all traced procedures
 >  Author:         A.Sloman 1982 (see revisions)
 >  Documentation:  HELP * UNTRACEALL
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define global 1 untraceall;
    dlocal tracing = false;
    appdic(procedure(word);
            lvars id pdr val word;
            if (isdefined(word) ->> id)
            and (isprocedure(nonactive_idval(id) ->> val))
            and (istraced(val) ->> pdr)
            then
                pdr -> nonactive_idval(id)
            endif
        endprocedure)
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Oct 23 1989
        Fixed BR rudil.1
--- John Williams, Mar  8 1989
        Now an operator, not a macro; also uses -istraced- and -isdefined-
--- Aaron Sloman, Feb 14 1987
        Fixed for new version of trace + nasty active variables
*/
