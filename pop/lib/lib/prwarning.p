/* --- Copyright University of Birmingham 1997. All rights reserved. ------
 > File:            $poplocal/local/lib/prwarning.p
 > Purpose:         Alter prwarning to give line number and file number
 > Author:          Aaron Sloman, Jun 20 1997 (see revisions)
 > Documentation:
 > Related Files:
 */

section;

vars popwarnings = [];

define global vars procedure prwarning(word);
    dlocal cucharout = cucharerr;
    printf(word, ';;; DECLARING VARIABLE %P\n');
    if popfilename then
        printf(popfilename, ';;; IN FILE %P\n');
        if isinteger(poplinenum) then
            printf(poplinenum, ';;; LINE %P\n');
        endif
    endif;
    if popwarnings then conspair(word, popwarnings) -> popwarnings endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jun 22 1997
    Reinstated the popwarnings mechanism.
 */
