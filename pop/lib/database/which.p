/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 *  File:           C.all/lib/database/which.p
 *  Purpose:        an extionsion to the database
 *  Author:         Aaron Sloman, May 1983 (see revisions)
 *  Documentation:  HELP * WHICH
 *  Related Files:
 */

section;

define global procedure which(Vars, Pattern) -> List;
lvars Vars Pattern List;
    if ispair(Vars) or isword(Vars) then
        if ispair(Pattern) then
            [%forevery Pattern do
                if isword(Vars) then valof(Vars)
                else maplist(Vars,valof)
                endif
            endforevery%] -> List
        else mishap('LIST NEEDED FOR "WHICH"', [^Pattern])
        endif;
    else
        mishap('WORD OR LIST NEEDED FOR "WHICH"', [^Vars] )
    endif;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 27 1985 - lvarsed and sectioned.
 */
