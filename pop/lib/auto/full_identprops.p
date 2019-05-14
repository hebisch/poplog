/* --- Copyright University of Sussex 1987.  All rights reserved. ---------
 > File:           C.all/lib/auto/full_identprops.p
 > Purpose:        Finding full information about an identifier
 > Author:         Jonathan Laventhol, Aug 14 1984 (see revisions)
 > Documentation:  HELP * FULL_IDENTPROPS
 > Related Files:
 */

section;

define global constant procedure full_identprops(word);
    lvars idp, temp, word;
    if (identprops(word) ->> idp) == "undef" then
        return("undef")
    endif;
    [%
        if isprotected(word) then
            "protected"
        endif;
        if isword(word) and isglobal(word) then
            "global"
        endif;
        if (isconstant(word) ->> temp) then
            if temp == "undef" then
                "assignable"
            endif;
            "constant"
        else
            "vars"
        endif;
        if (identtype(word) ->> temp) == "procedure" then
            "procedure"
        endif;
        if isword(idp) and issubstring('syntax\s', 1, idp) == 1 then
            "syntax",
            strnumber(allbutfirst(7, idp sys_>< ''))
        elseunless idp == 0 then
            idp
        endif
     %]
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 28 1987
        Fixed bug introduced by last change (mishapped when given a "syntax"
        word).
--- John Gibson, Jul 24 1987
        Removed tests for "active", etc.
--- Aaron Sloman, Nov  9 1986
        copes with active variables
---John Williams, Apr 23 1986
        now returns both 'syntax' and 'procedure' when appropriate
*/
