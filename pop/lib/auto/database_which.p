/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/auto/database_which.p
 > Purpose:         Get values of variables
 > Author:          John Gibson, Dec 27 1995
 > Documentation:   REF * DATABASE
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define database_which(Vars, Pattern) /* -> List */;
    unless islist(Vars) or isident(Vars) or isword(Vars) then
        mishap(Vars, 1, 'WORD, IDENTIFIER, OR LIST NEEDED')
    elseunless islist(Pattern) then
        mishap(Pattern, 1, 'LIST NEEDED')
    endunless;

    [%  for _ allequalto Pattern do
            if islist(Vars) then
                maplist(Vars, valof)
            else
                valof(Vars)
            endif
        endfor
    %]
enddefine;

endsection;
