/*  --- Copyright University of Sussex 1995. All rights reserved. ----------
 *  File:           C.all/lib/auto/sysrepeat.p
 *  Purpose:        Procedural call of "repeat" syntax.
 *  Author:         Aaron Sloman, July 1982 (see revisions)
 *  Documentation:  HELP * SYREPEAT (and HELP * REPEAT)
 */
compile_mode :pop11 +strict;

section;

define sysrepeat(num, pdr);
    lvars num, procedure pdr;
    if isinteger(num) then
        while num fi_> 0 do num fi_- 1 -> num; pdr() endwhile
    else
        repeat num times pdr() endrepeat
    endif
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Jul 19 1995
        Tidied
--- Aaron Sloman, Apr  2 1988  - speeded up when an integer is given.
 */
