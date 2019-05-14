/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/auto/poptrace.p
 *  Purpose:        prints out the current calling sequence (SYSCALLERS?)
 *  Author:         Aaron Sloman, May 23 1978 (see revisions)
 *  Documentation:  See HELP * SYSCALLERS
 *  Related Files:  LIB * SYSCALLERS
 */

section;

define global poptrace();
lvars x n = 1;
    pr('\ndoing:- ');
    until (caller(n) ->>x) == setpop do
        pdprops(x) -> x;
        if x then
            if issubstring('sys',1,x) == 1 or issubstring('ved',1,x) ==1 then
                if popsyscall then spr(x) endif;
            else
                spr(x)
            endif
        endif;
        n + 1 -> n
    enduntil
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Sep 26 1985 - lvarsed and sectioned.
 */
