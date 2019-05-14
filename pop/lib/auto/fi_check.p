/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:          C.all/lib/auto/fi_check.p
 > Purpose:       FAST Check that an object is an integer in a range.
 > Author:        Aaron Sloman May 1990 (see revisions)
 > Documentation: HELP * CHECKINTEGER
 > Related Files: LIB * CHECKINTEGER
 */
compile_mode :pop11 +strict :vm -pentch;

section;

    /*  Returns the obj if it is an integer and >= low and <= high.
        Otherwise causes an error.
        This version does not check that low and high are integers.
    */
define fi_check(obj, low, high) -> obj with_props false;
    lvars obj, low, high;
    returnif(   isinteger(obj)
            and (not(low) or obj fi_>= low)
            and (not(high) or obj fi_<= high)
        );
    mishap(obj, 1,
        if low and high then
            'INTEGER BETWEEN ' sys_>< low sys_>< ' AND ' sys_>< high sys_>< ' NEEDED'
        elseif low then
            'INTEGER GREATER OR EQUAL TO ' sys_>< low sys_>< ' NEEDED'
        elseif high then
            'INTEGER LESS OR EQUAL TO ' sys_>< high sys_>< ' NEEDED'
        else
            'INTEGER NEEDED'
        endif);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 27 1996
        Simplified checking code (boolean expressions are now optimised
        better).
--- John Gibson, Oct 12 1991
        Changed mishap message
--- Aaron Sloman, May 23 1990
        Optimised version of checkinteger (because compiler is not too good
        on complex boolean expressions). Also returns the integer
 */
