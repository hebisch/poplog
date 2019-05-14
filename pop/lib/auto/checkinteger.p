/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:          C.all/lib/auto/checkinteger.p
 > Purpose:       Check that an object is an integer in a range.
 > Author:        Ben Rubinstein, Feb  3 1987
 > Documentation: HELP * CHECKINTEGER
 */
compile_mode :pop11 +strict :vm -pentch;

section;

define checkinteger(obj, low, high) with_props false;
    lvars obj, low, high;
    if obj.isinteger then
        unless (low and obj < low) or (high and obj > high) then
            return;
        endunless;
    endif;
    mishap(obj, 1,
        if low and high then
            'INTEGER BETWEEN ' sys_>< low sys_>< ' AND ' sys_>< high sys_>< ' NEEDED'
        elseif low then
            'INTEGER GREATER THAN ' sys_>< (low - 1) sys_>< ' NEEDED'
        elseif high then
            'INTEGER LESS THAN ' sys_>< (high + 1) sys_>< ' NEEDED'
        else
            'INTEGER NEEDED'
        endif);
enddefine;

endsection;
