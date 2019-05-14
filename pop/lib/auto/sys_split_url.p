/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_split_url.p
 > Purpose:
 > Author:          John Gibson, Oct  8 1998
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

    /*  Splits string at split_char into part1 and part2.
        Bits in flags arg as follows:
            0   Decode %hexcode in part2
            1   Decode %hexcode in part1
            2   Determines behaviour when split_char is not present in string:
                if set, (string, false) is returned, otherwise (false, string)
    */
define sys_split_url(split_char, string, flags) /* -> (part1, part2) */;
    lvars string, flags, n = locchar(split_char, 1, string);

    define decode(string);
        lvars string, i, len, c, d1, d2;

        define ishexcode(c);
            lvars c;
            if isnumbercode(c) then c fi_- `0`
            elseif `A` fi_<= c and c fi_<= `F` then c fi_- `7`
            elseif `a` fi_<= c and c fi_<= `f` then c fi_- `W`
            else false
            endif
        enddefine;

        returnunless(string and locchar(`%`, 1, string)) (string);
        datalength(string) -> len;
        consstring(#|
            fast_for i to len do
                if (fast_subscrs(i,string) ->> c) == `%` then
                    i+2 -> i;
                    if i fi_<= len
                    and (ishexcode(fast_subscrs(i-1,string)) ->> d1)
                    and (ishexcode(fast_subscrs(i,string)) ->> d2)
                    then
                        d1<<4 + d2
                    else
                        mishap(string, 1, 'INVALID "%" ENCODING IN URL')
                    endif
                else
                    c
                endif
            endfor
        |#)
    enddefine;

    if n then
        substring(1, n-1, string), allbutfirst(n, string)
    elseif flags &&/=_0 2:100 then
        string, false
    else
        false, string
    endif -> string;
    if flags &&/=_0 2:010 then decode() endif;
    string;
    if flags &&/=_0 2:001 then decode() endif
enddefine;

endsection;
