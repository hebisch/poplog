/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:           C.all/lib/ved/vedreadinteger.p
 > Purpose:        read an integer from the keyboard.
 > Author:         Chris Slymon, Nov 1983 (see revisions)
 > Documentation:
 > Related Files:
 */
compile_mode: pop11 +strict;

/*
Read a number X, displaying the running total on the status line.
(ESC-DEL can be used to delete the last figure typed)
The number is terminated by a non-integer character, or ESC followed by
something other than <delete>.
The procedure returns the integer and those characters that were required to
terminate the number, either as a single character or a string containing two
characters.
*/

section;

define vedreadinteger() -> (num, char);
    lvars char, num = 0;
    repeat
        if isnumbercode(vedinascii() ->> char) then
            (num * 10) + (char - `0`) -> num;
        elseif char = vedescape then
            if (vedinascii() ->> char) == 127 then
                num div 10 -> num;
            else
                consstring(vedescape,char,2) -> char;
                return;
            endif;
        else
            quitloop;
        endif;
        vedputmessage(nullstring sys_>< num);
        vedsetcursor();
    endrepeat;
    vedsetcursor();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Adrian Howard, Sep  8 1992
        Now uses sys_><
--- Mark Rubinstein, Jan 23 1986 - sectionised and lvarsed.
*/
