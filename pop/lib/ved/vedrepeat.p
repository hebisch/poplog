/*  --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:           C.all/lib/ved/vedrepeat.p
 > Purpose:        read a number and then a comand and do command num times
 > Author:         Chris Slymon, Nov 1983 (see revisions)
 > Documentation:
 > Related Files:  LIB * VEDREADINTEGER
 */
compile_mode :pop11 +strict;

/*
Read a number X, displaying the result so far on the status line.
(ESC-DEL can be used to delete the last number typed).
Then read a key sequence for accessing VEDS procedure tables.
Execute the procedure or insert the string X times.
*/

section;

define vedrepeat();
    lvars num, char, proc;
    vedreadinteger() -> (num, char);
    if num = 0 then
        vederror('REPEAT COUNT ZERO');
    else
        char :: ved_char_in_stream -> ved_char_in_stream;
        vedgetproctable(vedinascii()) -> proc;
        if isstring(proc) then
            [% repeat num times explode(proc) endrepeat %
                 ^^ved_char_in_stream] -> ved_char_in_stream;
        elseunless proc == undef then
            repeat num times proc() endrepeat;
        else
            vederror(char >< 'Undefined key sequence');
        endif;
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Jan 23 1986 - sectionised and lvarsed.
*/
