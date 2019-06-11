/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_get_password.p
 > Purpose:         Prompt for and read password in Ved
 > Author:          John Gibson, Sep 28 1998
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define ved_get_password(mess_string) /* -> string */;

    define get_password(mess_string, use_ved);
        lvars char, n = 0;

        if use_ved then
            if vedusewindows and wved_is_live_window(wvedwindow) then
                wved_open_window(wvedwindow)
            endif;
            vedclearinput();
            vedscreenbell();
            vedsetstatus(nullstring, false, true);
            vedsetstatus('(' <> mess_string, false, true)
        else
            sys_clear_input(poprawdevin);
            syswrite(poprawdevout, mess_string, datalength(dup()));
            sysflush(poprawdevout)
        endif;

        repeat
            if use_ved then vedinascii() else rawcharin() endif -> char;
            if char == `\n` or char == `\r` then
                if use_ved then
                    1000 ->> vedscreenline -> vedscreencolumn;
                    vedsetstatus(vedmessage, vedmessage /= nullstring, true)
                else
                    syswrite(poprawdevout, '\r\n', 2);
                    sysflush(poprawdevout)
                endif;
                return(consstring(n))
            elseif char == `\^U` then
                erasenum(n);
                0 -> n
            elseif char == `\^?` then
                unless n == 0 then
                    () -> ;
                    n fi_- 1 -> n
                endunless
            else
                char;
                n fi_+ 1 -> n
            endif
        endrepeat
    enddefine;

    vedputmessage_apply(mess_string, get_password)
enddefine;

endsection;
