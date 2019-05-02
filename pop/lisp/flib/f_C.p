/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_C.p
 > Purpose:         LIB * FORMAT_PRINT ~C directive
 > Author:          John Williams, Dec  9 1985 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;

section $-lisp$-fpr;

#_IF not(is_subsystem_loaded("lisp"))
global vars $-lisp$-print_escape, $-lisp$-print_pretty;
global constant procedure $-lisp$-char_name;
#_ENDIF

define lconstant Pop11_char_pr(char);
    cucharout(```);
    if char fi_<= `\s` then
        cucharout(`\\`);
        cucharout(if char == `\b` then `b`
                  elseif char == `\t` then `t`
                  elseif char == `\n` then `n`
                  elseif char == `\r` then `r`
                  elseif char == `\s` then `s`
                  else cucharout(`^`), char fi_+ 64
                  endif)
    elseif char == `\\` then
        cucharout(`\\`);
        cucharout(`\\`);
    elseif char == `\^?` then
        cucharout(`\\`);
        cucharout(`^`);
        cucharout(`?`);
    elseif char fi_> 127 then
        cucharout(`\\`);
        cucharout(`(`);
        sys_syspr(char);
        cucharout(`)`)
    else
        cucharout(char)
    endif;
    cucharout(```)
enddefine;


procedure();
    lvars char, lchar = false, name;
    next_f_arg() -> char;
    if f_subsystem == "lisp" then
        char -> lchar;
        destcharacter(lchar) -> char
    elseunless isinteger(char) and char fi_>= 0 and char fi_<= 255 then
        mishap(char, 1, 'Invalid Ascii character code')
    endif;
    if f_colon then
        /* CHAR MUST BE NAMED */
        if lchar then
            $-lisp$-char_name(lchar)
        elseif char fi_< `\s` then
            writeable 'Control *' ->> name;
            char fi_+ 64 -> fast_subscrs(9, name)
        elseif char == `\s` then
            'Space'
        elseif char == `\^?` then
            'Delete'
        else
            false
        endif -> name;
        if pop_true(name) then
            cuch_string(name)
        else
            cucharout(```);
            cucharout(char);
            cucharout(`'`)
        endif;
        if f_at then
            if char ==  `\b` then
                '(the BackSpace key)'
            elseif char ==  `\t` then
                '(the Tab key)'
            elseif char ==  `\n` then
                '(the LineFeed key)'
            elseif char ==  `\r` then
                '(the Return key)'
            elseif char ==  `\^[`then
                '(the Esc key)'
            elseif char ==  `\^?`then
                '(the Del key)'
            elseif lchar and char fi_< `\s` then
                writeable '(Control *)' ->> name;
                char fi_+ 64 -> fast_subscrs(10, name)
            else
                false
            endif -> name;
            if name then
                cucharout(`\s`);
                cuch_string(name)
            endif
        endif
    elseif f_at then
        /* CHAR MUST BE MACHINE-READABLE */
        if lchar then
            procedure() with_nargs 1;
              dlocal $-lisp$-print_escape = true, $-lisp$-print_pretty = nil;
              pr()
            endprocedure(lchar)
        else
            Pop11_char_pr(char)
        endif
    else
        /* JUST OUTPUT CHAR DIRECTLY */
        cucharout(char)
    endif
endprocedure -> f_proc(`C`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Mar  6 1996
        Uses `is_subsystem_loaded("lisp")' to decide whether to declare
        Lisp variables.
 */
