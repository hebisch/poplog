/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_ifsp.p
 > Purpose:         Insert Format SPaces in REF files
 > Author:          John Gibson, Apr 21 1993
 > Documentation:   REF * VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

define vars ved_ifsp;
    lvars ss_char, args, prev, scope, oldchanged = vedchanged;
    dlocal vedautowrite = false;

    sysparse_string(vedargument, false) -> args;
    if listlength(args) /== 0 and member(hd(args), ['l' '-l' 'r']) then
        dest(args) -> (scope, args);
        if scope = '-l' ->> prev then 'l' -> scope endif
    else
        'r' -> scope    ;;; default to range
    endif;

    if listlength(args) /== 0 then
        dest(args) -> (ss_char, args);
        if ss_char = 'f' then
            `\Sf`
        elseif ss_char = 'p' then
            `\Sp`
        elseif ss_char = '-' then
            `\s`
        else
            vederror('USAGE: \{b}ifsp \{i}scope ss_char')
        endif
    else
        `\Sf`
    endif -> ss_char;

    define lconstant do_lines(nlines);
        lvars nlines, c;
        dlocal vedstatic = true, vedcolumn = 1;
        repeat nlines times
            unless fast_lmember(vedcurrentchar(), [`\s` `\Sf` `\Sp`]) then
                vederror('LINE STARTS WITH NON-SPACE CHARACTER')
            endunless;
            ss_char -> vedcurrentchar();
            vedchardown()
        endrepeat
    enddefine;

    if scope = 'l' then
        ;;; current line
        if prev and vedline /== 1 then vedcharup() endif;
        do_lines(1)
    else
        ;;; marked range
        vedpositionpush();
        vedmarkfind();
        do_lines(vvedmarkhi-vvedmarklo+1);
        vedpositionpop()
    endif;

    if oldchanged then oldchanged + 1 else 1 endif -> vedchanged
enddefine;

endsection;
