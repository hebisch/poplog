/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_mkdref.p
 > Purpose:         Add standard attributes to a * doc ref
 > Author:          John Gibson, Feb 26 1996
 > Documentation:   REF * VEDCOMMS
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_mkdref;
    lvars dt_start, start, len, item, lcol, type, save, save_line, c;

    define lconstant skip_spaces();
        while strmember(vedcurrentchar(), '\s\Ss\t')
        and vedcolumn < vvedlinesize do
            vedcharright()
        endwhile
    enddefine;

    define lconstant get_doctype() -> (dt_start, nline, ncol);
        lvars   col = vedcolumn, doctype = false, item, line = vedline,
                tmp, nline = line, ncol = col, last_*_col = false,
                dt, dt_start;

        ;;; Treat string quotes (apostrophes), '.' and '-' as alphabetic
        define lconstant local_chartypes =
            applist(% [`'` `.` `-`], item_chartype %)
        enddefine;

        dlocal  vedcolumn = 1, vedline, vvedlinesize,
                3 % local_chartypes() % = dup(dup(1));

        if col <= vvedlinesize then
            while vedline == line and (vedcolumn->>tmp) <= col
            and (vedmoveitem() ->> item) /== termin do
                vedgetsysfilepdr(item) -> dt;
                if vedcolumn > col then
                    if dt and vednextitem() == "*" then
                        tmp -> dt_start;
                        vedline -> nline;
                        vedcolumn -> ncol
                    else
                        (item /== "*" and last_*_col) or tmp -> ncol;
                        if item == "*" then doctype -> dt_start endif
                    endif
                elseif item == "*" then
                    tmp -> last_*_col;
                    doctype -> dt_start;
                    false -> doctype
                else
                    dt and tmp -> doctype;
                    false -> last_*_col
                endif
            endwhile
        endif;
    enddefine;

    vedline -> save_line;
    skip_spaces();
    get_doctype() -> (dt_start, vedline, vedcolumn);
    vedsetlinesize();
    dt_start or vedcolumn -> start;
    if (vednextitem() ->> item) == "*" then
        vedmoveitem() -> ;
        if vedcolumn > vvedlinesize then vednextline() endif;
        vednextitem() -> item
    else
        vederror('NOT IN A * DOC REF')
    endif;
    if item == termin then vederror('END OF FILE') endif;

    skip_spaces();
    ;;; Read documentation file name
    datalength(ved_get_line_filename('\s\Ss\t\n\'"()/,;:', '.')) -> len;
    vedcolumn + len ->> vedcolumn -> save;
    if vedcurrentchar() == `.` then vedcharright() endif;
    if vedcurrentchar() == `/` then
        vedcharright();
        vedmoveitem() -> ;
        repeat vedcharleft(), quitunless(vedcurrentchar() == `\s`) endrepeat
    else
        save-1 -> vedcolumn
    endif;
    unless vedline == save_line then
        vederror('DOC REF IS SPLIT ACROSS LINES')
    endunless;
    vedcolumn -> lcol;
    start -> vedcolumn;
    if strmember(vedcurrentchar(), '\s\Sf\Sp') then vedwordright() endif;

    while vedcolumn <= lcol do
        if (vedcurrentchar() ->> c) == `\s` or c == `\Ss` then
            false -> dt_start;
            `\[2A]\Ss`
        else
            vedcurrentdchar() || `\[2A]`;
            if dt_start then () || `\[i]` endif
        endif -> vedcurrentdchar();
        vedcharright()
    endwhile
enddefine;

endsection;
