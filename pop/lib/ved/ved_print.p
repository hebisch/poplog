/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_print.p
 > Purpose:         Print files from VED
 > Author:          Mark Rubinstein, Sep 17 1985 (see revisions)
 > Documentation:   HELP * VED_PRINT
 > Related Files:   C.vms/lib/ved/ved_print.p
 */
compile_mode :pop11 +strict;

section;

uses postscript_line_consumer;

weak constant procedure (XptValue, XptGetFontProperty, XptAppColorComponents);
weak vars xved, xved$-xveddisplay;


/* ==== VED_PRINT INTERFACE ============================================ */

lvars
    command,    ;;; print command to use: lpr(1) or lp(1)
    using_lpr,  ;;; <true> if -command- is lpr
    files,      ;;; list of files to print
    printer,    ;;; printer to use
    filters,    ;;; list of pre-filters
    copies,     ;;; number of copies to print
    flags,      ;;; other flags to pass
    devtype,    ;;; "ps" if it is postscript - false otherwise
    ps_args,    ;;; args to postscript_line_consumer
    ps_file,    ;;; filename to write postscript to instead of printing
    ;


;;; writes a range of a ved buffer to a file, converting it to PostScript
define lconstant PS_write_range(device, lo, hi);
    lvars device, lo, hi;
    lconstant macro XV_WEAK = [weakref[xved]];

    define get_colours();
        define is_no_pixel();
            lconstant NO_PIXEL = 16:FFFFFFFF;
            () && NO_PIXEL = NO_PIXEL
        enddefine;

        define cval(name);
            XV_WEAK XptValue(wvedwindow, name, "ulong")
        enddefine;

        define get_pixval(name, n, even_def, odd_def) -> pix;
            lconstant NO_PIXEL = 16:FFFFFFFF;
            cval(name) -> pix;
            if is_no_pixel(pix) then
                if testbit(n, 0) then odd_def else even_def endif -> pix
            endif
        enddefine;

        lvars   c0fg = cval('foreground'), c0bg = cval('background'),
                c1fg = cval('highlightForeground'),
                c1bg = cval('highlightBackground'),
                a = false, i, s;

        if is_no_pixel(c1fg) then c0bg -> c1fg endif;
        if is_no_pixel(c1bg) then c0fg -> c1bg endif;

        c0fg, c0bg, c1fg, c1bg;
        repeat 2 times
            for i from 0 to 7 do
                nextif(i <= 1 and not(a));
                'color' sys_>< i -> s;
                if a then s <> 'A' -> s endif;
                get_pixval(s <> 'Foreground', i, c0fg, c1fg);
                get_pixval(s <> 'Background', i, c0bg, c1bg)
            endfor;
            true -> a
        endrepeat
    enddefine;

    ;;; write all the lines from lo to hi
    if lo <= vedline and vedline <= hi then vedtrimline() endif;
    min(vedusedsize(vedbuffer), hi) -> hi;

    if hi /== 0 and lo > hi then
        mishap(lo, hi, 2, 'IMPOSSIBLE RANGE TO WRITE')
    endif;

    lvars procedure consume =
        postscript_line_consumer(device, ps_args,
            if vedvarwidthmode then
                ;;; Only possible in XVed -- these args enable
                ;;; postscript_line_consumer to produce text laid out
                ;;; identically to the XVed window (including colours)
                lvars   font = XV_WEAK XptValue(wvedwindow, 'font', "exptr"),
                        dpy = XV_WEAK $-xved$-xveddisplay;
                XV_WEAK XptGetFontProperty(font, 'FAMILY_NAME', true, dpy),
                XV_WEAK XptGetFontProperty(font, 'POINT_SIZE', dpy) / 10,
                XV_WEAK XptGetFontProperty(font, 'PIXEL_SIZE', dpy),
                XV_WEAK XptValue(wvedwindow, 'fontHeight', "int"),
                consvector(XV_WEAK XptAppColorComponents(get_colours(),
                                        32, wvedwindow, consvector(%3%)),
                            32),
                vedvscr_substring_width(%1,1%)
            endif);

    while lo <= hi do
        ;;; write next line
        consume(subscrv(lo, vedbuffer));
        lo + 1 -> lo
    endwhile;

    consume(termin);
enddefine;

;;; writes contents of file to a device, converting it to PostScript
define lconstant PS_write_file(device, file);
    lvars device, file,
        procedure rep = vedfile_line_repeater(file),
        procedure consume = postscript_line_consumer(device, ps_args),
        procedure filter = rep <> dup <> consume,
      ;
    until filter() == termin do enduntil;
enddefine;

    ;;; choose which print command to use
define lconstant setup();
    returnunless(isundef(command));
    lconstant PATH = systranslate('$PATH');
    if (sys_search_unix_path('lp', PATH) ->> command) then
        false -> using_lpr
    elseif (sys_search_unix_path('lpr', PATH) ->> command) then
        true -> using_lpr
    else
        vederror('\{b}print: can\'t find print command (lp/lpr)');
    endif
enddefine;

    ;;; convert ved command flags into corresponding lpr/lp options
define lconstant translate_flags();
    consstring(#|
        if strmember(`m`, flags) then explode('-m ') endif;
        if using_lpr then
            if strmember(`f`, flags) then explode('-h ') endif;
            if strmember(`h`, flags) then explode('-p ') endif;
            if strmember(`l`, flags) then explode('-l ') endif;
        else
            if strmember(`f`, flags) or strmember(`l`, flags) then
                vederror('\{b}print: lp(1) doesn\'t support flags -f & -l');
            endif;
            if strmember(`h`, flags) then [^^filters 'pr '] -> filters endif;
            ;;; always add '-s' to suppress messages
            explode('-s ');
        endif;
    |#) -> flags;
enddefine;

    ;;; split vedargument into flags, filters and files
define lconstant parseargument();
    lconstant SPECIALS = '-#.$~/:=', FLAGS = 'fhlmd';
    lvars   item, procedure item_rep = incharitem(stringin(vedargument)),
            items = pdtolist(item_rep), nesting, s;

    ;;; modify the itemiser to treat SPECIAL characters as alphabetic
    appdata(
        SPECIALS,
        procedure(c);
            lvars c;
            1 -> item_chartype(c, item_rep);    ;;; alpha
        endprocedure);

    until null(items) do
        dest(items) -> (item, items);
        if isinteger(item) then
            item -> copies;
        elseif isstring(item) then
            ;;; quoted file name
            [^^files ^item] -> files;
        elseif isword(item) and isstartstring('-', item) then
            fast_word_string(item) -> s;
            if s = '-ps' then
                "ps" -> devtype;
                if not(null(items)) and hd(items) == "[" then
                    ;;; optional flags to postscript_line_consumer
                    tl(items) -> items;
                    [%  repeat
                            if null(items) then
                                vederror('\{b}print: missing closing bracket for -ps args');
                            else
                                dest(items) -> (item, items);
                                quitif(item == "]");
                                item
                            endif;
                        endrepeat
                    %] -> ps_args
                endif
            elseif s = '-psfile' then
                "ps" -> devtype;
                if null(items)
                or (dest(items) -> (item, items),
                    not(isstring(item) or isword(item)))
                then
                    vederror('\{b}print: filename expected after -psfile');
                else
                    if isword(item) then fast_word_string(item) -> item endif;
                    item -> ps_file
                endif;
            else
                ;;; parse flags
                lvars i, c, n = 0;
                for i from 2 to datalength(item) do
                    subscrw(i, item) -> c;
                    if c == `p` then
                        if null(items) then
                            vederror('\{b}print: printer name expected after -p flag');
                        else
                            dest(items) -> (printer, items)
                        endif;
                    elseif c == `o` then
                        if null(items) then
                            vederror('\{b}print: device output type expected after -p flag');
                        else
                            dest(items) -> (devtype, items)
                        endif;
                        if devtype /== "ps" then
                            vederror('\{b}print: unknown device type');
                        endif;
                    elseif c == `#` then
                        ;;; ignore
                    elseif isnumbercode(c) then
                        ;;; number of copies
                        n * 10 + c - `0` -> n;
                    elseif strmember(c, FLAGS) then
                        flags <> consstring(c,1) -> flags;
                    else
                        vederror('\{b}print: unrecognized flag - ' <> consstring(c,1));
                    endif;
                endfor;
                unless n == 0 then n -> copies endunless;
            endif
        elseif item == "(" then
            ;;; read a filter
            1 -> nesting;
            consstring(#|
                repeat
                    nextchar(item_rep) -> item;
                    if item == termin then
                        vederror('\{b}print: closing ) not found')
                    elseif item == `(` then
                        nesting fi_+ 1 -> nesting;
                    elseif item == `)` then
                        nesting fi_- 1 -> nesting;
                        quitif(nesting == 0)
                    endif;
                    item
                endrepeat;
            |#) -> item;
            [^^filters ^item] -> filters;
        else
            [^^files ^(item sys_>< nullstring)] -> files;
        endif;
    enduntil;
    translate_flags();
enddefine;

    ;;; construct a shell command to do the printing
define lconstant gen_print_command(file, title);
    lvars file, title;
    consstring(#|
        unless filters == [] then
            lvars filter;
            if file then
                explode('cat '), explode(file), explode(' | ');
                unless title then file -> title endunless;
                false -> file;
            endif;
            for filter in filters do
                explode(filter), explode(' | ');
            endfor;
        endunless;
        explode(command), ` `;
        explode(flags);
        if copies > 1 then
            explode(if using_lpr then '-#' else '-n' endif);
            dest_characters(copies), ` `;
        endif;
        unless printer = nullstring then
            explode(if using_lpr then '-P' else '-d' endif);
            explode(printer), ` `;
        endunless;
        if file then
            explode(file);
        elseif title then
            if using_lpr then
                explode('-J '), explode(title);
                explode(' -T '), explode(title);
            else
                explode('-t'), explode(title);
            endif;
        endif
    |#);
enddefine;


define lconstant do_pipeout(source, comm);
    lvars source, comm;
    if ps_file then
        ;;; just send output to given file
        cont(source)(ps_file)
    else
        pipeout(
            source,
            if strmember(`|`, comm) then
                '/bin/sh', ['sh' '-c' ^comm]
            else
                sysparse_string(comm, false) -> comm;
                hd(comm), comm
            endif,
            true)
    endif
enddefine;

    ;;; print a range of the current buffer
define lconstant vedprintmr(lo, hi);
    lvars lo, hi;
    dlocal vednotabs = true;    ;;; to give correct spacing
    unless hi >= lo then vederror('\{b}print: nothing to print') endunless;
    do_pipeout(
        consref(if devtype == "ps" then
                    PS_write_range(% lo, hi %)
                else
                    vedwriterange(% lo, hi %)
                endif),
        gen_print_command(false, sys_fname_name(vedcurrent)))
enddefine;

define lconstant vedprint(not_marked_range);
    lvars not_marked_range;
    dlocal
        devtype = false,
        files   = [],
        printer = systranslate('popprinter') or nullstring,
        filters = [],
        copies  = 1,
        flags   = nullstring,
        ps_args = [],
        ps_file = false,
    ;

    setup();
    parseargument();

    if not_marked_range == "ps" then
        if files == [] then
            mishap(0, 'PSPRINT - no files specified');
        endif;
        "ps" -> devtype;
    endif;

    if files == [] then
        vedprintmr(
            if not_marked_range then
                vedputmessage('\{b}printing whole file ...');
                1, vvedbuffersize
            else
                vedputmessage('\{b}printing marked range ...');
                vvedmarklo, vvedmarkhi
            endif);
    elseif not_marked_range then
        vedputmessage('\{b}printing named file(s) ...');
        lvars file;
        for file in files do
            if devtype == "ps" then
                do_pipeout(consref(PS_write_file(% file %)),
                            gen_print_command(false, sys_fname_name(file)))
            else
                sysobey(gen_print_command(file, false))
            endif
        endfor;
    else
        vederror('\{b}printmr: only works on current file');
    endif;

    vedputmessage(if ps_file then '\{b}done' else '\{b}print command queued' endif)
enddefine;

define vars ved_printmr =
    vedprint(% false %);
enddefine;

define vars ved_print =
    vedprint(% true %);
enddefine;

;;; used by vedprint command to parse command line arguments
define vars vedpsprint();
    dlocal vedargument;

    ;;; convert poparglist into a string
    consstring(#| procedure;
        dlocal cucharout = identfn;
        applist(poparglist, spr)
    endprocedure() |#) -> vedargument;
    vedprint("ps");
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 20 1999
        Now also passes XVed window colours to postscript_line_consumer when
        printing buffer/marked ranage and vedvarwidthmode is true.
--- John Gibson, May 24 1999
        Now passes extra args to postscript_line_consumer when printing
        buffer/marked ranage and vedvarwidthmode is true.
--- John Gibson, Apr 24 1999
        Added -ps and -psfile flags
--- John Williams, Apr  6 1998
        Changed do_pipeout so that it includes the print command as
        the first element of the arglist given to pipeout. Otherwise
        it fails under Solaris 2.6.
--- John Williams, Jan  4 1996
        Now selects lp in preference to lpr, if possible (cf. BR adrianh.69)
        Also, doesn't run /bin/sh unless necessary.
--- John Williams, Aug  2 1994
        parse_argument now copes with nested ( and ). (cf. BR adrianh.60)
--- Jonathan Meyer, Oct 12 1993
        Added '-o ps' option and PS_ procedures for printing to postscript.
--- John Gibson, Jul 22 1992
        Made vednotabs true inside vedprintmr so printing is done with
        correct spacing.
--- Robert John Duncan, Jun 25 1992
        Merged BSD and System V versions and moved to C.unix
--- John Gibson, Mar 10 1992
        Uses -vedwriterange- with -pipeout-. Cleaned up.
*/
