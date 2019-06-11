/* --- Copyright University of Sussex 2005. All rights reserved. ----------
 > File:            C.all/lib/ved/vedfilecomplete.p
 > Purpose:         File completion utility
 > Author:          Aaron Sloman, Oct 27 1990 (see revisions)
 >                  (With some ideas from Steve Knight and Ben Sloman)
 > Documentation:   HELP * VEDFILECOMPLETE
 > Related Files:
 */
compile_mode: pop11 +strict;

section;

;;; USER ASSIGNABLE GLOBAL VARIABLES

;;; Use this to determine which file endings indicate that the file
;;; should be excluded. See HELP * VEDFILECOMPLETE/vedcomplete_exclude
;;; It can hold [] or a list of strings and/or procedures used to check
;;; whether a filename should not be displayed
global vars vedcomplete_exclude;

if isundef(vedcomplete_exclude) then
    ;;; by default exclude only VED backup files. (UNIX only)
    ['-'] -> vedcomplete_exclude
endif;

;;; Procedure variable to control whether full path name or just
;;; file name gets displayed. Make it -identfn- to display full pathname
;;; Default displays only file name without directory part

global vars procedure vedcomplete_display;

if isundef(vedcomplete_display) then
    sys_fname_namev -> vedcomplete_display
endif;


;;; Name of buffer used for completion options
lconstant
    complete_buffer = 'FILE\sCOMPLETIONS',
    isunix = hd(sys_os_type) == "unix";

;;; Globals used to remember where the last options came from
lvars
    last_name,
    last_line, last_column,
    last_indent = false,
    last_cols = 0,
    last_file,
    last_status,
    last_status_limit = 0,
    last_list = writeable newproperty([], 2, false, "tmparg"),
    HEADER_LINES = 0,
;

lconstant
    waitmessage = 'please wait';


;;; This might be a useful separate library file
define lconstant sysfilecomplete(string) -> list;
    ;;; Make a list of possible filename completions of -string-
    lvars string, list,
        name = sysfileok(string),
        len = datalength(name),
        filerep = sys_file_match(name sys_>< '*', '', false, false),
        filename;

    [%
        repeat
            filerep() -> filename;
        quitif(filename == termin);
            if null(vedcomplete_exclude) then
                ;;; include all files including backups
                filename
            else
                ;;; Exclude files that either end with a string in the
                ;;; list complete_exclude, or for which a procedure in the
                ;;; list yields TRUE
              lblock; lvars test, exclude = false;
                for test in vedcomplete_exclude do
                    if isword(test) then valof(test) -> test endif;
                    if isprocedure(test) then
                        if test(filename) then
                            true -> exclude;
                            quitloop
                        endif;
                    elseif isendstring(test, filename) then
                        true -> exclude;
                        quitloop
                    endif
                endfor;
                unless exclude then filename endunless;
              endlblock;
            endif
        endrepeat
    %] -> list;
enddefine;


define lconstant insert_completion(old, new);
    lvars old, new, transformed_old = sysfileok(old, false);
    if old = new then
        if isunix and sysisdirectory(old) then vedcharinsert(`/`)
        else
            vedscreenbell()
        endif
    else
        if isstartstring(transformed_old, new) then
            vedinsertstring(allbutfirst(datalength(transformed_old), new))
        else
            repeat datalength(old) times
                vedchardelete();
            endrepeat;
            vedinsertstring(new);
        endif;
        if isunix and sysisdirectory(new) then vedcharinsert(`/`) endif
    endif
enddefine;

define lconstant get_number() -> char;
    ;;; read in character between 1 and 9. If RETURN is pressed, return
    ;;; false
    lvars char = vedinascii();
    if char == `\n` or char == `\r` then
        false
    elseif isnumbercode(char) then
        ;;; convert to number
        char fi_- `0`
    else
        vederror('NUMBER or RETURN needed')
    endif -> char;
enddefine;


define lconstant try_status_line(last_name, completions) -> result;
    ;;; prepare list of options to display on status line.
    lvars name, num = 1, len, string, completions, last_name, result,
        size = listlength(completions);

    ;;; put characters for string on the stack, and count them
    #|
        for name in completions do
            explode(num sys_>< '-'),
            explode(vedcomplete_display(name));
            if isunix and sysisdirectory(name) then `/` endif,
            ` `;
            num fi_+ 1 -> num;
        endfor;
        if num /== 1 then ->; endif;       ;;; erase final space
    |# -> len;

    ;;; See if the menu will fit on status line
    if len < vedscreenwidth - (vedstatusheaderlen + 5) then
        true -> result;
        ;;; make string menu for status line
        consstring(len) -> string;
        vedputmessage(string);
        if get_number() ->> num then
            if num == 0 or num fi_> size then
                vederror('NUMBER NOT BETWEEN 1 AND ' sys_>< size)
            else
                insert_completion(last_name, completions(num));
            endif
        else
            unless vedonstatus then vedputmessage(' ... continue ...')
            endunless
        endif;
    else
        erasenum(len);
        false -> result
    endif
enddefine;

define lconstant setup_options_file(completions);
    ;;; Create Menu File showing possible completions
    lvars name, completions, with_keys = false,
        len = listlength(completions);

    lvars MINCODE = 32; ;;; code for space

    ;;; See whether list is short enough to allow addressing by keys
    if len <= 126 - MINCODE then true -> with_keys endif;

    if with_keys then
        ;;; see if codes could start with `1` then try `a`,
        ;;; if not then with `A`, otherwise leave as 32
        if len <= 9 then
            `1` - 1
        elseif len <= 126 - `a` then
            `a` - 1
        elseif len <= 126 - `A` then
            `A` - 1
        else MINCODE
        endif  -> MINCODE
    endif;

    dlocal
        vveddump,
        pr = sys_syspr,
        pop_pr_quotes = false,
        ;
    ;;; Menu too big for status line
    ;;; Get the interaction file ready. Clear if necessary
    vededit(complete_buffer, vedhelpdefaults);
    if vvedbuffersize > 0 then ved_clear() endif;

    lblock lvars maxlen = 0, cols, num = 1 ;
        ;;; find maximum file name length (only name plus extension)
        fast_for name in completions do
            fi_max( maxlen, datalength(vedcomplete_display(name)) ) -> maxlen
        endfast_for;

        ;;; increase maxlen to include room for "/" and space
        ;;; and letter and ")" if appropriate.
        maxlen fi_+ if with_keys then 4 else 2 endif -> vedindentstep;
        false -> vedbreak;

        vedindentstep -> last_indent;   ;;; remember in global variable

        ;;; find number of columns to use, and save in global variable

        vedlinemax div vedindentstep ->> cols -> last_cols;


        vedinsertstring(len sys_>< ' POSSIBLE COMPLETIONS FOR:  ');
        vedinsertstring(
            if last_name = nullstring then 'empty string' else last_name endif);
        vedlinebelow();
        vedinsertstring('EITHER select option by typing code before ")" OR');
        vedlinebelow();
        ;;; vedinsertstring('Select name (chardown & ESC Tab) then Redo "COMPLETION" command (ESC F)');
        vedinsertstring('Select name (chardown & ESC Tab) then Redo "COMPLETION" command (ESC 3)or(ESC F)');
        vedline + 1 -> HEADER_LINES;
        true -> vedstatic;
        vedputmessage(waitmessage);
        dlocal vedediting = false;

        vedlinebelow();
        1 -> num;

        for name in completions do
            if with_keys then
                ;;; insert character for quick selection
                vedcharinsert(num fi_+ MINCODE);
                vedcharinsert(`)`);
            endif;
            vedinsertstring(vedcomplete_display(name));
            if isunix and sysisdirectory(name) then
                vedcharinsert(`/`);
            endif;
            if (num rem cols) == 0 then vedlinebelow() else vedtabright() endif;
            num fi_+ 1 -> num;
        endfor;

        vedtopfile();
        true -> vedediting;
        vedjumpto(3,1);
        vedrefresh();
        lvars char = vedinascii();  ;;; should it be vedscr_read_input?
        if char > MINCODE and char <= MINCODE + len  then
            ;;; user has selected a file.
            ;;; Go back and do the completion
            last_file -> vedargument;
            ved_qved();

            unless vedonstatus == last_status then vedstatusswitch() endunless;
            unless vedline == last_line and vedcolumn == last_column then
                vedjumpto(last_line, last_column); vedcheck();
            endunless;
            insert_completion(last_name, completions(char - MINCODE));
            false ->>last_line ->> last_column ->> last_file ->> last_status
                -> last_name;
            clearproperty(last_list);
            if last_status_limit /== 0 then
                last_status_limit -> vedstatusbufferlimit;
                0 ->last_status_limit
            endif;
        else
            ;;; stick it back on input stream
            vedinput(char)
        endif;
    endlblock
enddefine;

define lconstant lastfilename();
    ;;; file name to left of cursor
    dlocal vedcolumn;

    define lconstant isdelimiter(c);
        lvars c;
        c fi_<= `\s` or             ;;; white space or control
        c == `\'` or c == `"` or    ;;; string or word quote
        c fi_>= `\^?` and not(isalphacode(c))
                                    ;;; 8-bit and not ISO Latin alpha
    enddefine;

    lvars col = vedcolumn;
    until vedcolumn == 1 do
        vedcharleft();
        if isdelimiter(vedcurrentchar()) then
            vedcharright();
            quitloop;
        endif;
    enduntil;
    if vedcolumn == col then
        nullstring
    else
        substring(vedcolumn, col fi_- vedcolumn, vedthisline())
    endif;
enddefine;

define lconstant common_completion(name, strings) -> common;
    ;;; Assume all strings in the list have name as a common initial
    ;;; Substring. Find additional common initial substring, if any,
    ;;;  after name, in the list strings.

    lvars name, string, strings, common, lastchar,
        first = front(strings),
        maxlen = datalength(first),
        index,
        start;

    ;;; Don't try to find common completions if name is a pattern
    returnif(strmember(`*`, name) or strmember(`?`, name))(nullstring -> common);

    ;;; expand possible environment variables, etc
    sysfileok(name) -> name;
    datalength(name) -> start;

    back(strings) -> strings;
    start -> index;
    repeat
        index + 1 -> index;
    returnif(index fi_> maxlen)(
            substring(start + 1, index - 1 - start, first)-> common);

        ;;; Now see if all the strings agree with the first one.
        subscrs(index, first) -> lastchar;
        for string in strings do
            if index fi_> datalength(string) then
                index - 1 -> index;
                datalength(string) -> maxlen;
                nextloop(2)
            elseif subscrs(index, string) /== lastchar then
                index - 1 ->> index -> maxlen;
                nextloop(2)
            endif
        endfor
    endrepeat
enddefine;

define lconstant get_selection();
    ;;; run by vedfilecomplete when invoked in a menu file
    ;;; Find which item is selected, using cursor location
    lvars col, num, list, wanted;

    if vedline > vvedbuffersize then vederror('PLACE CURSOR AT FILENAME') endif;

    ;;; avoid problems of incomplete last row
    if vedcolumn > vvedlinesize then vedtextright() endif;

    ;;; Find which column cursor is in
    vedcolumn div last_indent + 1 -> col;

    ;;; Find corresponding number in list of alternatives
    (vedline - HEADER_LINES) * last_cols + col -> num;

    ;;; Get the required full file name. List stored in last_list
    false -> list;
    appproperty(last_list,
        procedure(); -> list; -> endprocedure);

    unless list and vedpresent(last_file) then
        vederror('ORIGINAL FILE NO LONGER IN VED')
    endunless;

    list(num) -> wanted;

    ;;; Go back and do the completion
    last_file -> vedargument;
    ved_qved();

    unless vedonstatus == last_status then vedstatusswitch() endunless;
    unless vedline == last_line and vedcolumn == last_column then
        vedjumpto(last_line, last_column); vedcheck();
    endunless;
    insert_completion(last_name, wanted);
    false ->>last_line ->> last_column ->> last_file ->> last_status
        -> last_name;
    clearproperty(last_list);
    if last_status_limit /== 0 then
        last_status_limit -> vedstatusbufferlimit;
        0 ->last_status_limit
    endif
enddefine;

define vedfilecomplete();
    ;;; Top level procedure, normally mapped onto key sequence
    ;;; If already in menu buffer, make selection, quit, and go
    ;;; to previous file then insert completion
    ;;; If not in menu buffer, then start trying to complete filename
    ;;; using characters to left of cursor

    if vedcurrent = complete_buffer and not(vedonstatus) then
        get_selection()
    else
        ;;; In User file or status line of options buffer.
        if vedcurrent = complete_buffer then ved_q() endif;
        vedputmessage(waitmessage);
        ;;; Try to get possible completions of name left of cursor
        lvars string,
            name = lastfilename(),
            list = sysfilecomplete(name);
        if list == [] then
            vedscreenbell()
        elseif listlength(list) == 1 then
            insert_completion(name, front(list))
        else
            ;;; First try common completions, then status line menu, and
            ;;; then other options
            if (common_completion(name, list) ->>string) /= nullstring then
                ;;; common substring found. Insert it.
                vedinsertstring(string);
                ;;; Now start again
                ;;; chain(vedfilecomplete);
                if listlength(list) < 4 then chain(vedfilecomplete) endif;
            elseif try_status_line(name, list) then ;;; success
            else
                ;;; Too big for status line, so try separate file menu
                ;;; Record globals for get_selection
                ;;; deal problem about vedstatusbuffer being truncated
                if vedonstatus then
                    if vvedbuffersize > vedstatusbufferlimit then
                        vedstatusbufferlimit -> last_status_limit;
                        vvedbuffersize + 30 -> vedstatusbufferlimit
                    endif;
                endif;
                vedline -> last_line;
                vedcolumn -> last_column;
                vedpathname -> last_file;
                vedonstatus -> last_status;
                name -> last_name;
                clearproperty(last_list);
                list -> last_list(vedcurrentfile);   ;;; associated with the file
                setup_options_file(list)
            endif
        endif
    endif;
    vedputmessage(nullstring)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan 14 2005
        Installed some minor changes from bham version
--- Robert Duncan, Jun  4 1996
        Changed to use a pseudo-buffer for completion options, because the
        temporary file name used previously is invalid under Windows
--- John Gibson, Jul 27 1995
        Changed to use vededit
--- Robert John Duncan, May 24 1995
        Minor fix from Aaron to position the cursor before the refresh in
        setup_options_file
--- Robert John Duncan, May 19 1995
        New version of lastfilename which excludes funny characters and
        quotes from filenames and interprets a space immediately before
        the cursor as the nullstring, i.e. matching all files.
        Fixed setup_options_file not to try inserting `\r` into a static
        file.
--- Aaron Sloman Jan 15 1994
    Made it NOT include characters immediately to right of where
        cursor was originally
    Introduced vedcomplete_exclude to control which file names should
        be excluded.
    Updated the help file to document this.

--- Aaron Sloman Jan 5 1994
    Made it insert common completions BEFORE trying the status line
        options.
    When there are too many options to fit on the status line, and
        few enough to be represented by printing characters, then indicate
        options with <char>) for quick selection via <char>
    Altered lastfilename() to cope when the cursor is not immediately to
        right of text.
    Updated HELP * VEDFILECOMPLETE

--- John Gibson, Nov 17 1992
        Removed key mapping (unnecessary, since the name is mapped to the
        key by default). Changed to use vedstatusheaderlen.
--- Adrian Howard, Nov 11 1992
        Added optional arg to sysfileok in insert_completion, otherwise it
        would give wrong error if no old file.
--- Aaron Sloman, Nov  4 1990
    Made insert_completion put "/" after directory if uniqe
--- Aaron Sloman, Oct 31 1990
    Made it display wait message
    Introduced vedcomplete_display, to control whether full path name is
    displayed, and altered instructions in completions buffer.
--- Aaron Sloman, Oct 30 1990
    Made it deal properly with garbage collected files, and files no longer
    in VED.
--- Aaron Sloman, Oct 29 1990
    Made it not replace environment variables. This involved changing
        insert_completion
    Altered common_completion so as not to get confused by substrings
        including environment variables
--- Aaron Sloman, Oct 28 1990
    Un dlocalised vedbreak and vedindentstep. Made vedstatic false to
    reduce import of typing errors in options file.
 */
