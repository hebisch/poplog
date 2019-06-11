/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_ls.p
 > Purpose:         VED interface to Unix "ls" command
 > Author:          Aaron Sloman, December 1983 (see revisions)
 > Documentation:   HELP * LS
 > Related Files:   LIB * VED_DIR (VMS equivalent), LIB * VED_PURGE, LIB * VED_DEL
 */

section;

#_IF hd(sys_os_type) == "vms"

define global ved_ls();
    vederror('Please use <ENTER> DIR instead')
enddefine;

#_ELSE  /* Unix */

global vars
    vedlsflags = false,
    vedlsindentstep = 2,
    ;

lconstant ls_fileprops = "'Select.ls'";

define global vars vedlsdefaults();
    vedveddefaults();
    true -> is_vedfile_local(ident vedlsflags, ved_current_file);
    false ->> vedcompileable ->> vedwriteable ->> vedbreak -> vednotabs;
    8 -> vedindentstep;
enddefine;


define lconstant Ls(args, indent);
    lvars dir = false, flags = false, one_only = true, name, shell, fref;
    dlocal vveddump;

    define lconstant Wild(s);
        lvars c;
        for c in_string s do returnif (strmember(c, '*?[{')) (c) endfor;
        false
    enddefine;

    sys_parse_string(
        args,
        procedure(substr);
            if isstartstring('-', substr) then
                substr -> flags
            elseif dir then
                false -> one_only
            else
                substr -> dir;
                if Wild(dir) then false -> one_only endif
            endif
        endprocedure);

    dir or current_directory -> dir;
    dir <> '\s' -> name;                /* The space is important */
    if not(one_only) then false -> dir endif;

    unless (systranslate('SHELL') ->> shell) then
        vedputmessage('$SHELL not defined - using /bin/sh');
        '/bin/sh' -> shell
    endunless;

    define lconstant Activate(line) -> line;
        lvars len, c, d, data, attr = `\{6A}`;
        returnif(strmember(`\s`, line) or (datalength(line) ->> len) == 0);
        if (line(len) ->> c) == `:` then
            allbutlast(1, line) -> d;
            unless d(1) == `/` then current_directory dir_>< d -> d endunless;
            if sysisdirectory(d) then
                d -> dir;
                return
            endif
        endif;
        if dir then dir dir_>< line else line endif -> data;
        if c == `*` or c == `@` then
            ;;; Trim trailing * or @ inserted by -F option
            allbutlast(1, data) -> data
        elseif c == `/` then
            `\{4A}` -> attr
        endif;
        consvedstring
            (#| repeat indent times
                    `\s`;
                    if vedusewindows /== "x" then fi_|| attr endif
                    ;;; to make whole line selectable by ESC-h
                endrepeat;
                for c in_string line do c fi_|| attr endfor
            |#) -> line;
        {% indent + 1, '(SNbp)edit_or_ls ' <> data %}
            -> vedstring_data_prop(line)
    enddefine;

    define lconstant Readin(cons_p) -> text;
        cons_p(vedreadin(pipein(
                shell,
                [% sys_fname_name(shell), '-c', 'ls ' <> args %],
                false))) -> text;
        if islist(text) then
            ncmaplist(text, Activate)
        else
            ncmapdata(text, Activate)
        endif -> text
    enddefine;

    define lconstant Get_buff(_);
        Readin(consvector)
    enddefine;

    if indent > 0 then
        Readin(conslist) -> vveddump;
        veddo('y')
    else
        consref({[^name ^ls_fileprops] ^Get_buff}) -> fref;
        if vedpresent(name) then
            vededit(name);
            vedqget(vededit(% fref, vedlsdefaults %))
        else
            vededit(fref, vedlsdefaults)
        endif;
        if flags then flags -> vedlsflags endif
    endif
enddefine;


define global ved_ls();
    Ls(vedargument, 0)
enddefine;


define global ved_edit_or_ls();
    lvars flags = vedlsflags, indent = 0;
    if sysisdirectory(vedargument) then
        if vedfileprops == ls_fileprops then
            ;;; get indent - assume cursor is on the correct line
            skipchar(`\s`, 1, vedthisline()) - 1 + vedlsindentstep -> indent
        endif;
        Ls(if flags then flags <> '\s' <> vedargument else vedargument endif,
           indent)
    else
        ved_ved()
    endif
enddefine;

#_ENDIF

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Oct 21 1996
        Now invokes shell with just '-c' instead of '-ce', as the '-e'
        causes problems on some Sun machines.
--- John Gibson, Apr 18 1996
        Changed vedfileprops for ls to "'Select.ls'"
--- John Williams, Apr 16 1996
        Output is active in normal Ved too (users can select with ESC-h).
--- John Williams, Apr 15 1996
        In Xved, ordinary files are blue and directories green. Also, draws
        a box around the selected line, and no message on status line.
--- John Williams, Apr  3 1996
        Listings produced by ved_edit_or_ls now inserted into the current file.
        ved_edit_or_ls uses the flags given to the original call of ved_ls.
--- John Williams, Apr  1 1996
        Ignores flag arguments when getting a value for dir.
--- John Williams, Mar 27 1996
        Lines (of output) that are just a single file or directory name
        are now made into hotlinks to that file or directory (Xved only).
--- John Williams, Nov 17 1992
        Completely rewritten, using vedpipein, to match new version of
        LIB * VED_DIR
 */
