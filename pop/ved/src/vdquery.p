/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/ved/src/vdquery.p
 > Purpose:         New <ENTER> ?? facility
 > Author:          John Williams, Jan  3 1990 (see revisions)
 > Documentation:
 > Related Files:   C.all/src/sys_search_doc_index.p
                    C.all/src/sys_read_lines.p
                    C.all/lib/lib/mkrefindex.p
 */

section;

#_INCLUDE 'vddeclare.ph'
#_INCLUDE '../../lib/include/doc_index.ph'

constant procedure
    (vedinsertstring, vednextitem, $-Sys$-Ved$-Trimwhite)
    ;

vars veddocsubsystem;

vars
    ved_??_list             = [vedreflist],
    ved_??_max_lines        = 12,
    ved_??_status_display   = true,
    ved_??_tmpfile          = false,
    ;


define vars ved_??_defaults();
    vedhelpdefaults()
enddefine;


define lconstant Edit_??_tmpfile();
    unless ved_??_tmpfile do
        systmpfile(current_directory, 'info', '') -> ved_??_tmpfile
    endunless;
    vededit(ved_??_tmpfile, ved_??_defaults)
enddefine;


define lconstant Extract_info_lines(info, brief) -> more -> lines;
    lvars brief info lines more _a _b _max;
    Info_hdr_start(info) -> _a;
    Info_hdr_end(info) -> _b;
    false -> more;
    unless brief do
        _b + ved_??_max_lines -> _max;
        Info_txt_end(info) -> _b;
        if _b > _max then
            _max -> _b;
            true -> more
        endif
    endunless;
    conslist(sys_read_lines(Info_file(info), _a, _b, true)) -> lines
enddefine;


define lconstant Info_title(info, props);
    lvars info props fname;

    define lconstant Hasuppercode() with_nargs 1;
        appdata(procedure(c);
                    lvars c;
                    if isuppercode(c) then
                        true, exitfrom(Hasuppercode)
                    endif
                endprocedure);
        false
    enddefine;

    sys_fname_name(Info_file(info)) -> fname;
    unless Hasuppercode(fname) do
        lowertoupper(fname) -> fname
    endunless;
    lowertoupper(word_string(props)) <> '\Ss*\Ss' <> fname
enddefine;


define lconstant Display_dir_infos(/* args */) with_nargs 2;
    vedendfile();
    vednextline();
    procedure(dir_infos, brief);
        lvars brief dir_info dir_infos info line lines more props;
        dlocal  vedcolumn vedline vvedlinesize,
                vedleftmargin=0, vedlinemax = 78;
        fast_for dir_info in dir_infos do
            destpair(dir_info) -> dir_info -> props;
            fast_for info in dir_info do
                if props then
                    vedinsertstring(Info_title(info, props));
                    vednextline()
                endif;
                Extract_info_lines(info, brief) -> more -> lines;
                fast_for line in lines do
                    vedinsertstring(line);
                    vednextline()
                endfast_for;
                if more then
                    vedinsertstring('\t-Etc-\n')
                endif;
                vedlinebelow()
            endfast_for
        endfast_for
    endprocedure(/* args */)
enddefine;


define lconstant Try_status_display(info);
    lvars i info line lines;
    dlocal ved_??_max_lines = 1;
    Extract_info_lines(info, false) -> -> lines;
    fast_front(lines) -> line;

    /* Strip trailing [...] */
    if (locchar(`[`, 2, line) ->> i) then
        skipchar_back(`\s`, i - 1, line) -> i;
        substring(1, i, line) -> line
    endif;

    /* Use second line if line = name */
    if line = Info_name(info)
    and fast_back(lines) /== [] then
        fast_front(fast_back(lines)) -> line;
        $-Sys$-Ved$-Trimwhite(line, datalength(line)) -> line ->
    endif;

    /* See if line will fit on status line */
    if vedscreenwidth > (STATUS_HEADER_LEN
                            + datalength(vedcommand)
                            + datalength(line)
                            + 3)
    then
        consdstring(vedscreenstatus_-|_mark, `\s`, 2) sys_>< line -> line;
        vedrestorescreen();
        vedsetstatus(line, false, true);
        vedsetcursor();
        chainfrom(vedprocesschar, vedprocesschar)
    endif
enddefine;


define ved_??_search_doc_index(name, dirs);
    lvars dir dirs dir_info name props;
    [% for dir in dirs do
        if isword(dir) then
            dl(ved_??_search_doc_index(name, valof(dir)))
        elseif isident(dir) then
            dl(ved_??_search_doc_index(name, idval(dir)))
        elseif isprocedure(dir) then
            nextloop
        else
            false -> props;
            if ispair(dir) then
                if ispair(fast_back(dir)) then
                    fast_front(fast_back(dir)) -> props
                endif;
                fast_front(dir) -> dir
            endif;
            conslist(sys_search_doc_index(name, dir, 2:00)) -> dir_info;
            unless dir_info == [] do
                conspair(props, dir_info)
            endunless
        endif
    endfor %]
enddefine;


define ved_try_do_??(name, brief);
    lvars brief dir_infos name;
    ved_??_search_doc_index(name, ved_??_list) -> dir_infos;
    if dir_infos == [] then
        false
    else
        if vedinvedprocess then
            if brief
            and ved_??_status_display
            and back(dir_infos) == []
            and listlength(front(dir_infos)) == 2 then
                Try_status_display(front(back(front(dir_infos))))
            endif;
            Edit_??_tmpfile();
            Display_dir_infos(dir_infos, brief)
        else
            Display_dir_infos(% dir_infos, brief %)
                :: ved_char_in_stream -> ved_char_in_stream;
            Edit_??_tmpfile();
        endif;
        true
    endif
enddefine;


define ved_do_??(brief);
    lvars brief word;
    dlocal vedargument;
    if vedargument = nullstring then
        vednextitem() sys_>< nullstring -> vedargument
    endif;
    unless ved_try_do_??(vedargument, brief) do
        consword(vedargument) -> word;
        if isdefined(word) and not(isinheap(word)) then
            vedputmessage('\{b}undocumented system identifier')
        else
            vederror('\{b}no information available')
        endif
    endunless
enddefine;

define vars ved_?();
    lvars   list = subsystem_searchlist("ved_??_name", veddocsubsystem);
    dlocal  ved_??_list;
    unless list == [] then list -> ved_??_list endunless;
    ved_do_??(true);
enddefine;

define vars ved_??();
    lvars   list = subsystem_searchlist("ved_??_name", veddocsubsystem);
    dlocal  ved_??_list;
    unless list == [] then list -> ved_??_list endunless;
    ved_do_??(false);
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan  5 1996
        Changed third argument to sys_search_doc_index from false to 2:00
--- John Gibson, Mar  7 1994
        Uses vededit
--- John Williams, Jun  1 1993
        Now calls sys_read_lines with 4'th argument true (i.e. VED mode)
--- John Williams, May 28 1993
        Info_title now only capitalises all lower case file names
--- Adrian Howard, Feb 24 1993
        Display_dir_infos now works if margins have been altered by the
        user
--- John Gibson, Jan 11 1993
        Replaced ved_? and ved_?? with subsystem versions
--- John Williams, Jul 11 1990
        -ved_??_search_doc_index- now accepts idents in -ved_??_list-
--- John Williams, Mar 28 1990
        -Try_status_display- is smarter about variables, datatypes, etc.
--- John Williams, Feb  1 1990
        Now -ved_do_??- ensures -vedargument- is a string.
 */
