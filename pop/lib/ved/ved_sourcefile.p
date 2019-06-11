/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:           C.all/lib/ved/ved_sourcefile.p
 > Purpose:        examine source of procedures in source and libraries
 > Author:         Chris Slymon, June 1983 (see revisions)
 > Documentation:  HELP * SOURCEFILE, * POPINDEX
 > Related Files:  LIB * POPINDEX, * POPWHERE
 */

section;

compile_mode :pop11 +strict;

define vars vedsrcfiledefaults();
    vedhelpdefaults();
    false -> vednotabs
enddefine;


define lconstant Get_index_file(string, num);
    ;;; string is a line from index file, obtained by popindex
    lvars l, i, file, name, dir;
    dlocal vedediting;

    ;;; extract file name and procedure name
    datalength(string) -> l;
    locchar_back(`\s`, l, string) -> i;
    sysfileok(substring(i + 1, l - i, string)) -> file;
    locchar(`\s`, 1, string) -> i;
    consword(substring(1, i - 1, string)) -> name;

    ;;; choose vedfileprops and read into Ved
    uppertolower(sys_fname(file, 3)) -> dir;
    if issubstring('lib', dir) or issubstring('auto', dir) then
        "lib"
    else
        "src"
    endif -> dir;
    false -> vedediting;
    vededit([^file ^dir], vedsrcfiledefaults);

    ;;; look for definition. If exact match fails, try inexact match!
    vedfindpdheader(name, true) or vedfindpdheader(name, false) -> l;
    if l then vedjumpto(l, 1) endif;
    if vvedbuffersize > vedstartwindow then
        vedline - 1 -> vedlineoffset
    endif;
    chain(if num == 1 then vedrefresh else vedrestorewindows endif)
enddefine;


define lconstant Xved_display_options(name, entry_list);
    dlvars entry_list;
    lvars fref;

    define lconstant Xget_index_file();
        vedqget(Get_index_file(% vedthisline(), vedline %), true)
    enddefine;

    define lconstant Get_buff(_);
        lvars entry, line;
        lconstant data = {1 ^(conspair(Xget_index_file, '(Sbp)'))};
        {% for entry in entry_list do
            consvedstring(#| appdata(entry, nonop fi_||(% `\{6A}` %)) |#)
                ->> line;
            data -> vedstring_data_prop(line);
        endfor %}
    enddefine;

    'sourcefile ' <> name -> name;
    consref({[%name, "'Select.sourcefile'"%] ^Get_buff
                                'Use mouse or ESC-h to select'}) -> fref;
    if vedpresent(name) then
        vededit(name);
        vedqget(vededit(% fref, vedhelpdefaults %))
    else
        vededit(fref, vedhelpdefaults)
    endif
enddefine;


define ved_sourcefile();
    lvars entry_list, entry_num, n, l, string;
    dlocal popnewline, vedargument, pop_pr_quotes = false;

    if vedargument = nullstring then
        vednextitem() sys_>< nullstring -> vedargument;
        vedcommand sys_>< vedspacestring sys_>< vedargument -> string;
        ;;; put enlarged command on status line, before current command
        true -> ved_on_status;
        unless vedline > 1 and string = vedbuffer(vedline - 1) then
        ;;; put a copy of translated command in command line buffer
            vedlineabove();
            string -> vedthisline();
            vednextline();
        endunless;
        false -> ved_on_status;
    endif;

    popindex(vedargument) -> entry_list;
    listlength(entry_list) -> l;
    if l == 0 then
        vedputmessage('No matches found in index');
    elseif l == 1 then
        ;;; ensure status line of current file gets set if needed
        ;;; only need this because of vedediting off in Get_index_file
        if vedediting and not(vedusewindows)
        and vedstartwindow < vedscreenlength
        and vedwindowlength > vedstartwindow
        then vedsetwindow()
        endif;
        Get_index_file(hd(entry_list), l)
    else
        if vedusewindows == "x" then
            chain(vedargument, entry_list, Xved_display_options)
        endif;
        pr('\n\nChoose one of the following:\n\n');
        for entry_num from 1 to l do
            pr(tab); pr(entry_num);
            pr(tab); pr(entry_list(entry_num));
            pr(newline);
            if entry_num > vedscreenlength - 5 then
                pr(l >< ' altogether ...'); quitloop()
            endif;
        endfor;
        true -> popnewline;
        repeat
            sys_clear_input(pop_charin_device);
            pr('\nType number <return> to access file');
            incharitem(charin)()-> entry_num;
            if entry_num = newline then
                false -> vedprintingdone; chain(vedrefresh)
            elseif entry_num.isinteger and entry_num > 0 and entry_num <= l
            then
                    quitloop();
            endif;
        endrepeat;
        if vedusewindows then
            vedrefresh()
        elseif vedediting and vedstartwindow < vedscreenlength then
            false -> vedediting;
            if vedwindowlength > vedstartwindow then vedsetwindow() endif;
            true-> vedediting;
            ;;;vedrefresh();
        endif;
        false -> vedprintingdone;
        Get_index_file(entry_list(entry_num), l);
    endif;
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Gibson, Sep 12 1996
        Made Xget_index_file use new boolean 2nd arg to vedqget to force
        a new window.
--- John Gibson, Apr 18 1996
        Changed vedfileprops to "'Select.sourcefile'"
--- John Williams, Apr 15 1996
        In Xved, creates a file of hotlinks when > 1 option.
--- Adrian Howard, Aug 26 1992
        Set -pop_pr_quotes- to -false- to stop the profusion of ugly quote marks.
--- John Williams, Jul 29 1992
        Now sets -vedfileprops- correctly (c.f. BR pop.4)
--- John Gibson, Oct 29 1991
        Added wved_raise_window(pop_charout_device) for XVed
--- John Gibson, Jun 11 1991
        Replaced popdevin with pop_charin_device
--- Jonathan Meyer, Jun  3 1991
        Changed vedusepw*mwindows to vedusewindows
--- Aaron Sloman, May 21 1990
    fixed to behave better after menu. Removed unnecessary section by
    making Get_index_file lconstant.

--- Aaron Sloman, Sep 27 1988
    If given no argument, now uses word to right of cursor, and inserts
    command in status buffer.
--- Aled Morris, Jan  6 1988
        added "vedfileprops" argument to call of -ved*getfile- to set file
        props to "src" - see bugreport "aledm@psune.uucp.16"
 */
