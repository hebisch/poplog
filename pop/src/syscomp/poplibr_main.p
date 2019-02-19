/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/src/syscomp/poplibr_main.p
 > Purpose:
 > Author:          John Gibson, Dec 24 1987 (see revisions)
 */

/* -------------------------------------------------------------------------

                POP SYSTEM LIBRARY UTILITY MAIN ROUTINES

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'
#_INCLUDE 'wdefs.ph'


section $-Popas;

lconstant stat_vec = writeable initv(2);


define wlibr_update(lib_in_file, creating, lib_out_file, delete_list,
                                                            replace_list);
    lvars   name, f, lib_in_file,
            lib_out_file, lib_out_dev, lib_out_modtab, out_mod_num,
            dbase, delete_list, replace_list, abort = false, modtab_start,
            indexvec_start, creating
        ;

    define lconstant write_counted_str_or_word(str, dev);
        lvars str, len, dev;
        lconstant buf = writeable inits(1);
        if isword(str) then fast_word_string(str) -> str endif;
        datalength(str) ->> len -> f_subs(1, buf);
        f_syswrite(dev, 1, buf, 1);
        f_syswrite(dev, 1, str, len)
    enddefine;

    define lconstant out_file_position();
        sysseek(lib_out_dev, 0, 1, true)    ;;; return output position
    enddefine;

    define lconstant write_wmodule(modhead, file);
        lvars modhead, file;
        out_mod_num fi_+ 1 -> out_mod_num;
        out_file_position();        ;;; return start position
        ;;; write module header info
        unless isvector(file) then
            ;;; w-file input -- get file date
            if sys_file_stat(file, stat_vec) then
                stat_vec(2)
            else
                ;;; just make 0 -- will mishap when trying to open the file
                0
            endif -> f_subv(WMH_DATE, modhead)
        endunless;
        ;;; write header -- name and date
        write_counted_str_or_word(f_subv(WMH_NAME, modhead), lib_out_dev);
        write_next_int(f_subv(WMH_DATE, modhead), lib_out_dev);
        ;;; copy module across
        read_wmodule(file, modhead, lib_out_dev, out_mod_num, dbase,
                                                            ident abort) -> ->
    enddefine;

    define lconstant do_in_mod(modhead, wlib);
        lvars d, r, modname = f_subv(WMH_NAME, modhead), modhead, wlib;
        if lmember(modname, delete_list) ->> d then
            false -> hd(d)      ;;; record deleted
        endif;
        if lmember(modname, replace_list) ->> r then
            false -> hd(r);     ;;; record included
            write_wmodule(modhead, hd(tl(r)))
        elseunless d then
            ;;; copy over existing module
            write_wmodule(modhead, wlib)
        endif
    enddefine;

    dlocal 0 %  false -> lib_out_dev,
                if isdevice(lib_out_dev) then
                    sysclose(lib_out_dev);
                    sysdelete(device_full_name(lib_out_dev)) -> ;
                endif %;

    ;;; open output library file
    syscreate(lib_out_file, 2, true) -> lib_out_dev;
    add_created_file(lib_out_file);

    lvars procedure write_nexti = write_next_int(%lib_out_dev%);
    ;;; write magic number and format version number, and then skip two ints
    ;;; for recording the start positions of the module table and the
    ;;; index vector
    write_nexti(WLIBR_MAGIC);
    write_nexti(CURRENT_WLIBR_VERSION);
    sysseek(lib_out_dev, WINT_BYTES*2, 1);
    0 -> out_mod_num;

    ;;; get assoc lists for delete and replace lists
    get_module_assoc(delete_list) -> delete_list;
    get_module_assoc(replace_list) -> replace_list;

    cons_dbase() -> dbase;

    ;;; cons vector of start positions in output file
    {%  if creating then
            if open_w_input(lib_in_file, false, "wlib") ->> lib_in_file then
                mishap(w_open_name(lib_in_file), 1,
                            'CREATING W-LIBRARY -- FILE ALREADY EXISTS')
            endif
        else
            ;;; copy over existing modules not replaced or deleted, and add
            ;;; actual replacements (new ones added after)
            app_wlib_input(lib_in_file, do_in_mod)
        endif;

        ;;; check all deletions done
        check_module_assoc(delete_list, ident abort);

        ;;; then add new ones
        until replace_list == [] do
            dest(dest(replace_list)) -> replace_list -> f -> name;
            if name then
                ;;; create module header structure
                lvars modhead = initv(WMH_VEC_LEN);
                name -> f_subv(WMH_NAME, modhead);
                write_wmodule(modhead, f)
            endif
        enduntil

    %} -> lib_out_modtab;

    if abort then
        mishap(0, 'ABORTED (see above)')
    endif;

    ;;; write out module table
    out_file_position() -> modtab_start;
    write_nexti(out_mod_num);           ;;; table length
    appdata(lib_out_modtab, write_nexti);

    ;;; --- build index -----------------------------------
    dlvars indexvec = {% repeat INDEX_LEN times [] endrepeat %};

    define lconstant do_sect(pathname, sect);
        lvars sect;
        dlvars pathname, pathstr = fast_word_string(pathname);

        define lconstant do_word(word, info);
            lvars info, word, cell, type = f_subv(INFO_TYPE, info);
            ;;; only index explicitly declared identifiers
            if type &&/=_0 IDT_DECLARED and type &&/=_0 IDT_EXPLICIT then
                path_index_cell(pathstr, word) -> cell;
                info :: indexvec(cell) -> indexvec(cell)
            endif
        enddefine;

        fast_appproperty(f_subv(SECT_WORD_INFO, sect), do_word)
    enddefine;

    define lconstant write_index_entry(info);
        lvars   n, ws, ps, info, len, init_mod,
                init = f_subv(INFO_INIT, info), buf = w_buffer;

        ;;; identprops
        f_subv(INFO_IDPROPS, info) -> subscr_nbyte(WLINDX_IDPROPS, buf, 3);

        ;;; other perm flags used for declarations
        f_subv(INFO_TYPE,info) && 16:FF -> f_subs(WLINDX_PRMFLAGS, buf);

        ;;; module number in which initialised
        if ispair(init) then front(init) else init endif -> init_mod;
        if isinteger(init_mod) then init_mod else 0 endif
                                -> subscr_nbyte(WLINDX_MOD_NUM, buf, 2);

        ;;; name -- word chars followed by pathname chars
        datalength(f_subv(INFO_WORD, info) ->> ws) -> n;
        datalength(f_subv(INFO_PATHNAME, info) ->> ps) -> len;
        n fi_+ len -> f_subs(WLINDX_NAME_LEN, buf);
        ws -> substring(WLINDX_NAME, n, buf);
        WLINDX_NAME fi_+ n -> n;
        ps -> substring(n, len, buf);
        n fi_+ len -> n;

        ;;; data for incremental ident if it is one
        if ispair(init) then
            back(back(init)) -> init;
            n+2;
            if isinteger(init) then
                ;;; list, procedure -- init is integer flags
                init, 0
            else
                ;;; property -- init is closure
                appdata(init, wbuf_insert_strword);
                WRINCRT_PROPERTY, datalength(init)
            endif -> (n, f_subs(n,buf), f_subs(n+1,buf))
        endif;

        n fi_- 1 -> subscr_nbyte(n, buf, 2);        ;;; entry length
        f_syswrite(lib_out_dev, n, buf, 2);
        f_syswrite(lib_out_dev, 1, buf, n fi_- 1)
    enddefine;

    ;;; build chains in indexvec
    fast_appproperty(dbase(DBASE_SECT_PROP), do_sect);

    ;;; write out index chains, remembering start position of each
    lvars n, list;
    fast_for n to datalength(indexvec) do
        if (indexvec(n) ->> list) == [] then
            0 -> indexvec(n)
        else
            out_file_position() -> indexvec(n);
            applist(list, write_index_entry);
            ;;; 0-length rec at end of chain
            f_syswrite(lib_out_dev, 1, '\(0)\(0)', 2)
        endif
    endfor;

    ;;; write out index vec -- remember where it starts
    out_file_position() -> indexvec_start;
    write_nexti(datalength(indexvec));
    appdata(indexvec, write_nexti);

    ;;; seek back to after magic number at beginning and write positions of
    ;;; module table and and index vec
    sysseek(lib_out_dev, MODTAB_PTR, 0);
    write_nexti(modtab_start);
    write_nexti(indexvec_start);

    sysclose(lib_out_dev);
    false -> lib_out_dev
enddefine;

define wlibr_extract(lib_in_file, extract_list) -> list;
    lvars lib_in_file, dbase, extract_list, abort, list;

    define lconstant do_in_mod(modhead, wlib);
        lvars d, f, modname = f_subv(WMH_NAME, modhead), modhead, wlib, wdev;
        if extract_list == [] then
            ;;; extract all, and return module names
            fast_word_string(modname) ->> f
        elseif lmember(modname, extract_list) ->> d then
            hd(tl(d)) -> f;
            false -> hd(d)      ;;; record extracted
        else
            return
        endif;

        ;;; create .w file
        syscreate(f, 1, true) -> wdev;
        write_next_int(WFILE_MAGIC, wdev);
        read_wmodule(wlib, modhead, wdev, false, dbase, ident abort) -> -> ;
        sysclose(wdev)
    enddefine;

    ;;; get assoc lists for extract list
    get_module_assoc(extract_list) -> extract_list;

    cons_dbase() -> dbase;

    ;;; process input library modules
    [% app_wlib_input(lib_in_file, do_in_mod) %] -> list;

    ;;; check all extractions done
    check_module_assoc(extract_list, ident abort)
enddefine;

define wlibr_list(lib_in_file, w_list, option);
    lvars   lib_in_file, w_list, option, dummy;

    ;;; print module names from input library file
    define lconstant do_in_mod(modhead, wlib);
        lvars   d, modname = f_subv(WMH_NAME, modhead), modhead, date,
                nam, extn, src, wlib;

        if w_list /== [] then
            returnunless(lmember(modname, w_list) ->> d);
            false -> hd(d)      ;;; record printed
        endif;
        f_subv(WMH_DATE, modhead) -> date;
        if option == "lcs" then
            ;;; list changed sources
            sys_fname_nam(modname) -> nam;
            fast_for extn in ['.p' '.s'] do
                nam <> extn -> src;
                nextunless(sys_file_stat(src, stat_vec));
                if date < stat_vec(2) then printf(src, '%p\n') endif;
                return
            endfor;
            ;;; put hyphens in this so it comes out as a single argument
            printf(modname, 'MODULE-%p-HAS-NO-SOURCE-FILE\n')
        else
            printf( modname,
                    if option == "ld" then
                        sys_convert_date(date, true), '%p\s\s%p\n'
                    else
                        '%p\n'
                    endif)
        endif
    enddefine;

    ;;; get assoc lists for w names
    get_module_assoc(w_list) -> w_list;

    ;;; process input library modules
    app_wlib_input(lib_in_file, do_in_mod);

    ;;; warnings for any missing
    check_module_assoc(w_list, ident dummy)
enddefine;


;;; --- MAIN PROCEDURE ---------------------------------------------------


define $-Pop$-Main();
    lvars c, option, tmp_name, w_lib, o_lib, w_files, o_files;
#_IF pop_debugging
    dlocal  pop_mishap_doing_lim = false;
#_ELSE
    dlocal  pop_mishap_doing_lim = 0, popgctrace = false, popsyscall = false;
    pop_null_device -> pop_charin_device;
#_ENDIF

    250000 -> popmemlim;

    dlocal
        pop_file_versions   = use_file_versions(),
        pop_arglist         = process_arglist(poparglist),
        % file_create_control(dlocal_context) %,
        ;

    define lconstant map_to_o =
        maplist(%new_fname_extn(%OBJ_EXTENSION%)%)
    enddefine;

    define dlocal prmishap(mess, list);
        lvars mess, list;
        sysprmishap('POPLIBR: ' <> mess, list)
    enddefine;

    define dlocal interrupt();
        false -> pop_exit_ok;
        chainfrom($-Pop$-Main, sysexit)
    enddefine;

    unless listlength(pop_arglist) >= 2 then
        mishap(pop_arglist, 1, 'INSUFFICIENT ARGUMENTS')
    endunless;

    dest(dest(pop_arglist)) -> (option, w_lib, w_files);
    if is_option(option) then
        consword(allbutfirst(1, option))
    else
        false
    endif -> c;

    get_wlib_name(w_lib) -> w_lib;
    new_fname_extn(w_lib, OLB_EXTENSION) -> o_lib;
    map_to_o(w_files) -> o_files;

    if fast_lmember(c, [l ld lcs]) then
        ;;; list module names etc
        wlibr_list(w_lib, w_files, c)

    elseif c == "x" then
        ;;; extract modules -- if w_files empty, returns list of module names
        wlibr_extract(w_lib, w_files) -> w_files;
        if w_files /== [] then map_to_o(w_files) -> o_files endif;
        os_library_command(c, o_lib, o_files)

    elseif fast_lmember(c, [c r d]) then
        ;;; update library (create/replace/delete)
        new_tmp_file(sys_fname_path(w_lib), 'plb', WLB_EXTENSION) -> tmp_name;
        wlibr_update(w_lib, c=="c", tmp_name,
                                    if c=="d" then w_files, []
                                    else [], w_files
                                    endif);
        os_library_command(c, o_lib, o_files);
        ;;; O/S library update successful
        dlocal pop_file_versions = 1;   ;;; ensure old version removed
        sys_file_move(tmp_name, w_lib)

    else
        mishap(option, 1, 'UNKNOWN OPTION')
    endif
enddefine;      /* $-Pop$-Main */


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 20 1992
        Fixed bug in extract all option
--- John Gibson, Aug 23 1991
        Made wlibr_list put hyphens in no sourcefile message
--- John Gibson, Sep 12 1990
        Added -lcs option to list names of source files with later date
        than corresponding w-module
--- John Gibson, Aug 31 1990
        Changes to cope with list args in poparglist from 13.83
--- John Gibson, May 18 1990
        Changed to write w-libs without nullword at beginning of module
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 24 1989
        Moved -os_library_command- to os_comms.p
--- John Gibson, Jul 17 1989
        Version 13.66
--- John Gibson, Nov 17 1988
        Moved running of unix "ar" etc to procedure -unix_archive- in
        asmout.p.
--- John Gibson, Oct  9 1988
        Changes to allow w-library modules to have dates
--- John Gibson, Jun 24 1988
        Added $-Pop$-Main.
--- John Gibson, Feb  9 1988
        Various revisions.
 */
