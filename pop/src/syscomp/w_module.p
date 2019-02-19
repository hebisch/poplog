/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/syscomp/w_module.p
 > Purpose:
 > Author:          John Gibson, Dec  2 1988 (see revisions)
 */

/* -------------------------------------------------------------------------

                READING W-MODULES FROM FILES/LIBRARIES

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'
#_INCLUDE 'wdefs.ph'

section $-Popas;

define read_counted_str_or_word(wdev, want_word);
    lvars len, want_word, wdev;
    sysread_check(wdev, w_buffer, 1);           ;;; name length
    f_subs(1, w_buffer) -> len;
    unless len == 0 then
        sysread_check(wdev, w_buffer, len)          ;;; name chars
    endunless;
    if want_word then
        subword(1, len, w_buffer)
    elseif len == 0 then
        nullstring
    else
        substring(1, len, w_buffer)
    endif
enddefine;

define get_module_head(wlib, modnum) -> modhead;
    lvars ldev, modnum, modhead = initv(WMH_VEC_LEN), wlib;
    wlib_device(wlib) -> ldev;
    ;;; get to module start on file
    sysseek(ldev, wlib_modtab(wlib, true)(modnum), 0);
    ;;; read module name as word
    read_counted_str_or_word(ldev, true) -> f_subv(WMH_NAME, modhead);
    ;;; module date
    read_next_int(ldev, true) -> f_subv(WMH_DATE, modhead)
enddefine;

define app_wlib_input(file_name, app_p);
    lvars modnum, wlib, file_name, procedure app_p;
    open_w_input(file_name, true, "wlib") -> wlib;
    ;;; read the module table
    for modnum to datalength(wlib_modtab(wlib, true)) do
        ;;; get to module start and read header info (name etc)
        app_p(get_module_head(wlib, modnum), wlib)
    endfor;
    sysclose(wlib_device(wlib))
enddefine;

define set_sect_gen_flag(sect, flag);
    lvars sect, flag, flags;
    while (f_subv(SECT_FLAGS, sect) ->> flags) &&=_0 flag do
        flags fi_|| flag fi_|| SECF_GENERATE -> f_subv(SECT_FLAGS, sect);
        quitunless(f_subv(SECT_SUPERSECT, sect) ->> sect);
        SECF_GENERATE -> flag
    endwhile
enddefine;


;;; --- READING A W-MODULE ---------------------------------------------

vars
    doing_wlib_extract      = false,
    wlibmod_needs_forcing   = false,
    id_export_idprops       = 0,
    id_export_quoted        = false,
    ;

lvars
    curr_sect,
    abort_id,
    out_mod_num,
    uninit_chain,
    uninit_last,
    procedure word_info_prop,
    ;


define lconstant ident_error(sect, word, mess);
    lvars sect, word, mess;
    printf(id_display_str(f_subv(SECT_PATHNAME, sect), word), mess,
                                            '%p IDENTIFIER %p\n');
    true -> idval(abort_id)
enddefine;

define lconstant add_incr_entry(sect, word, winit, wtype);
    lvars   sect, word, winit, wtype, type,
            info = f_subv(SECT_WORD_INFO, sect)(word),
            incrpair = f_subv(INFO_INIT, info),     ;;; already a pair
            init = back(incrpair);

    if ispair(init) then
        back(init) -> type;
        if isinteger(type) and isinteger(wtype)
        and type fi_&& WRINCRF_TYPE == wtype fi_&& WRINCRF_TYPE
        then
            ;;; list or procedure
            type fi_|| wtype -> back(init)
        elseif isclosure(type) and isclosure(wtype) then
            ;;; property
            if pdprops(type) == nullstring then
                pdprops(wtype) -> pdprops(type)
            endif
        else
            ident_error(sect, word, 'INCREMENTAL DECLARATION CLASH')
        endif
    else
        conspair([], if isclosure(wtype) then copy(wtype) else wtype endif)
                        ->> init -> back(incrpair)
    endif;
    unless winit == [] or winit == nullstring or out_mod_num then
        winit :: fast_front(init) -> fast_front(init)
    endunless
enddefine;

define lconstant ident_def(word, wtype, widprops, winit) -> info;
    lvars   info, type, ntype, word, wtype, widprops, winit, init,
            incrpair = false;

    word_info_prop(word) -> info;
    f_subv(INFO_TYPE, info) -> type;

    ;;; check declaration consistency if already declared
    if type &&/=_0 IDT_DECLARED then
        if f_subv(INFO_IDPROPS, info) /== widprops
        and wtype &&/=_0 IDT_DECLARED then
            ident_error(curr_sect, word, 'DECLARATION CLASH')
        endif
    else
        ;;; new declaration (or strong use without declaration)
        widprops -> f_subv(INFO_IDPROPS, info);
        ;;; if not initialised, add to uninit chain (a previous strong use
        ;;; without declaration will have set IDT_STRONG_REF)
        if type &&=_0 IDT_STRONG_REF and winit == nullstring then
            info ->> if uninit_chain then
                        f_subv(INFO_NEXT_INFO, uninit_last)
                     else
                        uninit_chain
                     endif
                 -> uninit_last
        endif;
        if widprops &&/=_0 id_export_idprops then
            wtype fi_|| IDT_GEN_IN_DICT -> wtype
        endif
    endif;

    if wtype &&/=_0 IDT_GEN_IN_DICT then
        set_sect_gen_flag(curr_sect, SECF_HAS_DICT_IDS)
    endif;

    wtype fi_|| type ->> ntype -> f_subv(INFO_TYPE, info);

    if ntype &&/=_0 IDT_INCREMENTAL then
        f_subv(INFO_INIT, info) -> incrpair;
        if (not(ispair(incrpair)) and incrpair)
        or (winit /== nullstring and wtype &&=_0 IDT_INCREMENTAL)
        then
            ident_error(curr_sect, word, 'ORDINARY INITIALISATION FOR INCREMENTAL');
        endif;
        unless ispair(incrpair) then
            conspair(false, false) ->> incrpair -> f_subv(INFO_INIT, info)
        endunless
    endif;

    returnif(winit == nullstring);

    if doing_wlib_extract and widprops &&=_0 M_ID_CONSTANT then
        wtype fi_|| IDT_OPTIONAL_INIT -> wtype
    endif;
    if incrpair then fast_front(incrpair) else f_subv(INFO_INIT, info) endif
                                                    -> init;
    if init then
        ;;; already initialised
        if (wtype fi_||/& type) &&=_0 IDT_OPTIONAL_INIT then
            ident_error(curr_sect, word, 'MULTIPLE INITIALISATION');
        else
            ;;; one optional, other not (only for winit with poplink)
            if type &&/=_0 IDT_OPTIONAL_INIT then
                ;;; existing optional -- change init to new one
                winit -> init;
                type fi_&&~~ IDT_OPTIONAL_INIT -> type
            else
                wtype fi_&&~~ IDT_OPTIONAL_INIT -> wtype
            endif
        endif
    else
        if out_mod_num then out_mod_num else winit endif -> init;

        ;;; wlibmod_needs_forcing is set true when including a library module
        if wlibmod_needs_forcing then
            if incrpair then
                ;;; incremental values are always defined in the obj module
                ;;; and will be referenced from an identifier
                false -> wlibmod_needs_forcing
            elseif wtype &&/=_0 IDT_OBJMOD_INIT then
                ;;; winit is actually defined in the obj module, so will
                ;;; force it to be included
                if type &&/=_0 IDT_VALUE_USED or ntype &&/=_0 IDT_GEN_IDENT
                or (type &&=_0 IDT_NON_POP
                    and (f_subv(SECT_FLAGS, curr_sect) &&/=_0 SECF_RUN_TIME
                         or ntype &&/=_0 IDT_GEN_IN_DICT))
                then
                    ;;; either the value has a ref to it from a previous
                    ;;; module, or it will be referenced from an identifier
                    false -> wlibmod_needs_forcing
                else
                    ;;; use winit to force the module to be included
                    winit -> wlibmod_needs_forcing
                endif
            endif
        endif
    endif;

    init -> if incrpair then fast_front(incrpair)
            else f_subv(INFO_INIT, info)
            endif;
    wtype fi_|| type -> f_subv(INFO_TYPE, info)
enddefine;

define add_ident_def(word, curr_sect, wtype, widprops, winit, abort_id)
                                                -> (uninit_last, uninit_chain);
    lvars   word, wtype, widprops, winit;
    dlocal  curr_sect, abort_id, out_mod_num = false, uninit_chain = false,
            uninit_last, word_info_prop = f_subv(SECT_WORD_INFO, curr_sect),
        ;

    ident_def(word, wtype, widprops, winit) -> ;
    if uninit_chain then
        false -> f_subv(INFO_NEXT_INFO, uninit_last)
    endif
enddefine;

    ;;; read in (and optionally write out) a .w module from a wfile/wlib
define read_wmodule(file, modhead, out_dev, out_mod_num, dbase, abort_id)
                                                -> (uninit_last, uninit_chain);
    lvars   n, type, word, info, winit, glab, wdev, flags, list, fname,
            out_dev, file, modhead, info2, dbase, count, propdata = false,
            wfile_version, item, pair, bufflen = datalength(w_buffer), props,
            topsect,
            procedure (topsect_word_info, sect_prop, gassign_prop,
            weak_depend_prop)
        ;

    dlocal  curr_sect, abort_id, out_mod_num, uninit_chain = false,
            uninit_last, word_info_prop,
        ;

    define :inline lconstant READ(n);
        sysread_check(wdev, w_buffer, n);
        if out_dev then f_syswrite(out_dev, 1, w_buffer, n) endif;
    enddefine;


    if isvector(file) then
        ;;; w-library
        wlib_device(file)
    elseif isdevice(file) then
        ;;; open w-file
        file
    else
        ;;; unopened w-file
        false -> modhead;
        open_w_input(file, true, "w")
    endif -> wdev;
    device_full_name(wdev) -> fname;

    dbase(DBASE_SECT_PROP) -> sect_prop;
    dbase(DBASE_WEAK_DEPEND_PROP) -> weak_depend_prop;

    sect_prop(nullword) -> topsect;
    f_subv(SECT_WORD_INFO, topsect) -> topsect_word_info;

    ;;; read format version num
    read_next_int(wdev, true) -> wfile_version;
    if out_dev then write_next_int(wfile_version, out_dev) endif;

    ;;; read first rec length -- thereafter, succeeding rec lengths are
    ;;; are read with preceding record (means only 1 sysread per record).
    1 -> n;
    READ(2);

    repeat
        ;;; get record length + 2 for next rec length
        (f_subs(n fi_+ 1,w_buffer) fi_<< 8) fi_+ f_subs(n,w_buffer) -> n;
        if n == 16:FFFF then
            ;;; overflows 2-byte length -- actual length is in next 4 bytes
            READ(4); subscr_nbyte(1, w_buffer, 4) -> n
        endif;
        n fi_+ 2 -> n;          ;;; include next rec length
        if n fi_> bufflen then
            ;;; expand buffer
            0 -> w_buffer;
            inits(1 << integer_length(n) ->> bufflen) -> w_buffer
        endif;

        ;;; read next record
        READ(n);

        ;;; switch on record type
        go_on f_subs(WR_TYPE, w_buffer) to
            WORD            SECT            IDENT           WORD_ASSIGN
            IDENT_ASSIGN    WORDID_ASSIGN   IMPORT          EXPORT
            WEAK_DEPEND     LINK_INCR       EXLOAD_STRINGS  UNIQUE_ASSIGN
            WLIB_STRINGS
        else END_OR_ERROR;

        WORD:
            ;;; quoted word
            wbuf_extract_strword(WRWORD_NAME, true) -> word ;;; the word
                                                   -> n;    ;;; next reclen
            topsect_word_info(word) -> info;
            f_subv(INFO_TYPE, info) fi_|| #_< IDT_GEN_WORD || IDT_GLOB_WORD >_#
                                                        -> type;
            if id_export_quoted then
                type fi_|| IDT_GEN_IN_DICT -> type;
                if type &&/=_0 IDT_DECLARED then
                    set_sect_gen_flag(topsect, SECF_HAS_DICT_IDS)
                endif
            endif;
            type -> f_subv(INFO_TYPE, info);
            nextloop;

        SECT:
            ;;; section header
            f_subs(WRSECT_FLAGS, w_buffer) -> flags;
            wbuf_extract_strword(WRSECT_PATHNAME, true) -> word ;;; sect name
                                                       -> n;
            sect_prop(word) -> curr_sect;
            f_subv(SECT_FLAGS, curr_sect) fi_|| flags
                                        -> f_subv(SECT_FLAGS, curr_sect);
            f_subv(SECT_WORD_INFO, curr_sect) -> word_info_prop;
            nextloop;

        IDENT:
            ;;; identifier within section
            ;;; get word within sect and init string
            wbuf_extract_strword(WRI_REST, true) -> word,   ;;; the word
            wbuf_extract_strword((), false) -> winit        ;;; init string
                                           -> n;            ;;; next reclen

            ident_def(
                    word,
                    subscr_nbyte(WRI_IDTYPE, w_buffer, 3),  ;;; type flags
                    subscr_nbyte(WRI_IDPROPS, w_buffer, 3), ;;; idprops
                    winit) -> info;

            nextloop;


        WORD_ASSIGN:
        IDENT_ASSIGN:
        WORDID_ASSIGN:
            ;;; assignment of global label to last
            ;;; quoted word/identifier/word identifier
            wbuf_extract_strword(WRASS_LABEL, false) -> (n, glab);
            f_subs(WR_TYPE, w_buffer) -> type;
            if     type == WRTYPE_WORD_ASSIGN then
                word, DBASE_GENERAL_ASSIGN_PROP
            elseif type == WRTYPE_IDENT_ASSIGN then
                info, DBASE_IDENT_ASSIGN_PROP
            else
                info, DBASE_WORDID_ASSIGN_PROP
            endif.dbase -> (type, gassign_prop);
            glab :: gassign_prop(type) -> gassign_prop(type);
            nextloop;


        IMPORT:
        EXPORT:
            ;;;  import/export of current section
            wbuf_extract_strword(WRIMEX_NAME, true) -> word ;;; the word
                                                   -> n;    ;;; next reclen
            if f_subs(WR_TYPE, w_buffer) == WRTYPE_IMPORT then SECT_IMPORTS
            else SECT_EXPORTS
            endif -> type;
            unless fast_lmember(word, f_subv(type, curr_sect) ->> list) then
                word :: list -> f_subv(type, curr_sect)
            endunless;
            nextloop;

            ;;;  export of current section
            wbuf_extract_strword(WRIMEX_NAME, true) -> word ;;; word
                                                   -> n;    ;;; next reclen
            word :: f_subv(SECT_EXPORTS, curr_sect)
                                    -> f_subv(SECT_EXPORTS, curr_sect);
            nextloop;


        WEAK_DEPEND:
            ;;; identifier last identifier's strength depends on
            wbuf_extract_strword(WRWKDP_REST, true) -> glab,    ;;; section path
            wbuf_extract_strword((), true) -> word          ;;; id name
                                          -> n;             ;;; next reclen
            ;;; mark last as dependent
            f_subv(INFO_TYPE, info) fi_|| IDT_WEAK_DEPENDS
                                            -> f_subv(INFO_TYPE, info);
            ;;; add this id's info structure onto the depend list of the last
            f_subv(SECT_WORD_INFO, sect_prop(glab))(word) -> info2;
            unless lmember(info2, weak_depend_prop(info) ->> list) then
                info2 :: list -> weak_depend_prop(info)
            endunless;
            nextloop;


        LINK_INCR:
            ;;; entries for incrementally-linked property, list or procedure
            f_subs(WRINCR_FLAGS, w_buffer) -> flags;        ;;; flags
            f_subs(WRINCR_NSTRINGS, w_buffer) -> count;     ;;; nstrings
            wbuf_extract_strword(WRINCR_REST, true) -> glab, ;;; section path
            wbuf_extract_strword((), true) -> word;         ;;; id name

            if flags fi_&& WRINCRF_TYPE == WRINCRT_PROPERTY then
                ;;; property -- first 2 strings are init and pdprops
                wbuf_extract_strwords((), 2, false) -> (winit, props, n);
                count-2 -> count;
                unless propdata and datalength(propdata) == count then
                    consclosure(identfn, dupnum(0,count), count) -> propdata
                endunless;
                props -> pdprops(propdata);
                ;;; extract these as words to save creating garbage
                wbuf_extract_strwords(n, count, true)
                                        -> (explode(propdata), n);
                propdata
            else
                ;;; list or procedure
                wbuf_extract_strwords((), count, false) -> n;
                conslist(count) -> winit;
                if winit /== [] then
                    ;;; put integer precedence on front
                    subscr_nbyte(WRINCR_PREC, w_buffer, 2) -> count;
                    ;;; sign extend to 16 bits
                    lconstant MASK = -1 << 15;
                    if count &&/=_0 MASK then count fi_|| MASK -> count endif;
                    count :: winit -> winit
                endif;
                flags
            endif -> info2;
            add_incr_entry(sect_prop(glab), word, winit, info2);
            nextloop;


        WLIB_STRINGS:
                ;;; name strings of w-libraries used by this module
        EXLOAD_STRINGS:
                ;;; obj file & library arg strings from external load

            f_subs(WRSTRS_NSTRINGS, w_buffer) -> count;     ;;; nstrings
            wbuf_extract_strwords(WRSTRS_REST, count, true) -> n;
            ;;; apply the relevant procedure to deal with them
            dbase(if f_subs(WR_TYPE, w_buffer) == WRTYPE_WLIB_STRINGS then
                      DBASE_WLIB_STR_ACTION
                  else
                      DBASE_EXLOAD_STR_ACTION
                  endif) (count);
            nextloop;


        UNIQUE_ASSIGN:
            ;;; assignment of global label to unique structure generated
            ;;; by poplink
            f_subs(WRUNIQ_NUMBER, w_buffer) -> type;
            wbuf_extract_strword(WRUNIQ_LABEL, false) -> (n, glab);
            fast_for item, pair in_property poplink_unique_struct do
                if fast_back(pair) == type then
                    dbase(DBASE_GENERAL_ASSIGN_PROP) -> gassign_prop;
                    glab :: gassign_prop(item) -> gassign_prop(item);
                    nextloop(2)
                endif;
            endfor;
            mishap(type, 1, 'SYSTEM ERROR (unknown unique struct number)');
            nextloop;


        END_OR_ERROR:
            if f_subs(WR_TYPE, w_buffer) /== WRTYPE_END then
                mishap(fname, 1, 'UNKNOWN RECORD TYPE ON W-MODULE')
            endif;

            ;;; end of file record
            quitloop

    endrepeat;

    if uninit_chain then
        false -> f_subv(INFO_NEXT_INFO, uninit_last)
    endif;
    unless modhead then
        ;;; check end correct on .w file
        unless sysread(wdev, w_buffer, 1) == 0 then
            mishap(fname, 1, 'UNEXPECTED DATA AT END OF W-FILE')
        endunless;
        sysclose(wdev)
    endunless
enddefine;


;;; -----------------------------------------------------------------------

define split_pathname(pathname);
    lvars n, m, pathname;
    3 -> n;     ;;; must start with $-
    while issubstring('$-', n, pathname) ->> m do m fi_+ 2 -> n endwhile;
    if n == 3 then
        nullword
    else
        allbutlast(datalength(pathname) fi_- n fi_+ 3, pathname)
    endif;
    allbutfirst(n fi_- 1, pathname)         ;;; name
enddefine;

define init_word_info(word, pathname) -> info;
    lvars word, pathname, info = initv(INFO_VEC_LEN);
    0        ->> f_subv(INFO_TYPE, info)
             ->  f_subv(INFO_IDPROPS, info);
    false    ->  f_subv(INFO_INIT, info);
    pathname ->  f_subv(INFO_PATHNAME, info);
    word     ->  f_subv(INFO_WORD, info)
enddefine;

define cons_dbase() -> dbase;
    lvars dbase;

    ;;; construct a new section
    define lconstant cons_sect(pathname, sect_prop) -> sect;
        lvars sect, pathname, name, super_sect, sect_prop;

        define lconstant cons_info(word, info_prop);
            lvars word, info_prop;
            ;;; first occurrence of word in sect
            init_word_info(word, pathname) ->> info_prop(word)
        enddefine;

        returnif(pathname == "$-") (sect_prop(nullword) -> sect);

        initv(SECT_VEC_LEN) ->> sect -> sect_prop(pathname);
        if pathname == nullword then
            0, false, false
        else
            ;;; get supersect
            sect_prop(split_pathname(pathname) -> name) -> super_sect;
            ;;; add as subsect of supersect
            sect :: f_subv(SECT_SUBSECTS, super_sect)
                                        -> f_subv(SECT_SUBSECTS, super_sect);
            f_subv(SECT_LEVEL, super_sect) fi_+ 1,  ;;; level
            super_sect,                             ;;; supersect
            name                                    ;;; name
        endif    -> (f_subv(SECT_LEVEL, sect),
                     f_subv(SECT_SUPERSECT, sect),
                     f_subv(SECT_NAME, sect));

        0        ->  f_subv(SECT_FLAGS, sect);
        pathname ->  f_subv(SECT_PATHNAME, sect);
        []       ->> f_subv(SECT_SUBSECTS, sect),
                 ->> f_subv(SECT_IMPORTS, sect),
                 ->  f_subv(SECT_EXPORTS, sect);
        newactproperty([], if pathname==nullword then 512 else 64 endif,
                            false, true, cons_info)
                 ->  f_subv(SECT_WORD_INFO, sect)
    enddefine;

    initv(DBASE_VEC_LEN) -> dbase;
    ;;; property for mapping section names to section structures
    newactproperty([], 8, false, true, cons_sect) -> dbase(DBASE_SECT_PROP);
    ;;; property for mapping identifier info structures to info structs of
    ;;; identifiers their strength depends on
    newproperty([], 32, [], true) -> dbase(DBASE_WEAK_DEPEND_PROP);
    ;;; properties for recording assignments of global labels to
    ;;; ... words and unique structures
    newproperty([], 8, [], true) -> dbase(DBASE_GENERAL_ASSIGN_PROP);
    ;;; ... identifiers (keyed on info struct)
    newproperty([], 8, [], true) -> dbase(DBASE_IDENT_ASSIGN_PROP);
    ;;; ... word identifiers (keyed on info struct)
    newproperty([], 8, [], true) -> dbase(DBASE_WORDID_ASSIGN_PROP);

    erasenum ->> dbase(DBASE_EXLOAD_STR_ACTION) -> dbase(DBASE_WLIB_STR_ACTION)
enddefine;

define get_module_assoc(list);
    lvars list;
    maplist(list,   procedure(f);
                        lvars f;
                        consword(sys_fname_name(f)), f
                    endprocedure)
enddefine;

define check_module_assoc(list, abort_id);
    lvars list, name, abort_id;
    until list == [] do
        if (dest(dest(list)) -> list -> ->> name) then
            printf(name, 'MODULE %p NOT FOUND IN LIBRARY\n');
            true -> idval(abort_id)
        endif
    enduntil
enddefine;


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 27 1993
        Added EXLOAD_STRINGS record type, etc
--- John Gibson, May 10 1993
        Add init for DBASE_EXLOAD_STR_ACTION
--- John Gibson, Sep 29 1992
        Added support for incremental idents
--- John Gibson, Apr  4 1992
        Mods to -ident_def-
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 17 1989
        Veresion 13.66
--- John Gibson, May 17 1989
        popc/poplink version  13.6403 changes
--- John Gibson, Jan 29 1989
        Changes for popc and poplink
--- John Gibson, Dec  5 1988
        This file and w_util.p replace old wcommon.p
 */
