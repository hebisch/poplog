/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/syscomp/popc_main.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                    POPLOG OBJECT COMPILER MAIN ROUTINES

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'
#_INCLUDE 'wdefs.ph'
#_INCLUDE '$usepop/pop/lib/include/pop11_flags.ph'
#_INCLUDE '$usepop/pop/lib/include/vm_flags.ph'
#_INCLUDE '$usepop/pop/lib/include/popc_flags.ph'


section $-Popas;

lvars
    auto_declare = false,
    incremental_idents,
    exload_args_lists,
    declaration_libfile = false,

    procedure (
        Sect_idents, Sect_imports, Sect_exports,
        Id_output_init,
        Id_depend_list,
        Id_libfile,
        Link_assign,
        Token_dummy_id,
        )
    ;


;;;--- VM INTERFACE PROCEDURES --------------------------------------------

lvars unique_ID = 0;

define $- pas_assemble(/*   nargs,
                            props,
                                d_locals,       ;;; 1
                                l_locals,       ;;; 2
                                nlgoto_var,     ;;; 3
                                rtid_args,      ;;; 4
                                lblock_instr,   ;;; 5
                                pdr_nl_labels,  ;;; 6
                                dloc_rtid_list, ;;; 7
                                dlexpr_list,    ;;; 8
                                pdr_entry_type, ;;; 9
                                codelist        ;;; 10
                        */) -> p;

    lvars id, p, plab, genp_args, lvar_depth;

    define lconstant alloc_lnum(n, idlist) -> depth;
        lvars n, depth = n, id, idlist;
        fast_for id in idlist do
            if isident(id) then
                if nonpop_ident(id) and isident(identtype(id)) then
                    ;;; don't allow type-3 nonpop lvars
                    mishap(0, 'ILLEGAL NON-LOCAL USE OF NONPOP IDENTIFIER')
                endif;
                n fi_+ 1 -> n;
                n ->> depth -> idval(id)
            else
                ;;; id is a nested lblock list at end
                fi_max(alloc_lnum(n, id), depth) -> depth
            endif
        endfor
    enddefine;

    consvector( (),                 ;;; all args except nargs, props
                0,                  ;;; flags
                [],                 ;;; additional structs for table
                10+2) -> genp_args;

    ;;; compute lvar depth for interpreter (genp_args(2) is l_locals)
    alloc_lnum(0, genp_args(2)) -> lvar_depth;

    fast_for id in genp_args(1) do      ;;; d_locals
        nextunless(lstackmem_data(id));
        mishap(0, 'ILLEGAL NON-LOCAL USE OF lstackmem IDENTIFIER')
    endfor;

    ;;; Construct an outer procedure to attach genp_args to
    ;;; (vm_interpret can access this as caller(1)). unique_ID ensures
    ;;; that two such procedures are /= to one another.
    vm_interpret(% lvar_depth, unique_ID %) <> identfn -> p;
    /* props */ -> pdprops(p);
    /* nargs */ -> pdnargs(p);
    gen_procedure_key -> data_key(p);
    genp_args -> gen_procedure_args(p);
    unique_ID + 1 -> unique_ID
enddefine;

define ident_error(token, mess);
    lvars token, mess, name;
    dlocal cucharout = cucharerr;
    unless islist(file_has_errors) then [] -> file_has_errors endunless;
    unless lmember(token, file_has_errors) then
        id_display_str(false, token) -> name;
        printf( if pop_vm_compiling_list /== [] then
                    '\s\s\s\s%s (IN %p)\n', [% mess, name,
                            pdprops(hd(pop_vm_compiling_list)) %]
                else
                    '\s\s\s\s%s\n', [%mess, name%]
                endif);
        token :: file_has_errors -> file_has_errors
    endunless
enddefine;

define $- pas_declare_perm(id);
    lvars id, token, type, idprops;

    if isinteger(id) then
        ;;; extra info
        ((), id) -> (id, type);
        if type == `G` then
            ;;; global
            Id_type(id) fi_|| M_PERM_GLOBAL -> Id_type(id)
        elseif type == `N` then
            ;;; nonglobal
            Id_type(id) fi_&&~~ M_PERM_GLOBAL -> Id_type(id)
        elseif type == `P` then
            ;;; protected
            note_perm_ident(id, IDT_PROTECT fi_|| NPA_WEAK_OPTION) ->
        elseif type == `S` then
            ;;; prolog static
            (Id_type(id) fi_|| IDT_PROLOG_PRED) fi_&&~~ IDT_PROLOG_DYNAMIC
                                    -> Id_type(id)
        elseif type == `D` then
            ;;; prolog dynamic
            Id_type(id) fi_|| #_< IDT_PROLOG_PRED || IDT_PROLOG_DYNAMIC >_#
                                    -> Id_type(id)
        endif;
        return
    endif;

    ;;; else declaring identifier
    idprops_flags(id, false) -> idprops;
    get_ident_token(id) -> token;
    if (Id_type(id) ->> type) &&/=_0 IDT_DECLARED then
        ;;; already declared
        unless Id_idprops(id) == idprops then
            ident_error(token, 'REDECLARING STATUS OF PERM IDENTIFIER %p')
        endunless
    else
        ;;; previously undef
        idprops -> Id_idprops(id);
        if nonpop_ident(id) then type || IDT_NON_POP -> type endif;
        type || IDT_DECLARED -> type
    endif;
    unless auto_declare then type || IDT_EXPLICIT -> type endunless;
    type -> Id_type(id);
    if declaration_libfile then
        declaration_libfile -> Id_libfile(id)
    endif
enddefine;

define lconstant imp_exp(word, prop);
    lvars word, sect = current_section, l, procedure prop;
    unless nonpop_trans(word, true) /== word    ;;; ignore nonpops
    or fast_lmember(word, prop(sect) ->> l) then
        word :: l -> prop(sect)
    endunless;
    ;;; make sure sect appears in Sect_idents
    Sect_idents(sect) -> Sect_idents(sect)
enddefine;

define $- pas_section_import();
    imp_exp((), Sect_imports)
enddefine;

define $- pas_section_export();
    imp_exp((), Sect_exports)
enddefine;

    /*  Called by uses_lib_idents
    */
define $- popc_uses(libidname, idnames, by_name);
    lvars libidname, idname, idnames, by_name, flags = IDT_USED, token, id;
    if libidname then
        note_perm_ident(libidname, flags) -> token;
        unless isdeclared(token) ->> id then
            ident_error(token, 'UNDECLARED LIBRARY IDENTIFIER %p')
        elseunless Id_libfile(id) then
            ident_error(token, 'LIBRARY IDENTIFIER %p NOT FOUND')
        endunless
    endif;
    if by_name then flags || IDT_GEN_IN_DICT -> flags endif;
    for idname in idnames do
        note_perm_ident(idname, flags) ->
    endfor
enddefine;

#_IF pop_internal_version >= 150220

    /*  Called from sysCOMPILE to run compiler procedure
    */
define $- popc_compstream_apply(compile_p);
    dlocal current_dlocal_ids = [];
    ;;; define an lconstant macro POPC_COMPILING = true
    ;;; (has to be done this way, since pop_pas_mode is set false
    ;;; inside the condition of a #_IF)
    sysLCONSTANT("POPC_COMPILING", "macro");
    sysPASSIGN(true, "POPC_COMPILING");
    compile_p();
enddefine;

define $- popc_compstream_dlocal(id);
    id :: current_dlocal_ids -> current_dlocal_ids
enddefine;

#_ELSE

    /*  Called from sysCOMPILE before running compiler procedure
    */
define $- popc_sysCOMPILE_trap();
    ;;; define an lconstant macro POPC_COMPILING = true
    ;;; (has to be done this way, since pop_pas_mode is set false
    ;;; inside the condition of a #_IF)
    sysLCONSTANT("POPC_COMPILING", "macro");
    sysPASSIGN(true, "POPC_COMPILING");
enddefine;

#_ENDIF

    /*  Returns the weak identifier for word
        depend_vec can be either "weakref", a vector, or false
        for a noncommital reference
    */
define $- popc_weakref_ident(word, depend_vec, weak_option);
    lvars word, id, p, depend_vec, weak_option;
    lconstant weak_ids_of = newproperty([], 16, false, true);

    define lconstant cons_weak_id(id, mode_flag) -> weak_id;
        lvars id, weak_id = copy(id), mode_flag;
        mode_flag -> fast_idval(weak_id);
        id -> Weak_id(weak_id)
    enddefine;

    if caller(1) == sys_use_current_ident then
        isdeclared(word)
    else
        get_perm_ident(word)
    endif -> id;
    unless weak_ids_of(id) ->> p then
        conspair(cons_weak_id(id, 0), cons_weak_id(id, IDT_WEAK_REF))
                            ->> p -> weak_ids_of(id)
    endunless;
    if weak_option then fast_front(p) else fast_back(p) endif;  ;;; result
    returnunless(isvector(depend_vec));

    ;;; record identifier dependencies
    fast_for word in_vector depend_vec do
        note_perm_ident(popc_weakref_ident(word, false, weak_option), 0)
                                                        -> word;
        unless lmember(word, Id_depend_list(id) ->> p) then
            word :: p -> Id_depend_list(id)
        endunless
    endfor
enddefine;

define $- popc_testdef_code(word);
    lvars tlab, word;
    if pop_vm_compiling_list == [] then
        mishap(word, 1, 'testdef NOT ALLOWED OUTSIDE PROCEDURES')
    endif;
    consref(popc_weakref_ident(word, false, false)) -> tlab;
    testdef_label_key -> data_key(tlab);
    sysPUSHQ(tlab);
#_IF not(DEF AIX)
    ;;; poplink will set label value to 1 if defined, 0 if not
    sysCALL(nonpop_trans_vm("_nonzero"));
;;; AIX doesn't support global absolute symbols, so label value is
;;; true/false directly
#_ENDIF
    pop11_FLUSHED -> pop_expr_inst
enddefine;

    /*  Called by declare_updater
    */
define $- popc_declare_updater(idname);
    lvars idname;
    lconstant weakpair = writeable conspair(0, consref("weakref"));
    idname -> fast_front(weakpair);
    popc_updater(sys_current_val(weakpair)) ->
enddefine;


;;; ------------------------------------------------------------------------

define lconstant expand_wlib_list(list);
    lvars list, id_list = [];

    define lconstant expand_arg(arg);
        lvars arg, v;

        if isstring(arg) and isstartstring('^', arg) then
            if isstartstring('^^', arg) then
                consword(allbutfirst(2,arg)) -> arg
            else
                valof(consword(allbutfirst(1,arg))) -> arg
            endif
        endif;

        if islist(arg) then hd(arg) -> arg endif;   ;;; annotated
        if isword(arg) then identof(arg) -> arg endif;
        if isstring(arg) then
            arg
        elseif isident(arg) then
            idval(arg) -> v;
            v :: (arg :: id_list) -> id_list;
            chain(v, expand_arg, applist)
        elseif isprocedure(arg) then
            return
        else
            mishap(arg, 1, 'INVALID ITEM IN LIBRARY LIST')
        endif
    enddefine;

    maplist(list, expand_arg), id_list
enddefine;


;;; --- AUTOLOADING DECLARATIONS -------------------------------------------

vars
    wlib_declare_liblist = []
    ;


lvars
    save_wlib_declare_liblist = [],
    wlib_idents_found = [],
    wlib_cache = [],
    wlib_decl_vec,
    procedure sect_words_checked
    ;

define lconstant sect_words_auto_failed =
    newproperty([], 16, [], true)
enddefine;


define lconstant do_auto_declare(id_flags, prmflags, incr_flags, incr_vec,
                                                name, declaration_libfile);
    lvars   id_flags, prmflags, incr_flags, incr_vec, name, p, propexpr,
            pt_active, pt_hash_pdr, pt_eq_pdr, pt_default, pt_expand, pt_count,
            tab_size, gctype;

    dlocal  declaration_libfile,
            auto_declare = true,    ;;; so IDT_EXPLICIT doesn't get set
            pop_syntax_only = false,
            pop_vm_flags = pop_vm_flags||VM_NOPROT_PVARS;

    sysSYNTAX(name, flags_idprops(id_flags) || 2:10);
    if prmflags &&/=_0 M_PERM_GLOBAL then sysGLOBAL(name) endif;
    if prmflags &&/=_0 IDT_HAS_UPDATER
    and isprocedure(fast_idval(isdeclared(name)) ->> p) then
        ;;; 'declare' updater
        popc_updater(p) ->
    endif;
    returnunless(incr_flags);

    ;;; incremental prop/list/procedure
    if incr_flags && WRINCRF_TYPE == WRINCRT_PROPERTY then
        ;;; property -- incr_vec is a vector of words

        define lconstant twd_val(tword);
            lvars   tword, class = subscrw(1,tword),
                    str = allbutfirst(1,fast_word_string(tword));
            go_on class to INT FLOAT CONST WORD IDENT WORDID;
                INT:    return(mcint(strnumber(str)));
                FLOAT:  ;;; not implemented
                CONST:  return(consword(str));      ;;; pathnames ?
                WORD:   return(""", str, """);
                IDENT:  return("ident", consword(str));
                WORDID: return(""", "ident", consword(str), """);
        enddefine;

        explode(incr_vec) -> (/*pdpart_lab*/, /*updpart_lab*/, gctype,
                              tab_size, pt_count, pt_expand,
                              pt_default, pt_eq_pdr, pt_hash_pdr, pt_active);
        [ newanyproperty([],
            ^(strnumber(tab_size)), ^(twd_val(pt_expand)), ^(twd_val(pt_count)),
            ^(twd_val(pt_hash_pdr)), ^(twd_val(pt_eq_pdr)), "^gctype",
            ^(twd_val(pt_default)), ^(twd_val(pt_active))
        ) ]
    else
        ;;; list/procedure
        false
    endif -> propexpr;
    pop_declare_incremental(name, incr_flags, propexpr)
enddefine;

define lconstant wlib_load_decl(name, sect);
    lvars   i, vec, wlib, name, sect, file, tname, sectpath, state,
            d_i, d_file, d_wlib, modnum, swc;
    lconstant macro CACHE_LEN = 10;

    define lconstant file_wlib =
        newactproperty([], 4, false, "tmpboth",
                    procedure(file, prop);
                        lvars file, prop;
                        open_w_input(file, false, "wlib") ->> prop(file)
                    endprocedure)
    enddefine;

    ;;; return true if already tried in current file
    sect_words_checked(sect) -> swc;
    returnif(fast_lmember(name, swc)) (true);   ;;; true if tried already
    name :: swc -> sect_words_checked(sect);

    ;;; return false if already tried and failed (in any file)
    sect_words_auto_failed(sect) -> swc;
    returnif(fast_lmember(name, swc)) (false);

    nonpop_trans(name,true) -> tname;
    section_pathname(sect) -> sectpath;
    1 -> state;
    wlib_decl_vec -> vec;

    fast_for i to datalength(vec) do
        nextunless(isstring(fast_subscrv(i,vec) ->> file));
        unless file_wlib(file) ->> wlib then
            ;;; dir with no src.wlb
            false -> fast_subscrv(i,vec);
            nextloop
        endunless;
        unless fast_lmember(wlib, wlib_cache) then
            if listlength(wlib_cache) fi_< CACHE_LEN then
                wlib_cache nc_<> (wlib::[]) -> wlib_cache
            else
                wlib -> last(wlib_cache)
            endif
        endunless;

        if search_wlib_index(sectpath, tname, wlib, state) then
            (i, file, wlib) -> (d_i, d_file, d_wlib);
            ;;; for state == 1, has left
            ;;; id_flags, prmflags, incr_flags, incr_vec, modnum on stack
            if state == 1 then
                2 -> state
            else
                true
            endif;
            quitif(() ->> modnum)       ;;; intialises identifier
        endif
    endfor;

    if state == 1 then
        ;;; failed
        name :: swc -> sect_words_auto_failed(sect);
        return(false)
    endif;

    ;;; declare weakly
    do_auto_declare(/*id_flags, prmflags, incr_flags, incr_vec,*/
                    word_identifier(name, sect, "undef"), modnum and d_file);
    if d_i /== 1 then
        fast_subscrv(d_i-1,vec) -> fast_subscrv(d_i,vec);
        d_file -> fast_subscrv(d_i-1,vec)
    endif;
    setfrontlist(d_wlib, wlib_cache) -> wlib_cache;
    true
enddefine;

define $- popc_auto_declare(word);
    lvars word, status, sect, name, id, limit_sect, l;
    lconstant EX_DECL = IDT_DECLARED || IDT_EXPLICIT;

    define lconstant idvals_have_changed();
        lvars l = wlib_idents_found;
        while l /== [] do
            fast_destpair(fast_destpair(l)) -> l;
            returnif(() /== idval()) (true)
        endwhile;
        false
    enddefine;

    if isdeclared(word) ->> id then
        ;;; nothing to do if current id is explicitly declared
        returnif(Id_type(id) fi_&& EX_DECL == EX_DECL)
    endif;

    returnif(wlib_declare_liblist == [] or auto_declare);

#_IF pop_internal_version < 142301
    returnif(pop_syntax_only and caller(1) == sys_use_current_ident);
#_ENDIF

    ;;; not interested unless a dictionary word/word identifier
    returnunless(word_dict_status(word) ->> status);

    ;;; check for change in lib list
    if wlib_declare_liblist /== save_wlib_declare_liblist
    or idvals_have_changed() then
        expand_wlib_list(wlib_declare_liblist) -> (l, wlib_idents_found);
        {% dl(l) %} -> wlib_decl_vec;
        sys_grbg_list(l);
        wlib_declare_liblist -> save_wlib_declare_liblist;
        clearproperty(sect_words_checked);
        clearproperty(sect_words_auto_failed);
    endif;

    ;;; test for word identifier
    returnif(ispair(status)) (wlib_load_decl(destpair(status)) ->);

    ;;; else ordinary dictionary word
    word -> name, current_section -> sect;
    ;;; return if done already
    returnif(wlib_load_decl(name, sect));

    ;;; else in current section
    if id then
        word_identifier(name, sect, true) -> word
    else
        repeat
            returnunless(section_supersect(sect) ->> sect);
            quitif(word_identifier(name, sect, true) ->> word);
            returnif(wlib_load_decl(name, sect))
        endrepeat;
        returnif(Id_type(identof(word)) &&/=_0 IDT_DECLARED
                 or wlib_load_decl(name, sect))
    endif;
    ;;; section in which id is local
    destpair(word_dict_status(word)) -> (, limit_sect);
    until sect == limit_sect do
        section_supersect(sect) -> sect;
        quitif(wlib_load_decl(name, sect))
    enduntil
enddefine;

define autodef_int(word);
    lvars word, num;

    returnunless(syspop_mode)(false);

    unless datalength(word) fi_>= 2 and word(1) == `_`
    and (strnumber(allbutfirst(1, word)) ->> num) then
        return(false)
    endunless;

    define lconstant macp(numstr);
        lvars c1, c2, numstr, more_number;
        if ispair(proglist) and isprocedure(back(proglist)) then
            ;;; proglist not expanded
            ;;; see if next char is `.` (floating-point) or `:` (radix)
            nextchar(readitem) -> c1;
            if c1 == `.` then
                nextchar(readitem) ->> c2 -> nextchar(readitem);
                `0` fi_<= c2 and c2 fi_<= `9`
            else
                c1 == `:`
            endif -> more_number;
            c1 -> nextchar(readitem);
            if more_number then
                ;;; put all the characters except the `_` back
                explode(numstr);
                fast_repeat datalength(numstr) times
                    -> nextchar(readitem)
                endfast_repeat;
                syspop\:_: (readitem());
                proglist -> tl(proglist_macro_pair)
            else
                numstr
            endif -> hd(proglist_macro_pair);
            proglist_macro_pair -> proglist
        else
            ;;; produce the int string
            numstr
        endif
    enddefine;

    sysLCONSTANT(word, "macro");
    sysPASSIGN(macp(% syspop\:_int(num) %), word);

    true
enddefine;


;;; --- RECORDING WORDS & PERMANENT IDENTIFIERS USED --------------------------

define lconstant check_declared(id, token) -> type;
    lvars token, id, type = Id_type(id);
    returnif(type &&/=_0 IDT_DECLARED);
    ;;; no declaration -- save for later mishap
    ident_error(token, 'UNDECLARED IDENTIFIER %p');
    pas_declare_perm(id);
    Id_type(id) -> type
enddefine;

define lconstant init_incremental_val(id, token);
    lvars id, token, val, flags, propexpr, props;
    returnif(Id_init(id) /== undef_init);
    destpair(hd(tl(fast_lmember(id, incremental_idents))))
                                                -> (flags, propexpr);
    flags fi_&& WRINCRF_TYPE -> flags;
    if flags == WRINCRT_PROPERTY then
        ;;; propexpr must be a property expression
        if isproperty(pop11_compile(propexpr) ->> val) then
            wordid_name_sect(token) -> (props, );
            pop11_define_props(token, props, false) -> pdprops(val)
        else
            mishap(token, val, 2, 'PROPERTY NEEDED FOR INCREMENTAL PROPERTY')
        endif;
        val
    elseif flags == WRINCRT_LIST then
        []
    else
        ;;; procedure
        identfn
    endif -> Id_init(id)
enddefine;

define note_perm_ident(id, flags) -> token;
    lvars   id, type, token, flags, status, mode_flag = IDT_STRONG_REF,
            dummy_id, sect, idname, was_word;

    define lconstant try_auto(idname, status, context);
        lvars idname, status, context;
        popc_auto_declare(if ispair(status) or current_section == pop_section
                          then idname
                          else word_identifier(idname, pop_section, context)
                          endif);
    enddefine;

    if (isword(id) ->> was_word) and not(isdeclared(id->>idname) ->> id) then
        ;;; undeclared word/word ident -- get ident token
        ;;; assume it's at top-level if not a word ident
        word_identifier(idname, pop_section, "undef") -> idname;
        returnunless(word_dict_status(idname) ->> status) (false -> token);
        if back(status) == pop_section then
            front(status) -> idname, true -> status
        endif;
        try_auto(idname, status, "undef");
        unless isdeclared(idname) ->> id then
            nonpop_trans(idname, true) -> token;
            unless Token_dummy_id(token) ->> id then
                ;;; create a dummy id
                consident(0, 2:10, "perm") ->> id -> Token_dummy_id(token);
                idname -> pas_ident_token(id)
            endunless
        endunless;
        "noauto" -> was_word
    endif;

    if Weak_id(id) ->> status then
        fast_idval(id) -> mode_flag;    ;;; IDT_WEAK_REF or 0
        status -> id
    endif;

    unless isident(id) == "perm"
    and (word_dict_status(get_ident_token(id) ->> token) ->> status) then
        return(false -> token)
    endunless;

    Id_type(id) -> type;
    unless type &&/=_0 IDT_DECLARED or was_word == "noauto" then
        ;;; try autoloading declaration (probably tried already)
        try_auto(nonpop_trans(token, false), status, false);
        if was_word then Id_type(id) else check_declared(id, token) endif
                                                    -> type
    endunless;

    if ispair(status) then fast_back(status) else pop_section endif -> sect;

    if (Token_dummy_id(token) ->> dummy_id) and id /== dummy_id then
        false -> Token_dummy_id(token);
        type fi_|| Id_type(dummy_id) ->> type -> Id_type(id);
        if type &&/=_0 IDT_OUTPUT then
            id :: fast_ncdelete(dummy_id, Sect_idents(sect), nonop ==)
                                            -> Sect_idents(sect)
        endif
    endif;

    if type &&=_0 IDT_OUTPUT then
        ;;; not yet recorded for output
        returnif(External_dummy_id(id));
        if type &&/=_0 IDT_INCREMENTAL then
            init_incremental_val(id, token)
        endif;
        id :: Sect_idents(sect) -> Sect_idents(sect);
        flags || IDT_OUTPUT -> flags
    endif;

    unless flags &&/=_0 NPA_WEAK_OPTION then
        flags || mode_flag -> flags
    endunless;

    flags && POPC_IDT_MASK -> flags;
    lconstant both = IDT_WEAK_REF || IDT_STRONG_REF;
    if (type fi_|| flags ->> Id_type(id)) fi_&& both == both then
        ident_error(token, 'MIXED WEAK/STRONG REFERENCES TO IDENTIFIER %p')
    endif
enddefine;


define gen_perm_inits(w_name, doing_asm);
    lvars   id, token, val, init_lab, info, slab, doing_asm, pair, item,
            incrs, vec, file_unique_name, labnum = 0, flags, myflags, w_name;

    define lconstant next_filelab(incr_id) -> lab;
        lvars incr_id, lab, token;
        unless file_unique_name then
            mishap(0, 'NO PERM IDENTS INITIALISED IN FILE -- CAN\'T DERIVE FILE-UNIQUE NAME')
/*
            ;;; hack ...
            fast_word_string(get_ident_token(incr_id)) -> token;
            if sys_fname_nam(w_name) = token then
                token -> file_unique_name
            else
                mishap(0, 'NO PERM IDENTS INITIALISED IN FILE -- CAN\'T DERIVE FILE-UNIQUE NAME')
            endif
*/
        endunless;
        symlabel(file_unique_name sys_>< labnum) -> lab;
        `f` -> subscrs(1,lab);      ;;; replaces `c`
        labnum+1 -> labnum
    enddefine;

    pop_section -> current_section;
    do_leftover_updaters();

    rev(Id_init_list) -> Id_init_list;
    ;;; ensure global labels attached to each init
    assign_perm_init_labels(Id_init_list) -> file_unique_name;

    unless doing_asm then
        ;;; allocate incremental list/procedure labels
        incremental_idents -> incrs;
        until incrs == [] do
            dest(dest(incrs)) -> (id, pair, incrs);
            nextif((Id_init(id) ->> val) == undef_init
                or val == [] or val == identfn
                or (front(pair) ->> flags)&&WRINCRF_TYPE == WRINCRT_PROPERTY);

            ;;; non-empty list/procedure
            if isprocedure(val) then
                ;;; save generating composite procedures
                define lconstant explode_pcomp(p);
                    lvars p;
                    while ispcomposite(p)
                    and data_key(p) /== gen_procedure_key do
                        explode_pcomp(explode(p) -> p)
                    endwhile;
                    if p /== identfn then p endif
                enddefine;
                [% explode_pcomp(val) %]
            elseif flags &&/=_0 WRINCRF_SUBLISTS and listlength(val) fi_>= 2
            then
                ;;; make a sublist
                val :: []
            else
                val
            endif ->> val -> Id_init(id);
            {%  for item in val do
                    ;;; returns actual global label
                    label_perm_init(item, next_filelab(id), true)
                endfor
            %} -> back(pair)
        enduntil;

        ;;; output any constant assignments
        fast_for pair in const_assign_list do outlabset(destpair(pair)) endfor
    endunless;


    ;;; generate all structures compiled
    until Id_init_list == [] do
        f_dest(f_dest(Id_init_list)) -> (id, slab, Id_init_list);

        Id_init(id) -> val;
        get_ident_token(id) -> token;
        if doing_asm then
            label_of(val, false) -> init_lab;
            if islabel(init_lab) &&/=_0 LAB_OF_STRUCT then
                ident_error(token, 'INVALID INITIALISATION IN ASSEMBLER FILE %p');
                nextloop
            endif
        elseif Id_type(id) &&/=_0 IDT_INCREMENTAL then
            ;;; init label is just a dummy
            slab -> init_lab
        else
            if nonactive_isconstant(id) and isprocedure(val) and updater(val)
            then
                Id_type(id) || IDT_HAS_UPDATER -> Id_type(id)
            endif;
            genstructure(val) -> init_lab
        endif;

        islabel(init_lab) -> info;
        if slab /= init_lab and nonactive_isconstant(id) then
            if info &&/=_0 LAB_LINK_ASSIGN then
                ;;; structure poplink handles assignments for
                ;;; record this assignment on the .w file for poplink
                if poplink_unique_struct(val) then
                    ;;; false, true, etc
                    slab
                else
                    slab -> init_lab;
                    if isident(val) then
                        ;;; perm identifier
                        Weak_id(val) or val -> val;
                        WRTYPE_IDENT_ASSIGN
                    elseif ispair(word_dict_status(val)) then
                        ;;; perm word identifier
                        get_perm_ident(val) -> val;
                        WRTYPE_WORDID_ASSIGN
                    else
                        ;;; dictionary word
                        WRTYPE_WORD_ASSIGN
                    endif;
                    conspair((), slab)
                endif :: Link_assign(val) -> Link_assign(val)
            elseif islabel(slab) &&/=_0 #_< LAB_LITERAL||LAB_OF_STRUCT >_# then
                slab -> init_lab
            else
                ident_error(token, 'INVALID INITIALISATION FOR PERM CONSTANT %p')
            endif
        elseif info &&=_0 LAB_GLOBAL then
            slab -> init_lab
        endif;
        init_lab -> Id_output_init(id);
        islabel(init_lab) -> info;
        if info &&/=_0 LAB_OF_STRUCT
        or (nonactive_isconstant(id) and info &&/=_0 LAB_LITERAL)
        or doing_asm
        then
            ;;; referencing init_lab will extract the object module
            Id_type(id) || IDT_OBJMOD_INIT -> Id_type(id)
        endif
    enduntil;

    ;;; generate incremental values
    lvars did_gen = true, next_incrs, out_incrs = [];

    while did_gen do
        false -> did_gen;
        incremental_idents -> next_incrs;
        until (next_incrs ->> incrs) == [] do
            dest(dest(incrs)) -> (id, pair, next_incrs);
            nextunless(id and (Id_init(id) ->> val) /== undef_init);

            if doing_asm and length(val) /== 0 then
                mishap(0, 'CANNOT GENERATE INCREMENTAL ENTRIES WITH ASSEMBLER SOURCE FILE')
            endif;

            false -> f_hd(incrs);
            front(pair) -> flags;
            if flags && WRINCRF_TYPE == WRINCRT_PROPERTY then
                ;;; generate entry chain and return vec of prop info labels
                if datalength(val) == 0 then
                    nullstring
                else
                    next_filelab(id)
                endif -> init_lab;
                gen_incremental_property(val, init_lab) -> vec;
                if init_lab == nullstring then
                    init_lab -> subscrv(1,vec)
                endif;
                true -> did_gen
            else
                if isvector(back(pair) ->> vec) then
                    ;;; was non-empty
                    for item in val do
                        genstructure(item) -> ;
                        true -> did_gen
                    endfor
                else
                    ;;; empty
                    nullvector -> vec
                endif
            endif;
            consvector(id, flags, vec, 3) :: out_incrs -> out_incrs
        enduntil
    endwhile;
    out_incrs -> incremental_idents;

    unless doing_asm then
        ;;; output any non-constant assignments
        fast_for pair in label_assign_list do outlabset(destpair(pair)) endfor
    endunless
enddefine;


;;; --- EXTERNAL LOADS -------------------------------------------------

define $-popc_exload_objfile_args(objfiles);
    lvars obj, l, objfiles;

    ;;; list may contain dummy strings '-|' marking end of each separate exload
    ;;; (since we don't want them merged until poplink)
    repeat
        returnif(objfiles == []);
        [%  while objfiles /== []
            and (dest(objfiles) -> (obj, objfiles), obj /= '-|') do
                if isword(obj) then
                    fast_word_string(obj) -> obj;
#_IF DEF UNIX
                    unless isstartstring('-', obj) then
                        obj <> '.o' -> obj
                    endunless
#_ELSEIF DEF WINDOWS
                    obj <> '.dll' -> obj
#_ENDIF
                endif;
                obj
            endwhile
        %] -> l;
        unless l == [] or member(l, exload_args_lists) then
            l :: exload_args_lists -> exload_args_lists
        endunless
    endrepeat
enddefine;


;;; --- SYNTAX ---------------------------------------------------------

define lconstant setidbits(bits, keyword);
    lvars word, bits, keyword, need_vars, globarg;
    dlvars procedure (decl_p);

    if (readitem() ->> word) == "define" then
        pop11_define_declare -> decl_p;

        define dlocal pop11_define_declare(pdr_name, globl, decl, idprops);
            lvars decl, pdr_name, globl, idprops;
            if decl and decl /== sysVARS and decl /== sysCONSTANT then
                mishap(pdr_name, 1, 'NOT A PERM IDENTIFIER FOR ' sys_>< keyword)
            endif;
            decl_p(pdr_name, globl, decl, idprops);
            note_perm_ident(identof(pdr_name), bits) -> ;
            ;;; disable for nested defines
            decl_p -> pop11_define_declare
        enddefine;

        nonsyntax define();
        return
    endif;

    "undef" -> globarg;
    false -> need_vars;
    if word == "register" then
        true -> need_vars;
        bits || M_PERM_REGISTER -> bits;
        readitem() -> word
    endif;
    if word == "global" then
        true -> need_vars;
        "global" -> globarg;
        readitem() -> word
    endif;
    if word == "vars" or word == "constant" then
        if word == "vars" then sysVARS else sysCONSTANT endif -> decl_p;
        pop11_comp_declaration(
                        procedure(word, idprops);
                            lvars word, idprops;
                            decl_p(word, idprops);
                            note_perm_ident(identof(word), bits) ->
                        endprocedure, globarg)
    elseunless need_vars then
        until word == ";" do
            if word == "," then
                readitem() -> word, nextloop
            endif;
            sys_read_path(word, false, false) -> word;
            note_perm_ident(identof(word), bits) -> ;
            readitem() -> word
        enduntil;
        ";" :: proglist -> proglist
    else
        mishap(word, 1, 'vars OR constant EXPECTED AFTER ' sys_>< keyword)
    endif
enddefine;

constant syntax (
    $- protected        = setidbits(%IDT_PROTECT||NPA_WEAK_OPTION, "protected"%),
    );

constant syntax $- end_library_declare_section = pop_undef;
;;;
define syntax $- library_declare_section;
    dlocal declaration_libfile;
    unless isstring(itemread() ->> declaration_libfile) then
        mishap(declaration_libfile, 1, 'LIBRARY NAME STRING NEEDED')
    endunless;
    pop11_exec_stmnt_seq_to("end_library_declare_section") ->
enddefine;


;;; --- SWITCHING IN AND OUT OF POPC COMPILE MODE -----------------------

define lconstant define_props(p_idname, props, upd) -> props;
    lvars p_idname, props, upd, id = sys_current_ident(p_idname);
    unless id then sys_use_current_ident(p_idname) -> (id, ) endunless;
    if isident(id) == "perm" then
        ;;; props label
        consref(id) -> props;
        pdprops_label_key -> data_key(props)
    elseif pop_pop11_flags &&/=_0 POP11_NO_LEX_PDPROPS then
        ;;; false for lex when flag set
        false -> props
    endif
enddefine;

define lconstant vars_default_check(idname);
    lvars idname;
    dlocal cucharout = cucharerr;
    printf('\s\s\s\sNO DECLARATION FOR %p IN %p\n',
                            [%idname, pdprops(hd(pop_vm_compiling_list))%]);
    unless islist(file_has_errors) then [] -> file_has_errors endunless;
    if ispair(word_dict_status(idname)) then
        procedure(idname, idprops);
            lvars idname, idprops;
            sysSYNTAX(idname, idprops, false);
            sysLOCAL(idname)
        endprocedure
    else
        sysLVARS
    endif
enddefine;

define lconstant popc_flags_val();
    popc_flags
enddefine;
;;;
define updaterof popc_flags_val(flags);
    lvars flags;
    if (flags ||/& popc_flags) &&/=_0 POPC_SYSPOP_MODE then
        if flags &&/=_0 POPC_SYSPOP_MODE then
            unless syspop_mode then
                if syspop_mode_established then
                    mishap(0, 'CAN\'T SWITCH TO sysPOP AFTER BEGINNING OF FILE')
                else
                    set_syspop_mode(true)
                endif
            endunless
        elseif syspop_mode then
            mishap(0, 'CAN\'T SWITCH OUT OF sysPOP ONCE ENTERED')
        endif
    endif;
    flags -> popc_flags
enddefine;

define lconstant popc_prwarning(word);
    lvars word;
    note_perm_ident(identof(word), NPA_WEAK_OPTION) ->
enddefine;

define lconstant exec_apply();
    dlocal popc_perm_assign_mode = true;
    fast_apply()
enddefine;

define lconstant decl_incremental(idname, flags, propexpr);
    lvars   idname, flags, propexpr, id = get_perm_ident(idname), type,
            pair, currflags;

    check_declared(id, idname) -> type;
    if type &&/=_0 IDT_INCREMENTAL then
        hd(tl(fast_lmember(id, incremental_idents))) -> pair;
        fast_front(pair) -> currflags;
        if currflags && WRINCRF_TYPE == flags && WRINCRF_TYPE then
            ;;; *** for property, should check the props are the same ***
            (currflags && 16:FF) || flags -> fast_front(pair);
            return
        endif;
        ident_error(idname, 'REDECLARING STATUS OF INCREMENTAL IDENTIFIER %p');
    elseif fast_lmember(id, Id_init_list) then
        ident_error(idname, 'INCREMENTAL IDENTIFIER ALREADY ASSIGNED A VALUE %p');
    else
        ;;; new
        if nonactive_isconstant(id)
        and flags fi_&& WRINCRF_TYPE /== WRINCRT_PROPERTY then
            ident_error(idname, 'INCREMENTAL LIST/PROCEDURE MUST BE A VARIABLE')
        endif;
        type || IDT_INCREMENTAL -> Id_type(id);
        id :: (conspair(flags,propexpr) :: incremental_idents)
                                                    -> incremental_idents;
        if type &&/=_0 IDT_OUTPUT then
            init_incremental_val(id, idname)
        endif
    endif;
enddefine;

lblock
compile_mode:vm -prmprt;

lvars saved_globals = false;

define lconstant switch_globals();

    lconstant macro GLOBALS = [
           (pop11_define_props,
            pop11_vars_default_check,
            prwarning,
            pop_pop11_flags,
            pop_vm_flags,
            nonactive pop_popc_flags,
            pop_vm_exec_apply,
            pop_declare_incremental)
        ];

    define lconstant active curr_globals;
        [% GLOBALS %]
    enddefine;
    ;;;
    define updaterof active curr_globals;
        dl(dup()) -> GLOBALS;
        sys_grbg_list()
    enddefine;

    unless saved_globals then
        ;;; first time switching into popc mode
        [%  define_props,
            vars_default_check,
            popc_prwarning,
            pop_pop11_flags,
            pop_vm_flags,
            popc_flags_val,
            exec_apply,
            decl_incremental
        %] -> saved_globals
    endunless;

    curr_globals, saved_globals -> (saved_globals, curr_globals)
enddefine;

#_IF isactive("pop_pas_mode")

define $- pop_change_pas_mode(oldval, newval);
    lvars oldval, newval;

    if oldval == "popc" or newval == "popc" then
        chain(switch_globals)
    endif
enddefine;

#_ELSE

define vars set_pop_pas_mode(turn_on);
    lvars turn_on;
    if turn_on then
        returnif(pop_pas_mode);         ;;; already on
        "popc" -> pop_pas_mode;         ;;; reroute intermediate code etc
        true -> \^P\^O\^P\^C;           ;;; say doing POPC
    else
        returnunless(pop_pas_mode);     ;;; already off
        false ->> pop_pas_mode -> \^P\^O\^P\^C
    endif;
    switch_globals()
enddefine;

#_ENDIF

endlblock;


;;; --- COMPILING POP SOURCE FILES ----------------------------------------

define lconstant do_compile(fname_list, a_name, w_name, pr_fname,
                                                            include_list);
    lvars   fname_list, a_name, w_name, include_list, procedure pr_fname;

    dlocal  asmf_charout,
            syspop_mode_established = false,
            %syspop_mode, set_syspop_mode()%,
        ;

    ;;; pretend it's not `runtime'
compile_mode:vm -prmprt;
    dlocal  pop_runtime = false;

#_IF DEF VMS
    dlocal popc_vms_macdef_ops = [];
#_ENDIF

    ;;; compile the files, building Id_init property
    popc_compile_files(fname_list, pr_fname, include_list, []);

    ;;; produce assembler file
    discout(a_name) -> asmf_charout;
    add_created_file(a_name);
    gen_assembler(w_name, gen_perm_inits(%w_name, false%));
    asmf_charout(termin)
enddefine;


;;; --- MAIN PROCEDURES --------------------------------------------------

define lconstant do_popc(source_fname_list, a_name, w_name, pr_fname,
                                                        include_list);
    lvars   fname, source_fname_list, extn, a_name, w_name, include_list,
            procedure pr_fname, l
        ;

    dlocal  Id_type, Id_idprops, Id_init, Id_output_init,
            Sect_idents, Sect_imports, Sect_exports, Link_assign,
            Id_depend_list, pre_updaters, file_has_errors, quoted_dict_words,
            const_assign_list, label_assign_list,
            Id_init_list, nextlabel, sect_words_checked, Token_dummy_id,
            gen_procedure_args, popc_flags = 0,
            current_section = pop_section,
            pop_debugging = false, pop_record_writeable = true,
            incremental_idents = [],
            exload_args_lists = [],
        ;

#_IF isactive("pop_pas_mode")
        dlocal pop_pas_mode;
#_ELSE
        dlocal %pop_pas_mode, set_pop_pas_mode()%;
#_ENDIF

    init_syspop_idents();

    1 -> nextlabel;
    init_labels();
    newactproperty([], 4, false, true,
                        procedure(); -> ->, [] endprocedure) -> Sect_idents;
    newproperty([], 4, [], true) -> Sect_imports;
    newproperty([], 4, [], true) -> Sect_exports;
    newproperty([], 128, 0, true)    -> Id_type;
    newproperty([], 128, 0, true)    -> Id_idprops;
    newproperty([], 32, false, true) -> Id_libfile;
    newproperty([], 8, [], true)     -> Id_depend_list;
    newproperty([], 4, [], true) -> Link_assign;
    newproperty([], 8, false, "perm") -> Token_dummy_id;
    newproperty([], 4, false, false) -> pre_updaters;
    newproperty([], 32, undef_init, true) -> Id_init;
    newproperty([], 32, nullstring, true) -> Id_output_init;
    newproperty([], 16, [], true) -> sect_words_checked;
    newproperty([], 32, false, false) -> gen_procedure_args;
    [] ->> Id_init_list ->> const_assign_list ->> label_assign_list
                                    -> quoted_dict_words;
    false -> file_has_errors;

#_IF isactive("pop_pas_mode")
    "popc" -> pop_pas_mode;
#_ELSE
    set_pop_pas_mode(true);
#_ENDIF

    sys_fname_extn(hd(source_fname_list)) -> extn;
    for fname in tl(source_fname_list) do
        if sys_fname_extn(fname) /= extn then
            mishap(dl(source_fname_list), listlength(source_fname_list),
                            'GROUPED SOURCE FILES HAVE DIFFERENT EXTENSIONS')
        endif
    endfor;

    unless lmember(autodef_int, popautolist) ->> l then
        autodef_int :: popautolist -> popautolist
    elseunless l == popautolist then
        ncdelete(autodef_int, popautolist, nonop ==) -> ;
        autodef_int :: popautolist -> popautolist
    endunless;

    if extn = '.s' then
        do_asm
    elseif extn = '.p' then
        do_compile
    else
        mishap(hd(source_fname_list), 1, 'SOURCE FILE EXTENSION NOT .p OR .s')
    endif (source_fname_list, a_name, w_name, pr_fname, include_list);

#_IF isactive("pop_pas_mode")
    false -> pop_pas_mode;
#_ELSE
    set_pop_pas_mode(false);
#_ENDIF
    identfn -> Id_init;     ;;; no longer needed

    ;;; mishap if any errors
    if file_has_errors then
        mishap(0, 'FILE HAS COMPILATION ERRORS (see above)')
    endif;


    ;;; --- produce dictionary word file -----------------------------

    dlvars wdev, libfiles_used = [];

    define lconstant write_rec(nextindex);
        lvars nextindex, reclen = nextindex fi_- 1, len2 = reclen;
        lconstant lenbuf = writeable inits(4), LIM2 = 16:FFFF;
        if reclen fi_>= LIM2 then LIM2 -> len2 endif;
        len2 fi_&& 16:FF -> f_subs(1, lenbuf);
        len2 fi_>> 8     -> f_subs(2, lenbuf);
        syswrite(wdev, lenbuf, 2);
        if len2 == LIM2 then
            ;;; follow by 4-byte real length
            reclen -> subscr_nbyte(1, lenbuf, 4);
            syswrite(wdev, lenbuf, 4)
        endif;
        if reclen /== 0 then syswrite(wdev, w_buffer, reclen) endif
    enddefine;

    define lconstant write_simple_str_rec(str_or_word, rectype);
        lvars str_or_word, rectype;
        rectype -> f_subs(WR_TYPE, w_buffer);           ;;; rec type
        wbuf_insert_strword(WRWORD_NAME, str_or_word);
        write_rec(())
    enddefine;

    define lconstant write_link_assign(pair);
        lvars lab, rectype, pair;
        destpair(pair) -> (rectype, lab);
        write_simple_str_rec(lab, rectype)
    enddefine;

    define lconstant write_quoted_word(word);
        lvars word;
        write_simple_str_rec(word, WRTYPE_WORD);
        applist(Link_assign(word), write_link_assign)
    enddefine;

    define lconstant write_full_path(index, idname);
        lvars index, path, (idname, sect) = wordid_name_sect(idname);
        if sect then section_pathname(sect) else nullstring endif -> path;
        wbuf_insert_strword(index, path),
        wbuf_insert_strword((), idname)
    enddefine;

    define lconstant write_id(id);
        lvars id, name, p, type, file, init;

        define lconstant write_depend_name(idname);
            lvars idname;
            WRTYPE_WEAK_DEPEND -> f_subs(WR_TYPE, w_buffer);
            write_full_path(WRWKDP_REST, idname);
            write_rec(())
        enddefine;

        WRTYPE_IDENT -> f_subs(WR_TYPE, w_buffer);          ;;; rec type
        ;;; type flags (3 bytes)
        Id_type(id) -> type;
        type fi_&& WFILE_IDT_MASK -> subscr_nbyte(WRI_IDTYPE, w_buffer, 3);
        ;;; identprops (3 bytes)
        Id_idprops(id) -> subscr_nbyte(WRI_IDPROPS, w_buffer, 3);
        ;;; word string
        wordid_name_sect(get_ident_token(id)) -> (name, );
        wbuf_insert_strword(WRI_REST, name),
        ;;; init string
        wbuf_insert_strword((), Id_output_init(id) ->> init),
        write_rec(());

        applist(Id_depend_list(id), write_depend_name);
        applist(Link_assign(id), write_link_assign);

        if (Id_libfile(id) ->> file)    ;;; and type &&/=_0 IDT_STRONG_REF
        and init == nullstring
        and not(member(file, libfiles_used)) then
            file :: libfiles_used -> libfiles_used
        endif
    enddefine;

    define lconstant write_sect(sect, idlist);
        lvars sect, idlist;
        WRTYPE_SECT     -> f_subs(WR_TYPE, w_buffer);       ;;; rec type
        0               -> f_subs(WRSECT_FLAGS, w_buffer);  ;;; sect flags
        wbuf_insert_strword(WRSECT_PATHNAME, section_pathname(sect));
        write_rec(());

        ;;; identifiers in sect
        applist(idlist, write_id);

        ;;; imports & exports
        applist(Sect_imports(sect), write_simple_str_rec(%WRTYPE_IMPORT%));
        applist(Sect_exports(sect), write_simple_str_rec(%WRTYPE_EXPORT%));
    enddefine;

    define lconstant write_unique(item, pair);
        lvars item, pair, num = fast_back(pair), lab;
        fast_for lab in Link_assign(item) do
            WRTYPE_UNIQUE_ASSIGN -> f_subs(WR_TYPE, w_buffer);  ;;; rec type
            num -> f_subs(WRUNIQ_NUMBER, w_buffer);             ;;; number
            wbuf_insert_strword(WRUNIQ_LABEL, lab);         ;;; label
            write_rec(())
        endfor
    enddefine;

    define lconstant write_incr_entry(vec);
        lvars vec, (id, flags, labvec) = explode(vec);
        WRTYPE_LINK_INCR    -> f_subs(WR_TYPE,w_buffer);        ;;; rec type
        flags && 16:FF      -> f_subs(WRINCR_FLAGS,w_buffer);   ;;; flags
        flags>>8 -> subscr_nbyte(WRINCR_PREC, w_buffer, 2);     ;;; precedence
        datalength(labvec) -> f_subs(WRINCR_NSTRINGS,w_buffer); ;;; nstrings
        write_full_path(WRINCR_REST, get_ident_token(id));
        appdata(labvec, wbuf_insert_strword);
        write_rec(())
    enddefine;

    define lconstant write_strings_rec(strlist, rectype);
        lvars strlist, rectype;
        returnif(strlist == []);
        rectype -> f_subs(WR_TYPE,w_buffer);                     ;;; rec type
        listlength(strlist) -> f_subs(WRSTRS_NSTRINGS,w_buffer); ;;; nstrings
        applist(WRSTRS_REST, strlist, wbuf_insert_strword);
        write_rec(())
    enddefine;


    ;;; create w-file
    syscreate(w_name, 1, true) -> wdev;
    add_created_file(w_name);

    ;;; magic number and file format version number
    WFILE_MAGIC           -> subscr_nbyte(1, w_buffer, 4);
    CURRENT_WFILE_VERSION -> subscr_nbyte(5, w_buffer, 4);
    syswrite(wdev, w_buffer, 8);

    ;;; quoted dictionary words
    applist(quoted_dict_words, write_quoted_word);

    ;;; identifier records, etc for each section
    appproperty(Sect_idents, write_sect);

    ;;; label assignments to unique structs generated by poplink
    appproperty(poplink_unique_struct, write_unique);

    ;;; incremental list/prop entries
    applist(incremental_idents, write_incr_entry);

    ;;; lists of obj file/library args from external loading
    applist(ncrev(exload_args_lists),
                                write_strings_rec(%WRTYPE_EXLOAD_STRINGS%));

    ;;; list of w-libraries used -- will be used for linking
    write_strings_rec(libfiles_used, WRTYPE_WLIB_STRINGS);

    ;;; end record
    WRTYPE_END  -> f_subs(WR_TYPE, w_buffer);           ;;; rec type
    write_rec(2);

    ;;; finish with dummy 0 rec length
    write_rec(1);
    sysclose(wdev)
enddefine;

define lconstant get_asm_extn(source);
    lvars source;
#_IF DEF VMS
    if isstartstring('a32_', sys_fname_nam(source)) then
        '.a32'
    else
        ASM_EXTENSION
    endif
#_ELSE
    ASM_EXTENSION
#_ENDIF
enddefine;

#_IF pop_debugging

define syntax $- popc;
    lvars arg;
    dlocal popnewline = true;
    rdstringto([^newline ; ^termin]) -> arg;
    false -> popnewline;

    sysCALLQ(
        sysPUSHQ([^arg]),
        sysPUSHQ(new_fname_extn(arg, get_asm_extn(arg))),
        sysPUSHQ(new_fname_extn(arg, W_EXTENSION)),
        sysPUSHQ(erase),
        sysPUSHQ([]),
        do_popc);

    ";" :: proglist -> proglist
enddefine;

#_ENDIF


;;; --------------------------------------------------------------------


/*  Property that tells POPC about non-file poplink options:
            0  =  no arg,
            1  =  1 arg
            L  =  list or 1 arg
*/
lconstant procedure poplink_nf_options = newassoc([
    [ a         0 ]
    [ date      1 ]
    [ e         1 ]
    [ emb       0 ]
    [ exmain    0 ]
    [ exlink    0 ]
    [ lf        L ]
    [ lo        L ]
    [ ident     1 ]
    [ idexp     1 ]
    [ o         1 ]
    [ noshare   0 ]
    [ p         0 ]
    [ ponly     0 ]
    [ port      0 ]
    [ q         0 ]
    [ s         L ]
    [ tr        0 ]
    [ ubn       L ]
    [ uses      L ]
    [ unum      1 ]
;;; [ x*        0 ]

    ;;; Sun-2/3 floating-point option (ignored for other systems)
    [ sunfp     1 ]

    ]);


define $-Pop$-Main();
    lvars   c, arg, dev, n,
            asm_only    = false,
            comp_only   = false,
            l_flag      = false,
            out_dir     = false,
            link_args   = [],
            wlib_create = false,
            norecomp    = false,
            inter_args,
            file_args,
            n_compile   = 0,
            wlib_last   = 0::[],
            set_wlib_decl = true,
            syslib      = true,
            include_list    = [],
            end_wlibs   = [],
            poplink_pflag = false,
        ;

    dlocal  pop_arglist,
            pop_file_versions = use_file_versions(),
            cucharout = cucharerr,
            % file_create_control(dlocal_context) %,
        ;

#_IF pop_debugging
    dlocal  pop_mishap_doing_lim = false;
#_ELSE
    dlocal  pop_mishap_doing_lim = 0, popgctrace = false, popsyscall = false;
    pop_null_device -> pop_charin_device;
#_ENDIF

    500000 -> popmemlim;

    define dlocal prmishap(mess, list);
        lvars mess, list;
        sysprmishap('POPC: ' <> mess, list)
    enddefine;

#_IF DEF keyboard_interrupt
    define dlocal keyboard_interrupt();
        false -> pop_exit_ok;
        chainfrom($-Pop$-Main, sysexit)
    enddefine;

    dlocal interrupt = keyboard_interrupt;
#_ELSE
    define lconstant interrupt_exit();
        false -> pop_exit_ok;
        chainfrom($-Pop$-Main, sysexit)
    enddefine;

    dlocal interrupt = interrupt_exit;
#_ENDIF

    define lconstant add_wlibs(list);
        lvars x, list;
        fast_for x in list do
            x :: [] ->> back(wlib_last) -> wlib_last
        endfast_for;
        true -> set_wlib_decl;
        "wlb", list
    enddefine;

    define lconstant count_source_arg();
        if set_wlib_decl then
            "set_wlib_decl", wlib_last;
            false -> set_wlib_decl
        endif;
        n_compile fi_+ 1 -> n_compile
    enddefine;

    define lconstant do_file_arg(name);
        lvars name, e;
        sys_fname_extn(name) -> e;
        if e = WLB_EXTENSION or is_dir_path(name) then
            if not(null(pop_arglist))
            and (hd(pop_arglist) ->> e) = '-ex' or e = '-in' then
                tl(pop_arglist) -> pop_arglist;
                {%  name, e, '(', dl(get_option_arg(consword(e), false)), ')'
                %}
            else
                add_wlibs(name::[])
            endif
        elseif e = W_EXTENSION then
            consref(name)
        else
            count_source_arg();
            name
        endif
    enddefine;


    ;;; see if popc.args exists
    process_arglist(if sysopen('popc.args',0,"line",`F`) ->> dev then
                        dev :: poparglist
                    else
                        poparglist
                    endif) -> pop_arglist;

    ;;; first pass on args -- build list for second
    [%  until null(pop_arglist) do
            dest(pop_arglist) -> (arg, pop_arglist);
            if is_option(arg) then
                consword(arg) -> arg;
                allbutfirst(1, arg) -> c;
                if     c == "a" then
                    ;;; produce assembler/w-file output only
                    true -> asm_only

                elseif c == "c" then
                    ;;; compile only, no link
                    true -> comp_only

                elseif c == "createwlib" then
                    ;;; compile files and run poplibr to create w-library
                    ;;; -- do nothing if w-library exists already
                    get_option_arg(arg, true) -> wlib_create;
                    if open_w_input(wlib_create, false, "wlib") then
                        printf(get_wlib_name(wlib_create), '%p ALREADY EXISTS\n');
                        return
                    endif;
                    true -> comp_only

                elseif c == "ex" or c == "in" then
                    mishap(0, arg >< ' MUST FOLLOW W-LIBRARY')

                elseif c == "g" then
                    ;;; source file group to compile to single module
                    ;;; (optionally followed by module name)
                    if not(null(pop_arglist)) and isstring(hd(pop_arglist))
                    then
                        dest(pop_arglist) -> pop_arglist
                    else
                        false
                    endif -> c;
                    if null(pop_arglist) or isstring(hd(pop_arglist)) then
                        mishap(0, 'MISSING FILE LIST AFTER -g OPTION')
                    else
                        c :: get_option_arg(arg, false) -> c;
                        if listlength(c) == 1 then
                            mishap(0, 'EMPTY FILE LIST FOR -g OPTION')
                        else
                            true -> l_flag;
                            count_source_arg();
                            c
                        endif
                    endif

                elseif c == "l" then
                    ;;; list names of files compiled (now done
                    ;;; automatically for more than 1 file)
                    true -> l_flag

                elseif c == "m" then
                    ;;; define macro(s)
                    pop11_compile(stringin(consstring(#|
                        explode('section; global constant macro ('),
                        for c in get_option_arg(arg, false) do
                            explode(c),
                            unless locchar(`=`, 1, c) then
                                explode('=true')
                            endunless,
                            `,`
                        endfor,
                        explode('); endsection;')
                    |#) ))

                elseif c == "norec" then
                    ;;; don't recompile files for which .w/.o already exist
                    true -> norecomp

                elseif c == "od" then
                    ;;; output directory
                    c, get_option_arg(arg, true)

                elseif c == "nosys" then
                    ;;; no system libraries
                    false -> syslib

                elseif c == "u" then
                    ;;; use things
                    pop11_compile(stringin(consstring(#|
                        explode('section;'),
                        for c in get_option_arg(arg, false) do
                            explode('uses-now '), explode(c), `,`
                        endfor,
                        explode('; endsection;')
                    |#) ))

                elseif c == "wlb" or c == "wlib" then
                    ;;; w-library list for autoloading declarations
                    ;;; from w-libraries
                    expand_wlib_list(get_option_arg(arg, false)) -> (arg, );
                    if c == "wlb" then
                        add_wlibs(arg)
                    else
                        ;;; "wlib" gets added at end
                        end_wlibs nc_<> arg -> end_wlibs
                    endif

                elseif c == "INCLUDE" or c == "include" then
                    ;;; include files to add at beginning of every file
                    include_list nc_<> get_option_arg(arg, false)
                            -> include_list

                elseif (poplink_nf_options(c) ->> n)
                or (isstartstring('x',c) and 0 ->> n)
                then
                    link_args nc_<> [%
                        word_string(arg),
                        if n == 1 then
                            get_option_arg(arg, true)
                        elseif n /== 0 then
                            '(', dl(get_option_arg(arg, false)), ')'
                        endif
                    %] -> link_args;
                    if c == "p" or c == "ponly" then
                        true -> poplink_pflag
                    endif

                else
                    mishap(arg, 1, 'UNKNOWN OPTION')
                endif

            elseunless isstring(arg) then
                mishap(arg, 1, 'UNEXPECTED LIST ARGUMENT')

            else
                do_file_arg(arg)
            endif
        enduntil;

        if end_wlibs /== [] then add_wlibs(end_wlibs) endif;
        if syslib then
            add_wlibs([% ident popsyslist %])
        endif;

    %] -> inter_args;
    if n_compile > 1 then true -> l_flag endif;


    define lconstant compile_file(source);
        lvars   nam, a_name, o_name, w_name, source, asm_extn;
        dlvars  n, len;
        dlocal  % file_create_control(dlocal_context) %;

        sysflush(popdeverr);
        if ispair(source) then
            unless (dest(source) -> source ->> nam) then
                hd(source) -> nam
            endunless
        else
            source -> nam, source :: [] -> source
        endif;

        get_asm_extn(nam) -> asm_extn;
        sys_fname_nam(nam) -> nam;
        if asm_only then
            nam <> asm_extn -> a_name;
            if out_dir then out_dir dir_>< a_name -> a_name endif
        else
            new_tmp_file(false, 'popc', asm_extn) -> a_name
        endif;
        nam <> W_EXTENSION -> w_name;
        nam <> OBJ_EXTENSION -> o_name;
        if out_dir then
            out_dir dir_>< w_name -> w_name;
            out_dir dir_>< o_name -> o_name
        endif;
        unless comp_only or asm_only or n_compile /== 1 or poplink_pflag then
            conspair(w_name, o_name) -> n_compile
        endunless;

        define dlocal interrupt();
            false -> pop_exit_ok;
#_IF not(DEF keyboard_interrupt)
            if caller(1) /== mishap then interrupt_exit() endif;
#_ENDIF
            cucharerr(`\n`);
            sysflush(popdeverr);
            if not(asm_only) and pop_file_versions == 1 then
                sysdelete(w_name) -> , sysdelete(o_name) ->
            endif;
            true -> comp_only;      ;;; prevent linking
            false -> wlib_create;
            clearstack();
            exitfrom(false, compile_file)
        enddefine;

        define lconstant pr_fname(fname);
            lvars fname;
            dlocal cucharout = cucharerr;
            returnunless(l_flag);

            printf(fname, if len == 1 then
                            '%p\n'
                          elseif n == 1 then
                            '(%p\n'
                          elseif n == len then
                            '\s%p)\n'
                          else
                            '\s%p\n'
                          endif);
            sysflush(popdeverr);
            n+1 -> n
        enddefine;

        unless norecomp and isinteger(n_compile) and sys_file_exists(w_name)
        and sys_file_exists(if asm_only then a_name else w_name endif)
        then
            ;;; produce assembler file
            listlength(source) -> len;
            1 -> n;
            do_popc(source, a_name, w_name, pr_fname, include_list);
            sysflush(popdeverr);

            ;;; assemble it
            unless asm_only then
                assemble_files(conspair(a_name, o_name))
            endunless
        endunless;
        w_name, true
    enddefine;


    ;;; accumulate link file args (in reverse) -- musn't use a list
    ;;; constructor here, as the stack must be empty before and after
    ;;; compiling each file.

    [] -> file_args;
    until inter_args == [] do
        dest(inter_args) -> (arg, inter_args);
        if isstring(arg) or ispair(arg) then
            ;;; file or file group to compile
            nextunless(compile_file(arg));
            ;;; w-file name on stack
            if ispair(n_compile) then
                lvars (w, o) = destpair(n_compile);
                true ->> is_tmp_file(o) -> is_tmp_file(w);
                add_created_file(o), add_created_file(w)
            endif;
            1
        elseif isref(arg) then
            ;;; w-file
            fast_cont(arg), 1
        elseif isvector(arg) then
            ;;; w-lib with include/exclude
            explode(arg), datalength(arg)
        elseif arg == "set_wlib_decl" then
            ;;; set wlib_declare_liblist
            copylist(back(dest(inter_args) -> inter_args))
                                            -> wlib_declare_liblist;
            nextloop
        elseif arg == "od" then
            dest(inter_args) -> (out_dir, inter_args);
            nextloop
        elseif arg == "wlb" then
            ;;; w-lib group
            dest(inter_args) -> (, inter_args);
            nextif(wlib_create);
            '-wlbused', 1
/*
            expand_wlib_list(dest(inter_args) -> inter_args) -> (arg, );
            if (listlength(arg) ->> c) == 1 then
                hd(arg), 1
            else
                '-wlb', '(', dl(arg), ')', c+3
            endif
*/
        else
            mishap(arg, 1, 'INTERNAL ERROR IN ARG PROCESSING')
        endif -> c;     ;;; count to add to list

        ;;; add args from stack in correct order
        for arg from c by -1 to 1 do
            subscr_stack(arg) :: file_args -> file_args
        endfor;
        erasenum(c)

    enduntil;

    returnif(comp_only and not(wlib_create));           ;;; no link

    fast_ncrev(file_args) -> file_args;
    sysflush(popdevout);
    sysflush(popdeverr);

    if wlib_create then
        ;;; run poplibr
        run_comp_util(['-c' ^wlib_create] nc_<> file_args, 'poplibr');
        ;;; lib created -- remove .w/.o files
        fast_for arg in file_args do
            sysdelete(arg) -> ;
            sysdelete(new_fname_extn(arg, OBJ_EXTENSION)) ->
        endfor;
        printf(get_wlib_name(wlib_create), '%p CREATED\n')

    else
        ;;; run poplink
        if l_flag then
            charout -> cucharout;
            printf('Linking ...\n');
            sysflush(popdevout)
        endif;
        if asm_only then '-a' :: link_args -> link_args endif;

        run_comp_util(link_args nc_<> file_args, 'poplink')

    endif
enddefine;      /* $-Pop$-Main */


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 17 1993
        Changed pas_declare_perm to set IDT_PROLOG_DYNAMIC for dynamic prolog
        preds and clear it for static ones
--- John Gibson, Aug 16 1993
        Added pop_change_pas_mode for new system with active pop_pas_mode
--- John Gibson, May 27 1993
        Added recording of declaration libraries etc for linking
--- John Gibson, May 10 1993
        Added some new options
--- John Gibson, May  2 1993
        Improved treatment of liblists
--- John Gibson, Oct 16 1992
        14.22 changes
--- John Gibson, Sep 29 1992
        Added incremental ident interface
--- John Gibson, Jul 21 1992
        Version 14.21 changes
--- Andreas Schoter, Sep  9 1991
        Changed occurrances of -popliblist- to -popautolist-
--- John Gibson, Jun 25 1991
        Fix to -imp_exp- to stop it recording nonpop words
--- John Gibson, Jan 21 1991
        Added "noshare" to poplink_nf_options
--- John Gibson, Sep  5 1990
        Changed -Main- so that nothing is left on the stack while
        compiling a file (can mask a stack underflow)
--- John Gibson, Aug 31 1990
        Changes to cope with list args in poparglist from 13.83
--- John Gibson, Mar  8 1990
        Added "sunfp" floating-point poplink option
--- John Gibson, Jan  7 1990
        Version 13.7 for new pointers.
--- John Gibson, Nov  1 1989
        Changed format of -Id_init_list- to include section in which
        initialisation done
--- John Gibson, Aug 18 1989
        Added -g option to deal with a group of similar files to go into
        a single object module.
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 17 1989
        Version 13.66
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, May  5 1989
        Version 13.6402 changes
--- Roger Evans, Mar 23 1989
        Modified do_asm to allow for #_INCLUDE of .s files (inside #_EXEC)
--- John Gibson, Jan 29 1989
        New version of popc
--- John Gibson, Nov 23 1988
        Changes to cope with lexical blocks in the VM.
--- Roger Evans, Sep 19 1988
        Fixed bug in macro arg handling = ',' should have been `,`
--- John Gibson, Sep  2 1988
        Added _SVB_OFFS macro for getting offset of an identifier into
        special var block
--- Roger Evans, Jul  4 1988
        Removed prohibition of arg values that look like flags
--- John Gibson, Jun 24 1988
        Renamed popc_main.p (was popas_main.p). Added $-Pop$-Main.
--- Roger Evans, Feb 24 1988
        Added -macro_args- to decode '+m' flag in saved image arguments
--- John Gibson, Feb  9 1988
        Further changes for sections, weakref, etc.
--- John Gibson, Jan 17 1988
        Changes for coping with sections, weakrefs, new format for assembler
        files, etc, etc.  Created this file from most of old 'popas.p' (which
        now just has load commands for building popas).
 */
