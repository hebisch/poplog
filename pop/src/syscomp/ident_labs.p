/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/syscomp/ident_labs.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                OUTPUT CODE GENERATION PROCEDURES (ALL SYSTEMS)

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'
#_INCLUDE 'wdefs.ph'

section $-Popas;

constant procedure (
    Weak_id         = newproperty([], 16, false, false),
    is_lexid_undef  = newproperty([], 16, false, false),
    label_of,
    assign_struct_label,
    ident_error,
    popc_updater,
    get_sfloat_int,
    read_paren_path,
    read_input_path_token,
    note_perm_ident,

    $-popc_auto_declare,
    $-popc_weakref_ident,
    );

constant
    undef_init  = '',   ;;; default value in Id_init property
    ;

vars
    procedure (equiv_labels, Id_type, Id_init, Id_idprops),
    const_assign_list, label_assign_list, quoted_dict_words,
    syspop_mode, syspop_mode_established, file_has_errors,
    ;


;;; --- LABEL PROCESSING ------------------------------------------------

define genlab() -> lab;
    lvars lab = nextlab();
    0 -> islabel(lab)
enddefine;

define lconstant prefixlabel(word, flags, asm_lab, prop_id) -> lab;
    lvars word, lab, flags, prop, prop_id, procedure asm_lab;
    unless isword(word) then false -> prop_id endunless;
    unless prop_id and (fast_idval(prop_id) -> prop, prop(word) ->> lab) then
        asm_lab(word) -> lab;
        flags || LAB_GLOBAL -> islabel(lab);
        if prop_id then lab -> prop(word) endif
    endunless
enddefine;

lvars procedure (
    constlab_prop,
    wordlab_prop,
    identlab_prop,
    wordidlab_prop,
    execlab_prop,
    updlab_prop,
    );

#_IF not(DEF asm_testdefval)
constant procedure asm_testdefval = asm_testdeflabel;
#_ENDIF

define symlabel =
    prefixlabel(% 0, asm_symlabel, ident constlab_prop %)
enddefine;
define wordlabel =
    prefixlabel(% LAB_LINK_ASSIGN, asm_wordlabel, ident wordlab_prop %)
enddefine;
define identlabel =
    prefixlabel(% LAB_LINK_ASSIGN, asm_identlabel, ident identlab_prop %)
enddefine;
define wordidlabel =
    prefixlabel(% LAB_LINK_ASSIGN, asm_wordidlabel, ident wordidlab_prop %)
enddefine;

define pdpropslabel =
    prefixlabel(% 0, asm_pdpropslabel, false %)
enddefine;
define testdeflabel =
    prefixlabel(% 0, asm_testdefval, false %)
enddefine;

define lconstant auxlabof(label, gen, pre, prop_id) -> lab;
    lvars lab, label, gen, pre, elab, prop_id,
        procedure prop = fast_idval(prop_id);
    unless prop(label) ->> lab then
        if gen then
            pre <> label ->> lab -> prop(label);
            islabel(label) -> islabel(lab);
            ;;; generate same for any equivalent labels
            fast_for elab in equiv_labels(label) do
                auxlabof(elab, true, pre, prop_id) ->
            endfast_for
        endif
    endunless
enddefine;

define execlabof    = auxlabof(% 'x', ident execlab_prop %) enddefine;

define updlabof() -> lab with_nargs 2;
    lvars lab;
    auxlabof((), 'u', ident updlab_prop) -> lab;
    if lab then execlabof(lab, true) -> endif
enddefine;

define lconstant label_assign(newlab, lab);
    lvars newlab, lab, alab, info;

    define lconstant add_equiv(newlab, lab);
        lvars newlab, lab;
        islabel(newlab) || (islabel(lab) && LAB_OF_STRUCT) -> islabel(newlab);
        newlab :: equiv_labels(lab) -> equiv_labels(lab)
    enddefine;

    if newlab = lab then
        return
    elseif (islabel(lab) ->> info) &&/=_0 LAB_LITERAL then
        ;;; literal (i.e. constant) label - put out assembler assignment
        conspair(newlab,lab) :: const_assign_list -> const_assign_list;
        islabel(newlab) fi_|| LAB_LITERAL -> islabel(newlab)
    elseif info &&/=_0 LAB_EXPR then
        ;;; non-constant label
        conspair(newlab,lab) :: label_assign_list -> label_assign_list;
        islabel(newlab) fi_|| (info&&LAB_OF_STRUCT) fi_|| LAB_EXPR
                                                    -> islabel(newlab)
    else
        add_equiv(newlab, lab);
        if execlabof(lab, false) ->> alab then
            add_equiv(execlabof(newlab,true), alab);
            if updlabof(lab, false) ->> alab then
                label_assign(updlabof(newlab,true), alab)
            endif
        endif
    endif
enddefine;

vars procedure (
    struct_label_prop,
    generateable_struct,
    );

lvars
    label_of_item = '',
    procedure (ident_name_prop, id_path_cache)
    ;

define init_labels();
    lvars x;
    appproperty(islabel,
        procedure(lab, info);
            lvars lab, info;
            info &&~~ (LAB_GENERATED||LAB_OF_STRUCT) -> islabel(lab)
        endprocedure);

    newproperty([], 64, false, false)   -> constlab_prop;
    newproperty([], 16, false, false)   -> wordlab_prop;
    newproperty([], 16, false, false)   -> identlab_prop;
    newproperty([], 8,  false, false)   -> wordidlab_prop;
    newproperty([], 32, false, false)   -> execlab_prop;
    newproperty([], 16, false, false)   -> updlab_prop;
    newproperty([], 8, [], false)       -> equiv_labels;
    newproperty([], 256, false, false)  -> ident_name_prop;
    newproperty([], 128, false, false)  -> struct_label_prop;
    newproperty([], 8, false, false)    -> id_path_cache;
    newproperty([], 16, false, false)   -> generateable_struct
enddefine;

define typedstring_from_lab(label);
    dlvars label;

    define lconstant cons_tstring(class, token);
        lvars class, token;
        consstring(#| class, explode(token) |#)
    enddefine;

    define lconstant do_prop(class, prop);
        lvars token, lab, class, prop;
        fast_for token, lab in_property prop do
            if lab == label then
                exitfrom(cons_tstring(class, token), typedstring_from_lab)
            endif
        endfor
    enddefine;

    returnif(_intval(label)) (cons_tstring(TYPESTR_INT, label));
    do_prop(TYPESTR_CONST,  constlab_prop);
    do_prop(TYPESTR_WORD,   wordlab_prop);
    do_prop(TYPESTR_IDENT,  identlab_prop);
    do_prop(TYPESTR_WORDID, wordidlab_prop);
    false
enddefine;


;;; --- IDENTIFIERS ----------------------------------------------------

define wordid_name_sect(word);
    lvars word, pair;
    if ispair(word_dict_status(word) ->> pair) then
        fast_destpair(pair)
    else
        word, false
    endif
enddefine;

define nonpop_trans(word, from_) -> word;
    lvars c, name, word, sect, from_, len;
    fast_word_string(wordid_name_sect(word) -> sect) -> name;
    returnif((datalength(name) ->> len) == 0);
    fast_subscrs(1, name) -> c;
    if from_ then
        unless c == `_` and len /== 1 then      ;;; exclude anon var "_"
            returnunless(c == `\^_` and syspop_mode);
            mishap(word, 1, 'ILLEGAL USE OF CTRL-_ IDENTIFIER')
        endunless
    else
        returnunless(c == `\^_`)
    endif;
    if syspop_mode then
        ;;; construct word with 1st char switched
        copy(name) -> name;
        c fi_||/& 64 -> fast_subscrs(1, name);
        consword(name),
            if sect then word_identifier((), sect, "undef") endif -> word
    endif;
    true -> syspop_mode_established
enddefine;

define nonpop_trans_vm(word);
    lvars word;
    if syspop_mode then
        word
    else
        dlocal syspop_mode = true;
        nonpop_trans(word, true)
    endif
enddefine;

define lconstant get_main_id(id) -> main_id;
    lvars id, main_id = id;
    if Weak_id(main_id) ->> id then id -> main_id endif;
enddefine;

define $- pas_ident_token(id);
    lvars id;
    ident_name_prop(get_main_id(id))
enddefine;

define updaterof $- pas_ident_token(token, id);
    lvars id, token;
    if isident(id) == "perm" then
        ;;; token must be a word
        nonpop_trans(token, true) -> ident_name_prop(get_main_id(id))
    else
        ;;; not interested in the names of lexical identifiers, except
        ;;; insofar as they're pop or nonpop
        if isword(token) and datalength(token) fi_>= 2
        and fast_subscrw(1, token) == `_` then
            if syspop_mode then "\^_" -> ident_name_prop(id) endif;
        endif
    endif
enddefine;

define recursive_app_sect_idents(sect, p);
    lvars l; dlvars sect, p;
    fast_app_sect_idents(sect, procedure; chain((), sect, p) endprocedure);
    section_subsect(sect) -> l;
    fast_for sect in l do recursive_app_sect_idents(sect, p) endfor;
    sys_grbg_list(l)
enddefine;

define get_ident_token(id) -> token;
    lvars id, token;
    dlvars main_id;

    define lconstant try_id(word, id, sect);
        lvars id, word, sect;
        returnunless(id == main_id);
        ;;; found it -- use sys_current_ident just to assign pas_ident_token
        sys_current_ident(word_identifier(word, sect, false)) -> ;
        exitfrom(pas_ident_token(id), get_ident_token)
    enddefine;

    returnif((pas_ident_token(id) ->> token) or isident(id) /== "perm");

    get_main_id(id) -> main_id;
    recursive_app_sect_idents(pop_section, try_id);
    ;;; failed to find it
    mishap(id, 1, 'NO NAME FOR IDENTIFIER')
enddefine;

define nonpop_ident(/*id*/) with_nargs 1;
    lvars token;
    if isword(pas_ident_token() ->> token) then
        wordid_name_sect(token) -> -> token;
        datalength(token) /== 0 and fast_subscrw(1, token) == `\^_`
    else
        false
    endif
enddefine;

define new_lex_id(nonpop) -> id;
    lvars id, nonpop, kind = "lextoken";
    if isword(nonpop) then (), nonpop -> (nonpop, kind) endif;
    consident(0, false, kind) -> id;
    if nonpop then
        "\^_" -> ident_name_prop(id)            ;;; make nonpop
    endif
enddefine;

define get_perm_ident(word) -> id;
    lvars id, word, trans, isweak = false;
    if ispair(word) then word -> isweak, fast_front(word) -> word endif;
    nonpop_trans(word, false) -> trans;
    if isweak then
        if word == trans then isweak else conspair(trans,fast_back(word)) endif
                -> trans
    endif;
    sys_use_current_ident(trans) -> (id, );
    if isident(id) /== "perm" then
        mishap(word, 1, 'PERMANENT IDENTIFIER NEEDED')
    endif
enddefine;

define path_wordid(path);
    lvars path, name, sect = pop_section;
    repeat
        f_dest(path) -> path -> name;
        quitif(path == []);
        section_subsect(name, sect, true) -> sect
    endrepeat;
    chain(name, sect, "undef", word_identifier)
enddefine;

define identof_path(path) -> id;
    lvars path, id;
    unless id_path_cache(path) ->> id then
        get_perm_ident(path_wordid(path)) ->> id -> id_path_cache(path)
    endunless
enddefine;

define test_identof_path(path) -> id;
    lvars path, id, wid;
    returnif(id_path_cache(path) ->> id);
    nonpop_trans(path_wordid(path), false) -> wid;
    popc_auto_declare(wid);
    if isdeclared(wid) ->> id then id -> id_path_cache(path) endif
enddefine;

lvars procedure svb_prop;
;;;
define get_svb_offset(token);
    lvars n, token, svb;

    define lconstant normalise_token(token) -> token;
        lvars token, n;
        if isword(token) then fast_word_string(token) -> token endif;
        returnunless(issubstring('$-', 1, token) ->> n);
        if n == 1 then
            unless issubstring('$-', 3, token) then
                allbutfirst(2, token) -> token
            endunless;
        else
            '$-' <> token -> token
        endif
    enddefine;

    unless isproperty(svb_prop) then
        newmapping([], 32, false, false) -> svb_prop;
        special_var_block -> svb;
        fast_for n to datalength(svb) do
            (n-1)*WORD_OFFS -> svb_prop(normalise_token(f_subv(n,svb)))
        endfor;
#_IF DEF special_var_block_neg
        ;;; table for negative offsets
        special_var_block_neg -> svb;
        fast_for n to datalength(svb) do
            -n*WORD_OFFS -> svb_prop(normalise_token(f_subv(n,svb)))
        endfor;
#_ENDIF
    endunless;

    svb_prop(normalise_token(token))
enddefine;

define read_input_svb_offset() -> offs;
    lvars offs, token = read_input_path_token();
    if get_svb_offset(token) ->> offs then
        syspop\:_int(offs) -> offs
    else
        syspop\:_int(0) -> offs;
        ident_error(token, '%p NOT IN _special_var_block')
    endif
enddefine;

define label_perm_init(init, globlab, do_lab_assign) -> lab;
    lvars init, globlab, do_lab_assign, lab, info;
#_IF DEF USE_LOCAL_LABELS
    label_of(init, isprocedure(init) or globlab) -> lab;
#_ELSE
    label_of(init, globlab) -> lab;
#_ENDIF
    returnunless(do_lab_assign and lab /== globlab);

    ;;; is a label/integer or was already labeled
    if (islabel(lab) ->> info) &&=_0 LAB_GLOBAL
    or (do_lab_assign == "constant" and info &&=_0 LAB_LINK_ASSIGN) then
        ;;; assign existing label to the global label
;;;     if info &&/=_0 LAB_EXPR then
;;;         ident_error(token, 'INVALID INITIALISATION FOR PERM IDENTIFIER %p')
;;;     endif;
        label_assign(globlab, lab)
    endif
enddefine;

define lconstant get_perm_id_arg(id_word_path, flags) -> (id, token);
    lvars id_word_path, flags, id, token;
    if isinteger(id_word_path) then
        ;;; flags for note_perm_ident
        (), id_word_path fi_|| flags -> (id_word_path, flags)
    endif;
    if islist(id_word_path) then
        identof_path(id_word_path)
    elseif isword(id_word_path) or ispair(id_word_path) then
        get_perm_ident(id_word_path)
    elseif isident(id_word_path) then
        id_word_path
    else
        mishap(id_word_path, 1, 'INVALID ARG TO perm_const_lab/perm_ident_lab')
    endif -> id;
    note_perm_ident(id, flags) -> token
enddefine;

define perm_ident_lab(id_word_path) -> lab;
    lvars id_word_path, token, lab;
    get_perm_id_arg(id_word_path, IDT_GEN_IDENT) -> (, token);
    identlabel(token) -> lab
enddefine;

vars perm_const_svb_check = false;
;;;
define perm_const_lab(id_word_path) -> lab;
    lvars   id_word_path, flags = IDT_VALUE_USED, id, token, lab, init,
            pcheck = false, globlab;
    if isboolean(id_word_path) then
        (), id_word_path -> (id_word_path, pcheck)
    endif;
    get_perm_id_arg(id_word_path, flags) -> (id, token);
    get_main_id(id) -> id;
    if pcheck then
        unless nonactive_isconstant(id)
        and nonactive_identtype(id) == "procedure" then
            ident_error(token, 'PERM IDENTIFIER %p EXPECTED TO BE A CONSTANT PROCEDURE')
        endunless
    elseunless nonactive_isconstant(id) then
        ident_error(token, 'PERM IDENTIFIER %p EXPECTED TO BE A CONSTANT')
    endif;

    ;;; see if there's an initialisation for the perm identifier
    ;;; and if so use whatever label it has
    symlabel(token) ->> lab -> globlab;     ;;; label of constant value
    Id_init(id) -> init;
    if init /== undef_init and init /== label_of_item
    and Id_type(id) &&=_0 IDT_INCREMENTAL then
        label_perm_init(init, lab, false) -> lab
    elseif nonactive_identtype(id) == "procedure" then
        execlabof(lab, true) ->
    endif;
    if perm_const_svb_check then
        ;;; check if in _special_var_block, return integer offset if so
        lab = globlab and get_svb_offset(token) -> perm_const_svb_check
    endif
enddefine;

define set_from_perm_const(item, wid);
    lvars wid, x, item;
    returnif(isprocedure(struct_label_prop(item) ->> x) or isstring(x));
    unless x then
        wid -> struct_label_prop(item)
    elseif ispair(x) then
        unless fast_lmember(wid, x) then
            x nc_<> (wid :: []) -> struct_label_prop(item)
        endunless
    elseunless x == wid then
        x :: (wid :: []) -> struct_label_prop(item)
    endunless;
    if isprocedure(item) and not(isundef(item)) then
        popc_updater(item) ->
    endif
enddefine;

;;; -----------------------------------------------------------------------

define assign_struct_label(baselab, item, not_gen);
    lvars baselab, item, upd, lab, ulab, elab, info, isproc, not_gen;
    unless islabel(baselab) ->> info then
        mishap(baselab, 1, 'LABEL NEEDED')
    endunless;
    isprocedure(item) -> isproc;
    baselab -> struct_label_prop(item);
    info || LAB_OF_STRUCT -> islabel(baselab);
    ;;; record that this is a generateable structure unless being called
    ;;; from genstructure (in which case there's no need)
    if not_gen then true -> generateable_struct(item) endif;
    if isproc then execlabof(baselab, true) -> endif;
    ;;; also for any equivalent labels
    fast_for elab in equiv_labels(baselab) do
        if isproc then execlabof(elab, true) -> endif;
        islabel(elab) || LAB_OF_STRUCT -> islabel(elab)
    endfast_for;
    returnunless(isproc and (updater(item) ->> upd));

    ;;; assign updater lab
    updlabof(baselab, true) -> ulab;
    if (label_of(upd, false) ->> lab) then
        label_assign(ulab, lab ->> ulab)
    else
        assign_struct_label(ulab, upd, true)
    endif;
    fast_for elab in equiv_labels(baselab) do
        label_assign(updlabof(elab,true), ulab)
    endfast_for
enddefine;

define label_of(item, want_lab);
    lvars item, dk, lab_data, token, want_lab;
    lconstant IDT_GEN_FULL = IDT_GEN_IDENT || IDT_GEN_FULL_ID;
    dlocal label_of_item = item;

    ;;; item is undef or a proper procedure
    define lconstant find_perm_const(item);
        dlvars item, try_word;

        define lconstant try_sect(sect);
            lvars wid;
            dlvars sect;

            define lconstant try_id(word, id);
                lvars x, id, word, wid;
                returnunless(nonactive_isconstant(id));
                fast_idval(id) -> x;
                if x == item then
                    word_identifier(word, sect, false) -> wid;
                    set_from_perm_const(item, wid);
                    exitfrom(wid, find_perm_const)
                elseif isprocedure(x) and updater(x) == item then
                    word_identifier(word, sect, false) -> wid;
                    set_from_perm_const(x, wid);
                    popc_updater(x) -> ;
                    exitfrom(x, find_perm_const)
                endif
            enddefine;

            if try_word then
                if word_identifier(try_word, sect, false) ->> wid then
                    try_id(try_word, isdeclared(wid))
                endif
            else
                fast_app_sect_idents(sect, try_id)
            endif;
            fast_for sect in section_subsect(sect) do try_sect(sect) endfor
        enddefine;

        if isundef(item) then
            undefword(item)
        elseif isprocedure(item) then
            pdprops(item)
        else
            false
        endif -> try_word;
        if isword(try_word) then try_sect(pop_section) endif;
        false -> try_word;
        try_sect(pop_section);
        false
    enddefine;

    ;;; get lab for appropriate undef structure u
    define lconstant get_standard_undef(u);
        lvars   u, mult = false, idprops, postfix, name, wordid, pdpt,
                lab, upd = false;
        if isprocedure(u) then
            if (pdpart(u) ->> pdpt) == identfn then
                ;;; active undef
                datalength(u) -> mult
            elseif pdpt == erasenum then
                ;;; active undef updater
                true -> upd;
                fast_frozval(2,u)-1 -> mult
            endif;
            if mult then
                if mult == 0 then false else fast_frozval(1,u) endif -> u
            endif
        endif;
        if isprocedure(u) then
            ;;; procedure type
            unless mult or updater(u) then true -> upd endunless;
            "procedure", '_p'
        else
            ;;; untyped
            0, '_u'
        endif -> (idprops, postfix);
        if mult then
            conspair(idprops, mult) -> idprops;
            'active' sys_>< mult
        else
            'normal'
        endif -> name;
        consword(name <> postfix) -> name;
        path_wordid([Sys Undef ^name]) -> wordid;
        sysSYNTAX(wordid, idprops, 2:11);       ;;; weak constant ...
        ;;; these must only be referred to weakly, so that poplink generates
        ;;; undefs for them
        perm_const_lab(popc_weakref_ident(wordid, false, false)) -> lab;
        islabel(lab) || LAB_LINK_ASSIGN -> islabel(lab);
        if upd then updlabof(lab, true) else lab endif
    enddefine;

    define lconstant lab_from_perm_const(lab_data);
        lvars id, main_id, lab_data, const_id, token;

        define lconstant get_id(id) -> id;
            lvars id;
            if isword(id) then get_perm_ident(id) -> id endif;
            if Id_type(id) &&=_0 IDT_DECLARED and identprops(id) == "macro"
            then
                false -> id
            endif
        enddefine;

        returnif(isprocedure(lab_data))
                    (updlabof(label_of(lab_data, false), true));

        if ispair(lab_data) then
            ;;; list of (word) identifiers
            false -> const_id;
            fast_for id in lab_data do
                nextunless(get_id(id) ->> id);
                get_main_id(id) -> main_id;
                unless const_id then main_id -> const_id endunless;
                if main_id == const_id and id /== main_id then
                    note_perm_ident(id, IDT_VALUE_USED) ->
                endif
            endfor
        else
            ;;; single (word) identifier
            get_id(lab_data) -> const_id
        endif;
        if const_id then perm_const_lab(const_id) else false endif
    enddefine;

    define lconstant string_props_id(p);
        lvars s, l, id, word, upd, p;
        returnunless(
            isstring(pdprops(p) ->> s) and (datalength(s) ->> l) fi_> 2
             and f_subs(1,s) == `(` and f_subs(l,s) == `)`
                    ) (false);
        dlocal proglist_state = proglist_new_state(stringin(s));
        read_paren_path(true) -> word -> upd;
        get_perm_ident(word) -> id;
        unless nonactive_identtype(id) == "procedure" then
            ident_error(word, 'PERM IDENTIFIER %p EXPECTED TO BE PROCEDURE-TYPE');
        endunless;
        perm_const_lab(id), if upd then updlabof((), true) endif
    enddefine;


    if isintegral(item) and integer_length(item) <= POPINT_BITS then
        return(syspop\:_int(popint(item)))
    elseif issimple(item) and isdecimal(item) then
        ;;; decimal -- mark decimal_key 'used'
;;;     perm_const_lab([decimal_key]) -> ;
        return(get_sfloat_int(item, true))
    elseif islabel(item) then
        return(item)
    elseif isstring(struct_label_prop(item) ->> lab_data) then
        return(lab_data)
    elseunless lab_data and (lab_from_perm_const(lab_data) ->> lab_data) then
        data_key(item) -> dk;
        if dk == ident_key then
            if note_perm_ident(item, IDT_GEN_FULL) ->> token then
                ;;; dictionary permanent identifier
                identlabel(token) -> lab_data
            endif
        elseif dk == word_key then
            if word_dict_status(item) ->> dk then
                ;;; dictionary quoted word
                if ispair(dk) then
                    ;;; word identifier
                    note_perm_ident(item, #_< IDT_GEN_FULL||IDT_GEN_WORDID >_#)
                                                        -> token;
                    wordidlabel(token)
                else
                    unless fast_lmember(item, quoted_dict_words) then
                        item :: quoted_dict_words -> quoted_dict_words
                    endunless;
                    wordlabel(item)
                endif -> lab_data
            endif
        elseif dk == pdprops_label_key then
            pdpropslabel(note_perm_ident(fast_cont(item),
                                #_< IDT_GEN_PDPROPS || NPA_WEAK_OPTION >_#))
                                                        -> lab_data
        elseif dk == testdef_label_key then
            testdeflabel(note_perm_ident(fast_cont(item), IDT_GEN_TESTLAB ))
                                                        -> lab_data
        elseif dk == undef_key then
            if undefword(item)
            or (isinheap(item) and not(is_lexid_undef(item))) then
                find_perm_const(item)
            else
                ;;; assume it's a system standard undef
                get_standard_undef(item)
            endif -> lab_data
        elseif dk == procedure_key then
            unless string_props_id(item) ->> lab_data then
                find_perm_const(item) -> lab_data
            endunless
        elseif dk == key_key then
            class_dataword(item) <> "_key" -> dk;
            if class_attribute(item, "prop_entry") ->> lab_data then
                lab_data <> "_" <> dk -> dk
            endif;
            test_identof_path([Sys ^dk]) -> lab_data
        elseif dk == popc_pointer_key then
            ;;; item is a pair
            returnif(isstring(want_lab) and back(item) == 0)
                                        (label_of(front(item), want_lab));
            label_of(front(item), true) -> dk;
            dk label_+ back(item) -> lab_data;
            if islabel(dk) &&/=_0 LAB_OF_STRUCT then
                assign_struct_label(lab_data, item, true)
            endif
        endif
    endif;

    if lab_data then
        unless isstring(lab_data) then
            lab_from_perm_const(lab_data) -> lab_data
        endunless;
        lab_data -> struct_label_prop(item)
    elseif want_lab then
        if isstring(want_lab) then want_lab else genlab() endif -> lab_data;
        assign_struct_label(lab_data, item, true)
    endif;
    lab_data
enddefine;

define lconstant setup_selected_consts();
    lvars item, pair, word, lab;
    lconstant other_uniques = [^nullstring nullstring ^termin termin];

    define lconstant set_unique(item, word);
        lvars item, word;
        false -> struct_label_prop(item);
        set_from_perm_const(item, word_identifier(word, pop_section, false))
    enddefine;

    fast_for item, pair in_property poplink_unique_struct do
        fast_front(pair) -> word;
        set_unique(item, word);
        symlabel(word) -> lab;
        islabel(lab) || LAB_LINK_ASSIGN -> islabel(lab)
    endfor;

    other_uniques -> pair;
    until pair == [] do
        set_unique(f_dest(f_dest(pair)) -> pair)
    enduntil;

    define lconstant do_id(word, id, sect);
        lvars item, word, id, wid, sect;
        fast_idval(id) -> item;
        unless (isprocedure(item) and not(isclosure(item)))
                or isundef(item)
                or (isword(item) and word_dict_status(item))
                or issimple(item)
                or struct_label_prop(item)
                or not(nonactive_isconstant(id))
        then
            set_from_perm_const(item, word_identifier(word, sect, false))
        endunless
    enddefine;

    recursive_app_sect_idents(pop_section, do_id)
enddefine;

define assign_perm_init_labels(init_list) -> file_unique;
    lvars   lab, init, token, info, id, slab, init_list, init_sect, const,
            ipair, name, file_unique = false;

    setup_selected_consts();

    until init_list == [] do
        ;;; id and section in which it was initialised
        f_dest(init_list) -> (id, ipair);
        f_dest(ipair) -> (init_sect, init_list);

        get_ident_token(id) -> token;
        if nonactive_isconstant(id) ->> const then
            fast_word_string(token) -> name
        else
            ;;; for variable, use dummy subsection <blank> of section
            ;;; in which initialisation occurred (allows for different
            ;;; initialisations from different sections)
            section_pathname(init_sect) <> '$-$-'
                    sys_>< (wordid_name_sect(token) ->) ->> name -> token
        endif;

        ;;; use the alphabetically-first name in the file as the
        ;;; root for generating other file-unique labels
        unless file_unique and alphabefore(file_unique, name) then
            name -> file_unique
        endunless;

        ;;; replace init_sect with label
        External_dummy_id(id) or symlabel(token) ->> slab -> f_hd(ipair);

        unless Id_type(id) &&/=_0 IDT_INCREMENTAL then
            label_perm_init(Id_init(id), slab, not(const) or "constant") ->
        endunless
    enduntil
enddefine;


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan 13 1996
        Excluded "_" as a nonpop ident
--- John Gibson, Oct  7 1994
        # Made get_svb_offset construct a newmapping property from
          special_var_block to speed up search
        # Made perm_const_lab use get_svb_offset on constants when requested
--- John Gibson, Oct 16 1992
        14.22 changes
--- John Gibson, Jul 21 1992
        Version 14.21 changes
--- John Gibson, Nov  1 1989
        Perm variable init labs now generated in dummy subsection of
        section in which initialisation done
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, Jan 29 1989
        Split from genstruct.p, revised for new version
 */
