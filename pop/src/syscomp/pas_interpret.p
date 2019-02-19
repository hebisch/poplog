/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/syscomp/pas_interpret.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                    VM (pas_) CODE INTERPRETER

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas;

constant
    procedure (ident_error)
    ;

vars
    procedure (from_perm_const, pre_updaters),
    Id_init_list
    ;

vars
    popc_perm_assign_mode   = false,
    current_dlocal_ids      = [],
    ;

lconstant
    pas_interpret_p = newactproperty([], 16, false, true,
                            procedure();
                                mishap(->, 1, 'UNKNOWN VM INSTRUCTION (interpret)')
                            endprocedure);

lconstant procedure na_shadow_idval;

lvars
    lvar_locals,
    vm_codelist,
    vm_instr,
    ;

define lconstant Get_ident(item, defer) -> opnd;
    lvars item, opnd = item, word, defer, is_id;
    unless isident(item) ->> is_id then
        mishap(item, 1, 'INVALID OPERAND FOR Get_ident')
    endunless;
    returnunless(is_id == "lextoken");
    ;;; lexical token -- idval contains the translation, which is either
    ;;; a non-local lex ident or an integer subscript into -lvar_locals-
    idval(item) -> opnd;        ;;; get translation
    if isident(identtype(item)) then
        ;;; lvar will contain a run-time ident
        f_subv(opnd, lvar_locals) -> opnd;
        if defer then nonactive_idval(opnd) -> opnd endif
    elseif isinteger(opnd) then
        if defer == true then
            f_subv(opnd, lvar_locals) -> opnd
        else
            ;;; can't quote it
            mishap(item, 1, 'INVALID QUOTED OPERAND FOR Get_ident')
        endif
    endif
enddefine;

define lconstant val_id = Get_ident(%true%) <> nonactive_idval enddefine;
define lconstant val_qid= Get_ident(%false%) <> nonactive_idval enddefine;

lvars
    next_call_num       = 0,
    my_dlocal_context   = false,
    current_procedure,
;


    /*  Used only as
            vm_interpret(% lvar_depth, unique_ID %) <> identfn
    */
define vm_interpret(lvar_depth, unique_ID);
    lvars   id, pair, d_locals, l_locals, nlgoto_var, rtid_args, lblock_instr,
            pdr_nl_labels, dloc_rtid_list, dlexpr_list, pdr_entry_type,
            lvar_depth, unique_ID, save_d_locals, dlocals_saved, codelist;

    dlvars  lx, lx_index = 0;

    dlocal  lvar_locals, current_dlocal_ids, my_dlocal_context = false,
            current_procedure = caller(1);

    lconstant
            VARS = 1, GET = 2, PUT = 3,
            NORMAL = 1, ABNORMEXIT = 2, SUSPEND = 3, RESUME = 4,
        ;

    define lconstant interpret_vmcode(vm_codelist);
        dlocal vm_codelist, vm_instr;
        while vm_codelist /== [] do
            dest(vm_codelist) -> (vm_instr, vm_codelist);
            if isvector(vm_instr) then
                pas_interpret_p(f_subv(1, vm_instr))()
            endif
        endwhile
    enddefine;

    define lconstant do_lxcode(el, my_dlocal_context);
        lvars el; dlocal my_dlocal_context;
        interpret_vmcode(lx(el))
    enddefine;

    define lconstant action(context);
        lvars context;
        false -> dlocals_saved;
    enddefine;
    ;;;
    define updaterof action(context);
        lvars pair, context;
        returnunless(dlocals_saved);
        ;;; dlexpr code for exit
        until lx_index == 0 do
            subscrl(lx_index, dlexpr_list) -> lx;
            lx_index - 1 -> lx_index;
            applist(lx(VARS), val_id);
            do_lxcode(PUT, context)
        enduntil;
        for pair in dloc_rtid_list do
            ;;; front is save var, back is rtid -- restore value
            val_id(front(pair)) -> val_id(back(pair))
        endfor;
        save_d_locals -> maplist(d_locals, na_shadow_idval)
    enddefine;

    dlocal 0 % action(dlocal_context) %;

    explode(gen_procedure_args(current_procedure))
                    -> /* struct tab (always []) */
                    -> /* flags (always 0) */
                    -> codelist,
                    -> pdr_entry_type,
                    -> dlexpr_list,
                    -> dloc_rtid_list,
                    -> pdr_nl_labels,
                    -> lblock_instr,
                    -> rtid_args,
                    -> nlgoto_var,
                    -> l_locals,
                    -> d_locals;

    d_locals <> current_dlocal_ids -> current_dlocal_ids;
    maplist(d_locals, na_shadow_idval) -> save_d_locals;
    {% repeat lvar_depth times consident(0, false, "lex") endrepeat %}
                                                        -> lvar_locals;
    for id in rtid_args do -> val_qid(id) endfor;
    for pair in dloc_rtid_list do
        ;;; front is save var, back is rtid -- save in save var
        val_id(back(pair)) -> val_id(front(pair))
    endfor;
    true -> dlocals_saved;

    if nlgoto_var then
        ;;; generate integer to identify this call
        next_call_num fi_+ 1 ->> next_call_num -> val_id(nlgoto_var)
    endif;

    if lblock_instr then lblock_instr :: codelist -> codelist endif;

    ;;; dlexpr code for normal entry
    for lx in dlexpr_list do
        do_lxcode(GET, NORMAL) -> applist(lx(VARS), val_id);
        lx_index + 1 -> lx_index
    endfor;

    interpret_vmcode(codelist)
enddefine;

lvars restricted_autolist = false;
lvars restricted_useslist = false;
;;;
define lconstant restricted_autoload(word);
    lvars word, l;
    dlocal popautolist;

    ;;; redefine loadlib to deal with uses-by_name autoloading
    define dlocal loadlib(name);
        lvars name;
        dlocal subsystem_compile_warn = erase;
        if islist(name) then () -> name endif;
        subsystem_libcompile(name, restricted_useslist) ->
    enddefine;

    unless restricted_autolist then
        [%  '$poplocalauto/' dir_>< nullstring,
            '$popautolib/' dir_>< nullstring
        %] -> restricted_autolist;

        [%  ident popautolist,
            '$poplocal/local/lib/' dir_>< nullstring,
            popliblibdir
        %] -> restricted_useslist
    endunless;

    restricted_autolist -> popautolist;
    sys_autoload(word) ->
enddefine;

define lconstant na_shadow_idval(id) -> val;
    lvars main_id, id, val, token, status, isweak = false, shadowing;
    if isident(id) /== "perm" then
        ;;; lexical
        return(nonactive_idval(id) -> val)
    elseif Weak_id(id) ->> main_id then
        true -> isweak
    else
        id -> main_id
    endif;

    word_dict_status(get_ident_token(main_id) ->> token) -> status;
    status and (popc_perm_assign_mode or Id_init(main_id) /== undef_init)
                                                -> shadowing;
    if shadowing and Id_type(main_id) &&/=_0 IDT_INCREMENTAL then
        note_perm_ident(id, IDT_INCR_VAL_USED) ->
    endif;

    returnif(shadowing and (Id_init(main_id) ->> val) /== undef_init);

    ;;; not directly executing, or not a dictionary permanent identifier,
    ;;; or dict perm identifier with no popc value -- return idval
    unless isweak or isdefined(main_id) or status /== true
    or (shadowing and nonpop_ident(main_id)) then
        ;;; Try autoloading, but only from those directories
        ;;; containing code that would be supported in the safepop11
        ;;; we're running in. (This restriction is not made in
        ;;; popc_synmac_idval below, since syntax words must be made
        ;;; available.)
        restricted_autoload(token)
    endunless;
    nonactive_idval(main_id) -> val;
    if shadowing and iscompound(val) and nonactive_isconstant(main_id) then
        set_from_perm_const(val, id)
    endif
enddefine;
;;;
define updaterof na_shadow_idval(val, id);
    lvars id, val, token, main_id, init;

    define lconstant add_to_init_list(id);
        lvars id;
        unless fast_lmember(id, Id_init_list) then
            ;;; remember section in which id initialised
            current_section :: (id :: Id_init_list) -> Id_init_list
        endunless;
    enddefine;

    if isident(id) /== "perm" then
        ;;; lexical
        val -> nonactive_idval(id);
        return
    elseunless Weak_id(id) ->> main_id then
        id -> main_id
    endif;

    unless (popc_perm_assign_mode or Id_init(main_id) /== undef_init)
    and (note_perm_ident(id, 0) ->> token) then
        ;;; not directly executing, or not a dictionary permanent identifier
        val -> nonactive_idval(main_id);
        return
    endunless;

    Id_init(main_id) -> init;

    if Id_type(main_id) &&/=_0 IDT_INCREMENTAL then
        if isproperty(init) then
            if isproperty(val) then
                ;;; directly assigning a property -- take this to mean
                ;;; the file provides the 'core' initialisation.
                ;;; *** should check the properties are the same ***
                add_to_init_list(main_id);
                appproperty(val, procedure(a,v);
                                    lvars a,v;
                                    v -> init(a)
                                 endprocedure);
            else
                ident_error(token, 'INVALID INITIALISATION FOR INCREMENTAL PROPERTY %p')
            endif
        elseif (islist(init) and islist(val))
                or (isprocedure(init) and isprocedure(val))
        then
            val -> Id_init(main_id);
            if Id_type(main_id) &&=_0 IDT_INCR_VAL_USED then
                ;;; value not previously accessed -- take this to mean
                ;;; the file provides the 'core' initialisation
                add_to_init_list(main_id)
            endif
        else
            ident_error(token, if islist(init) then
                                    'ASSIGNING NON-LIST TO INCREMENTAL LIST %p'
                               else
                                    'ASSIGNING NON-PROCEDURE TO INCREMENTAL PROCEDURE %p'
                               endif)
        endif;
        return
    elseif nonactive_identtype(main_id) == "procedure"
    and not(isprocedure(val)) then
        ident_error(token, 'ASSIGNING NON-PROCEDURE TO PROCEDURE IDENTIFIER %p');
        return
    elseif nonactive_isconstant(main_id) then
        if id /== main_id then
            ident_error(token, 'WEAK INITIALISATION FOR PERM CONSTANT %p')
        elseif init /== undef_init then
            ident_error(token, 'MULTIPLE INITIALISATION FOR PERM CONSTANT %p')
        endif
    elseif fast_lmember(main_id, current_dlocal_ids) then
        if init == undef_init then
            val -> nonactive_idval(main_id)
        else
            val -> Id_init(main_id)
        endif;
        return
    endif;

    val -> Id_init(main_id);
    add_to_init_list(main_id);

    if pre_updaters(main_id) then
        pre_updaters(main_id) -> popc_updater(val);
        false -> pre_updaters(main_id)
    endif
enddefine;

    /*  Called from sys_current_val
    */
define global $-popc_idval(id, nonactiv);
    lvars id, nonactiv, val;
    dlocal popc_perm_assign_mode = true;
    na_shadow_idval(id) -> val;
    if not(nonactiv) and isactive(id) and isident(id) /== "lextoken" then
        val()
    else
        val
    endif
enddefine;
;;;
define updaterof $-popc_idval(id, nonactiv);
    lvars id, nonactiv;
    dlocal popc_perm_assign_mode = true;
    if not(nonactiv) and isactive(id) and isident(id) /== "lextoken" then
        -> na_shadow_idval(id)()
    else
        -> na_shadow_idval(id)
    endif
enddefine;

    /*  Called from Next_valof in itemread.p to get a popc value for
        a weakly-declared macro or syntax word
    */
define global $-popc_synmac_idval(word);
    lvars id = isdeclared(word), word, val = Id_init(id);
    lconstant gave_value = newproperty([], 8, false, "tmparg");

    returnif(val == undef_init) (valof(word));
    ;;; has a value, but try autoloading it first
    unless gave_value(Id_init) then
        ;;; started new file
        clearproperty(gave_value);
        true -> gave_value(Id_init)
    endunless;

    unless gave_value(id) then
        sys_autoload(word) -> ;
        returnif(isdefined(id)) (idval(id));
        true -> gave_value(id)
    endunless;
    val
enddefine;

define do_leftover_updaters();

    define lconstant do_upd(id, upd);
        lvars id, upd, p;
        idval(identof_path([Sys Exec_only_updater]))(%0%) -> p;
        p -> frozval(1, p);
        pdprops(upd) -> pdprops(p);
        p -> popc_idval(id, true)
    enddefine;

    appproperty(pre_updaters, do_upd)   ;;; NOT fast!!
enddefine;


;;; --- POPC VERSIONS OF PROCEDURES ---------------------------------------

define popc_updater(p) -> upd;
    lvars p, upd;
    if (updater(p) ->> upd) and struct_label_prop(p)
    and not(struct_label_prop(upd)) then
        p -> struct_label_prop(upd)
    endif
enddefine;
;;;
define updaterof popc_updater(upd, p);
    lvars upd, p;
    updater(p) -> ;     ;;; check it's a procedure
    if struct_label_prop(p) then
        mishap(p, 1, 'INVALID ASSIGNMENT TO UPDATER')
    endif;
    upd -> updater(p)
enddefine;

define lconstant my_chain(p);
    lvars p;
    chainfrom(current_procedure, p)
enddefine;

define lconstant my_dlcontext(iscontext);
    lvars iscontext;
    if my_dlocal_context then
        iscontext and my_dlocal_context     ;;; false for process
    else
        mishap(0, 'INVALID USE OF dlocal_context, ETC')
    endif
enddefine;

define lconstant popc_alt_pdr =
    newassoc([
        [% updater,                     popc_updater %]
        [% chain,                       my_chain %]
        [% fast_chain,                  my_chain %]
        [% nonactive dlocal_context,    my_dlcontext(%true%) %]
        [% nonactive dlocal_process,    my_dlcontext(%false%) %]
    ])
enddefine;

    ;;; make default value be procedure given as arg
erase -> property_active(popc_alt_pdr); ;;; i.e. erase prop, leave arg


;;; --- SYNTAX FOR INTERPRETER PROCEDURES ----------------------------------

define :define_form lconstant instr;
    lvars idname = readitem();
    [define lconstant ^idname ^^proglist] -> proglist;
    pop11_comp_expr();
    sys_current_val(idname) -> pas_interpret_p(idname)
enddefine;

;;; -----------------------------------------------------------------------

define :instr pas_PUSH();
    na_shadow_idval(Get_ident(vm_instr(2), true))
enddefine;

define :instr pas_PUSHQ();
    vm_instr(2)
enddefine;

define :instr pas_PUSHS();
    dup()
enddefine;

define :instr pas_SWAP();
    lvars i = vm_instr(2), j = vm_instr(3), x;
    subscr_stack(i) -> x;
    subscr_stack(j) -> subscr_stack(i);
    x -> subscr_stack(j)
enddefine;

define :instr pas_IDENT();
    Get_ident(vm_instr(2), "rtid")
enddefine;

define :instr pas_UPASSIGN(val);
    lvars id, val, init;
    Get_ident(vm_instr(2), true) -> id;
    unless note_perm_ident(id, 0) then
        ;;; not a dictionary permanent identifier
        val -> popc_updater(nonactive_idval(id))
    else
        if (Id_init(id) ->> init) == undef_init then
            val -> pre_updaters(id)
        else
            val -> popc_updater(init)
        endif
    endunless
enddefine;

define :instr pas_POP();
    () -> na_shadow_idval(Get_ident(vm_instr(2), true))
enddefine;

define :instr pas_ERASE();
    erase()
enddefine;

define :instr pas_CALL();
    popc_alt_pdr(na_shadow_idval(Get_ident(vm_instr(2), true))) ()
enddefine;

define :instr pas_CALLQ();
    popc_alt_pdr(vm_instr(2))()
enddefine;

define :instr pas_CALLS();
    popc_alt_pdr()()
enddefine;

define :instr pas_UCALL();
    -> popc_alt_pdr(na_shadow_idval(Get_ident(vm_instr(2), true))) ()
enddefine;

define :instr pas_UCALLQ();
    -> popc_alt_pdr(vm_instr(2))()
enddefine;

define :instr pas_UCALLS();
    -> popc_alt_pdr()()
enddefine;

define lconstant jump_to_lab(lab);
    lvars lab;
    ;;; dereference label
    while ispair(f_front(lab)) do
        f_front(lab) -> lab
    endwhile;
    lab -> vm_codelist
enddefine;

define lconstant intercond(x, bool);
    lvars x, test = vm_instr(3), bool;
    if (x and test) or not(x or test) then
        if bool then x endif;
        jump_to_lab(vm_instr(2))
    endif
enddefine;

define :instr pas_IF();
    intercond(false)
enddefine;

define :instr pas_BOOL();
    intercond(true)
enddefine;

define :instr pas_GOTO();
    jump_to_lab(vm_instr(2))
enddefine;

define :instr pas_NL_GOTO();
    lvars id = vm_instr(3);

    define lconstant nl_goto(plab, call_num);
        lvars plab, call_num, lab, id, owner_p;
        dest_procedure_label(plab) -> lab -> id -> owner_p;
        if caller(2) /== owner_p
        or (call_num and fast_idval(id) /== call_num) then
            chain(plab, call_num, nl_goto, chain)
        else
            ;;; reached target (caller(1) is vm_interpret)
            jump_to_lab(lab)
        endif
    enddefine;

    chain(vm_instr(2), if id then val_qid(id) else false endif, nl_goto)
enddefine;

define :instr pas_LBLOCK();
    lvars id, idprops, m, s_list, f_list, clos_inits;
    explode(vm_instr) -> (, s_list, f_list, clos_inits);
    ;;; initialise rt idents
    fast_for id in s_list <> f_list do
        identtype(identtype(id)) -> idprops;
        if (isactive(id) ->> m) then conspair(idprops, m) -> idprops endif;
        consident(idprops, false, "lex") -> val_qid(id)
    endfor;
    ;;; code to initialise local lvars to cache lexical closures
    fast_for id in clos_inits do false -> val_id(id) endfast_for
enddefine;

define :instr pas_LEX_CLOSURE() -> lex_clos;
    lvars clos_template = vm_instr(2), lex_clos, upd;
    fill(copy(clos_template)) -> lex_clos;
    updater(clos_template) -> upd;
    if isclosure(upd) then
        fill(copy(upd)) -> updater(lex_clos)
    endif
enddefine;

define :instr pas_CHECK();
enddefine;

define :instr pas_TYPE_CHECK();
    lvars key = vm_instr(2);
    unless datakey(dup()) == key then
        mishap((), 1, lowertoupper(class_dataword(key))
                            sys_>< ' NEEDED (e.g. assigning to identifier)')
    endunless
enddefine;


    ;;; bits in -exptr- argument
lconstant macro (
    MODE_LEVEL          = 2:1e8 - 1,    ;;; mask for access level
    MODE_ADDR_MODE      = 2:1e9,        ;;; address access
    );

    ;;; {pas_FIELD <field num> <spec> <checking> <exptr> <update>}
define :instr pas_FIELD();
    lvars   exlevels, m, want, acc_p, item,
            (, n, spec, checking, exptr, upd) = explode(vm_instr);
    unless exptr then 0 -> exptr endunless;
    exptr && MODE_LEVEL -> exlevels;
    if exlevels == 0 or isref(spec) then
        ;;; pop access or exptr value access/update -- use cons_access
        if upd then true else consref(true) endif -> want;
        if islist(spec) then
            ;;; record field
            1 -> m;
            [%  fast_for item in spec do
                    nextif(item == ">->");
                    if m == n then want else false endif;
                    m+1 -> m
                endfor
            %] -> want
        elseunless isref(spec) then
            ;;; vector element
            if n then () -> item, n, item endif
        endif;
        cons_access(want, spec, checking, exptr) -> acc_p;
        if islist(want) then
            sys_grbg_list(want);
            acc_p(n) -> acc_p
        endif;
        if upd then -> acc_p() else acc_p() endif;
        return;
    endif;

    ;;; other external -- only allows getting struct field offset with
    ;;; address mode plus null pointer. (This must be done for the struct
    ;;; as it will be on the target machine, not the host one.)
    if exptr &&/=_0 MODE_ADDR_MODE and exlevels == 1
    and () = null_external_ptr then
        false -> subscrv(4,vm_instr);
        exacc_field_offset(vm_instr)
    else
        mishap(0, 'UNSUPPORTED COMPILE-TIME EXTERNAL ACCESS')
    endif
enddefine;

define :instr pas_END();
enddefine;

endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct  5 1995
        Fixed to interpret chain and fast_chain correctly
--- John Gibson, Sep 26 1995
        Fixed bug in pas_FIELD -- was not getting the n-th element of
        the vector returned by cons_access.
--- John Gibson, Aug 11 1995
        Replaced some calls of f_hd with f_front (label in an instruction
        may be a pair, not a list).
--- John Gibson, Jun  8 1993
        Made vm_interpret cope with dlocal expressions.
        Moved popc_pdrs.p stuff into this file and removed latter file.
--- John Gibson, Oct 16 1992
        14.22 changes
--- John Gibson, Oct  9 1992
        Added pas_FIELD
--- John Gibson, Oct  2 1992
        Added instr define form
--- John Gibson, Nov  3 1991
        Fixed na_shadow_idval not to autoload nonpop idents
--- John Gibson, Nov  1 1989
        Changed format of -Id_init_list- to include section in which
        initialisation done
--- John Gibson, Jul 17 1989
        Version 13.66
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, May  5 1989
        Version 13.6402 changes
--- John Gibson, Apr 26 1989
        Version 13.64 changes
--- John Gibson, Feb 16 1989
        Interpreter now copes with pas_NL_GOTO, etc
--- John Gibson, Feb 13 1989
        Vm code procedures now represented by <> procedure
--- John Gibson, Jan 29 1989
        Split from popc_main.p
 */
