/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/auto/exload.p
 > Purpose:         Syntax for external loading
 > Author:          John Gibson, Jul  4 1990 (see revisions)
 > Documentation:   REF *EXTERNAL
 */
compile_mode:pop11 +strict;

section $-typespec_utils =>
        exload_batch
        endexload_batch,
        exload_do_batch_load,
        exload_strict_batching,
        exload,
        endexload,
        ;

lconstant names = [
    exload_batch endexload_batch exload_do_batch_load
    exload endexload
    popclosebracket
];
applist(names, sysunprotect);

uses
    typespec_utils,
    p_typespec,
    exacc,
    exload_merge_objfiles,
;

vars
    exload_strict_batching  = false,    ;;; set true for strict batching
    exld_batch_count        = 0,    ;;; number of nested exload_batch commands
    ;

lvars
    batch_buffer = false,;;; vector of things to pass external_do_load
    batching,       ;;; are we batching identifiers at the moment
    reloading,      ;;; are we reloading identifiers at the moment
    langname,       ;;; the current language string for identifiers
    prefix,         ;;; the current prefix word for (pop) identifiers
    symbol_type,    ;;; type of symbols
;

lconstant
    ;;; get some undef records
    rt_undef_rec    = copy(pop_undef),
    rt_undefp_rec   = copy(pop_undef_p),

    SYM_ID      = 4,
    SYM_ACC_P   = 5,
;



;;; BATCH LOADING PROCEDURES

;;; exload_isundef:
;;;     a name is undefined if it has no current ident associated with it
;;;     or its value is undef and the ident doesn't already appear in the
;;;     load buffer.

define exload_isundef(idname);
    lvars id, idname, v;

    define lconstant isbatched(id);
        lvars id, symspec;
        if batch_buffer then
            fast_for symspec in subscrv(1,batch_buffer) do
                returnif(subscrv(SYM_ID,subscrv(5,symspec)) == id) (true);
            endfor;
        endif;
        false;
    enddefine;

    not((sys_current_ident(idname) ->> id) and isdefined(id))
    or (isundef(idval(id) ->> v)
        and v /== rt_undef_rec and v /== rt_undefp_rec
        and not(isbatched(id)))
enddefine;

    /*
        Get symbol values and assign to identifiers.

        If declare is true, declare symbols.
        If load_done is true (or a symbol already has an exptr allocated,
        with no access procedure), assign values to symbols.
        If both are true, use sysPASSIGN, which works with lconstants.

        If only declare is true, the ident of the identifier is remembered.
        A later use of definesymbols with define=true,declare=false will
        then get the ident and update its value.

        In all cases, if acc_p is a procedure, it and the assignment must be
        done at runtime.
    */
define lconstant define_symbols(symbol_list, declare, load_done);
    lvars   symbol_list, declare, load_done, vec, acc_p, idname, entry, exptr,
            decl_p, idprops, global_p, id, n, rt_assigns = [];

    define lconstant assign_id(val, idname);
        lvars val, idname;
        returnunless(idname);
        if isword(idname) then
            sysPASSIGN(val, idname)
        elseif isident(idname) then
            val -> idval(idname)
        else
            mishap(idname, 1, 'UNEXPECTED IDENT FIELD')
        endif
    enddefine;

    fast_for vec in symbol_list do
        explode(explode(vec)) -> (, , exptr, entry,
                                 (global_p, decl_p, idprops, idname, acc_p));
        ;;; try to get a pointer if we haven't got one
        unless isexternal_ptr(exptr) then
            external_load_consptr() ->> exptr -> subscrv(3,vec)
        endunless;

        if declare then
            ;;; coerce the identifier declaration if necessary
            if isprocedure(acc_p) then
                ;;; coerce to a variable so assignment can be done at runtime
                if decl_p == sysCONSTANT then
                    unless pop_runtime then sysVARS -> decl_p endunless
                elseif decl_p == sysLCONSTANT then
                    sysLVARS -> decl_p
                endif
            elseunless decl_p /== sysLCONSTANT or ispair(acc_p) or exptr then
                ;;; lconstant we can't assign straight away, so coerce to lvar
                sysLVARS -> decl_p
            endif;
            ;;; declare the identifier
            pop11_define_declare(idname, global_p, decl_p, idprops);

            if ispair(acc_p) then
                ;;; pair whose front is the object to assign NOW, and whose
                ;;; back is a runtime-action procedure to be applied to exptr
                destpair(acc_p) ->> acc_p -> subscrv(SYM_ACC_P,subscrv(5,vec));
                assign_id((), idname);
                false -> idname
            elseunless load_done then
                sys_use_current_ident(idname) -> (id, );
                if isident(id) == "perm" then
                    ;;; replace idname with its word identifier
                    word_identifier(idname, current_section, true)
                else
                    ;;; lexical -- just replace idname with id
                    id
                endif -> idname;
                ;;; if there's no acc_p, but we already have an exptr, then do
                ;;; the assign straight away
                if not(acc_p) and exptr then
                    assign_id(exptr, idname);
                    false -> idname
                endif
            endif;
            idname -> subscrv(SYM_ID,subscrv(5,vec))
        endif;

        nextunless(load_done);

        if entry then
            ;;; for history list
            exptr -> fast_prop_entry_value(entry)
        endif;

        if acc_p then
            if pop_runtime then
                ;;; can do the access now (NB use sys_runtime_apply as
                ;;; this will change pop_runtime to true)
                sys_runtime_apply(exptr, acc_p);
                nextunless(idname)      ;;; no result, no assignment
            else
                ;;; access and assignment must be deferred
                ;;; see exload_runtime_assign for arg combinations
                if isword(idname) then
                    sys_use_current_ident(idname) -> (id, )
                else
                    ;;; idname can be false -- try to optimise away a
                    ;;; 1-frozval closure in this case
                    if idname
                    or not(isclosure(acc_p)) or datalength(acc_p) /== 1
                    or not(frozval(1,acc_p) ->> id) or isident(id)
                    then
                        idname -> id
                    else
                        ;;; can optimise closure
                        pdpart(acc_p) -> acc_p
                    endif
                endif;
                [^exptr ^acc_p ^id] nc_<> rt_assigns -> rt_assigns;
                nextunless(idname);     ;;; no result, no assignment
                ;;; get a value to assign as the default
                if isident(id) and identtype(id) == "procedure" then
                    rt_undefp_rec
                else
                    rt_undef_rec
                endif
            endif -> exptr
        endif;

        assign_id(exptr, idname);

    endfor;

    if (listlength(rt_assigns) ->> n) /== 0 then
        sys_runtime_apply(nonwriteable
                consclosure(exload_runtime_assign, dl(rt_assigns), n/3, n+1) )
    endif
enddefine;

define lconstant load_&_assign(mark, objfiles, symbol_list, do_declare);
    lvars mark, objfiles, symbol_list, do_declare;
    external_do_load(mark, exload_flatten_objfiles(objfiles), symbol_list);
    define_symbols(symbol_list, do_declare, true);
    sys_grbg_list(objfiles);
enddefine;

;;; add a new set of symbols to the batch_buffer of symbols needing loading
define lconstant add_batch(mark, objfiles, symbol_list, do_declare);
    lvars   mark, objfiles, symbol_list, load_objfiles, load_symbol_list,
            do_declare;

    returnif(symbol_list == [] and objfiles == []);

    if pop_pas_mode == "popc" then
        ;;; Don't actually batch -- there's no point, and the actual merging
        ;;; of objfiles must be left until the POPLINK stage.
        ;;; (This also keeps POPC mode and non-POPC mode loads separate.)
        exload_merge_objfiles([], objfiles, false) -> objfiles;
        load_&_assign(mark, objfiles, symbol_list, do_declare);
        sys_grbg_list(symbol_list);
        return
    endif;

    if do_declare then define_symbols(symbol_list, true, false) endif;

    unless batch_buffer then
        ;;; no existing batch
        consvector([], [], mark, 3) -> batch_buffer
    endunless;

    explode(batch_buffer) -> (load_symbol_list, load_objfiles, /*mark*/);
    load_symbol_list nc_<> symbol_list -> subscrv(1,batch_buffer);

    ;;; combine objfiles
    exload_merge_objfiles(load_objfiles, objfiles, false)
                                                -> subscrv(2,batch_buffer);
    sys_grbg_list(load_objfiles)
enddefine;

define exload_addbatch = add_batch(%false%) enddefine;

;;; load all symbols in the symbol batch_buffer
define exload_do_batch_load();
    lvars mark, objfiles, symbol_list;

    returnif(pop_pas_mode == "popc");       ;;; never anything to do
    returnunless(batch_buffer);

    explode(batch_buffer) -> (symbol_list, objfiles, mark);
    ;;; clear batch_buffer even if load fails.
    false -> batch_buffer;
    load_&_assign(mark, objfiles, symbol_list, false);
    sys_grbg_list(symbol_list);
enddefine;

define active exload_isbatching;
    ;;; returns false, or a number indicating level of batching
    (exld_batch_count /== 0) and exld_batch_count;
enddefine;
;;;
define updaterof active exload_isbatching val;
    lvars val;
    ;;; lets a procedure locally set batching true
    if val == true then
        exld_batch_count fi_+ 1 -> exld_batch_count;
    elseif val.isinteger then
        val -> exld_batch_count;
    else
        0 -> exld_batch_count;
    endif;
    if exld_batch_count == 0 then exload_do_batch_load(); endif;
enddefine;


;;; READING EXLOAD EXPRESSIONS

define lconstant read_attributes();
    lvars item, val;
    repeat
        true -> val;
        need_nextitem([language batching reloading prefix type no]) -> item;
        if item == "no" then
            false -> val; readitem() -> item;
        endif;
        if item == "language" then
            unless val then
                false -> langname
            elseunless isstring(itemread() ->> langname) then
                word_string(checkr_name(langname, false)) -> langname
            endunless;
        elseif item == "batching" then
            val -> batching
        elseif item == "reloading" then
            val -> reloading;
        elseif item == "prefix" then
            unless val then
                false -> prefix
            elseif isstring(itemread() ->> prefix) then
                consword(prefix) -> prefix
            else
                checkr_name(prefix, false) ->
            endunless;
        elseif item == "type" then
            unless val then
                false -> symbol_type
            elseunless isstring(itemread() ->> symbol_type) then
                word_string(checkr_name(symbol_type, false)) -> symbol_type
            endunless;
        endif;
        quitif(need_nextitem([, )]) == ")");
    endrepeat;
enddefine;

define get_exload_initial(default_obj, try_declarator)
                                -> (mark, objfiles, declarator, attributes);
    lvars   default_obj, try_declarator, mark, objfiles, declarator,
            attributes, item;
    dlocal  pop_autoload = false,
            langname, prefix, batching, reloading, symbol_type;

    itemread() -> mark;
    ;;; list of object files, etc
    nextitem() -> item;
    if item == "[" then
        readitem() -> ;
        pop11_exec_compile(nonsyntax [, false) ;;; sets pop_autoload true
    elseif islist(item) then
        readitem()      ;;; will return item!
    elseif isword(default_obj) then
        true -> pop_autoload;
        valof(default_obj);
        false -> pop_autoload
    else
        []
    endif -> objfiles;
    [] -> attributes;
    false -> declarator;
    if try_declarator then
        while nextitem() == "(" do
            attributes nc_<> (readitem() :: proglist_read_by(read_attributes))
                        -> attributes
        endwhile;
        if (get_declarator(nextitem())->) then
            readitem() -> declarator
        endif
    endif;
enddefine;


define lconstant read_symbols() -> (batch_symbol_list, symbol_list);
    lvars   item, spec, fldmode, acc_p, idname, symname, prop, symtype,
            global_p, decl_p, idprops, entry, value, symbol_list,
            batch_symbol_list = [];
    dlocal  pop_autoload = false;

    lconstant
        norm_acc_p_prop = newanyproperty([], 8, false, false, syshash,
                                            nonop =, "tmpval", false, false),
        popc_acc_p_prop = copy(norm_acc_p_prop);

    ;;; read identifier declarations
    [%  repeat
            while (itemread() ->> item) == "(" do
                read_attributes()
            endwhile;
            get_declarator(item) -> (decl_p, global_p);
            if decl_p then itemread() -> item endif;
            quitunless(decl_p or item /== popclosebracket);
            repeat
                if item == "procedure" then
                    item, itemread() -> item
                else
                    0
                endif -> idprops;
                checkr_name(item, false) ->> idname -> symname;
                ;;; apply prefix (if any) to pop identifier name
                if prefix then prefix <> idname -> idname endif;
                ;;; default symbol type false is code
                symbol_type -> symtype;

                if ispair(nextitem() ->> item) then
                    ;;; special for internal use by macros -- front(item) is
                    ;;; thing to assign to identifer (now), and back(item)
                    ;;; is runtime action taking exptr as arg.
                    readitem() -> acc_p;
                    false ->> spec -> fldmode

                elseif item == "=" then
                    ;;; evaluate access procedure for given typespec on
                    ;;; external pointer containing symbol value
                    readitem() -> ;     ;;; remove =
                    read_typespec(false, false) -> (spec, , );
                    if ispair(spec) or isvector(spec) then
                        mishap(idname, 1, 'exload: COMPOUND TYPESPEC INVALID AFTER "="')
                    endif;
                    if pop_pas_mode == "popc" then
                        popc_acc_p_prop
                    else
                        norm_acc_p_prop
                    endif -> prop;
                    unless prop(spec) ->> acc_p then
                        cons_access(consref(true), spec, false, 1)
                                ->> acc_p -> prop(spec)
                    endunless;
                    deref_struct1(exacc_result_spec(spec)) -> (spec, fldmode);
                    ;;; default symbol type to data
                    unless symtype then 'data' -> symtype endunless

                else
                    ;;; just load external pointer with symbol value and
                    ;;; declare typespec for it under idname
                    if idprops = "procedure" then
                        mishap(idname, 1, 'exload: PROCEDURE IDENTIFIER ONLY VALID WITH "="')
                    endif;
                    read_typespec(false, true) -> (spec, fldmode, );
                    false -> acc_p;
                    ;;; default symbol type to code if func typespec or none
                    unless symtype or not(spec) or isvector(spec) then
                        'data' -> symtype
                    endunless
                endif;

                ;;; make typespec declaration for idname if appropriate
                if spec then
                    def_typespec(
                            idname,
                            decl_p and decl_p/==sysVARS and decl_p/==sysCONSTANT,
                            spec, fldmode)
                endif;

                if nextitem() == "<-" then
                    ;;; alternate symbol name
                    readitem() -> ;
                    readitem() -> symname
                endif;

                ;;; symbol vec for external_do_load
                if reloading or exload_isundef(idname) then
                    ;;; load this symbol
                    {%  symname,
                        if symtype then conspair(langname, symtype)
                        else langname       ;;; type = code
                        endif,
                        false,
                        false,
                        {% global_p, decl_p, idprops, idname, acc_p, %}
                    %};

                    if batching and exload_isbatching then
                        ;;; batch this symbol
                        () :: batch_symbol_list -> batch_symbol_list
                    endif;
                endif;
                need_nextitem([, ; ^popclosebracket]) -> item;
                if item == "," then itemread() -> item endif;
                quitif(item == ";")(1);
                quitif(item == popclosebracket)(2)
            endrepeat
        endrepeat
    %] -> symbol_list;
enddefine;

;;; PUBLIC CODE

constant syntax endexload_batch = pop_undef;
;;;
define syntax exload_batch;
    lvars exec = (popclosebracket == popclosebracket_exec), lab, tmp;
    lconstant bc_wid = "ident exld_batch_count";
    dlocal pop_new_lvar_list;

#_IF pop_internal_version < 150210
    ;;; sysLOCAL not allowed at execute level
    if exec then
        ;;; dlocal ensures count is reset if load fails
        dlocal exld_batch_count = exld_batch_count fi_+ 1;
        pop11_exec_stmnt_seq_to("endexload_batch") -> ;
        if exld_batch_count == 1 then exload_do_batch_load() endif;
        return
    endif;
#_ENDIF

    sysLOCAL(bc_wid);
    sysPUSH(bc_wid) -> sysPUSH(sysNEW_LVAR() ->> tmp);
    sysCALL(sysPUSH(tmp), sysPUSHQ(1), "+") -> sysPUSH(bc_wid);
    if exec then
        sysEXECUTE();
        pop11_exec_stmnt_seq_to
    else
        pop11_comp_stmnt_seq_to
    endif ("endexload_batch") -> ;
    sysCALL(sysPUSH(tmp), sysPUSHQ(0), "==");
    sysIFNOT(sysNEW_LABEL() ->> lab);
    sysCALL("exload_do_batch_load");
    sysLABEL(lab);
    sysPUSH(tmp) -> sysPUSH(bc_wid);
    if exec then sysEXECUTE() endif
enddefine;

constant syntax endexload = pop_undef;
;;;
define syntax exload;
    lvars   batch_symbol_list, symbol_list, objfiles, mark;
    dlocal  popclosebracket = "endexload",
            langname = false, prefix = false,
            batching = true, reloading = true,
            symbol_type = false;

    get_exload_initial(false, false) -> (mark, objfiles, ,);
    read_symbols() -> (batch_symbol_list, symbol_list);
    unless batch_symbol_list == [] then
        add_batch(mark, objfiles, batch_symbol_list, true)
    endunless;

    returnif(symbol_list == []);

    exload_merge_objfiles([], objfiles, false) -> objfiles;
    load_&_assign(mark, objfiles, symbol_list, true);
    if popclosebracket == popclosebracket_exec then sysEXECUTE() endif;
enddefine;

applist(names, sysprotect);

endsection;     /* $-typespec_utils */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 16 1996
        Added new type attribute.
--- John Gibson, May 29 1996
        Modified to use new external_load_consptr so that lconstant exptrs
        do not cause separate non-batched loads.
--- John Gibson, Mar 26 1996
        From v15.021, changed exload_batch to plant VM code in all contexts.
--- John Gibson, Mar 13 1996
        Changed exload_batch to plant VM code if inside a procedure
--- John Gibson, May 16 1994
        Put code from read*_all into exload itself
--- Robert John Duncan, May 13 1994
        Added test to read*_all so as not to call add_batch unless there
        really are symbols for batch loading: a recent change to add_batch
        means that it now adds object files to the batch buffer even when
        the symbol list is empty.
--- John Gibson, Aug 16 1993
        Test for Popc now pop_pas_mode == "popc"
--- John Gibson, May  8 1993
        Moved objfile merging code to LIB * exload_merge_objfiles.
--- John Gibson, Apr 29 1993
        Improved objfile merging algorithm in exload_addbatch
--- John Gibson, Apr 27 1993
        Moved in get_exload_initial from typespec_utils.p
--- John Gibson, Apr  2 1993
        o Changed to merge object-file arguments when batching.
        o Allows "procedure" identprops before identifier with "=" form.
        o Now uses get_exload_initial to read mark, etc.
--- John Gibson, Oct  5 1992
        Made read_symbols call co*erce_decl_p to coerce constants to vars
        when there's an access procedure and not(pop_runtime)
--- John Gibson, Sep 23 1992
        o Made read_symbols cache cons_access procedures in a property.
        o Made define_symbols accumulate runtime assignments into a single
          sys_runtime_apply'ed call of exload_runtime_assign
--- John Gibson, Aug 26 1992
        Added "prefix" parameter to allow automatic prefixing of pop
        identifier names from external names
--- John Gibson, Jul 23 1992
        Changes for POPC
--- John Gibson, Mar 11 1992
        Added -rt_undefp_rec-, and made -define_symbols- assign it to a
        procedure identifier rather than -rt_undef_rec-.
--- John Gibson, May 23 1991
        Got rid of exload_ prefix on lexical procedure names.
        Made -exload_isbatching- an active variable.
        Rewrote -define_symbols- so as to use -sys_runtime_apply- where
        appropriate.
--- Robert John Duncan, Mar 20 1991
        Rewrote -exload_isundef- to fix two bugs
--- Roger Evans, Dec  6 1990
        Changed algorithm for constructing batches again - now each distinct
        objfile list has a separate batch, andd added exload_strict_batching
--- Jonathan Meyer, Nov 11 1990
        Changed algorithm for constructing batch objfile list to just
        concatenate the objfile lists.
        Removed test for LCONSTANTS that produced mishap.
        Added updater of exload_isbatching to let procedures set batching
        mode using dlocal.
--- Jonathan Meyer, Oct 21 1990
        Set props field of external pointer before calling acc_p,
        added pop11_exec_stmnt_seq_to to exload_batch, fixed bug in
        read*_all. Made read_symbols automatically load
        lconstants in non-batching mode.
--- Roger Evans, Oct 19 1990
        now allows literal list for objfiles spec
--- Jonathan Meyer, Oct 18 1990
        Changed batch and reload parameters to batching and reloading,
        and added the "no" operator.
--- Jonathan Meyer, Oct 17 1990
        Made exload_readlangname read_attributes. Added batch and reload
        parameters, and exload_batch command.
--- Jonathan Meyer, Oct 15 1990
        Broke exload into two procedures, to make it possible to implement
        exload_require. New procedures are called read*_all and
        define_symbols. exload now calls these procedures.
--- John Gibson, Sep 21 1990
        From 13.84, -external_do_load- returns exptrs WITH symbol values
        rather than TO symbol values.
 */
