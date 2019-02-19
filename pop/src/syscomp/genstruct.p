/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/syscomp/genstruct.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                    STRUCTURE GENERATION PROCEDURES

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'
#_INCLUDE '$usepop/pop/lib/include/popc_flags.ph'
#_INCLUDE '../gctypes.ph'

section $-Popas;

constant
        procedure (generate_gen_procedure, generate_closure, generate_array,
        generate_pcomposite, try_recreate_access_p)
    ;

vars
        procedure (gen_procedure_args),
        nextlabel, popc_flags, current_asm_segment = false
    ;

weak constant procedure (asm_outdouble, asm_align_word);


;;; ---------------------------------------------------------------------

define pointers_into    = newproperty([], 2, false, "tmparg") enddefine;

define dest_procedure_label();
    explode()
enddefine;

newproperty([], 4, false, "tmparg") -> popc_is_array_upd;

;;; ---------------------------------------------------------------------

lvars
    file_has_nonwriteable,
    file_has_writeable,
    writ_nonpop_list,
    nwrit_nonpop_list,
    lex_ident_list,
    glob_lab_generated,
    ;

define is_writeable(struc);
    lvars struc, w;
    if (sys_writeable_prop(struc) ->> w) == "undef" then
        if (class_attribute(datakey(struc), "writeable") ->> w) == "undef" then
            popc_flags &&=_0 POPC_NONWRITEABLE_DEFAULT -> w
        elseif isclosure(struc) == true then    ;;; exclude protected closure
            popc_flags &&/=_0 POPC_WRITEABLE_CLOSURES -> w
        endif
    elseunless w then
        return(false)       ;;; even for fixed-address
    endif;
    w or (is_fixed(struc) and isinheap(struc))
enddefine;

updater(sys_writeable_prop) -> updater(is_writeable);


define out_objmod_pad(len);
    lvars len;
    asm_outword(len, perm_const_lab([Sys objmod_pad_key]), 2);
    asmf_charout(`\n`)
enddefine;

define lconstant out_word_or_double(/*nvals*/, use_double);
    lvars use_double;
    unless use_double then
        asm_outword()
    elseif testdef asm_outdouble then
        weakref asm_outdouble()
    else
        mishap((), 'INVALID DOUBLE FIELD (asm_outdouble not defined)')
    endunless
enddefine;

define lconstant start_writeable();
    unless current_asm_segment == ASMSEG_WRITEABLE then
        asm_startdata();
        ASMSEG_WRITEABLE -> current_asm_segment;
        unless file_has_writeable then
            ;;; start seg with an objmod_pad structure -- -1 for the
            ;;; length of this structure tells GC it's 2 words and followed
            ;;; by a proper pop structure
            out_objmod_pad(-1);
            true -> file_has_writeable
        endunless
    endunless
enddefine;

define lconstant start_nonwriteable();
#_IF DEF asmseg_nonwriteable
    unless current_asm_segment == asmseg_nonwriteable then
        if asmseg_nonwriteable == ASMSEG_WRITEABLE then
            chain(start_writeable)
        endif;
#_ELSE
    unless current_asm_segment == ASMSEG_NONWRITEABLE then
#_ENDIF
        asm_startcode();
        ASMSEG_NONWRITEABLE -> current_asm_segment;
        true -> file_has_nonwriteable
    endunless
enddefine;

define outlab(lab);
    lvars lab, info, elab;
    unless islabel(lab) ->> info then
        mishap(lab, 1, 'LABEL NEEDED')
    endunless;
    fast_for elab in equiv_labels(lab) do
        outlab(elab)
    endfor;
    if info && LAB_GLOBAL /== 0 then
        true -> glob_lab_generated;
        asm_outglab(lab)
    else
        asm_outlab(lab)
    endif
enddefine;

define outlabset(vallab, val);
    lvars vallab, val, info;
    unless islabel(vallab) ->> info then
        mishap(vallab, 1, 'LABEL NEEDED')
    endunless;
    if info && LAB_GLOBAL /== 0 then
        true -> glob_lab_generated;
        asm_outglabset(vallab, val)
    else
        asm_outlabset(vallab, val)
    endif
enddefine;

define lconstant woffs_struct_wrap(code_p_list, keypath);
    lvars code_p_list, keypath;
#_IF DEF asm_struct_wrap
    asm_struct_wrap(perm_const_lab(keypath), code_p_list);
#_ELSE
    lvars startlab = genlab(), endlab = genlab(), sizelab = genlab(), code_p;
    outlab(startlab);

    ;;; RAW_SIZE, KEY
    asm_outword(sizelab, perm_const_lab(keypath), 2);

    ;;; N.B. code this generates must be an exact number of words
    ;;; since size of a rawstruct/exptr_mem is a word offset
    fast_for code_p in code_p_list do code_p() endfor;

    outlab(endlab);
    outlabset(sizelab, endlab label_- startlab);    ;;; word offs of whole struct
#_ENDIF
enddefine;

define lconstant rawstruct_wrap =
    woffs_struct_wrap(% [Sys rawstruct_key] %)
enddefine;

define lconstant out_double_align =
    rawstruct_wrap(% [^asm_align_double] %)
enddefine;


define setseg(structure, dalign);
    lvars structure, dalign;
    asmf_charout(`\n`);
    if is_writeable(structure) then
        start_writeable()
    else
        start_nonwriteable()
    endif;
    if dalign then out_double_align() endif
enddefine;

define lconstant output_lex_idents();
    lvars id, idflags, num, idlab, l, pair;

    define lconstant outlabval(pair);
        lvars pair;
        outlab(fast_back(pair)), asm_outword(fast_front(pair), 1)
    enddefine;

    [%fast_for pair in lex_ident_list do
        fast_back(pair) -> id;
        struct_label_prop(id) ->> idlab -> fast_back(pair);
        if nonpop_ident(id) then
            ;;; nonpop identifier -- hide it inside a string at the end
            outlabval(%pair%),
                if is_writeable(id) then
                    () :: writ_nonpop_list -> writ_nonpop_list
                else
                    () :: nwrit_nonpop_list -> nwrit_nonpop_list
                endif
        elseif islabel(idlab) &&=_0 LAB_GEN_FULL_ID and is_writeable(id) then
            ;;; use cell inside a vector for pop var not used as data object
            pair
        else
            ;;; full pop ident record
            idprops_flags(id, true) -> num -> idflags;
            if nonactive_isconstant(id) then
                idflags || M_ID_ASSIGNED_CONST -> idflags
            endif;
            if isident(id) == "lextoken" then
                idflags || M_ID_LEX_TOKEN -> idflags
            endif;
            ;;; produce ident record
            setseg(id, false);
            asm_outshort(idflags, 1);   ;;; ID_IDENTPROPS
            asm_outbyte(0, num, 2);     ;;; ID_LEX_FLAGS, ID_NUM_ATTRIBUTE
            if testdef asm_align_word then weakref asm_align_word() endif;
            asm_outword(perm_const_lab([ident_key]), 1);
            outlabval(pair)                 ;;; label, ID_VALOF
        endif
    endfor %] -> l;

    ;;; generate vector
    if l /== [] then
        start_writeable();
        asm_outword(listlength(l), perm_const_lab([vector_key]), 2);
        fast_for pair in l do outlabval(pair) endfor
    endif
enddefine;

define gen_assembler(w_name, start_gen_p);
    lvars   w_name, procedure start_gen_p, lab;
    dlocal  writ_nonpop_list    = [],
            nwrit_nonpop_list   = [],
            lex_ident_list      = [],
            current_asm_segment = ASMSEG_UNDEF,
            file_has_writeable      = false,
            file_has_nonwriteable       = false,
            glob_lab_generated  = false,
        ;

    asm_startfile(w_name);

    ;;; start generation of code
    start_gen_p();

    ;;; deal with held-over structures

    ;;; lexical idents
    output_lex_idents();

    ;;; remaining nonpop idents, etc inside strings
    if writ_nonpop_list /== [] then
        start_writeable();
        rawstruct_wrap(writ_nonpop_list)
    endif;
    if nwrit_nonpop_list /== [] then
        start_nonwriteable();
        rawstruct_wrap(nwrit_nonpop_list)
    endif;

#_IF not(DEF AIX)   ;;; because AIX doesn't like abs val global syms
    unless glob_lab_generated then
        ;;; nothing of interest in file --- generate a dummy global
        ;;; label to keep o/s utilities happy
        symlabel(sys_real_time() sys_>< sys_fname_nam(w_name)) -> lab;
        `d` -> subscrs(1,lab);      ;;; `d` for dummy -- replaces `c`
        outlabset(lab, 0)
    endunless;
#_ENDIF

    ;;; put out end-of-file padding alignment (if necessary)
#_IF DEF asm_align_file
    if file_has_nonwriteable then start_nonwriteable(), asm_align_file() endif;
#_ENDIF
    if file_has_writeable then
        start_writeable();
        ;;; Length of objmod_pad structure /== -1 tells GC to treat this
        ;;; as a string (of given length, 0 in this case), then skip over
        ;;; words until another objmod_pad structure is found (thus skipping
        ;;; any alignment padding added either explicitly by -asm_align_file-
        ;;; or implicitly by the assembler or linker).
        out_objmod_pad(0);
#_IF DEF asm_align_file
        asm_align_file()
#_ENDIF
    endif;
    asm_endfile()
enddefine;


;;; --- GENERATE STRUCTURES --------------------------------------------

constant procedure (genstructure, gen_recordclass, gen_vectorclass);

lvars
    genstruct_set_flags = LAB_GEN_FULL_ID;

define lconstant gen_error(item, mess);
    lvars item, mess;
    dlocal cucharout = cucharerr;
    if mess then
        '\s(' <> mess <> ')'
    else
        nullstring
    endif -> mess;
    printf('\s\s\s\sNON-GENERATABLE VALUE%p\n\s\s\s\s\s\s\s\sINVOLVING:\s%p\n', [^mess ^item]);
    unless file_has_errors then true -> file_has_errors endunless
enddefine;

define lconstant no_gen = gen_error(% false %) enddefine;

lconstant
    generate_p = newactproperty([], 16, no_gen, true,
                    procedure(dk, prop);
                        lvars dk, s, prop;
                        returnunless(class_field_spec(dk) ->> s) (no_gen);
                        genstructure(dk) -> ;
                        if islist(s) then
                            gen_recordclass
                        else
                            gen_vectorclass
                        endif ->> prop(dk)
                    endprocedure);


define genstructure(item) -> lab;
    lvars item, info, lab, p;
    label_of(item, false) -> lab;
    if item == lab then
        ;;; item is a label
        return
    elseif lab then
        islabel(lab) -> info;
        unless info &&=_0 LAB_GENERATED and info &&/=_0 LAB_OF_STRUCT
        and generateable_struct(item) then
            info fi_|| genstruct_set_flags -> islabel(lab);
            return
        endunless
    else
        genlab() -> lab;
        ;;; 3rd arg false means no need to assign true to
        ;;; generateable_struct(item)
        assign_struct_label(lab, item, false);
        islabel(lab) -> info
    endif;
    info fi_|| genstruct_set_flags fi_|| LAB_GENERATED -> islabel(lab);
    generate_p(data_key(item))(item)
enddefine;

define gen_lex_ident(id, val_access);
    lvars id, val_access;
    dlocal genstruct_set_flags;
    if val_access then
         genstruct_set_flags fi_&&~~ LAB_GEN_FULL_ID -> genstruct_set_flags
    endif;
    genstructure(id)
enddefine;


;;; --- SYNTAX FOR GENERATOR PROCEDURES -----------------------------------

define :define_form lconstant GEN;
    lvars idname, dw = readitem(), idname = "GEN_" <> dw, k;
    [define lconstant ^idname ^^proglist] -> proglist;
    pop11_comp_expr();
    if isdefined(dw <> "_key" ->> k) then
        valof(k)
    else
        key_of_dataword(dw)
    endif -> k;
    [; ^idname -> generate_p(^k); ^^proglist] -> proglist
enddefine;


;;; --- GENERAL ----------------------------------------------------------

define lconstant gen_header(/*firstword,key_path*/ item, dalign) with_nargs 4;
    lvars item, dalign;
    setseg(item, dalign);
    asm_outword((/*firstword*/), perm_const_lab(/*key_path*/), 2);
    outlab(struct_label_prop(item))
enddefine;

define lconstant gen_full1(item, key_path);
    lvars x, item, key_path;
    gen_header(genstructure(class_dest(datakey(item))(item)), key_path, item,
                                                            false)
enddefine;

define lconstant gen_full2(item, key_path);
    lvars x, y, item, key_path, key = datakey(item);
    if key == ratio_key then
        destratio(item)
    else
        class_dest(key)(item)
    endif -> (x, y);
    genstructure(x) -> x, genstructure(y) -> y;
    gen_header(x, key_path, item, false);
    asm_outword(y, 1)
enddefine;


;;; --- FLOATING-POINT ---------------------------------------------------

define get_sfloat_int(sfloat, pop_d);
    lvars sfloat, fexp, fsign, pop_d;
    float_decode(sfloat, true) -> fsign -> fexp -> sfloat;
    intof(fsign) * sfloat -> sfloat;
    if pop_d then pd_float_val else s_float_val endif(sfloat, fexp)
enddefine;

define lconstant get_dfloat_ints(dfloat);
    lvars df, dfloat, fsign, fexp;
    float_decode(dfloat, true) -> fsign -> fexp -> df;
    d_float_vals(intof(fsign)*df, fexp) /* -> lo -> hi */
enddefine;

define :GEN dfloat(dfloat);
    lvars   dfloat, (hi, lo) = get_dfloat_ints(dfloat),
            mylab = struct_label_prop(dfloat);
    procedure();
        asm_align_double();
        outlab(mylab);
        asm_out_dfloat(hi, lo)
    endprocedure,   if is_writeable(dfloat) then
                        () :: writ_nonpop_list -> writ_nonpop_list
                    else
                        () :: nwrit_nonpop_list -> nwrit_nonpop_list
                    endif
enddefine;

define :GEN ddecimal(dfloat);
    lvars dfloat, (hi, lo) = get_dfloat_ints(dfloat);
#_IF WORD_BITS==DOUBLE_BITS
    setseg(dfloat, false);
    asm_out_dfloat(hi, lo);
    asm_outword(perm_const_lab([ddecimal_key]), 1);
    outlab(struct_label_prop(dfloat))
#_ELSE
    gen_header(hi, [ddecimal_key], dfloat, false);
    asm_outword(lo, 1)
#_ENDIF
enddefine;


;;; --- OTHER STRUCTURES ------------------------------------------------

define :GEN undef(u);
    dlvars u;
    lvars uword = undefword(u);

    define lconstant test_var(word, id, sect);
        lvars id, word, sect;
        if fast_idval(id) == u then
            ;;; can only happen for a variable
            gen_error(u, 'undef value of perm variable');
            exitfrom(GEN_undef)
        endif
    enddefine;

    if uword then recursive_app_sect_idents(pop_section, test_var) endif;
    if isprocedure(u) then
        ;;; undef closure
        chain(u, generate_closure)
    else
        gen_header(genstructure(uword), [undef_key], u, false)
    endif
enddefine;

define :GEN biginteger(i);
    lvars i, ilen, slice_bits, slice_mask, nslices, perword, npad;
#_IF "BIGINT_SPEC".valof == "short"
    lconstant macro (OUT_P = "asm_outshort", EL_BITS = SHORT_BITS);
#_ELSEIF "BIGINT_SPEC".valof == "int"
    lconstant macro (OUT_P = "asm_outint", EL_BITS = INT_BITS);
#_ELSEIF "BIGINT_SPEC".valof == "double"
    lconstant macro (OUT_P = "asm_outdouble", EL_BITS = DOUBLE_BITS);
#_ELSE_ERROR
#_ENDIF

    unless (integer_length(i) ->> ilen) > POPINT_BITS then
        mishap(i, 1, 'SYSTEM ERROR GENERATING BIGINTEGER')
    endunless;

    EL_BITS - 1 -> slice_bits;
    (1 << slice_bits) - 1 -> slice_mask;
    (ilen+slice_bits-1) div slice_bits -> nslices;
    ;;; Need special case when host integers are bigger
    ;;; than target integers
    if isinteger(i) then
        asmf_charout(`\n`);
        start_nonwriteable();
        asm_outword(nslices, perm_const_lab([biginteger_key]), 2);
        outlab(struct_label_prop(i))
    else
        gen_header(nslices, [biginteger_key], i, false);
    endif;
    fast_repeat nslices-1 times
        i && slice_mask, i >> slice_bits -> i
    endrepeat, i;
    WORD_BITS/EL_BITS -> perword;
    (perword-nslices) mod perword -> npad;
    dupnum(0, npad);
    OUT_P((), nslices+npad)
enddefine;
;;;
generate_p(biginteger_key) -> generate_p(integer_key);


define :GEN ratio   = gen_full2(% [ratio_key] %) enddefine;
define :GEN complex = gen_full2(% [complex_key] %) enddefine;
define :GEN pair    = gen_full2(% [pair_key] %) enddefine;

define lconstant gen_anystring_body(s, per_word, out_p);
    lvars   n, nstacked, s, size, rsize = datalength(s), pointers, pointer,
            per_word, procedure out_p;
    ;;; if already a multiple of per_word, add another word to ensure
    ;;; null termination
    (rsize+per_word) div per_word * per_word -> size;
    pointers_into(s) -> pointers;
    0 -> nstacked;
    fast_for n to size do
        if pointers and (pointers(n) ->> pointer) then
            out_p(nstacked); 0 -> nstacked;
            outlab(label_of(pointer, true))
        endif;
        if n fi_> rsize then 0 else fast_subscrs(n,s) endif;
        nstacked fi_+ 1 -> nstacked;
        if nstacked fi_>= 16 then
            out_p(nstacked); 0 -> nstacked
        endif
    endfor;
    out_p(nstacked);
    if pointers and (pointers(n) ->> pointer) then
        outlab(label_of(pointer, true))
    endif;

#_IF DEF vedstring_data_prop
    ;;; generate vedstring data vec
    lvars vec = vedstring_data_prop(s);
    if vec then vec -> sys_current_val("vedstring_data_prop")(s) endif
#_ENDIF
enddefine;

define lconstant gen_string_body();
    gen_anystring_body((), WORD_BYTES, asm_outbyte)
enddefine;

define :GEN string(s);
    lvars s;
    gen_header(datalength(s), [string_key], s, false);
    gen_string_body(s)
enddefine;

#_IF DEF dstring_key

define :GEN dstring(s);
    lvars s, rsize = datalength(s);
    gen_header(rsize, [dstring_key], s, false);
    gen_string_body(s);
    ;;; generate the attributes as a separate string body
    gen_string_body(consstring(appdata(s, #_< nonop fi_>>(%16%) >_#), rsize))
enddefine;

#_ENDIF

#_IF DEF string16_key

define lconstant gen_string16_body();
    gen_anystring_body((), #_< WORD_BITS/SHORT_BITS >_#, asm_outshort)
enddefine;

define :GEN string16(s);
    lvars s;
    gen_header(datalength(s), [string16_key], s, false);
    gen_string16_body(s)
enddefine;

define :GEN dstring16(s);
    lvars s, rsize = datalength(s);
    gen_header(rsize, [dstring16_key], s, false);
    gen_string16_body(s);
    ;;; generate the attributes as a separate string body
    gen_string_body(consstring(appdata(s, #_< nonop fi_>>(%16%) >_#), rsize))
enddefine;

#_ENDIF


define :GEN ref         = gen_full1(% [ref_key] %) enddefine;
define :GEN prologvar   = gen_full1(% [prologvar_key] %) enddefine;

define :GEN prologterm(pt);
    lvars i, n, pt, func;
    genstructure(prolog_functor(pt)) -> func;
    prolog_arity(pt) -> n;
    fast_for i to n do genstructure(prolog_arg(i, pt)) -> endfor;
    gen_header(n+1, [prologterm_key], pt, false);
    asm_outword(func, 1);
    fast_for i to n do
        asm_outword(label_of(prolog_arg(i, pt), false), 1)
    endfor
enddefine;

    /*  Generate (lexical) identifier (actually, just record
        for generation at end).
    */
define :GEN ident(id);
    lvars id, val;
    dlocal genstruct_set_flags = genstruct_set_flags fi_|| LAB_GEN_FULL_ID;

    ;;; generate idval
    if isident(id) == "lextoken" then
        false -> is_writeable(id);
        0 -> val
    else
        ;;; nonwriteable if constant
        if nonactive_isconstant(id) then false -> is_writeable(id) endif;
        nonactive_idval(id) -> val;
        if isundef(val) then
            true -> is_lexid_undef(val);
            if nonpop_ident(id) then
                syspop\:_int(0) -> val
            endif
        endif
    endif;

    ;;; remember on lex ident list
    conspair(genstructure(val), id) :: lex_ident_list -> lex_ident_list
enddefine;

define :GEN procedure_label(plab);
    lvars plab, owner, id, lab;
    dest_procedure_label(plab),
        -> lab,
        genstructure() -> id,
        genstructure() -> owner;

    gen_header( owner,                      ;;; PLAB_OWNER
                [Sys procedure_label_key],  ;;; KEY
                plab, false);
    asm_outword(id,                         ;;; PLAB_IDENT
                ;;; true in PLAB_LABEL means PLAB_OFFSET is an absolute address
                genstructure(true),         ;;; PLAB_LABEL
                f_hd(lab),                  ;;; PLAB_OFFSET
                3)
enddefine;

define gen_popc_pointer(ptr);
    lvars ptr, base = front(ptr);
    if is_writeable(ptr) then true -> is_writeable(base) endif;
    genstructure(base) ->
enddefine;

define :GEN popc_pointer        = gen_popc_pointer(%%) enddefine;
define :GEN popc_pointer_into   = gen_popc_pointer(%%) enddefine;

define lconstant gen_prop_entries(prop_rec, firstentry_lab)
                                -> (nextentry_lab, pt_active, pt_count,
                                    pt_default, pt_entry_key, pt_eq_pdr,
                                    pt_expand, pt_hash_pdr, tab_size,
                                    entry_key);

    lvars   prop_rec, firstentry_lab, pt_active, pt_count, pt_default,
            pt_entry_key, pt_eq_pdr, pt_expand, pt_hash_pdr, tab_size,
            nextentry_lab, entry_lab, lastarg, lastval, entry_key;

    explode(prop_rec) -> ,          ;;; PT_REHASH, don't need
        genstructure() -> pt_hash_pdr,
        genstructure() -> pt_expand,
        genstructure() -> pt_eq_pdr,
        genstructure(->> entry_key) -> pt_entry_key,
        genstructure() -> pt_default,
        genstructure() -> pt_count,
        genstructure() -> pt_active,
        datalength() -> tab_size;   ;;; table -- only need size

    ;;; generate chain of entry records

    define lconstant gen_entry(/*arg, val*/);
        lvars arg, val;
        if nextentry_lab then
            ;;; not first
            genstructure(lastarg) -> arg;
            genstructure(lastval) -> val;
            ;;; output an entry record
            setseg(prop_rec, false);            ;;; make sure in data
            asm_outword(arg,                        ;;; PTE_ARG
                        pt_entry_key,               ;;; KEY
                        2);
            unless entry_lab then genlab() -> entry_lab endunless;
            outlab(entry_lab);
            asm_outword(val,                        ;;; PTE_VALUE
                        nextentry_lab,              ;;; PTE_NEXT
                        0,                          ;;; PTE_LINK
                        3);
            entry_lab
        else
            ;;; first
            genstructure(0)     ;;; popint 0 is end of entry chain
        endif -> nextentry_lab;
        false -> entry_lab;
        /*arg, val*/ -> (lastarg, lastval)
    enddefine;

    false -> nextentry_lab;
    ;;; (n.b. fast_appproperty is allowed to take an actual prop rec)
    fast_appproperty(prop_rec, gen_entry);
    firstentry_lab -> entry_lab;
    gen_entry(0, 0)
enddefine;

define gen_incremental_property(prop, firstentry_lab);
    lvars   prop, firstentry_lab, prop_rec = frozval(1, prop),
            pt_active, pt_count, pt_default, pt_eq_pdr,
            pt_expand, pt_hash_pdr, tab_size, entry_key;

    define lconstant get_typedstring(lab) -> typedstr;
        lvars lab, typedstr;
        unless typedstring_from_lab(lab) ->> typedstr then
            mishap(lab, 1, 'INVALID VALUE/PROCEDURE IN INCREMENTAL PROPERTY')
        endunless
    enddefine;

    gen_prop_entries(prop_rec, firstentry_lab)
                                -> (firstentry_lab, pt_active, pt_count,
                                    pt_default, /*pt_entry_key*/, pt_eq_pdr,
                                    pt_expand, pt_hash_pdr, tab_size,
                                    entry_key);

    {%  firstentry_lab,
        if pdprops(prop) then genstructure(pdprops(prop))
        else nullstring
        endif,                                      ;;; pdprops

        genstructure(pdpart(prop)),                 ;;; pdpart
        genstructure(pdpart(updater(prop))),        ;;; updater pdpart
        class_attribute(entry_key, "prop_entry"),
        tab_size sys_>< nullstring,                 ;;; table size -> string
        get_typedstring(pt_count),
        get_typedstring(pt_expand),
        get_typedstring(pt_default),
        get_typedstring(pt_eq_pdr),
        get_typedstring(pt_hash_pdr),
        get_typedstring(pt_active)
    %}
enddefine;

define :GEN property(prop_rec);
    lvars   prop_rec, table,
            (firstentry_lab, pt_active, pt_count, pt_default,
             pt_entry_key, pt_eq_pdr, pt_expand, pt_hash_pdr, tab_size,
             /*entry_key*/)
                = gen_prop_entries(prop_rec, false);

    ;;; vector of entry chains -- single chain goes in first element
    {% firstentry_lab, repeat tab_size-1 times 0 endrepeat %} -> table;
    true -> is_writeable(table);
    genstructure(table) -> table;

    ;;; output property record. REHASH field is set true so that first
    ;;; time prop gets used entries will be hashed properly
    gen_header( table,                  ;;; PT_TABLE
                [Sys property_key],     ;;; KEY
                prop_rec, false);
    asm_outword(pt_active,              ;;; PT_ACTIVE
                pt_count,               ;;; PT_COUNT
                pt_default,             ;;; PT_DEFAULT
                pt_entry_key,           ;;; PT_ENTRY_KEY
                pt_eq_pdr,              ;;; PT_EQ_PDR
                pt_expand,              ;;; PT_EXPAND
                pt_hash_pdr,            ;;; PT_HASH_PDR
                genstructure(0),        ;;; PT_REHASH   ;;; 0 = always rehash
                8)
enddefine;

define lconstant no_gen_p =
    gen_error(% 'procedure not compiled in popc mode' %)
enddefine;

define :GEN gen_procedure(p);
    lvars p;
    if gen_procedure_args(p) or try_recreate_access_p(p) then
        chain(p, generate_gen_procedure)
    else
        no_gen_p(p)
    endif
enddefine;

define :GEN closure(clos);
    lvars clos;
    chain(clos, generate_closure)
enddefine;

define :GEN procedure(p);
    lvars p, a, lab;
    if ispcomposite(p) then
        ;;; p1 <> p2
        chain(p, generate_pcomposite)
    elseif isarray(p) then
        ;;; array
        chain(p, generate_array)
    elseif popc_is_array_upd(p) ->> a then
        ;;; array updater
        label_of(p, false) -> lab;
        islabel(lab) fi_&&~~ LAB_GENERATED -> islabel(lab);
        true -> generateable_struct(p);
        genstructure(a) -> ;
        returnif(gen_procedure_args(p))
    endif;
    no_gen_p(p)
enddefine;

define :GEN device(dev);
    lvars   dev, user_data, open_name, full_name, readp, writep, seekp, flushp,
            closep, clear_inputp, test_inputp, false_lab, flags, enc_own_id;

lconstant macro (
    ;;; device flag bits in D_FLAGS
    M_D_USER_DEV            = 2:1e5,    ;;; is a user device
    M_D_LOGICAL_TERM        = 2:1e6,    ;;; is a (logical) terminal
    );

    unless device_user_data(dev) ->> user_data then
        gen_error(dev, 'system device');
        return
    endunless;
    genstructure(user_data) -> user_data;
    genstructure(device_open_name(dev)) -> open_name;
    genstructure(device_full_name(dev)) -> full_name;
    ;;; explode returns the device procedures for POPC
    explode(dev),
        genstructure() -> test_inputp,
        genstructure() -> clear_inputp,
        genstructure() -> closep,
        genstructure() -> flushp,
        genstructure() -> seekp,
        genstructure() -> writep,
        genstructure() -> readp;

    consident(0, false, "lex") -> enc_own_id;
    false -> idval(enc_own_id);
    genstructure(enc_own_id) -> enc_own_id;

    genstructure(false) -> false_lab;
    M_D_USER_DEV -> flags;
    if systrmdev(dev) then flags || M_D_LOGICAL_TERM -> flags endif;

    setseg(dev, false);
    asm_outshort(flags,         ;;; D_FLAGS
                 -1,            ;;; D_FILE_DESC
                 2);
    if testdef asm_align_word then weakref asm_align_word() endif;
    asm_outword(perm_const_lab([device_key]), 1);   ;;; KEY
    outlab(struct_label_prop(dev));

    asm_outword(user_data,      ;;; D_CTRL_BLK
                false_lab,      ;;; D_UNIT_N
                false_lab,      ;;; D_UNIT_P
                open_name,      ;;; D_OPEN_NAME
                full_name,      ;;; D_FULL_NAME
                false_lab,      ;;; D_IN_BUFFER
                false_lab,      ;;; D_OUT_BUFFER
                7);
    asm_outword(readp,          ;;; D_READ
                writep,         ;;; D_WRITE
                seekp,          ;;; D_SEEK
                flushp,         ;;; D_FLUSH
                closep,         ;;; D_CLOSE
                clear_inputp,   ;;; D_CLEAR_INPUT
                test_inputp,    ;;; D_TEST_INPUT
                perm_ident_lab([Sys default_device_encoding]),
                                ;;; D_ENCODING_ID
                enc_own_id,     ;;; D_ENCODING_OWN_ID
                9);
enddefine;

define :GEN external_ptr(ptr);
    lvars   ptr, props = external_ptr_props(ptr), val, dalign,
            props_lab = genstructure(props);
    lconstant workptr = writeable consexternal_ptr();

    if iscompound(props) and is_fixed(props)
    and ptr = fill_external_ptr(props,workptr) then
        ;;; points to props
        props_lab
    elseif isstring(props) then
        ;;; assume string is symbol name
        ;;; external_load_consptr makes these writeable
        false -> sys_writeable_prop(ptr);
#_IF pop_internal_version >= 150301
        ;;; names put in by external_do_load require translation
        get_extern_label(props)
#_ELSE
        props
#_ENDIF
    elseif is_valid_external_ptr(ptr) then
        gen_error(ptr, 'external pointer with invalid pointer val');
        return
    else
        ;;; presumably 0 or -1
        exacc ^int ptr
    endif -> val;

    structs_prop("EXTERNAL_PTR")(STRUC_FLAGS) &&/=_0 M_STRUC_DALIGN -> dalign;
    gen_header(props_lab, [external_ptr_key], ptr, dalign);
    out_word_or_double(val, 1, dalign)
enddefine;

define :GEN exptr_mem(em);
    dlvars em, dalign;

    define lconstant code_gen();
        lvars   ptrlab = genlab(),
                nwords = (datalength(em)+WORD_BYTES-1) div WORD_BYTES;
        outlab(struct_label_prop(em));
        out_word_or_double(ptrlab, 1, dalign);          ;;; XP_PTR
        if nwords > 1
        and eval_type_spec(get_named_field_spec("XP_PTR")(FIELD_TYPE_SPEC))
            && t_BASE_TYPE /== t_DOUBLE then
            asm_align_double()
        endif;
        outlab(ptrlab);
        ;;; just initialise the memory to 0
        fast_repeat nwords times asm_outword(0, 1) endrepeat;
    enddefine;

    structs_prop("EXTERNAL_PTR")(STRUC_FLAGS) &&/=_0 M_STRUC_DALIGN -> dalign;
    setseg(em, dalign);
    woffs_struct_wrap([^code_gen], [exptr_mem_key])
enddefine;

define :GEN exfunc_closure(efc);
    lvars efc, (func, arg, dst) = explode(efc);     ;;; POPC special
    if dst == `A` then
        ;;; _extern pop_exfunc_arg
        get_extern_label('pop_exfunc_arg:data')
    elseif dst = `X` then
        ;;; ident _in_X_call
        perm_ident_lab([Sys _in_X_call])
    else
        gen_error(efc, 'exfunc_closure with unknown arg destination field');
        return;
    endif -> dst;
    genstructure(func) -> func;
    genstructure(arg) -> arg;
    false -> sys_writeable_prop(efc);   ;;; make nonwriteable

    ;;; EFC_FUNC, KEY
    ;;; (Alpha VMS EFC_CODE is actually a null-frame procedure descriptor,
    ;;; which must be double aligned)
    gen_header(func, [Sys exfunc_closure_key], efc, DEF ALPHA_VMS);

    ;;; EFC_CODE
    asm_gen_exfunc_clos_code(perm_const_lab([Sys \^_exfunc_clos_action]));
    asm_outword(arg,            ;;; EFC_ARG
                dst,            ;;; EFC_ARG_DEST
                2);
enddefine;

#_IF DEF matchvar_key

define :GEN matchvar(mv);
    lvars mv, (name, id, restriction, flags) = destmatchvar(mv);
    genstructure(name) -> name;
    genstructure(id) -> id;
    genstructure(restriction) -> restriction;
    genstructure(flags) -> flags;
    gen_header(name, [matchvar_key], mv, false);    ;;; MV_NAME, KEY
    asm_outword(id,                                 ;;; MV_IDENT
                restriction,                        ;;; MV_RESTRICTION
                flags,                              ;;; MV_FLAGS
                3)
enddefine;

#_ENDIF


;;; --- FREE STRUCTURE ---------------------------------------------------

lvars gf_acc_bits, gf_acc_nbits, gf_structure;

define lconstant gf_flush_bits();
    if gf_acc_bits then
        asm_outbits(gf_acc_bits, gf_acc_nbits);
        false -> gf_acc_bits
    endif
enddefine;

define lconstant gen_field(type_spec, val);
    lvars type, val, size, type_spec, valtype;

    if type_spec == "pointer" then
        gf_flush_bits();
        outlab(struct_label_prop(gf_structure));
        return
    endif;

    if (type_spec && t_BASE_TYPE ->> type) == t_BIT then
        unless gf_acc_bits then 0 ->> gf_acc_bits -> gf_acc_nbits endunless;
        t_offset(type_spec, false) -> size;
        unless isintegral(val) then _intval(val) -> val endunless;
        val && (1<<size-1) -> val;
        asm_addbits(val, size, gf_acc_bits, gf_acc_nbits) -> gf_acc_bits;
        gf_acc_nbits fi_+ size -> gf_acc_nbits
    else
        gf_flush_bits();
        if type == t_WORD or type == t_DOUBLE then
            type_spec && tv_VAL_TYPE -> valtype;
            if valtype == tv_FULL then
                label_of(val, false)
            elseif type == t_DOUBLE and isdecimal(val) then
                ;;; val is a double float
                asm_out_dfloat(get_dfloat_ints(val));
                return
            elseif valtype == tv_FLOAT then
                get_sfloat_int(val, false)
            elseif valtype == tv_EXPTR or valtype == tv_EXVAL then
                if is_valid_external_ptr(val) then
                    mishap(val, 1, '"exptr"/"exval" FIELD VALUE MUST BE NULL')
                else
                    ;;; assume 0 or -1
                    exacc ^int val
                endif
            else
                val
            endif -> val;
            out_word_or_double(val, 1, type/==t_WORD)
        elseif type == t_BYTE then
            asm_outbyte(val, 1)
        elseif type == t_SHORT then
            asm_outshort(val, 1)
        elseif type == t_INT then
            asm_outint(val, 1)
        else
            mishap(type, 1, 'gen_field: UNKNOWN FIELD TYPE')
        endif
    endif
enddefine;

    /*  A free_struct is a vector of pairs of the form
            conspair(type_spec, value)
        (contained in an outer ref if needs double align)
    */
define :GEN free_struct(fs);
    lvars n, i, pair, fs, val, type_spec, dalign = false;
    dlocal gf_structure = fs, gf_acc_bits = false, gf_acc_nbits;
    if isref(fs) then fast_cont(fs) -> fs, true -> dalign endif;
    datalength(fs) -> n;
    fast_for i to n do
        destpair(fs(i) ->> pair) -> val -> type_spec;
        nextif(type_spec == "pointer");
        unless type_spec && t_BASE_TYPE == t_DOUBLE
        and type_spec && tv_VAL_TYPE == tv_FLOAT then
            genstructure(val) -> back(pair)
        endunless;
        type_spec &&~~ tv_VAL_TYPE -> front(pair)
    endfast_for;

    setseg(gf_structure, dalign);
    fast_for i to n do gen_field(destpair(fs(i))) endfor;
    gf_flush_bits()
enddefine;


;;; --- GENERAL RECORDCLASS, VECTORCLASS -----------------------------------

define lconstant get_class_data(item) -> (key, spec, acc_arg);
    lvars n, item, key = datakey(item), spec, keyspec, acc_arg;
    lconstant key_noconvp_spec = newassoc([]), ref0 = consref(0);

    define lconstant strip_convp(spec) -> spec;
        lvars spec;
        while isclosure(spec) do frozval(1, spec) -> spec endwhile
    enddefine;

    define lconstant get_access(/*wantp, spec*/);
#_IF isactive("pop_pas_mode")
        dlocal pop_pas_mode = false;
#_ELSE
        dlocal %pop_pas_mode, set_pop_pas_mode()% = false;
#_ENDIF
        cons_access((), false, 0)
    enddefine;

    key_noconvp_spec(key) -> spec;
    returnif(spec) (destpair(spec) -> (spec, acc_arg));
    class_field_spec(key) -> keyspec;
    if islist(keyspec) then
        ;;; recordclass
        maplist(keyspec, strip_convp) -> spec;
        get_access(
            [% for n to datalength(key) do
                fast_subscrl(n,spec) /== fast_subscrl(n,keyspec) and ref0
            endfor %], spec) -> acc_arg;
        for n to datalength(key) do
            unless f_subv(n,acc_arg) then
                class_access(n,key) -> f_subv(n,acc_arg)
            endunless
        endfor
    else
        ;;; vectorclass
        if (strip_convp(keyspec) ->> spec) == keyspec then
            class_fast_subscr(key)
        else
            get_access(ref0, conspair(spec, false))
        endif -> acc_arg
    endif;
    conspair(spec, acc_arg) -> key_noconvp_spec(key)
enddefine;

define lconstant gen_class(init, struct_spec, pop_arg, acc_arg, gf_structure);
    lvars struct_spec, init, pop_arg, acc_arg;
    dlocal gf_structure, gf_acc_bits = false, gf_acc_nbits;

    setseg(gf_structure, struct_spec(STRUC_FLAGS) &&/=_0 M_STRUC_DALIGN);
    match_struct_init(init, struct_spec, gen_field, pop_arg, acc_arg, true) ->;
    gf_flush_bits()
enddefine;

define gen_recordclass(r);
    lvars n = 1, spec, r, (key, speclist, accvec) = get_class_data(r);
    fast_for spec in speclist do
        nextif(spec == ">->");
        if spec == "full" then
            genstructure(f_subv(n, accvec)(r)) ->
        endif;
        n+1 -> n
    endfor;
    chain(r, pop_struct_spec(speclist,0), key, accvec, r, gen_class)
enddefine;

define gen_vectorclass(v);
    lvars   i, len = datalength(v), v, struct_spec,
            (key, spec, procedure subp) = get_class_data(v);

    pop_struct_spec(spec, 0) -> struct_spec;
    if spec == "full" then
        fast_for i to len do genstructure(subp(i, v)) -> endfor
    elseif len fi_rem WORD_BYTES == 0
    and front(struct_spec(STRUC_FIELD_LIST)(POP_VECTOR_FIELD)
            (FIELD_TYPE_SPEC))(FIELD_TYPE_SPEC) && t_BASE_TYPE == t_BYTE
    then
        ;;; add 0 to end to force extra 0 word to be generated for
        ;;; null termination
        v <> class_init(key)(1) -> v
    endif;

    ;;; create init struct for VECTOR struct
    chain(consvector(len, key, v, 3), struct_spec, true, subp, v, gen_class)
enddefine;


;;; --- KEYS -------------------------------------------------------------

define :GEN gen_key(key);
    lvars   n, flags, type, key, isrec, struct_spec, no_full_from_ptr,
            spec_list, all_full, after_key, spec, fid, full_table, gctype,
            s_equals_lab, getsize, accp_lab, type_code,
            init_lab, subs_lab, fsubs_lab, dataw_lab, kspec_lab, recog_lab,
            s_print_lab, print_lab, p, equals_lab, apply_lab, hash_lab,
            cons_lab, dest_lab, extern_type, key_spec = class_field_spec(key)
        ;

    islist(key_spec) -> isrec;
    pop_struct_spec(key_spec, 0) -> struct_spec;
    struct_spec(STRUC_FIELD_LIST) -> spec_list;

    true -> no_full_from_ptr;

    if isrec then
        ;;; recordclass
        key_spec /== [] -> all_full;
        false -> after_key;
        ;;; full field offset table (if needed)
        {%  for spec in spec_list do
                nextunless(spec(FIELD_IDENT) ->> fid);
                if fid == "KEY" then
                    true -> after_key
                elseif spec(FIELD_TYPE_SPEC) && tv_VAL_TYPE == tv_FULL then
                    not(after_key) -> no_full_from_ptr;
                    spec(FIELD_OFFSET)
                else
                    false -> all_full
                endif
            endfor
        %} -> full_table;
        if all_full then
            false -> full_table;
            [Sys Eq__Fullrec], GCTYPE_USERREC
        else
            [Sys Eq__Record], GCTYPE_USERNFREC
        endif -> (s_equals_lab, gctype);
        M_K_RECORD -> flags;
        [Sys Record_getsize] -> getsize;

        ;;; record-specific access procedures
        genstructure({% fast_for n to datalength(key) do
                            class_access(n, key)
                        endfor
                     %}) -> accp_lab;

    else
        ;;; vectorclass
        lconstant vgetsize = newassoc([
                                [^t_BIT     [Sys Bitvec_getsize]]
                                [^t_BYTE    [Sys Bytevec_getsize]]
                                [^t_SHORT   [Sys Shortvec_getsize]]
                                [^t_INT     [Sys Intvec_getsize]]
                                [^t_DOUBLE  [Sys Doublevec_getsize]]
                             ]);

        front(spec_list(POP_VECTOR_FIELD)(FIELD_TYPE_SPEC))(FIELD_TYPE_SPEC)
                                                -> type_code;
        type_code && t_BASE_TYPE -> type;
        vgetsize(type) -> getsize;
        M_K_VECTOR -> flags;
        if type_code && tv_VAL_TYPE == tv_FULL then
            false -> no_full_from_ptr;
            flags || M_K_FULL_VECTOR -> flags;
            [Sys Eq__Fullvec], GCTYPE_USERVEC
        else
            [Sys Eq__Nfullvec], GCTYPE_USERNFVEC
        endif -> (s_equals_lab, gctype);

        ;;; vector-specific procedures
        genstructure(class_init(key))   -> init_lab;
        genstructure(class_subscr(key)) -> subs_lab;
        genstructure(class_fast_subscr(key)) -> fsubs_lab;
    endif;

    ;;; generate remaining full fields

    genstructure(class_dataword(key))   -> dataw_lab;
    genstructure(key_spec)              -> kspec_lab;
    genstructure(class_recognise(key))  -> recog_lab;

    perm_const_lab([Sys Data_print])        -> s_print_lab;
    if (class_print(key) ->> p) == sys_syspr then s_print_lab -> p endif;
    genstructure(consref(p))                -> print_lab;
    perm_const_lab(s_equals_lab)            -> s_equals_lab;
    if (class_=(key) ->> p) == sys_= then s_equals_lab -> p endif;
    genstructure(consref(p))                -> equals_lab;
    genstructure(consref(class_apply(key))) -> apply_lab;
    genstructure(consref(class_hash(key)))  -> hash_lab;

    genstructure(class_cons(key))       -> cons_lab;
    genstructure(class_dest(key))       -> dest_lab;

    ;;; other flags
    flags || M_K_COPY -> flags;
    if no_full_from_ptr then
        flags || M_K_NO_FULL_FROM_PTR || M_K_BYTE_ACCESS -> flags
    endif;
    if struct_spec(STRUC_FLAGS) &&/=_0 M_STRUC_DALIGN then
        flags || M_K_DOUBLE_ALIGN -> flags
    endif;
    if (class_attribute(key, "writeable") ->> spec) /== "undef" then
        flags || if spec then M_K_WRITEABLE else M_K_NONWRITEABLE endif
                                                    -> flags
    endif;
    if class_attribute(key, "external_ptr") then
        (flags || M_K_EXTERN_PTR) &&~~ M_K_BYTE_ACCESS -> flags;
        if class_attribute(key, "external_ptr_props") then
            flags || M_K_EXTERN_PTR_PROPS -> flags
        endif
    endif;
    if class_attribute(key, "external_deref") then
        EXTERN_TYPE_DEREF
    else
        EXTERN_TYPE_NORMAL
    endif -> extern_type;

    ;;; now produce the key

    ;;; GC_KEY_FIELDS
    gen_header( 0,                              ;;; K_GC_RELOC
                [key_key],                      ;;; KEY
                key, false);
    asm_outint( flags,                          ;;; K_FLAGS
                gctype,                         ;;; K_GC_TYPE
                2);
    asm_outword(perm_const_lab(getsize),        ;;; K_GET_SIZE
                1);

    ;;; rest of SIMPLE_KEY_FIELDS
    asm_outword(dataw_lab,                      ;;; K_DATAWORD
                kspec_lab,                      ;;; K_SPEC
                recog_lab,                      ;;; K_RECOGNISER
                apply_lab,                      ;;; K_APPLY
                4);
    asm_outword(s_equals_lab,                   ;;; K_SYS_EQUALS
                equals_lab,                     ;;; K_EQUALS
                s_print_lab,                    ;;; K_SYS_PRINT
                print_lab,                      ;;; K_PRINT
                hash_lab,                       ;;; K_HASH
                5);
    asm_outbyte(NUMTYPE_NON_NUMBER,             ;;; K_NUMBER_TYPE
                PROLOG_TYPE_OTHER,              ;;; K_PLOG_TYPE
                extern_type,                    ;;; K_EXTERN_TYPE
                0,                              ;;; K_SPARE_BYTE
                4);

    if isrec then
        ;;; RECORD_KEY_FIELDS (KEY_R part)
        asm_outint( t_offset(struct_spec(STRUC_TYPE_SPEC),false), ;;; K_RECSIZE_R
                    1);
        asm_outword(cons_lab,                   ;;; K_CONS_R
                    dest_lab,                   ;;; K_DEST_R
                    accp_lab,                   ;;; K_ACCESS_R
                    3);
        if full_table then
            ;;; KEY_R_NAFULL part
            lvars tabsize = datalength(full_table)*INT_OFFS;
            asm_outint(tabsize, 1);                     ;;; K_FULL_OFFS_SIZE
            fast_for n to datalength(full_table) do     ;;; K_FULL_OFFS_TAB[]
                asm_outint(f_subv(n, full_table), 1)
            endfor;
            if testdef asm_align_word then weakref asm_align_word() endif
        endif
    else
        ;;; KEY_V part
        asm_outint( if type == t_BIT then       ;;; K_FIELD_CODE_V
                        -t_offset(type_code,false)
                    else
                        type
                    endif, 1);
        asm_outword(init_lab,                   ;;; K_INIT_V
                    cons_lab,                   ;;; K_CONS_V
                    dest_lab,                   ;;; K_DEST_V
                    subs_lab,                   ;;; K_SUBSCR_V
                    fsubs_lab,                  ;;; K_FAST_SUBSCR_V
                    5)
    endif
enddefine;


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 20 1997
        Fixed bug in GEN device and brought it up-to-date.
--- John Gibson, Mar 24 1997
        Fixed gen_full2 to work with ratios
--- John Gibson, Feb 18 1997
        Added GEN string16 and GEN dstring16(s).
        Added device encoding fields to GEN device.
--- John Gibson, Sep 18 1995
        Changed GEN procedure_label to generate an absolute addr in
        PLAB_OFFSET, indicated by true in PLAB_LABEL. Removed
        plab_off*set_lab property (now redundant).
--- John Gibson, Apr  8 1995
        Changes to key structure
--- John Gibson, Mar 21 1995
        Changes to cope with double external pointers etc
--- John Gibson, Feb  9 1995
        Changed current_asm_segment to have the ASMSEG_ values defined in
        common.ph
--- John Gibson, Feb  9 1995
        Made rawstruct_wrap and GEN exptr_mem use woffs_struct_wrap
--- John Gibson, Oct 11 1994
        Made current_asm_segment perm, with values "writeable", "nonwriteable"
        or "undef".
--- John Gibson, May 19 1993
        Added :GEN exfunc_closure
--- Robert Duncan, May 12 1993
        Allowed for optional definition of asm_struct_wrap (in "asmout.p")
        for generating rawstructs.
--- John Gibson, May  3 1993
        Fixed GEN gen_key to add M_K_EXTERN_PTR_PROPS when appropriate
--- John Gibson, Mar 22 1993
        Improved :GEN external_ptr
--- John Gibson, Oct  9 1992
        Added :GEN device
--- John Gibson, Sep 26 1992
        Added gen_incremental_property
--- John Gibson, Jul 21 1992
        Version 14.21 changes
--- John Gibson, Mar 25 1992
        Added 'define :GEN ...' syntax for generator procedures, and added
        dstring generator.
--- John Gibson, Sep 13 1991
        Changed string representation to guarantee null-termination.
        Now uses rawstruct structure for wrapping instead of strings
--- John Gibson, Mar 14 1990
        Changes to -GEN_gen_key-
--- John Gibson, Jan  7 1990
        Version 13.7 for new pointers.
--- Rob Duncan, Jun 27 1989
        Added missing second argument to -label_of- in -GEN_prologterm-
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, Apr 26 1989
        Version 13.64 changes
--- John Gibson, Feb 13 1989
        Further changes to generate more structures
--- John Gibson, Jan 29 1989
        Changes for new version of popc
--- John Gibson, Feb  9 1988
        Further changes for weakref, sectioning, etc
--- John Gibson, Jan 17 1988
        Changes for coping with sections, weakrefs, new format for assembler
        files, etc, etc.
 */
