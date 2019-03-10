/* --- Copyright University of Sussex 2005. All rights reserved. ----------
 > File:            C.all/src/syscomp/poplink_main.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                    POP SYSTEM LINKER MAIN ROUTINES

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'
#_INCLUDE 'wdefs.ph'

uses sys_translate_exlibs, exload_merge_objfiles;

vars 0 (XLINK_EXLIBFILES, XLINK_EXLIBDIRS);

section $-Popas;

weak constant procedure (sunfp_poplink_flag, asm_align_word);


;;; --- GENERATING ASSEMBLER OUTPUT FILES -------------------------------------

lconstant
    macro (
        CODE            = "asm_startcode",
        DATA            = "asm_startdata",
        )
    ;

lvars procedure (
    topsect_word_info,
    sect_word_info,
    word_string_lab,
    sect_prop,
    weak_depend_prop,
    general_assign_prop,
    ident_assign_prop,
    wordid_assign_prop,
    dummy_value_prop,
    dummy_ident_prop,
    fileout_1,
    fileout_2,
    fileout_3,
    fileout_4,
    struct_gen_prop,
    );

lvars
    top_section,
    run_time_sect,
    gen_whole_sect,
    sect_pathstring,
    sect_id_list,
    dictvec,
    attached_wids,
    in_vec_idents,
    lab_assign_list,
    module_forcing_labels,
    false_lab, true_lab, nil_lab,
    string_key_lab, word_key_lab, ident_key_lab, pair_key_lab,
    vector_key_lab,
    unique_struct_labels,
    abort,
    x_complete,
    prolog_closures,
    ;


define lconstant asm_outword_1 = asm_outword(%1%) enddefine;

define lconstant isweak(info, leave_open);
    lvars type, info, i, leave_open;
    f_subv(INFO_TYPE, info) -> type;
    if type &&/=_0 IDT_STRONG_REF then
        ;;; strong already
        return(false)
    elseif type &&=_0 IDT_WEAK_DEPENDS then
        ;;; not dependent on anything else
        return(true)
    endif;

    ;;; weakness depends on other identifiers all being weak
    ;;; -- check these recursively
    type fi_&&~~ IDT_WEAK_DEPENDS -> f_subv(INFO_TYPE, info);
    fast_for i in weak_depend_prop(info) do
        unless isweak(i, leave_open) then
            ;;; one is strong, so this becomes strong
            type fi_|| IDT_STRONG_REF -> f_subv(INFO_TYPE, info);
            [] -> weak_depend_prop(info);   ;;; might as well clear this
            return(false)
        endunless
    endfor;

    ;;; is weak (as yet)
    if leave_open then
        type fi_|| IDT_WEAK_DEPENDS -> f_subv(INFO_TYPE, info)
    endif;
    true
enddefine;      /* isweak */

lconstant charout_segp = newproperty([], 4, identfn, "tmparg");
;;;
define lconstant active asmf_charout_seg;
    charout_segp(asmf_charout)
enddefine;
;;;
define updaterof active asmf_charout_seg segp;
    lvars segp;
    unless charout_segp(asmf_charout) == segp or segp == identfn then
        segp();
        segp -> charout_segp(asmf_charout)
    endunless
enddefine;

define lconstant unique_struct_lab(/*item*/);
    lvars l = fast_lmember((), unique_struct_labels);
    l and f_hd(f_tl(l))
enddefine;

define lconstant out_typed_lab();
    if isref(dup()) then asm_outglab(fast_cont()) else asm_outlab() endif
enddefine;

define lconstant out_typed_labs = applist(%out_typed_lab%) enddefine;

define lconstant prefix_lab(lab, pre);
    lvars pre, lab;
    if isref(lab) then consref(pre sys_>< fast_cont(lab))
    else pre sys_>< lab
    endif
enddefine;

define lconstant get_sw_lab(swlab);
    lvars swlab;
    if isword(swlab) then
        f_subv(INFO_WORD_LAB, topsect_word_info(swlab))
    else
        swlab
    endif
enddefine;

define lconstant gen_nonpop_ident(pathname, info, asmf_charout);
    lvars pathname, info, init = f_subv(INFO_INIT, info), idlab;
    dlocal asmf_charout;
    unless init then '0' -> init endunless;
    asm_outglab(asm_identlabel(pathname));
    ;;; include labels for any global assignments to this identifier
    fast_for idlab in ident_assign_prop(info) do asm_outglab(idlab) endfor;
    asm_outword_1(init)
enddefine;

define lconstant gen_pair(/* lab, outlab_p, */ f, b);
    lvars f, b;
    ;;; P_FRONT, KEY
    asm_outword(get_sw_lab(f), pair_key_lab, 2);
    fast_apply();       ;;; outlab_p(lab)
    ;;; P_BACK
    asm_outword_1(get_sw_lab(b))
enddefine;

define lconstant gen_vec_header();
    ;;; V_LENGTH, KEY
    asm_outword(/*length*/, vector_key_lab, 2)
enddefine;

define lconstant get_id_initlab(word, info);
    lvars word, info;
    if f_subv(INFO_IDPROPS, info) &&/=_0 M_ID_CONSTANT then
        consref(asm_symlabel(if sect_pathstring then
                                sect_pathstring <> fast_word_string(word)
                             else
                                word
                             endif))
    else
        nextlab()
    endif
enddefine;

define lconstant getlabstr();
    if isref(dup()) then fast_cont() endif
enddefine;

define lconstant gen_closure(lab_list, props, nargs, flags, b_pdpt, u_pdpt,
                                                        b_froz, u_froz);
    lvars   lab_list, upd_labs, props, nargs, flags, b_pdpt, u_pdpt,
            b_froz, u_froz;

    define lconstant gen_clos(lab_list, nargs, upd, pdpt, frozl);
        lvars   f, lab_list, nargs, upd, len_lab, pdpt, frozl,
                nfroz = listlength(frozl),
                len = CLOSURE_HDR_LEN + nfroz;

        asm_outword(props,                      ;;; PD_PROPS
                    asm_symlabel('procedure_key'),  ;;; KEY
                    2);
        out_typed_labs(lab_list);               ;;; labels
        maplist(lab_list, prefix_lab(%'x'%)) -> lab_list;
        asm_outword(getlabstr(hd(lab_list)),    ;;; PD_EXECUTE
                    getlabstr(upd),             ;;; PD_UPDATER
                    2);
#_IF DEF ALPHA_OSF1
        ;;; crummy assembler doesn't like .long label where label is forward
        ;;; defined
        len -> len_lab;     ;;; OK since asm_gen_poplink_code returns 0
#_ELSE
        nextlab() -> len_lab;
#_ENDIF
        asm_outint(len_lab, 1);                 ;;; PD_LENGTH
        asm_outbyte(flags,                      ;;; PD_FLAGS
                    nargs,                      ;;; PD_NARGS
                    2);
        asm_outshort(nfroz, 1);                 ;;; PD_CLOS_NFROZ
        asm_outword_1(pdpt);                    ;;; PD_CLOS_PDPART
        fast_for f in frozl do
            asm_outword_1(getlabstr(f))         ;;; PD_CLOS_FROZVALS
        endfor;
        out_typed_labs(% lab_list %),           ;;; exec labs
        asm_gen_poplink_code((), nfroz, prefix_lab(pdpt,'x')) + len -> len;
#_IF not(DEF ALPHA_OSF1)
        asm_outlabset(len_lab, len)
#_ENDIF
    enddefine;

    CODE -> asmf_charout_seg;
    if u_pdpt then
        maplist(lab_list, #_< prefix_lab(%'u'%) >_#)
    else
        [^false_lab]
    endif -> upd_labs;
    flags fi_|| #_< M_PD_CLOSURE||M_PD_CLOS_PROTECT >_# -> flags;
    unless nargs then 16:FFFF -> nargs endunless;
    gen_clos(lab_list, nargs&&16:FF, hd(upd_labs), b_pdpt, b_froz);
    if u_pdpt then
        flags fi_&&~~ M_PD_CLOS_PROPERTY -> flags;      ;;; not for updater
        gen_clos(upd_labs, nargs>>8, false_lab, u_pdpt, u_froz)
    endif
enddefine;

define gen_incremental(word, info) -> last_lab;
    lvars   incr_lab, n, l, last_lab, pt_active, pt_count, pt_default,
            pt_entry_key, pt_eq_pdr, pt_expand, pt_hash_pdr, tab_size,
            pdprops_lab, pdpart_lab, updpart_lab, rehash, tab_lab, prop_lab,
            gctype, word, info, pair = f_subv(INFO_INIT, info),
            (incrs, type) = destpair(back(pair));
    dlocal  asmf_charout_seg;

    define lconstant tstr_lab(tstring);
        lvars tstring;
        lconstant asm_pdrs = {  ^identfn            ;;; TYPESTR_INT
                                ^identfn            ;;; TYPESTR_FLOAT (not impl)
                                ^asm_symlabel       ;;; TYPESTR_CONST
                                ^asm_wordlabel      ;;; TYPESTR_WORD
                                ^asm_identlabel     ;;; TYPESTR_IDENT
                                ^asm_wordidlabel    ;;; TYPESTR_WORDID
                             };
        if isword(tstring) then fast_word_string(tstring) -> tstring endif;
        f_subv(f_subs(1,tstring), asm_pdrs)(allbutfirst(1,tstring))
    enddefine;

    define lconstant gen_local_vec_header(/*len, seg*/);
        /*seg*/ -> asmf_charout_seg;
        gen_vec_header(/*len*/);
        asm_outlab(dup(nextlab()))      ;;; return lab
    enddefine;


    if isclosure(type) then
        ;;; property -- type is a closure containing (word) labs for prop values
        explode(type) -> (pdpart_lab, updpart_lab, gctype,
                          tab_size, pt_count, pt_expand,
                          pt_default, pt_eq_pdr, pt_hash_pdr, pt_active);
        if (pdprops(type) ->> pdprops_lab) == nullstring then
            false_lab -> pdprops_lab
        endif;
        strnumber(tab_size) -> tab_size;
        asm_symlabel('Sys$-' <> fast_word_string(gctype)
                            <> '_prop_entry_key') -> pt_entry_key;

        ;;; vector of entries
        gen_local_vec_header(tab_size, DATA) -> tab_lab;
        fast_repeat tab_size times
            asm_outword_1(if incrs == [] then
                            popint(0)
                          else
                            dest(incrs) -> incrs
                          endif)
        endrepeat;
        if (listlength(incrs) ->> n) /== 0 then
            ;;; need overflow vec (doesn't need to be writeable)
            gen_local_vec_header(n, CODE);
            applist(incrs, asm_outword_1)
        else
            popint(0)
        endif -> rehash;

        ;;; property record
        DATA -> asmf_charout_seg;
        asm_outword(tab_lab,                ;;; PT_TABLE
                    asm_symlabel('Sys$-property_key'),  ;;; KEY
                    2);
        asm_outlab(nextlab() ->> prop_lab);
        asm_outword(tstr_lab(pt_active),    ;;; PT_ACTIVE
                    tstr_lab(pt_count),     ;;; PT_COUNT
                    tstr_lab(pt_default),   ;;; PT_DEFAULT
                    pt_entry_key,           ;;; PT_ENTRY_KEY
                    tstr_lab(pt_eq_pdr),    ;;; PT_EQ_PDR
                    tstr_lab(pt_expand),    ;;; PT_EXPAND
                    tstr_lab(pt_hash_pdr),  ;;; PT_HASH_PDR
                    rehash,                 ;;; PT_REHASH
                    8);

        get_id_initlab(word, info) -> last_lab;
        ;;; outer closures
        gen_closure(last_lab::[], pdprops_lab, false, M_PD_CLOS_PROPERTY,
                        pdpart_lab, updpart_lab, dup(prop_lab::[]));

    else
        ;;; list or procedure -- type is flags integer
        ;;; sort the entries from individual files in order of precedence
        ;;; (backwards, so they're generated in the right order)
        syssort(incrs, false,   procedure(l1,l2);
                                    lvars l1,l2;
                                    f_hd(l1) fi_>= f_hd(l2)
                                endprocedure) -> incrs;

        if type fi_&& WRINCRF_TYPE == WRINCRT_LIST then
            ;;; list
            nil_lab -> last_lab;
#_IF DEF WINDOWS
            ;;; can't explain this, but for some reason incremental
            ;;; lists in the code section just don't seem to work
            DATA -> type;
#_ELSE
            if type &&/=_0 WRINCRF_WRITEABLE then DATA else CODE endif -> type;
#_ENDIF
            fast_for pair in incrs do
                fast_for incr_lab in fast_ncrev(f_tl(pair)) do  ;;; skip prec
                    type -> asmf_charout_seg;
                    gen_pair(dup(nextlab()), asm_outlab, incr_lab, last_lab)
                                                    -> last_lab
                endfor
            endfor
        else
            ;;; procedure
            [] -> l;
            fast_for pair in incrs do
                f_tl(pair) nc_<> l -> l         ;;; skip prec
            endfor;
            if (listlength(l) ->> n) == 0 then
                asm_symlabel('identfn') -> last_lab
            elseif n == 1 then
                f_hd(l) -> last_lab
            else
                ;;; appdata(%p_vec, fast_apply%)
                gen_local_vec_header(n, CODE) -> n;
                applist(l, asm_outword_1);
                nextlab() -> last_lab;
                gen_closure([^last_lab], false_lab, false, 0,
                            asm_symlabel('appdata'), false,
                            [%n, asm_symlabel('fast_apply')%], false);
            endif
        endif
    endif;

    sys_grbg_list(incrs)
enddefine;

    /*  Generate permanent identifier record
    */
define lconstant gen_ident(info, idlab);
    lvars type, idprops, info, prmflags, idlab, lab_p;

    f_subv(INFO_TYPE, info) -> type;
    f_subv(INFO_IDPROPS, info) -> idprops;

    if idprops &&/=_0 M_ID_CONSTANT then
        idprops fi_|| #_< M_ID_PROTECT || M_ID_ASSIGNED_CONST >_# -> idprops
    elseif type &&/=_0 IDT_PROTECT then
        idprops fi_|| M_ID_PROTECT -> idprops
    endif;

    type fi_&& M_PERM_FLAG_MASK -> prmflags;
    if type &&/=_0 IDT_GEN_IDENT then
        ;;; label is global
        prmflags fi_|| M_PERM_SYS_REFER -> prmflags;
        asm_outglab
    else
        ;;; local label
        asm_outlab
    endif -> lab_p;

    ;;; 2-word header
    ;;; ID_IDENTPROPS
    asm_outshort(idprops fi_&& #_< 1<<SHORT_BITS-1 >_#, 1);
    ;;; ID_PERM_FLAGS, ID_NUM_ATTRIBUTE
    asm_outbyte(prmflags, idprops fi_>> SHORT_BITS, 2);
    if testdef asm_align_word then weakref asm_align_word() endif;
    ;;; KEY
    asm_outword_1(ident_key_lab);

    ;;; identifier label
    fast_apply(idlab, lab_p);
    ;;; include labels for any global assignments to this identifier
    fast_for idlab in ident_assign_prop(info) do asm_outglab(idlab) endfor;

    ;;; ID_VALOF
    asm_outword_1(getlabstr(f_subv(INFO_INIT, info)))
enddefine;

define lconstant mark_topsect_word(word);
    lvars word, info = topsect_word_info(word);
    f_subv(INFO_TYPE, info) fi_|| IDT_GEN_WORD -> f_subv(INFO_TYPE, info);
enddefine;

    /*  Generate a word identifier
    */
define lconstant gen_wordid(pathname, info, idlab, dictnext_lab) -> widlab;
    lvars lab, info, pathname, idlab, dictnext_lab, widlab;

    ;;; W_IDENTIFIER, KEY
    asm_outword(idlab, word_key_lab, 2);

    ;;; label(s)
    asm_outglab(asm_wordidlabel(pathname) ->> widlab);
    ;;; include labels for other global assignments to this word identifier
    fast_for lab in wordid_assign_prop(info) do asm_outglab(lab) endfor;
    [] -> wordid_assign_prop(info);

    ;;; W_STRING, W_DICT_NEXT
    asm_outword(word_string_lab(pathname), dictnext_lab, 2)
enddefine;      /* gen_wordid */

define lconstant make_dummy_value(lab, idprops);
    lvars entry, n, lab, lablist, idprops;
    dummy_value_prop(
        if idprops &&/=_0 M_ID_ACTIVE then
            ;;; get multiplicity
            if idprops &&/=_0 M_ID_OPERATOR then
                1
            else
                idprops fi_>> SHORT_BITS
            endif
        else
            ;;; not active
            true
        endif) -> entry;
    if idprops &&=_0 M_ID_PROCEDURE then 1 else 2 endif -> n;
    f_subv(n, entry) -> lablist;
    if lab then
        ;;; just add new global lab (no result)
        consref(lab) :: lablist -> f_subv(n, entry)
    else
        ;;; return lab, generating local lab if none
        if lablist == [] then
            dup(nextlab()) :: [] -> f_subv(n, entry)
        elseif isref(dup(fast_front(lablist))) then
            fast_cont()
        endif
    endif
enddefine;

define lconstant gen_word(word, info, idlab);
    lvars word, lab, wlab, info, dictnext, dindex, idlab, assign_labs, type;

    returnunless(isword(f_subv(INFO_WORD_LAB, info)));  ;;; generated already

    ;;; W_IDENTIFIER, KEY
    asm_outword(if idlab then idlab else popint(0) endif, word_key_lab, 2);

    ;;; get label to be used for word
    if (general_assign_prop(word) ->> assign_labs) /== [] then
        [] -> general_assign_prop(word)
    endif;
    f_subv(INFO_TYPE, info) -> type;
    false -> wlab;
    if type &&/=_0 IDT_GLOB_WORD then
        asm_outglab(asm_wordlabel(word) ->> wlab)
    endif;
    if idlab and type &&/=_0 IDT_GEN_PDPROPS then
        asm_outglab(asm_pdpropslabel(word) ->> lab);
        unless wlab then lab -> wlab endunless
    elseunless wlab then
        if assign_labs /== [] then
            fast_destpair(assign_labs) -> assign_labs -> wlab;
            asm_outglab(wlab)
        else
            asm_outlab(nextlab() ->> wlab)
        endif
    endif;

    ;;; insert in dictionary
    word_dict_cell(word) -> dindex;
    dictvec(dindex) -> dictnext;
    wlab -> dictvec(dindex);
    ;;; save lab for later -- marks word generated
    wlab -> f_subv(INFO_WORD_LAB, info);

    ;;; include labels for other global assignments to this word
    fast_for lab in assign_labs do asm_outglab(lab) endfor;

    ;;; W_STRING, W_DICT_NEXT
    asm_outword(word_string_lab(word), dictnext, 2)
enddefine;      /* gen_word */

define lconstant gen_prolog_closure(pair);
    lvars   n, len, A, F, (word, info) = fast_destpair(pair),
            froz1, froz2, ppart, pair;

    define lconstant get_word_lab(word);
        lvars word, info = topsect_word_info(word);
        if f_subv(INFO_IDENT_LAB, info) == nullword then    ;;; top pathname
            false -> f_subv(INFO_IDENT_LAB, info)
        endif;
        gen_word(word, info, false);
        get_sw_lab(word)
    enddefine;

    ;;; parse name
    if locchar(`/`, 1, word) ->> n then
        datalength(word) - n -> len;
        substring(n+1, len, word) -> A;     ;;; arity
        allbutlast(len+1, word) -> F        ;;; functor
    endif;
    unless n and (strnumber(A) ->> A) then
        mishap(word, 1, 'INVALID PROLOG PREDICATE NAME')
    endunless;

    if f_subv(INFO_TYPE, info) &&/=_0 IDT_PROLOG_DYNAMIC then
        ;;; dynamic
        ;;; $-prolog$-prolog_interpreter(% A, writeable conspair([],[]) %)
        popint(A);
        gen_pair(dup(nextlab()), asm_outlab, nil_lab, nil_lab);
        '$-prolog$-prolog_interpreter'
    elseif F /== "call" then
        ;;; static
        ;;; $-prolog$-undefined/0(% F, A %)
        get_word_lab(F);
        popint(A);
        '$-prolog$-undefined/0'
    else
        ;;; special
        ;;; $-prolog$-call/N(% writeable initv(A - 1), A - 1 %)
        lvars ulab = get_word_lab("undef");
        A-1 -> A;
        gen_vec_header(A), asm_outlab(dup(nextlab()));      ;;; return lab
        repeat A times asm_outword_1(ulab) endrepeat;
        popint(A),
        '$-prolog$-call/N'
    endif -> (froz1, froz2, ppart);

    dlocal asmf_charout_seg = CODE;
    gen_closure(f_subv(INFO_INIT, info)::[], f_subv(INFO_NEXT_INFO, info),
                false, 0, asm_symlabel(ppart), false, [^froz1 ^froz2], false)
enddefine;

    ;;; Generate words and nonexported/nonpop identifiers
define lconstant gen_pass1(word, info);
    lvars   word, type, idprops, init, info, idlab, Weak, ValUsed,
            non_pop, genwid, lab, entry, item;
    dlvars  pathname;

    define :inline lconstant PATHNAME();
        (pathname or (sect_pathstring <> fast_word_string(word) ->> pathname))
    enddefine;

    ;;; called only for strongly-referenced identifiers
    define lconstant gen_info_init(word, info) -> type;
        lvars   word, info, init = f_subv(INFO_INIT, info), type;

        f_subv(INFO_TYPE, info) -> type;
        unless init then
            if f_subv(INFO_TYPE, info) &&/=_0 IDT_PROLOG_PRED then
                ;;; uninitialised prolog pred
                conspair(word,info) :: prolog_closures -> prolog_closures;
                asm_pdpropslabel(PATHNAME()) -> f_subv(INFO_NEXT_INFO, info);
                type fi_|| IDT_GEN_PDPROPS ->> type -> f_subv(INFO_TYPE, info);
                get_id_initlab(word, info)      ;;; closure label
            else
                ;;; get default initialisation
                make_dummy_value(false, f_subv(INFO_IDPROPS, info))
            endif
        elseif ispair(init) then
            ;;; initialised to incremental property, list or procedure
            gen_incremental(word, info)
        else
            init
        endunless -> f_subv(INFO_INIT, info)
    enddefine;

    define lconstant is_unique_lab(lab);
        lvars lab, ulab, l = unique_struct_labels, item;
        repeat
            f_dest(f_dest(l)) -> (item, ulab, l);
            returnif(ulab = lab) (item, true);
            returnif(l == []) (false)
        endrepeat
    enddefine;

    define :inline lconstant ID_WARN(mess);
        printf(id_display_str(sect_pathstring, word), mess,
                                            'WARNING -- %p %p\n')
    enddefine;

    define :inline lconstant ID_ERROR(mess);
        printf(id_display_str(sect_pathstring, word), mess, '%p %p\n');
        true -> abort;
    enddefine;


    if sect_pathstring then false else word endif -> pathname;

    ;;; ensure unmarked for gen_const_id
    false -> f_subv(INFO_IDENT_LAB, info);

    f_subv(INFO_TYPE, info) -> type;
    type &&/=_0 IDT_VALUE_USED -> ValUsed;

    if type &&=_0 IDT_DECLARED then
        ;;; undef word -- generate word record if needed
        if type &&/=_0 IDT_GEN_WORD then gen_word(word, info, false) endif;
        if type &&/=_0 IDT_GEN_WORDID then
            ID_ERROR('NON-EXISTENT (WORD-) IDENTIFIER')
        elseif type &&/=_0 IDT_STRONG_REF then
            ID_WARN('NON-EXISTENT IDENTIFIER')
        endif;
        return
    endif;

    ;;; word declared as perm identifier
    f_subv(INFO_INIT, info) -> init;
    f_subv(INFO_IDPROPS, info) -> idprops;
    isweak(info, false) -> Weak;    ;;; true if referenced weakly only
    type &&/=_0 IDT_NON_POP -> non_pop;

    if type &&/=_0 IDT_GEN_TESTLAB then
        ;;; needs testdef lab generated
        asm_testdeflabel(PATHNAME());
#_IF DEF AIX
        ;;; AIX doesn't support global absolute symbols, so set
        ;;; to true if strong, false if weak
        () :: general_assign_prop(not(Weak)) -> general_assign_prop(not(Weak))
#_ELSE
        ;;; set to 1 if strong, 0 if weak
        asm_outglabset((), if Weak then '0' else '1' endif)
#_ENDIF
    endif;

    if idprops &&/=_0 M_ID_CONSTANT then
        ;;; constant
        if init then
            ;;; initialised
            if not(ValUsed) and is_unique_lab(init) then
                ;;; remove unnecessary const lab assignment to unique struct
                () -> item;
                fast_ncdelete(asm_symlabel(PATHNAME()),
                        general_assign_prop(item)) -> general_assign_prop(item)
            endif
        elseif ValUsed then
            ;;; uninitialised constant whose value has been used
            if Weak then
                ;;; need to generate dummy constant value
                asm_symlabel(PATHNAME()),
                if non_pop then
                    asm_outglabset((), '0')
                else
                    make_dummy_value((), idprops)
                endif
            elseif type &&=_0 IDT_PROLOG_PRED then
                ;;; error for used, strongly-referenced undefined constant
                ID_ERROR('UNINITIALISED CONSTANT')
            endif
        elseif type &&/=_0 IDT_USED then
            ID_ERROR('UNINITIALISED (USED) CONSTANT')
        endif
;;; else
;;;     ;;; variable
;;;     if ValUsed and not(init or Weak) then
;;;         ID_WARN('UNINITIALISED VARIABLE')
;;;     endif
    endif;

    if Weak and type &&/=_0 IDT_GEN_IDENT and not(non_pop) then
        ;;; attach ident label to dummy identifier
        unless dummy_ident_prop(idprops) ->> entry then
            ;;; get an info struct
            init_word_info(undef, undef) ->> entry
                                        -> dummy_ident_prop(idprops);
            IDT_GEN_IDENT -> f_subv(INFO_TYPE, entry);
            idprops -> f_subv(INFO_IDPROPS, entry);
            make_dummy_value(false, idprops) -> f_subv(INFO_INIT, entry)
        endunless;
        asm_identlabel(PATHNAME()) ::
            (ident_assign_prop(info) nc_<> ident_assign_prop(entry))
                                        -> ident_assign_prop(entry);
        type fi_&&~~ IDT_GEN_IDENT -> type
    endif;

    if non_pop then
        ;;; generate a non-pop ident on 'poplink_2'
        if type &&/=_0 IDT_GEN_IDENT then
            gen_nonpop_ident(PATHNAME(), info, fileout_2)
        endif;
        if type &&/=_0 IDT_GEN_WORD then gen_word(word, info, false) endif;
        return
    endif;

    ;;; pop identifier
    type &&/=_0 IDT_GEN_WORDID -> genwid;
    if run_time_sect and (gen_whole_sect or type &&/=_0 IDT_GEN_IN_DICT)
    and not(Weak) then
        ;;; identifier in dictionary/section
        gen_info_init(word, info) -> type;
        ;;; get identifier label (-gen_ident- generates id later)
        if type &&/=_0 IDT_GEN_IDENT then
            ;;; need a global label
            asm_identlabel(PATHNAME())
        else
            ;;; use local label
            nextlab()
        endif -> idlab;
        if idprops &&/=_0 M_ID_CONSTANT then
            ;;; leave constants for next pass (gen_const_id)
            idlab -> f_subv(INFO_IDENT_LAB, info)   ;;; remember label
        else
            gen_ident(info, idlab)
        endif;
        if sect_pathstring then
            ;;; section -- add to id list
            word :: (idlab :: sect_id_list) -> sect_id_list;
            mark_topsect_word(word);
            if type &&/=_0 IDT_GEN_PDPROPS then
                asm_pdpropslabel(PATHNAME()) :: general_assign_prop(word)
                                                -> general_assign_prop(word)
            endif
        else
            ;;; top level
            gen_word(word, info, idlab)
        endif;
        if genwid then
            ;;; generate 'attached' word identifier
            nextlab() -> lab;
            consvector(lab, word, run_time_sect,
                            gen_wordid(PATHNAME(), info, idlab, lab), 4)
                                :: attached_wids -> attached_wids
        endif

    else
        ;;; no identifier in dictionary/section
        if type &&/=_0 IDT_GEN_IDENT then
            ;;; identifier cell needed
            gen_info_init(word, info) -> type;
            asm_identlabel(PATHNAME()) -> idlab;
            if idprops &&/=_0 M_ID_CONSTANT then
                ;;; leave constants for next pass (gen_const_id)
                idlab -> f_subv(INFO_IDENT_LAB, info)   ;;; remember label
            elseif type &&/=_0 IDT_GEN_FULL_ID or genwid then
                ;;; use a proper identifier record
                gen_ident(info, idlab)
            else
                ;;; just use a word in a vector to save space
                ;;; -- add to list to get generated later
                idlab -> f_subv(INFO_IDENT_LAB, info);  ;;; remember label
                info :: in_vec_idents -> in_vec_idents
            endif
        elseif idprops &&/=_0 M_ID_CONSTANT and not(Weak) then
            ;;; for constant incremental props -- generate the value label
            ;;; (also for prolog preds)
            gen_info_init(word, info) -> type
        endif;
        if genwid then
            ;;; generate 'unattached' word identifier
            gen_wordid(PATHNAME(), info, idlab, true_lab) ->
        endif;
        if type &&/=_0 IDT_GEN_WORD then
            ;;; top level sect only
            gen_word(word, info, false)
        endif;
        if type &&/=_0 IDT_GEN_PDPROPS then
            ;;; set pdprops label to false by putting it out as an
            ;;; equivalent label to false on 'poplink_1'
            fileout_1 -> asmf_charout;
            asm_outglab(asm_pdpropslabel(PATHNAME()));
            fileout_3 -> asmf_charout
        endif
    endif
enddefine;

define lconstant sect_pass1(pathname, sect);
    lvars   pathname, sect, flags = f_subv(SECT_FLAGS, sect);
    dlocal  run_time_sect, gen_whole_sect,
            sect_pathstring = false, sect_id_list = [];

    if flags &&/=_0 #_< SECF_RUN_TIME || SECF_HAS_DICT_IDS >_# then
        flags &&/=_0 SECF_RUN_TIME -> gen_whole_sect;
        sect
    else
        false
    endif -> run_time_sect;

    if pathname /= nullword then
        ;;; not top-level
        fast_word_string(pathname) <> '$-' -> sect_pathstring;
        if flags &&/=_0 SECF_GENERATE then
            ;;; mark extra words needed etc
            mark_topsect_word(f_subv(SECT_NAME, sect));
            applist(f_subv(SECT_IMPORTS, sect), mark_topsect_word);
            applist(f_subv(SECT_EXPORTS, sect), mark_topsect_word)
        endif
    endif;

    fast_appproperty(f_subv(SECT_WORD_INFO, sect), gen_pass1);
    sect_id_list -> f_subv(SECT_ID_LIST, sect)
enddefine;

define lconstant gen_sect(sect);
    lvars   word, l, pathname, sect, supersect, subsects, imports, exports,
            subsect, namelab, lab, superex, idlist
        ;

    define lconstant gen_list(list) -> lab;
        lvars list, item, lab;
        if list == [] then
            nil_lab -> lab
        else
            fast_destpair(list) -> (item, list);
            gen_list(list) -> list;
            gen_pair(nextlab() ->> lab, asm_outlab, item, list)
        endif
    enddefine;

    returnif(f_subv(SECT_FLAGS, sect) &&=_0 SECF_GENERATE) (false);

    ;;; get sect label
    if (f_subv(SECT_PATHNAME, sect) ->> pathname) == nullword then
        ;;; top-level
        [] ->> imports -> exports;
        false_lab ->> namelab -> supersect;
        asm_symlabel('pop_section')
    else
        f_subv(SECT_SUPERSECT, sect) -> supersect;
        f_subv(SECT_EXPORTS, supersect) -> superex;
        f_subv(SECT_EXPORTS, sect) -> exports;
        f_subv(SECT_IMPORTS, sect) -> imports;
        [%  fast_for word in exports do
                unless fast_lmember(word, imports) then
                    word :: imports -> imports
                endunless;
                word,
                if fast_lmember(word, superex) ->> l then
                    fast_front(fast_back(l))
                else
                    f_subv(SECT_LABEL, supersect)
                endif
            endfast_for
        %] ->> exports -> f_subv(SECT_EXPORTS, sect);
        get_sw_lab(f_subv(SECT_NAME, sect)) -> namelab;
        f_subv(SECT_LABEL, supersect) -> supersect;
        nextlab()
    endif ->> lab -> f_subv(SECT_LABEL, sect);

    ;;; generate list of subsection labels
    [%  fast_for subsect in f_subv(SECT_SUBSECTS, sect) do
            if gen_sect(subsect) ->> l then l endif
        endfast_for
    %] -> subsects;


    gen_list(subsects) -> subsects;
    gen_list(f_subv(SECT_ID_LIST, sect)) -> idlist;
    gen_list(imports) -> imports;
    gen_list(exports) -> exports;

    ;;; generate structure as defined in sections.ph
    asm_outword(
            namelab,                            ;;; SEC_NAME
            asm_symlabel('section_key'),        ;;; KEY
            2);
    if pathname == nullword then asm_outglab else asm_outlab endif(lab);
    asm_outword(
            word_string_lab(pathname),          ;;; SEC_PATH_NAME
            supersect,                          ;;; SEC_SUPERSECT
            subsects,                           ;;; SEC_SUBSECTS
            idlist,                             ;;; SEC_IDENTS
            gen_list([]),                       ;;; SEC_WORD_IDENTS
            imports,                            ;;; SEC_IMPORTS
            exports,                            ;;; SEC_EXPORTS
            gen_list([]),                       ;;; SEC_RESTORE
            popint(f_subv(SECT_LEVEL, sect)),   ;;; SEC_LEVEL
            9
            );

    ;;; return label to say generated
    lab
enddefine;

    ;;; generate a constant identifier on second pass
define lconstant gen_const_id(word, info);
    lvars info, idlab, word;
    if f_subv(INFO_IDENT_LAB, info) ->> idlab then
        chain(info, idlab, gen_ident)
    endif
enddefine;

define lconstant startfile(dir, name) -> asmf_charout;
    lvars dir, name;
    dlocal asmf_charout;
    if dir then
        dir dir_>< name <> ASM_EXTENSION
    else
        new_tmp_file(false, 'plnk', ASM_EXTENSION)
    endif -> name;
    discout(name) -> asmf_charout;
    add_created_file(name);
    asm_startfile(pdprops(asmf_charout));
    name -> pdprops(asmf_charout)
enddefine;

define lconstant out_objmod_pad(len);
    lvars len;
    asm_outword(len, asm_symlabel('Sys$-objmod_pad_key'), 2)
enddefine;

#_IF not(DEF asm_align_file)
lconstant procedure asm_align_file = identfn;
#_ENDIF

define lconstant endfile(asmf_charout, align);
    lvars align;
    dlocal asmf_charout;
    if align then asm_align_file() endif;
    asm_endfile();
    asmf_charout(termin)
enddefine;

define lconstant gen_string(/* lab, outlab_p */ string) with_nargs 3;
    lvars n, string, size, dlen = datalength(string), nullterm = false;
    ;;; V_LENGTH, KEY
    asm_outword(dlen, string_key_lab, 2);
    fast_apply();           ;;; output label

#_IF DEF asm_quick_genstring
    unless dlen /== 0 and (asm_quick_genstring(string) ->> nullterm) then
#_ENDIF
        (dlen+WORD_BYTES-1) div WORD_BYTES * WORD_BYTES -> size;
        fast_for n to size do
            if n fi_> dlen then 0 else f_subs(n,string) endif;
            if n fi_rem 15 == 0 then asm_outbyte(15) endif
        endfor;
        (n fi_- 1) fi_rem 15 -> n;
        if n /== 0 then asm_outbyte(n) endif;
#_IF DEF asm_quick_genstring
    endunless;
#_ENDIF
    ;;; if nullterm == "null" then string is already null-terminated
    if nullterm /== "null" and dlen fi_rem WORD_BYTES == 0 then
        ;;; extra zero word to ensure null-termination
        asm_outword_1(0)
    endif
enddefine;

define lconstant gen_dummies();
    lvars lab, item, pair, name, idprops, u_list, p_list;

    define lconstant lab_from(id);
        lvars id, l = fast_idval(id);
        if l /== [] then
            fast_front(l)
        else
            dup(consref(nextlab())) :: [] -> fast_idval(id)
        endif
    enddefine;

    define lconstant gen_act_undef(mult, vec);
        lvars mult, vec;

        define lconstant gen_act_clos(lab_list, id);
            lvars lab_list, id, flab;
            returnif(lab_list == []);
            lab_from(id) -> flab;
            gen_closure(lab_list, false_lab, (mult<<8), M_PD_CLOS_UNDEF,
                        asm_symlabel('identfn'),
                        asm_symlabel('erasenum'),
                        [% fast_repeat mult times flab endfast_repeat %],
                        [% flab, popint(mult+1) %] )
        enddefine;

        explode(vec),
        gen_act_clos((), ident p_list),
        gen_act_clos((), ident u_list)
    enddefine;

    ;;; set up standard undefs in dummy_value_prop first
    fast_for item, pair in_property poplink_unique_struct do
        nextunless(isundef(item));
        fast_front(pair) -> name;
        f_subv(INFO_IDPROPS, topsect_word_info(name)) -> idprops;
        fast_for lab in asm_symlabel(name) :: general_assign_prop(item) do
            make_dummy_value(lab, idprops)
        endfor
    endfor;

    explode(dummy_value_prop(true)) -> (u_list, p_list);
    false -> dummy_value_prop(true);
    fast_appproperty(dummy_value_prop, gen_act_undef);
    if p_list /== [] then
        gen_closure(p_list, false_lab, 0, M_PD_CLOS_UNDEF,
                        asm_symlabel('Sys$-Exec_nonpd'),
                        prefix_lab(dup(),'u'), dup([%lab_from(ident u_list)%]))
    endif;
    if u_list /== [] then
        asm_outword(false_lab, asm_symlabel('undef_key'), 2);
        out_typed_labs(u_list)
    endif
enddefine;

define lconstant cons_wordname(name);
    lvars name;
    unless isstartstring('$-', name) then '$-' <> name -> name endunless;
    if isstring(name) then consword(name) else name endif
enddefine;

define lconstant ident_word_sect(pathname) -> word -> sect;
    lvars pathname, word, sect;
    sect_prop(split_pathname(cons_wordname(pathname)) -> word) -> sect
enddefine;

define lconstant ident_info(pathname);
    lvars pathname, word, sect;
    ident_word_sect(pathname) -> word -> sect;
    f_subv(SECT_WORD_INFO, sect)(word)
enddefine;


    /*  Generate assembler output. The dictionary and all pop stuff
        (identifiers, exported pop words and their strings, and sections)
        go on 'poplink_3'. Most non-pop variables go on 'poplink_2', except
        for the special var block which goes on 'poplink_4'.
        <false>, <true>, [] and associated labels go on 'poplink_1'.
    */
define lconstant gen_asm_output(output_dir);
    lvars   a_files, pair, lab;
    dlvars  output_dir;
    dlocal  fileout_1, fileout_2, fileout_3, fileout_4, word_string_lab,
            dictvec, attached_wids = [], in_vec_idents = [],
            prolog_closures = []
        ;

        ;;; generate a value in _special_var_block
    define lconstant gen_special(pathname);
        lvars   pathname, info = ident_info(pathname),
                type = f_subv(INFO_TYPE,info);
        if f_subv(INFO_IDPROPS,info) &&/=_0 M_ID_CONSTANT then
            ;;; just generate a copy of the constant value
            asm_outword_1(asm_symlabel(pathname));
            type || IDT_VALUE_USED      ;;; mark constant value used
        elseif type &&/=_0 IDT_NON_POP then
            gen_nonpop_ident(pathname, info, fileout_2);
            type &&~~ IDT_GEN_IDENT     ;;; prevent re-generation by gen_pass1
        elseif type &&=_0 IDT_DECLARED then
            ;;; not declared at all
            asm_outword_1(0);
            type
        else
            mishap(pathname, 1, 'POP VARIABLES NOT ALLOWED IN _special_var_block')
        endif -> f_subv(INFO_TYPE,info)
    enddefine;

    define lconstant do_attached_wids();
        lvars n = 0, v, widlab, word, sect, pair_lab, iv_needed;
        lconstant iv_name = 'Sys$-Sect$-word_ident_vec';
        f_subv(INFO_TYPE, ident_info(iv_name)) &&/=_0 IDT_DECLARED
                                                    -> iv_needed;
        fast_for v in attached_wids do
            explode(v) -> (pair_lab, word, sect, widlab);
            gen_pair(pair_lab, asm_outlab, word, f_subv(SECT_LABEL, sect));
            if iv_needed then widlab, n fi_+ 1 -> n endif
        endfor;
        if iv_needed then
            ;;; vector of word identifiers used in sect_widprop.p
            gen_vec_header(n);
            asm_outglab(asm_symlabel(iv_name));
            asm_outword(n)
        endif
    enddefine;

    define lconstant gen_in_vec_idents();
        lvars info, lab;
        gen_vec_header(listlength(in_vec_idents));
        fast_for info in in_vec_idents do
            asm_outglab(f_subv(INFO_IDENT_LAB, info));
            ;;; include labels for any global assignments to this ident
            fast_for lab in ident_assign_prop(info) do asm_outglab(lab) endfor;
            asm_outword_1(getlabstr(f_subv(INFO_INIT, info)));
            ;;; so it gets ignored by gen_const_id
            false -> f_subv(INFO_IDENT_LAB, info)
        endfor
    enddefine;

        ;;; generate dictionary (wrapped in a rawstruct)
    define lconstant gen_dictionary();
        lvars n = 1, len = datalength(dictvec);
        ;;; RAW_SIZE, KEY
        asm_outword((len+2)*WORD_OFFS, asm_symlabel('Sys$-rawstruct_key'), 2);
        asm_outglab(asm_symlabel('Sys$-dictionary'));
        repeat
            if len fi_> 3 then
                f_subv(n, dictvec), n fi_+ 1 -> n;
                f_subv(n, dictvec), n fi_+ 1 -> n;
                f_subv(n, dictvec), n fi_+ 1 -> n;
                f_subv(n, dictvec), n fi_+ 1 -> n;
                asm_outword(4);
                len fi_- 4 -> len
            else
                repeat len times
                    f_subv(n, dictvec), n fi_+ 1 -> n;
                endrepeat;
                asm_outword(len);
                quitloop
            endif
        endrepeat
    enddefine;      /* gen_dictionary */

    define lconstant gen_file_align(seg);
        lvars seg;
        seg -> asmf_charout_seg;
        out_objmod_pad(0);
        asm_align_file()
    enddefine;

    define lconstant gen_arg_struct(item, lablist);
        lvars item, lablist;

        if isstring(item) then
            gen_string(lablist, out_typed_labs, item)
        elseif ispair(item) then
            gen_pair(lablist, out_typed_labs, fast_destpair(item))
        else
            gen_vec_header(datalength(item));
            out_typed_labs(lablist);
            appdata(item, #_< get_sw_lab <> asm_outword_1 >_#)
        endif
    enddefine;

    ;;; Set up, including the dictionary vector
    ;;; (K$-d is a synonym for -dict_chain_end-)
    {% repeat 1024 times #_< asm_symlabel('K$-d') >_# endrepeat %} -> dictvec;
    newactproperty([], 256, false, "perm",
                            procedure(word, prop);
                                lvars word, prop;
                                nextlab() ->> prop(word)
                            endprocedure
                    ) -> word_string_lab;


    ;;; properties to record dummy values and identifiers
    dlocal
        ;;; entries in this are keyed on idprops values
        dummy_ident_prop = newproperty([], 8, false, "perm"),
        ;;; entries in this are keyed on <true> or multiplicities 0, 1, 2 ...
        dummy_value_prop = newactproperty([], 2, false, "perm",
                                procedure(val, prop);
                                    lvars val, prop;
                                    ;;; elements are list of labels,
                                    ;;; 1 for untyped, 2 for procedure typed
                                    consvector([], [], 2) ->> prop(val)
                                endprocedure);



    ;;; Generate special var block first (in 'poplink_2')
    startfile(output_dir, 'poplink_2') ->> fileout_2 -> asmf_charout;
    DATA -> asmf_charout_seg;

    ;;; generate _special_var_block
#_IF DEF special_var_block_neg
    ;;; table for negative offsets (generate backwards)
    repeat destvector(special_var_block_neg) times gen_special() endrepeat;
#_ENDIF
    asm_outglab(asm_symlabel('\^_special_var_block'));  ;;; the label
    appdata(special_var_block, gen_special);            ;;; positive offsets


    ;;; N.B. 'poplink_1' must NOT contain any writeable data
    startfile(output_dir, 'poplink_1') ->> fileout_1 -> asmf_charout;
    CODE -> asmf_charout_seg;
        asm_align_word();

    ;;; output the first 2 words of <false> on 'poplink_1'
    ;;; rest output at end after pdprops labels equivalent to false)
    ;;; BOOL_POP_VAL, KEY
    asm_outword(popint(0), asm_symlabel('boolean_key'), 2);
    asm_outglab(asm_symlabel('false'));

    startfile(output_dir, 'poplink_3') ->> fileout_3 -> asmf_charout;
    DATA -> asmf_charout_seg;
    ;;; label that marks start of system pop data segment
    asm_outglab(asm_symlabel('Sys$-\^_data_seg_start'));

    ;;; Generate words and variable identifiers.
    ;;; Do sections first, then top level
    false -> sect_prop(nullword);       ;;; so we don't do top level
    fast_appproperty(sect_prop, sect_pass1);
    sect_pass1(nullword, top_section);
    top_section -> sect_prop(nullword);

    ;;; generate closures for uninitialised prolog predicates
    applist(prolog_closures, gen_prolog_closure);

    ;;; output pop identifiers needing only value cells, inside a vector
    gen_in_vec_idents();

    ;;; output dummy identifiers
    fast_appproperty(dummy_ident_prop,
                        procedure(idprops, info);
                            lvars info, idprops;
                            gen_ident(info,
                                dest(ident_assign_prop(info))
                                                -> ident_assign_prop(info) )
                        endprocedure);

    ;;; produce the dictionary (inside a rawstruct)
    gen_dictionary();

    ;;; produce run-time sections
    gen_sect(top_section) -> ;

    ;;; start of main non-writeable structures on poplink_3
    CODE -> asmf_charout_seg;

    ;;; produce word strings
    fast_appproperty(word_string_lab,
                        procedure(word, lab);
                                lvars word, lab;
                                gen_string(lab, asm_outlab, word,
                                            if isword(dup()) then
                                                fast_word_string()
                                            endif)
                        endprocedure);

    ;;; constant identifiers left over by gen_pass1
    fast_appproperty(sect_prop,
            procedure(pathname, sect);
                lvars pathname, sect;
                fast_appproperty(f_subv(SECT_WORD_INFO, sect), gen_const_id)
            endprocedure);

    ;;; stuff for 'attached' word identifiers
    do_attached_wids();

    ;;; generate dummy constant values (undef structures)
    gen_dummies();

    ;;; generate structures from args
    fast_appproperty(struct_gen_prop, gen_arg_struct);

    ;;; end of structures on 'poplink_3' -- align if necessary
    gen_file_align(CODE);
#_IF DEF AIX
    ;;; save/restore needs this to know where the non-poplink object files
    ;;; start
    DATA -> asmf_charout_seg;
    asm_outglab(asm_symlabel('Sys$-\^_data_seg_obj_start'));
#_ENDIF
    gen_file_align(DATA);


    ;;; output other (constant) label assignments
    fast_for pair in lab_assign_list do asm_outglabset(destpair(pair)) endfor;

    ;;; finish off 'poplink_1'
    fileout_1 -> asmf_charout;
    ;;; any other labels assigned to <false>
    applist(general_assign_prop(false), asm_outglab);
    ;;; last word of <false>
    asm_outword_1(0);       ;;; BOOL_VAL

    ;;; output <true> on 'poplink_1' ...
    ;;; BOOL_POP_VAL, KEY
    asm_outword(popint(1), asm_symlabel('boolean_key'), 2);
    asm_outglab(asm_symlabel('true'));
    ;;; ... and any other labels assigned to it
    applist(general_assign_prop(true), asm_outglab);
    ;;; ... then its last word
    asm_outword_1(1);       ;;; BOOL_VAL

    ;;; finally, output [] (3 words)
    asm_outword(nil_lab, asm_symlabel('nil_key'), 2);
    asm_outglab(nil_lab), applist(general_assign_prop([]), asm_outglab);
    asm_outword_1(nil_lab);

    ;;; output mem references to constant labels in wlib modules that ensure
    ;;; the O/S linker is forced to include those modules
    fileout_2 -> asmf_charout;
    asm_align_file();
    CODE -> asmf_charout_seg;
    applist(module_forcing_labels, asm_outword_1);

    ;;; finish with files 1 - 3
    endfile(fileout_3, false);
    endfile(fileout_1, true);
    endfile(fileout_2, true);

    ;;; label that marks end of system pop data segment. This goes on
    ;;; 'poplink_4' which comes after the pop object modules in the link
    startfile(output_dir, 'poplink_4') ->> fileout_4 -> asmf_charout;
    DATA -> asmf_charout_seg;
    out_objmod_pad(-1);
    asm_outglab(asm_symlabel('Sys$-\^_data_seg_end'));
        ;;; Make sure the label does not jump to the next file
        asm_outword(0, 1);
    endfile(fileout_4, true);


    ;;; return list of filenames created
    [%
        pdprops(fileout_1),
        pdprops(fileout_2),
        pdprops(fileout_3),
        pdprops(fileout_4)
    %] ->> a_files,
    ;;; ... plus list of those which need to be linked after the object files
    ;;; (currently just 'poplink_4'). This must be a sublist of a_files.
    allbutfirst(3, a_files);
enddefine;


;;; --- LINKING ----------------------------------------------------------


    /*  Link word files and generate assembler code for words etc
    */
define wfile_link(output_dir, entry_p, rt_sects, ids_used, ids_ubn, id_defs,
                    id_export_idprops, id_export_quoted, files, link_other,
                    trace_loading);

    lvars   file, files, rt_sects, name, sect, top_sect, info,
            magic_num, select_list, dbase, trace_loading, output_dir,
            ids_used, ids_ubn, id_defs, entry_p, procedure select_p,
            uninit_last, link_other,
            uninit_chain    = false,
            last_file_used  = false,
        ;

    dlvars
            curr_exload_objfiles    = [],
            module_wlibs_used       = [^false '$usepop/pop/src/'],
            xlink_type              = false,
            xlink_version, xlink_libs,
            xlink_exlibs, xlink_exlibdirs, xlink_exlibfiles,
        ;

    dlocal  sect_prop, weak_depend_prop, general_assign_prop,
            ident_assign_prop, wordid_assign_prop,
            top_section, topsect_word_info,
            id_export_idprops, id_export_quoted,
            lab_assign_list = [],
            module_forcing_labels = [],
            struct_gen_prop = newproperty([], 32, [], "perm"),
            abort = false,
        ;


    define :inline lconstant NEXT_INFO(chain_v);
        f_subv(INFO_NEXT_INFO, chain_v)
    enddefine;

    define :inline lconstant UNINIT(chain_v, last_v);
        if chain_v then NEXT_INFO(last_v) else chain_v endif -> last_v
    enddefine;

    define lconstant maplist_fws = maplist(%fast_word_string%) enddefine;

    define lconstant use_perm_ident(pathname, idprops, init, tflags);
        lvars   pathname, idprops, init, tflags,
                (sect, word) = ident_word_sect(pathname);
        tflags fi_|| IDT_STRONG_REF -> tflags;
        if dup(add_ident_def(word, sect, tflags, idprops, init, ident abort))
        then
            ;;; add to uninitialised chain
            -> UNINIT(uninit_chain, uninit_last)
        else
            -> (,)
        endif
    enddefine;

    define lconstant def_perm_ident(pathname, const, idprops, init, tflags);
        lvars pathname, const, idprops, init, tflags;
        idprops_flags(consident(idprops, const, "perm"), false) -> idprops;
        use_perm_ident(pathname, idprops, init,
                        tflags fi_|| #_< M_PERM_GLOBAL || IDT_DECLARED >_#)
    enddefine;

    define lconstant def_init_perm_ident(pathname, const, idprops, init);
        lvars pathname, const, idprops, init;

        define lconstant make_labelled(item, lab) -> lab;
            lvars item, lab, n, ulab;

            define lconstant label_struct(item, lab);
                lvars item, lab, currlabs = struct_gen_prop(item);
                if lab then
                    consref(lab) -> lab     ;;; global
                elseif currlabs == [] then
                    nextlab() -> lab
                endif;
                if lab then
                    lab :: currlabs ->> currlabs -> struct_gen_prop(item)
                endif;
                if isref(hd(currlabs) ->> lab) then cont(lab) else lab endif
            enddefine;

            if isinteger(item) then
                if lab then
                    conspair(lab, popint(item)) :: lab_assign_list
                                                    -> lab_assign_list
                else
                    popint(item) -> lab
                endif
            elseif unique_struct_lab(item) ->> ulab then
                if lab then
                    lab :: general_assign_prop(item) -> general_assign_prop(item)
                else
                    ulab -> lab
                endif
            elseif isword(item) then
                mark_topsect_word(item);
                if lab then
                    lab :: general_assign_prop(item) -> general_assign_prop(item)
                else
                    item -> lab
                endif
            elseif isstring(item) then
                label_struct(item, lab) -> lab
            elseif ispair(item) then
                label_struct(item, lab) -> lab;
                make_labelled(fast_front(item), false) -> fast_front(item);
                make_labelled(fast_back(item), false) -> fast_back(item)
            elseif isvector(item) then
                label_struct(item, lab) -> lab;
                fast_for n to datalength(item) do
                    make_labelled(f_subv(n,item), false) -> f_subv(n,item)
                endfor
            else
                mishap(item, 1, 'INVALID VALUE FOR -defc ETC')
            endif
        enddefine;

        make_labelled(init, const and asm_symlabel(pathname)) -> init;
        if isword(init) then
            ;;; variable initialised to word
            lvars info = topsect_word_info(init);
            f_subv(INFO_TYPE, info) fi_|| IDT_GLOB_WORD
                                            -> f_subv(INFO_TYPE, info);
            asm_wordlabel(init) -> init
        endif;
        def_perm_ident(pathname, const, idprops, init, 0)
    enddefine;      /* def_init_perm_ident */

    define lconstant sect_check();
        lvars info = topsect_word_info("pop_section");
        if f_subv(INFO_TYPE, info) &&/=_0 IDT_STRONG_REF then
            ;;; make gen_asm_output produce pop_section even if not run-time
            set_sect_gen_flag(top_section, SECF_GENERATE)
        endif;
        if f_subv(SECT_FLAGS,top_section) &&/=_0 SECF_GENERATE then
            use_perm_ident('section_key', 0, nullstring, 0)
        endif
    enddefine;

    define lconstant process_exload_objfiles(objfiles);
        lvars objfiles;
        dlvars allow_set_xtype = true;

        define lconstant do_xlink_type(xtype, xversion, xlibs);
            lvars xlibs, xtype, xversion;
            returnunless(xtype) (xlibs);

            unless allow_set_xtype then
                ;;; force type to be set and produce an error
                XLINK_EXLIBFILES() ->
            endunless;

            if xlink_type then
                ;;; already set
                if xtype /= xlink_type then
                    printf(xlink_type, xtype,
                            'XLINK TYPE %p INCOMPATIBLE WITH XLINK TYPE %p\n')
                elseif xlink_version /== xversion then
                    printf(xlink_version, xversion,
                            'XLINK VERSION %p INCOMPATIBLE WITH XLINK VERSION %p\n')
                elseif xlibs /= xlink_libs then
                    printf(xlink_libs, xlibs,
                            'XLINK LIBS %p INCOMPATIBLE WITH XLINK LIBS %p\n')
                else
                    return(xlink_exlibs)
                endif;
                true -> abort;
                return
            endif;

            ;;; else set it
            (xtype, xversion, xlibs)
                            -> (xlink_type, xlink_version, xlink_libs);

#_IF DEF X_COMPLETE_ENABLED
            if x_complete then
                ;;; use a command to expand the X libs, then assume
                ;;; XLINK_EXLIBDIRS/FILES should be empty
                sysfileok('$popsrc/' dir_>< ('poplink_' <> xtype <> '_xlibs '))
                                                -> x_complete;
                consword('`' <> x_complete sys_>< xversion <> '`') :: []
                                                    -> xlink_exlibs;
                [] ->> xlink_exlibdirs -> xlink_exlibfiles
            else
#_ENDIF
            xlibs -> xlink_exlibs;
            exload_merge_objfiles([], xlibs, false) -> xlibs;
            ;;; split off dirs
            [% while xlibs /== [] and isvector(hd(xlibs)) do
                dest(xlibs) -> xlibs
            endwhile %];
            exload_flatten_objfiles() -> xlink_exlibdirs;
            exload_flatten_objfiles(xlibs) -> xlink_exlibfiles;
#_IF DEF X_COMPLETE_ENABLED
            endif;
#_ENDIF

            def_init_perm_ident('popxlink_' <> xtype, true, 0, true);
            def_init_perm_ident('XLINK_TYPE', true, 0, xtype);
            if xversion then
                ;;; AIX doesn't allow this to be a constant
                def_init_perm_ident('XLINK_VERSION', not(DEF AIX), 0, xversion)
            endif;
            def_init_perm_ident('XLINK_EXLIBDIRS', false, 0,
                                        maplist_fws(xlink_exlibdirs));
            def_init_perm_ident('XLINK_EXLIBFILES', false, 0,
                                        maplist_fws(xlink_exlibfiles));
            return(xlink_exlibs)
        enddefine;

        if isinteger(objfiles) then
            ;;; called from read_wmodule with N strings on stack
            conslist((), objfiles) -> objfiles;
            false -> allow_set_xtype
        endif;
        exload_merge_objfiles(curr_exload_objfiles, objfiles, do_xlink_type)
                                            -> curr_exload_objfiles;
        unless allow_set_xtype then sys_grbg_list(objfiles) endunless
    enddefine;      /* process_exload_objfiles */

    define lconstant set_xlink_type();
        dlocal curr_exload_objfiles;
        lconstant default_spec = "'==POP_XLINK_EXLIBS'" ;
        returnif(xlink_type);
        ;;; xlink type not yet established -- use default
        [] -> curr_exload_objfiles;
        process_exload_objfiles([^default_spec]);
        if xlink_type then
            link_other nc_<> [^default_spec] -> link_other
        else
            mishap(0, 'NO DEFAULT XLINK TYPE SUPPLIED BY ENV VAR POP_XLINK_EXLIBS')
        endif
    enddefine;

    ;;; these will be called from sys_translate_exlibs to get a value
    ;;; for dummy env vars POP_XLINK_EXLIBDIRS & POP_XLINK_EXLIBFILES
    define dlocal XLINK_EXLIBDIRS();
        set_xlink_type(), xlink_exlibdirs
    enddefine;
    define dlocal XLINK_EXLIBFILES();
        set_xlink_type(), xlink_exlibfiles
    enddefine;

    define lconstant process_module_wlibs(N);   ;;; N words
        lvars N, wlib;
        fast_repeat N times
            () -> wlib;
            unless fast_lmember(wlib, module_wlibs_used) then
                module_wlibs_used nc_<> (wlib :: []) -> module_wlibs_used
            endunless
        endrepeat
    enddefine;

    define lconstant read_wmod(file, libmodhead, path, word);
        lvars file, libmodhead, path, word, filename, uncondlib;

        ;;; allows library initialisations for variables to be overidden
        ;;; by ones in w-files
        dlocal doing_wlib_extract = libmodhead;
        ;;; says we made need to generate dummy refs to force extraction
        dlocal wlibmod_needs_forcing = libmodhead;

        w_open_name(file) -> filename;
        unless file == last_file_used then
            file -> last_file_used;
            ;;; leave on stack to be collected up
            filename, if libmodhead then consref() endif
        endunless;
        if trace_loading then
            if libmodhead then
                printf('%p:\s%p%s\n',
                    [%filename, f_subv(WMH_NAME, libmodhead),
                                    if path then
                                        '\s(%p)', id_display_str(path, word)
                                    else
                                        nullstring
                                    endif%])
            else
                printf('%p\n', [^filename])
            endif
        endif;
        if dup(read_wmodule(file, libmodhead, false, false, dbase,
                                                    ident abort)) then
            ;;; new non-empty uninitialised chain
            -> UNINIT(uninit_chain, uninit_last)
        else
            -> ->
        endif;
        if isstring(wlibmod_needs_forcing) then
            ;;; add to a list of labels for which to generate mem references
            ;;; to force the O/S linker to include this module
            wlibmod_needs_forcing :: module_forcing_labels
                                        -> module_forcing_labels
        endif;
        sect_check()
    enddefine;      /* read_wmod */

    define lconstant isinitialised(info);
        lvars info;
        if ispair(f_subv(INFO_INIT,info) ->> info) then
            ;;; incremental
            fast_front(info)
        else
            info
        endif
    enddefine;

    define lconstant wlib_of_file = newassoc([]) enddefine;

    ;;; process the current list of uninitialised identifiers, looking
    ;;; for library modules that initialise them
    define lconstant wlib_extract(wlib);
        lvars   new_chain = false, new_last, weak_chain, weak_last, mod_loaded,
                modnum, path, word, wlib, file;

        returnunless(wlib);     ;;; dummy
        wlib -> file;
        unless isvector(wlib) or (wlib_of_file(file) ->> wlib) then
            open_w_input(file, true, "wlib") ->> wlib -> wlib_of_file(file)
        endunless;

        repeat
            false ->> mod_loaded -> weak_chain;

            while uninit_chain do
                if isinitialised(uninit_chain) then
                    ;;; now initialised -- ignore
                    NEXT_INFO(uninit_chain) -> uninit_chain;
                    nextloop
                endif;
                ;;; still uninitialised
                if isweak(uninit_chain, true) then
                    ;;; weak reference -- build a separate chain of these
                    uninit_chain ->> UNINIT(weak_chain, weak_last)
                else
                    ;;; strong reference -- see if initialised in library
                    ;;; module
                    f_subv(INFO_PATHNAME, uninit_chain) -> path;
                    f_subv(INFO_WORD, uninit_chain) -> word;
                    if search_wlib_index(path, word, wlib, false) ->> modnum
                    then
                        ;;; yes -- include library module
                        read_wmod(wlib, get_module_head(wlib, modnum),
                                                            path, word);
                        true -> mod_loaded
                    else
                        ;;; not initialised in library -- put back on new chain
                        uninit_chain ->> UNINIT(new_chain, new_last)
                    endif
                endif;

                ;;; next in chain
                NEXT_INFO(uninit_chain) -> uninit_chain
            endwhile;

            ;;; if any library modules where loaded, go back and redo the
            ;;; weak chain in case any have become strong
            quitunless(weak_chain and mod_loaded);
            false -> NEXT_INFO(weak_last);
            weak_chain -> uninit_chain;
            weak_last -> uninit_last
        endrepeat;

        ;;; add any remaining weak ones onto end of main chain
        if weak_chain then
            weak_last, weak_chain -> UNINIT(new_chain, new_last)
        endif;
        ;;; finish main chain
        if new_chain then false -> NEXT_INFO(new_last) endif;
        ;;; switch new chain to be current
        new_chain -> uninit_chain, new_last -> uninit_last
    enddefine;      /* wlib_extract */

    define lconstant select_module(modhead, wlib);
        lvars l, modhead, wlib;
        if lmember(f_subv(WMH_NAME, modhead), select_list) ->> l then
            false -> hd(l)      ;;; mark included/excluded
        endif;
        returnunless(select_p(l));
        read_wmod(wlib, modhead, false, false)
    enddefine;

    define lconstant wlib_group_extract(files);
        lvars   file, files, wlib, doing_libs_used = false;
        lconstant LIBSUSED = '-wlbused';

        define lconstant make_pass(uninit_chain) -> uninit_chain;
            lvars   info, last;
            dlocal  uninit_chain;
            uninit_chain -> info;
            while info do
                unless isinitialised(info) or isweak(info, true) then
                    f_subv(INFO_TYPE,info) fi_|| IDT_GROUP_MARK
                                                -> f_subv(INFO_TYPE,info);
                endunless;
                NEXT_INFO(info) -> info
            endwhile;
            applist(files, wlib_extract);
            uninit_chain -> info, false -> last;
            while info do
                quitif(f_subv(INFO_TYPE,info) &&=_0 IDT_GROUP_MARK);
                info -> last, NEXT_INFO(info) -> info
            endwhile;
            if info and not(isweak(info, true)) then
                make_pass(info) -> if last then NEXT_INFO(last)
                                   else uninit_chain
                                   endif
            endif
        enddefine;      /* make_pass */


        returnif(files == []);
        if member(LIBSUSED, files) then
            fast_ncdelete(LIBSUSED, files) nc_<> module_wlibs_used -> files;
            true -> doing_libs_used
        endif;

        if tl(files) == [] then
            wlib_extract(hd(files))     ;;; false if doing_libs_used
        else
            make_pass(uninit_chain) -> uninit_chain
        endif;

        for file in files do
            if wlib_of_file(file) ->> wlib then
                false -> wlib_of_file(file);
                sysclose(wlib_device(wlib))
            endif
        endfor;
        if doing_libs_used then [] -> tl(module_wlibs_used) endif
    enddefine;      /* wlib_group_extract */


    ;;; --- Set up identifier database
    cons_dbase() -> dbase;
    dbase(DBASE_SECT_PROP) -> sect_prop;
    dbase(DBASE_WEAK_DEPEND_PROP)       -> weak_depend_prop;
    dbase(DBASE_GENERAL_ASSIGN_PROP)    -> general_assign_prop;
    dbase(DBASE_IDENT_ASSIGN_PROP)      -> ident_assign_prop;
    dbase(DBASE_WORDID_ASSIGN_PROP)     -> wordid_assign_prop;
    sect_prop(nullword) -> top_section;
    f_subv(SECT_WORD_INFO, top_section) -> topsect_word_info;

    process_exload_objfiles -> dbase(DBASE_EXLOAD_STR_ACTION);
    process_module_wlibs    -> dbase(DBASE_WLIB_STR_ACTION);

    ;;; --- Set up identifiers prior to reading files
    1 -> nextlabel;

    if entry_p then
        ;;; Set up Sys$- _entry_point as an uninitialised constant
        ;;; (this will bring in main from amain.s)
        def_perm_ident('Sys$-\^_entry_point', true, 0, nullstring, IDT_USED);

        ;;; Set up entry procedure as an uninitialised constant procedure ...
        def_perm_ident(entry_p, true, "procedure", nullstring, IDT_VALUE_USED);
        ;;; ... then initialise variable procedure Pop$-Entry_Procedure to
        ;;; its value
        def_perm_ident('Pop$-Entry_Procedure', false, "procedure",
                                    asm_symlabel(entry_p), IDT_GEN_IDENT);
    else
        ;;; external startup -- ensure this is extracted from aextern.s
        def_perm_ident('Sys$-\^_external_callback_func', true, 0, nullstring,
                                                            IDT_USED)
    endif;

    ;;; Identifiers used or used by name
    applist(ids_used, use_perm_ident(%0, nullstring, IDT_USED%));
    applist(ids_ubn, use_perm_ident(%0, nullstring,
                                        IDT_USED || IDT_GEN_IN_DICT%));

    ;;; Identifiers defined via args
    applist(id_defs,procedure(defvec);
                        lvars defvec;
                        explode(defvec);
                        if datalength(defvec) == 3 then
                            ;;; uninitialised
                            def_perm_ident((), nullstring, 0)
                        else
                            ;;; initialised
                            def_init_perm_ident()
                        endif
                    endprocedure);

    ;;; mark run-time sections
    for name in rt_sects do
        set_sect_gen_flag(sect_prop(cons_wordname(name)), SECF_RUN_TIME)
    endfor;

    ;;; set xlink type first if it appears in link_other
    maplist(link_other, consword) -> link_other;
    process_exload_objfiles(link_other);
    sys_grbg_list(curr_exload_objfiles);
    [] -> curr_exload_objfiles;


    ;;; --- Read and store everything in the input files
    sect_check();


    ;;; -read_wmod- leaves used files on stack to be collected up
    [%  fast_for file in files do
            if isvector(file) then
                ;;; library with include/exclude module list
                explode(file) -> select_list -> select_p -> file;
                get_module_assoc(select_list) -> select_list;
                app_wlib_input(file, select_module);
                unless select_p == not then
                    check_module_assoc(select_list, ident abort)
                endunless
            elseif islist(file) then
                ;;; group of w-libraries for search extract
                wlib_group_extract(file)
            elseif isvector(open_w_input(file, true, true) ->> file) then
                ;;; single w-library for search extract
                wlib_extract(file);
                sysclose(wlib_device(file))
            else
                ;;; read single w-file, and accumulate uninitialised chain
                read_wmod(file, false, false, false)
            endif
        endfast_for
    %] -> files;

    lconstant abort_ms = 'ABORTED (see above)';
    if abort then mishap(0, abort_ms) endif;

    ;;; accumulate other objfile args to link into curr_exload_objfiles
    process_exload_objfiles(link_other);

    ;;; return list of object/library files used
    files;
    ;;; return other linker objfiles
    maplist_fws(exload_flatten_objfiles(curr_exload_objfiles));

    ;;; generate assembler output files (returns list of files)
    gen_asm_output(output_dir);

    if abort then mishap(0, abort_ms) endif;
enddefine;      /* wfile_link */

define lconstant gen_date_file(output_dir, date_string, image_ident);
    lvars fileout, output_dir, date_string, image_ident;

    startfile(output_dir, 'poplink_dat') ->> fileout -> asmf_charout;

    ;;; date string
    CODE -> asmf_charout_seg;
    gen_string(asm_symlabel('Sys$-image_date'), asm_outglab,
                                                '\s' <> date_string <> ')');
    ;;; image identification (pointer to the value because AIX doesn't
    ;;; support setting the label equal to the value
    asm_outglab(asm_symlabel('Sys$-\^_image_ident'));
    asm_outword(image_ident, 1);
    asm_align_file();

    endfile(fileout, false);
    pdprops(fileout)        ;;; return filename
enddefine;      /* gen_date_file */

define lconstant setup_labels();
    lvars lab, item, pair, name, idprops;
    [%  fast_for item, pair in_property poplink_unique_struct do
            item, asm_symlabel(fast_front(pair))
        endfor
    %] -> unique_struct_labels;

    unique_struct_lab(false)    -> false_lab;
    unique_struct_lab(true)     -> true_lab;
    unique_struct_lab([])       -> nil_lab;

    asm_symlabel('K$-s')        -> string_key_lab;      ;;; short name
    asm_symlabel('K$-w')        -> word_key_lab;        ;;; short name
    asm_symlabel('K$-i')        -> ident_key_lab;       ;;; short name
    asm_symlabel('pair_key')    -> pair_key_lab;
    asm_symlabel('vector_key')  -> vector_key_lab;
enddefine;


;;; --- MAIN PROCEDURE ---------------------------------------------------

lconstant unum_letters = [
    `b`     'biginteger_key'
    `r`     'ratio_key'
    `f`     'decimal_key'
    `c`     'complex_key'
];

define $-Pop$-Main();
    lvars   c, n, arg, wlast = false, wfiles, ofiles, keep_dir, link_cmnd,
            link_a_files, link_after, date_a_file, date_o_file,
            rt_sects        = [],
            image_name      = false,
            image_ident     = false,
            date_string     = false,
            entry_p         = false,
            a_flag          = false,
            p_flag          = false,
            ponly_flag      = false,
            q_flag          = false,
            t_flag          = false,
            sunfp_flag      = false,
            port_flag       = false,
            noshare_flag    = false,
            emb_flag        = false,
            exmain          = false,
            exlink          = false,
            link_flags      = [],
            link_other      = [],
            id_defs         = [],
            ids_used        = [],
            ids_ubn         = [],
            idexp           = 'w',      ;;; export ids with quoted words
            unum            = 'brf',    ;;; bigints, ratios, floats
        ;

    dlocal  pop_arglist = process_arglist(poparglist),
            x_complete = false,
            ;

#_IF pop_debugging
    dlocal  pop_mishap_doing_lim = false;
#_ELSE
    dlocal  pop_mishap_doing_lim = 0, popgctrace = false, popsyscall = false;
    pop_null_device -> pop_charin_device;
#_ENDIF

    dlocal  pop_file_versions = use_file_versions(),
            % file_create_control(dlocal_context) %
        ;

    300000 -> popmemlim;

    define dlocal prmishap(mess, list);
        lvars mess, list;
        sysprmishap('POPLINK: ' <> mess, list)
    enddefine;

    define dlocal interrupt();
        false -> pop_exit_ok;
        chainfrom($-Pop$-Main, sysexit)
    enddefine;

    define lconstant get_option_once(option, value);
        lvars option, value;
        if value then
            mishap(option, 1, 'OPTION SPECIFIED TWICE')
        else
            get_option_arg(option, true)
        endif
    enddefine;

    define lconstant get_id_define(arg, option) -> defvec;
        lvars arg, option, defvec, n;
        if locchar(`=`, 1, arg) ->> n then
            ;;; initialised -- value in defvec(4)
            initv(4) -> defvec;
            compile(stringin(allbutfirst(n, arg))) -> defvec(4);
            substring(1, n-1, arg) -> arg
        else
            ;;; uninitialised
            initv(3) -> defvec
        endif;
        ;;; name in defvec(1)
        arg -> defvec(1);
        ;;; const/var in defvec(2)
        option == "defcm" or option == "defc" -> defvec(2);
        ;;; identprops in defvec(3)
        if option == "defcm" or option == "defvm" then
            "macro"
        else
            0
        endif -> defvec(3)
    enddefine;


    /*  N.B. So that POPC can process them, all non-file options to POPLINK
        must have an entry in the property -poplink_nf_options- in popc_main.p
        (file options to POPLINK are explicitly recognised by POPC)
    */

    [%  until null(pop_arglist) do
            dest(pop_arglist) -> (arg, pop_arglist);
            if is_option(arg) then
                consword(arg) -> arg;
                allbutfirst(1, arg) -> c;

                if     c == "a" then
                    ;;; produce assembler output only
                    true -> a_flag

                elseif c == "date" then
                    ;;; date
                    get_option_once(arg, date_string) -> date_string

                elseif fast_lmember(c, [defc defv defcm defvm]) then
                    ;;; define perm identifier(s) (constant/vars untyped/macro)
                    ;;; in the form pathname=expr
                    id_defs nc_<>
                        maplist(get_option_arg(arg, false), get_id_define(%c%))
                                -> id_defs

                elseif c == "e" then
                    ;;; entry procedure name
                    get_option_once(arg, entry_p) -> entry_p

                elseif c == "ex" or c == "in" then
                    unless wlast then
                        mishap(0, arg >< ' MUST FOLLOW W-LIBRARY')
                    endunless;
                    consvector((), if c=="ex" then not else identfn endif,
                                            get_option_arg(arg, false), 3)

                elseif c = "emb" then
                    ;;; external make base (.stb file)
                    true -> emb_flag

                elseif c = "exlink" then
                    ;;; external startup and link
                    true ->> exlink ->> exmain ->> p_flag -> ponly_flag

                elseif c = "exmain" then
                    ;;; external startup
                    true -> exmain

                elseif c == "idexp" then
                    get_option_arg(arg, true) -> idexp

                elseif c == "lf" then
                    link_flags nc_<> get_option_arg(arg, false) -> link_flags

                elseif c == "lo" then
                    link_other nc_<> get_option_arg(arg, false) -> link_other

                elseif c == "ident" then
                    ;;; image identification
                    get_option_once(arg, image_ident) -> image_ident

                elseif c == "noshare" then
                    ;;; link without shareable libraries
                    true -> noshare_flag

                elseif c == "o" then
                    ;;; image name
                    get_option_once(arg, image_name) -> image_name

                elseif c == "p" then
                    ;;; leave poplink files for later use of -q
                    true -> p_flag

                elseif c == "ponly" then
                    ;;; leave poplink files, don't link
                    true ->> p_flag -> ponly_flag

                elseif c == "port" then
                    ;;; produce assembler files for porting
                    true ->> a_flag -> port_flag

                elseif c == "q" then
                    ;;; re-do object link after -p
                    true -> q_flag

                elseif c == "s" then
                    ;;; run-time section(s)
                    rt_sects nc_<> get_option_arg(arg, false) -> rt_sects

                elseif c == "sunfp" then
                    ;;; Sun-2/3 floating-point flag
                    get_option_once(arg, sunfp_flag) -> sunfp_flag

                elseif c == "tr" then
                    ;;; trace loading
                    true -> t_flag

                elseif c == "uses" then
                    ;;; mark ids used
                    ids_used nc_<> get_option_arg(arg, false) -> ids_used

                elseif c == "ubn" then
                    ;;; mark ids used by name
                    ids_ubn nc_<> get_option_arg(arg, false) -> ids_ubn

                elseif c == "unum" then
                    get_option_arg(arg, true) -> unum

                elseif c == "wlb" then
                    ;;; w-library group
                    get_option_arg(arg, false)

                elseif c == "wlbused" then
                    ;;; w-libraries used by object modules as a group (may
                    ;;; also occur inside a "wlb" group)
                    copylist(['-wlbused'])

                elseif c == "x_complete" then
                    ;;; see above
                    true -> x_complete

                elseif isstartstring('x', c) then
                    ;;; same as -lo ==POP_C_EXLIBS
                    [% '-lo', '==POP_' <> lowertoupper(fast_word_string(c))
                                        <> '_EXLIBS' %]
                        nc_<> pop_arglist -> pop_arglist

                else
                    mishap(arg, 1, 'UNKNOWN OPTION')
                endif;
                false -> wlast

            elseunless isstring(arg) then
                mishap(arg, 1, 'UNEXPECTED LIST ARGUMENT')

            else
                true -> wlast;
                arg
            endif
        enduntil
    %] -> wfiles;

    ;;; deal with -idexp arg
    lvars idexp_flags = 0, qwordexp = false;
    fast_for c in_string idexp do
        if     c == `s` then
            idexp_flags || M_ID_SYNTAX -> idexp_flags
        elseif c == `m` then
            idexp_flags || M_ID_MACRO  -> idexp_flags
        elseif c == `w` then
            true -> qwordexp
        else
            mishap(consstring(c,1), 1, 'INVALID CHARACTER IN -idexp OPTION')
        endif
    endfor;

    ;;; deal with -unum arg
    fast_for c in_string unum do
        if fast_lmember(c, unum_letters) ->> arg then
            hd(tl(arg)) :: ids_used -> ids_used
        else
            mishap(consstring(c,1), 1, 'INVALID CHARACTER IN -unum OPTION')
        endif
    endfor;

    if q_flag then
        if a_flag or entry_p or wfiles /== [] then
            mishap(0, 'INVALID ARGUMENTS IN COMBINATION WITH -q OPTION')
        endif
    elseif a_flag and p_flag then
        mishap(0, '-a AND -p SPECIFIED TOGETHER')
    endif;

    if exmain then
        if entry_p then
            mishap(0, '-e AND -exmain/-exlink SPECIFIED TOGETHER')
        endif
    elseunless entry_p then
        '$-Pop$-Main' -> entry_p
    endif;

    ;;; default values for other args
    unless date_string then sysdaytime() -> date_string endunless;
    unless image_ident then '256' -> image_ident endunless;
    unless image_name then
        'a.out' -> image_name
    endunless;

    if a_flag or p_flag or q_flag then
        dup(sys_fname_path(image_name)) dir_>< 'poplink_cmnd'
    else
        false, new_tmp_file(false, 'plnk', '')
    endif -> link_cmnd -> keep_dir;

    ;;; produce date file
    setup_labels();
    gen_date_file(keep_dir, date_string, image_ident) -> date_a_file;
    new_fname_extn(date_a_file, OBJ_EXTENSION) -> date_o_file;
    unless a_flag then
        true -> is_tmp_file(date_a_file);
        unless keep_dir then true -> is_tmp_file(date_o_file) endunless;
    endunless;

    if q_flag then
        [] -> link_a_files

    else

        ;;; produce wlink assembler files
        wfile_link(keep_dir, entry_p, rt_sects, ids_used, ids_ubn, id_defs,
                        idexp_flags, qwordexp, wfiles, link_other, t_flag)
                    -> (ofiles, link_other, link_a_files, link_after);

        if testdef sunfp_poplink_flag then
            weakref sunfp_poplink_flag(sunfp_flag, link_other) -> link_other
        ;;; else ignore it (if present)
        endif;

        ;;; transform ofiles to object file names (libraries are in refs)
        fast_for c on ofiles do
            hd(c) -> n;
            if isref(n) then
                ;;; library
                new_fname_extn(cont(n), OLB_EXTENSION) -> cont(n)
            else
                new_fname_extn(n, OBJ_EXTENSION) -> hd(c)
            endif
        endfor;

        ;;; likewise for link asm files
        fast_for c on link_a_files do
            hd(c) -> n;
            if p_flag then true -> is_tmp_file(n) endif;
            conspair(n, new_fname_extn(n, OBJ_EXTENSION) ->> n) -> hd(c);
            unless keep_dir then true -> is_tmp_file(n) endunless;
        endfast_for;

        ;;; add the date file and the link object files to ofiles:
        ;;; files in link_after should go at the end
        link_a_files -> c;
        [%
            until c == link_after do
                fast_back(fast_destpair(c) -> c);
            enduntil;
        %] nc_<>
        ofiles nc_<>
        [%
            until c == [] do
                fast_back(fast_destpair(c) -> c);
            enduntil;
            date_o_file;
        %] -> ofiles;

        ;;; directory containing pop external libraries
        lvars popexternlib =
            if port_flag then
                ;;; assume popexternlib might not be defined
#_IF DEF VMS
                'sys$disk:[-.extern.lib]'
#_ELSE
                '../extern/lib/'
#_ENDIF
            else
                ;;; Altered by A.S. 1 Nov 2003
                ;;;sysfileok('$popexternlib/')
                '$popexternlib/'
            endif;

        ;;; produce command file for object link
        gen_link_command(exlink, link_cmnd, image_name, ofiles, link_flags,
                        link_other, popexternlib, not(noshare_flag),
                        emb_flag, isstring(x_complete) and x_complete)

    endif;
    conspair(date_a_file, date_o_file) :: link_a_files -> link_a_files;

    unless a_flag then
        if ponly_flag then
            ;;; just assemble files without actually linking
            assemble_files(link_a_files)
        else
            assemble_and_link(link_cmnd, link_a_files, image_name, emb_flag)
        endif
    endunless
enddefine;      /* $-Pop$-Main */


endsection;     /* $-Popas */




/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan  4 2005
        Removed last change. The fix for getting unexpanded $popobjlib into
        $popsys/poplink_cmnd turned out to be in
            $popsys/pglink
--- Aaron Sloman, 1 Nov 2003
        added to the main procedure
            pop_translate_envvars = false,
        Replaced sysfileok('$popexternlib/') with '$popexternlib/'
--- John Gibson, Jun  1 1998
        Added _data_seg_obj_start label for AIX
--- John Gibson, Mar 20 1998
        Changed gen_date_file to generate _image_ident as a pointer to the
        value rather than the value itself (which AIX doesn't support)
--- John Gibson, Jul 26 1995
        Made image ident default to 256 (for user programs)
--- John Gibson, Apr  8 1995
        Changed procedure header layout
--- John Gibson, Mar 17 1995
        Changed so _special_var_block goes in poplink_2.a (it must come
        before the pop object modules, otherwise symbols it references
        will not be extracted from libraries)
--- John Gibson, Mar  7 1995
        Added ALPHA_OSF1 fix in gen_clos
--- Robert John Duncan, Nov  2 1994
        Small data section no longer used on MIPS
--- John Gibson, Oct 26 1994
        Made gen_asm_output conditionally generate negative-offset values
        in _special_var_block (i.e. if special_var_block_neg defined), and
        also allowed svb values to be any kind of constant.
--- Robert John Duncan, Oct 11 1994
        Fix to typo in previous fix (!)
--- Robert John Duncan, Sep  5 1994
        Added fix for incremental lists under Windows (some problem with
        the Microsoft linker)
--- Robert John Duncan, Mar 22 1994
        Changed gen_closure so that the responsibility for planting the
        execute labels is with asm_gen_poplink_code.
--- Robert John Duncan, Mar 14 1994
        Now uses the "small" data section for MIPS only if the appropriate
        procedure is defined
--- John Gibson, Aug 21 1993
        Added initialisations for prolog predicates
--- John Gibson, Jul 30 1993
        Fixed problem with section_key not being extracted
--- John Gibson, Jul 10 1993
        Added -emb option (External Make Base) to make image.stb
--- John Gibson, Jul  2 1993
        Added -idexp and -unum options
--- Robert John Duncan, Jun 11 1993
        Adding the pop external libraries to the link command is now done
        by gen_link_command which takes popexternlib as an additional argument
--- Robert John Duncan, Jun  2 1993
        Pop external stuff now in two separate libraries
--- John Gibson, May 10 1993
        Incorporated xlink stuff, added -x* option, etc.
--- John Gibson, May  6 1993
        Made popexternlib get added to link when port flag is true
--- John Gibson, Sep 26 1992
        Added gen_incremental
--- John Gibson, Sep 23 1992
        Changed PD_CLOS_NFROZ to be short instead of byte
--- John Gibson, Aug 26 1992
        Made <false> and <true> have extra BOOL_VAL field containing
        machine 0 or 1
--- John Gibson, Apr  4 1992
        Made unconditional inclusion of wlib modules generate mem references
        to constants in those modules to force their inclusion where necessary
--- John Gibson, Sep 13 1991
        Changed string representation to guarantee null-termination
--- John Gibson, May  6 1991
        Added -def* options. Also, [] now generated by poplink.
--- John Gibson, Jan 21 1991
        Added `-noshare' option
--- John Gibson, Aug 31 1990
        Changes to cope with list args in poparglist from 13.83
--- John Gibson, Aug 27 1990
        Changed lvar -weak- to -Weak-
--- Rob Duncan, Jun 19 1990
        Added 'poplink_4' file to contain the special var block and the
        data seg end label.
--- John Gibson, May 21 1990
        Added -port flag and addition of external library when flag
        not present
--- John Gibson, Mar  8 1990
        Added support for Sun-2/3 floating-point flag to poplink
--- John Gibson, Jan  7 1990
        Version 13.7 for new pointers.
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 17 1989
        Version 13.66
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, May  5 1989
        Instead of constant entry procedure $-Pop$-Main, variable
        procedure $-Pop$-Entry_Procedure is now called in setpop.p;
        this variable is initialised by poplink to a user-specified
        constant procedure given with the "-e" option, or to
        $-Pop$-Main by default.
--- John Gibson, Feb  3 1989
        Changes to generate undef structures (-asm_gen_poplink_code- now
        takes extra arg saying how many frozvals to push).
--- John Gibson, Jan 29 1989
        Changes to allow generation of word identifiers
 */
