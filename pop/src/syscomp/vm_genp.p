/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/syscomp/vm_genp.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                VM CODE GENERATION FOR SPECIAL PROCEDURES

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas;

define lconstant make_gen_p(l_locals, codelist, flags, struct_tab, p);
    lvars l_locals, codelist, flags, struct_tab, p;
    codelist nc_<> ({pas_END} :: []) -> codelist;
    consvector( [],         ;;; d_locals
                l_locals,   ;;; l_locals
                false,      ;;; nlgoto_var
                [],         ;;; rtid_args
                false,      ;;; lblock_instr
                [],         ;;; pdr_nl_labels
                [],         ;;; dloc_rtid_list
                [],         ;;; dlexpr_list
                0,          ;;; pdr_entry_type (0 = no entry check)
                codelist,   ;;; codelist
                flags,      ;;; flags
                struct_tab, ;;; struct_tab
                12)
            -> gen_procedure_args(p);
    gen_procedure_key -> data_key(p)
enddefine;

define lconstant pushq_p_opnd(opnd, fld_name, fixed);
    lvars opnd, fld_name, fixed;
    if fixed then
        {pas_PUSHQ ^opnd}
    else
        {pas_PUSH_PINDEX %field_##(fld_name)%}
    endif
enddefine;

define lconstant pushq(item);
    lvars item;
    {pas_PUSHQ ^item}
enddefine;

define lconstant push(id);
    lvars id;
    {pas_PUSH ^id}
enddefine;

define lconstant pop(id);
    lvars id;
    {pas_POP ^id}
enddefine;

define lconstant call_path(path);
    lvars path;
    {pas_CALL ^(identof_path(path))}
enddefine;

define generate_pcomposite(p);
    lvars   p, fixed = not(is_writeable(p)), (p1, p2) = explode(p),
            u = updater(p), up1, up2;

    if ispcomposite(u)
    and (explode(u) -> (up1, up2), p1 == up1 and updater(p2) == up2)
    and isundef(up2) and not(struct_label_prop(up2))
    then
        false -> updater(p)
    endif;

    make_gen_p( [],
                [%  pushq_p_opnd(p1, "PD_COMPOSITE_P1", fixed),
                    call_path([fast_apply]),
                    pushq_p_opnd(p2, "PD_COMPOSITE_P2", fixed),
                    call_path([fast_apply]),
                %],
                M_PD_COMPOSITE,
                [^p1 ^p2],
                p);
    chain(p, generate_gen_procedure)
enddefine;

define generate_array(array);
    lvars   array, fixed, vec, subscrp, min_sub, max_sub, by_row,
            revbounds, scale_facts, boundslst, array_params;

    ;;; mark this used to force inclusion of run-time array procedures
    perm_const_lab([isarray]) -> ;

    not( is_writeable(array) ->> is_writeable(updater(array)) ) -> fixed;
    arrayvector(array) -> vec;
    array_subscrp(array) -> subscrp;
    arrayvector_bounds(array) -> min_sub -> max_sub;
    isarray_by_row(array) -> by_row;

    lblock
        lvars f = 1, l, pair, bounds, lo, hi;
        ;;; make boundlist a list of pairs (easier to deal with)
        boundslist(array) ->> boundslst -> l;
        [% until l == [] do conspair(dest(dest(l)) -> l) enduntil %] -> l;
        rev(l) -> revbounds;
        ;;; compute scale factors for each dimension
        [] -> scale_facts;
        unless by_row then revbounds -> l endunless;
        fast_for pair in l do
            f :: scale_facts -> scale_facts;
            f * (1 - nonop -(destpair(pair))) -> f
        endfor;
        unless by_row then rev(scale_facts) -> scale_facts endunless;

        ;;; compute parameter table for _array_sub
        [%  min_sub,
            fast_for pair in revbounds do
                destpair(pair) -> (lo, hi);
                syspop\:_int( popint(hi+1) - popint(lo) );
                lo;             ;;; popint lower bound
                dest(scale_facts) -> (f, scale_facts);
                ;;; plant 0 for scale factor of 1 (easy for _array_sub to test for)
                syspop\:_int(if f == 1 then 0 else f endif)
            endfor;
            syspop\:_int(0)         ;;; marks end of parameters
        %] -> array_params;
    endlblock;

    lblock
        lvars p = array, sub_p = subscrp, flags = M_PD_ARRAY;
        ;;; repeat once for base, once for updater
        repeat 2 times
            make_gen_p( [],
                        [% call_path([\^_array_sub]) %],
                        flags,
                        [^vec ^sub_p ^boundslst ^min_sub ^max_sub ^by_row]
                            <> array_params,
                        p);

            updater(array) -> p;
            updater(subscrp) -> sub_p;
            0 -> flags      ;;; updater not marked as array
        endrepeat
    endlblock;

    chain(array, generate_gen_procedure)
enddefine;


;;; --- ACCESS PROCEDURES ----------------------------------------------

    ;;; allows cons_access procedures to be re-generated across multiple files
define lconstant access_p_params =
    newproperty([], 16, false, false)
enddefine;

lvars keep_access_p = false;

define lconstant make_access_p1(p, fid, spec, checking, exptr, upd);
    lvars   spec, checking, exptr, p, fid, clist, upd, rec_id = false,
            subs_id = false, locals = [];
    if upd then
        (new_lex_id(false) ->> rec_id) :: locals -> locals;
        if ispair(spec) and not(islist(spec)) then
            (new_lex_id(true) ->> subs_id) :: locals -> locals
        endif
    endif;
    [ {pas_FIELD ^fid ^spec ^checking ^exptr ^upd ^rec_id ^subs_id } ]
                                                            -> clist;
    make_gen_p(locals, clist, 0, [], p);

    if keep_access_p then
        {fid ^spec ^checking ^exptr ^upd} -> access_p_params(p)
    endif
enddefine;

define lconstant make_access_p(p, fid, spec, checking, exptr);
    lvars p, fid, spec, checking, exptr;
    returnunless(p);
    make_access_p1(p, fid, spec, checking, exptr, false);
    returnunless(updater(p) ->> p);
    make_access_p1(p, fid, spec, checking, exptr, true);
enddefine;

define try_recreate_access_p(p);
    lvars p, v;
    if access_p_params(p) ->> v then
        make_access_p1(p, explode(v));
        true
    else
        false
    endif
enddefine;

    /*  Called from -conskey- */
define global $- popc_conskey(key);
    lvars n, p, key, clist, key_spec, struct_spec, spec_list, fullvec;

    gen_key_key -> data_key(key);       ;;; makes it be generatable
    class_field_spec(key) -> key_spec;
    pop_struct_spec(key_spec, 0) -> struct_spec;
    struct_spec(STRUC_FIELD_LIST) -> spec_list;

    ;;; recogniser
    [%  pushq(key), call_path([\^_has_structkey]) %] -> clist;
    make_gen_p([], clist, 0, [], class_recognise(key));

    if islist(key_spec) then
        ;;; recordclass
        lvars nfields = datalength(key), np_rec_id, rec_id, result_id, locals,
                spec, m;

        define lconstant field(fid, upd);
            lvars fid, upd;
            {pas_FIELD ^fid ^key_spec ^false 0 ^upd}
        enddefine;

        ;;; constructor
        new_lex_id(true) -> result_id;      ;;; nonpop lvar
        result_id :: [] -> locals;
        false -> rec_id;
        [%  pushq(syspop\:_int(t_offset(struct_spec(STRUC_TYPE_SPEC), false))),
            call_path(if struct_spec(STRUC_FLAGS) &&/=_0 M_STRUC_DALIGN then
                        [Sys Get_store_dalign]
                      else
                        [Sys Get_store]
                      endif),
            pop(result_id);
            pushq(key), push(result_id), field("KEY", true);

            fast_for n from nfields by -1 to 1 do
                get_ident_field_spec(n, spec_list) -> spec;
                if not(rec_id) and datalength(spec) == FIELD_POP_VEC_LEN
                and spec(FIELD_CONV_P) /== [] then
                    ;;; uses conversion procedure(s) -- must make safe any full
                    ;;; fields not yet initialised and move rec to pop id
                    fast_for m from n by -1 to 1 do
                        if get_ident_field_spec(m, spec_list)(FIELD_TYPE_SPEC)
                                        && tv_VAL_TYPE == tv_FULL then
                            pushq(0), push(result_id), field(m, "noconvp")
                        endif
                    endfor;
                    push(result_id),
                    pop(new_lex_id(false) ->> rec_id ->> result_id);
                    rec_id :: locals -> locals
                endif;
                push(result_id), field(n, true)
            endfor;

            push(result_id)
        %] -> clist;
        make_gen_p(locals, clist, 0, [], class_cons(key));

        ;;; destructor
        new_lex_id(false) -> rec_id;        ;;; pop lvar
        [%  pushq(key), call_path([Sys Checkr_record]), pop(rec_id);
            fast_for n to nfields do
                push(rec_id), field(n, false)
            endfor,
        %] -> clist;
        make_gen_p([^rec_id], clist, 0, [], class_dest(key));

        ;;; access procedures
        fast_for n to nfields do
            make_access_p(class_access(n,key), n, key_spec, key, 0)
        endfor


    else
        ;;; vectorclass

        define lconstant icd(class_p, f_path, nf_path);
            lvars class_p, f_path, nf_path;
            [%  pushq(key),
                call_path(if fullvec then f_path else nf_path endif)
            %] -> clist;
            make_gen_p([], clist, 0, [], class_p(key))
        enddefine;

        conspair(key_spec, false) -> key_spec;
        front(spec_list(POP_VECTOR_FIELD)(FIELD_TYPE_SPEC))(FIELD_TYPE_SPEC)
                                    && tv_VAL_TYPE == tv_FULL -> fullvec;

        icd(class_init, [Sys Init_uservec], [Sys Init_usernfvec]);
        icd(class_cons, [Sys Cons_uservec], [Sys Cons_usernfvec]);
        icd(class_dest, [Sys Dest_uservec], [Sys Dest_usernfvec]);

        make_access_p(class_subscr(key), false, key_spec, key, 0);
        make_access_p(class_fast_subscr(key), false, key_spec, false, 0)
    endif
enddefine;      /* popc_conskey */

    /*  Called from -cons_access- */
define global $- popc_cons_access(consacc_result, spec, checking, exptr);
    lvars n, consacc_result, spec, checking, exptr;
    dlocal keep_access_p = true;

    if islist(spec) then
        fast_for n to datalength(consacc_result) do
            make_access_p(consacc_result(n), n, spec, checking, exptr)
        endfor
    else
        ;;; consacc_result is a single procedure or false
        make_access_p(consacc_result, false, spec, checking, exptr);
    endif
enddefine;      /* popc_cons_access */

endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 31 1994
        Changed generate_array for new array procedures
--- John Gibson, Jul 21 1992
        Version 14.21 changes
--- John Gibson, Apr 18 1992
        Made -generate_pcomposite- zap updater if it's from an undef whose
        updater hasn't been referenced.
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, May  5 1989
        Version 13.6402 changes
--- John Gibson, Mar 23 1989
        Overflow from m_trans.p
 */
