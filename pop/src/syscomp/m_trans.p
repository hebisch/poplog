/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/syscomp/m_trans.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                  TRANSLATING POP VM CODE TO M-CODE
                            (ALL SYSTEMS)

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas;

global vars
    procedure gen_procedure_args
    ;

section M_trans;


;;; --- GENERATING M-CODE OPERANDS --------------------------------------

lvars
    m_codelist,
    m_codelist_end,
    m_last_call_pair, m_last_call_name
    ;


define lconstant m_placei() with_nargs 1;
    () :: [] ->> f_tl(m_codelist_end) -> m_codelist_end
enddefine;

define lconstant m_place();
    consvector() :: [] ->> f_tl(m_codelist_end) -> m_codelist_end
enddefine;

define lconstant m_move(x,y);
    lvars x y;
    m_place(M_MOVE, x, y, 3)
enddefine;

define lconstant label_instr(lab);
    lvars lab;
    islabel(lab) fi_|| LAB_PROC_INNER -> islabel(lab);
    consvector(M_LABEL, lab, 2)
enddefine;

define lconstant m_label = label_instr <> m_placei enddefine;

define work_addr_reg(upd_mode);
    lvars upd_mode;
    if upd_mode then WK_ADDR_REG_2 else WK_ADDR_REG_1 endif
enddefine;

define lconstant getstr(item) -> opnd;
    lvars opnd, temp, item;
    genstructure(item) -> temp;
    unless _intval(temp) ->> opnd then
        ;;; make it quoted
        consref(temp) -> opnd;
        if execlabof(temp, false) then "procedure" -> operand_type(opnd) endif
    endunless
enddefine;

define lconstant push_str(src);
    lvars src;
    m_move(src.getstr, -_USP)
enddefine;

define lconstant get_ident_opnd(item, defer, upd_mode) -> opnd;
    lvars opnd, item, temp, is_id, token, defer, upd_mode, type;
    unless isident(item) ->> is_id then
        mishap(item, 1, 'INVALID OPERAND FOR get_ident_opnd')
    endunless;
    nonactive_identtype(item) -> type;
    if is_id == "lextoken" then
        ;;; lexical token -- idval contains the translation, which is either
        ;;; a non-local lex ident, a register, a stack offset operand, or an
        ;;; integer for a dummy lstackmem offset lvar
        idval(item) -> opnd;        ;;; get translation
        if isident(type) then
            ;;; lvar will contain a run-time ident -- deferred twice
            if defer then
                nonactive_identtype(type) -> type;  ;;; type of rt ident
                if CAN_DEFER_OPND(opnd, 0, T_WORD, upd_mode) ->> temp then
                    temp -> opnd
                else
                    ;;; have to move to a work address reg first
                    m_move(opnd, work_addr_reg(upd_mode) ->> temp);
                    {^temp 0} -> opnd
                endif
            else
                ;;; quoted -- just use stack operand or reg
                0 -> type;
                true -> defer
            endif
        elseif isident(opnd) then
            ;;; type-2 non local -- opnd is global id
            gen_lex_ident(opnd, defer) -> opnd
        elseunless defer then
            ;;; can't quote a reg or stack offset
            mishap(item, 1, 'INVALID QUOTED OPERAND FOR get_ident_opnd')
        elseif isinteger(opnd) then
            return
        endif
    elseif is_id == "perm" and (note_perm_ident(item, 0) ->> token) then
        ;;; dictionary permanent identifier
        unless defer then
            genstructure(item) -> opnd
        elseif nonactive_isconstant(item) then
            if upd_mode == "pop" then
                mishap(token, 1, 'COMPILING ASSIGNMENT TO CONSTANT IDENTIFIER')
            else
                return(perm_const_opnd(item) -> opnd)
            endif
#_IF DEF SPECIAL_VAR_BLOCK_REG
        elseif (get_svb_offset(token) ->> temp) then
            ;;; in _special_var_block -- temp is offset from start
            cons_access_opnd(SPECIAL_VAR_BLOCK_REG, temp, T_WORD) -> opnd
#_ENDIF
        else
            ;;; so we don't set IDT_GEN_FULL_ID
            identlabel(note_perm_ident(item, IDT_GEN_IDENT)) -> opnd
        endunless
    elseif defer and not(nonactive_isassignable(item)) then
        ;;; initialised lexical constant -- get value directly
        chain(idval(item), getstr)
    else
        ;;; non-constant lexical identifier
        gen_lex_ident(item, defer) -> opnd
    endif;

    if defer then
        type -> operand_type(opnd)
    else
        ;;; make it quoted
        opnd -> temp;
        consref(temp) -> opnd;
        if execlabof(temp, false) then "procedure" -> operand_type(opnd) endif
    endif
enddefine;

define lconstant getid      = get_ident_opnd(% true, false %) enddefine;
define lconstant getid_u    = get_ident_opnd(% true, "pop" %) enddefine;
define lconstant getqid     = get_ident_opnd(% false, false %) enddefine;
define lconstant getqid_u   = get_ident_opnd(% false, "pop" %) enddefine;

define lconstant push_id(src);
    lvars src;
    m_move(src.getid, -_USP)
enddefine;

define lconstant push_qid(src);
    lvars src;
    m_move(src.getqid, -_USP)
enddefine;

define lconstant pop_id(dst);
    lvars dst;
    m_move(USP_+, dst.getid_u)
enddefine;

define lconstant pop_qid(dst);
    lvars dst;
    m_move(USP_+, dst.getqid_u)
enddefine;

define lconstant erase_top();
    m_place(M_ERASE, USP_+, 2)
enddefine;

define perm_var_opnd(path);
    lvars path;
    identof_path(path).getid
enddefine;

define lconstant call_constp(path);
    lvars path;
    m_place(M_CALL, perm_constp_opnd(path, false), 2)
enddefine;

define lconstant ucall_constp(path);
    lvars path;
    m_place(M_CALL, perm_constp_opnd(path, true), 2)
enddefine;


;;; --- IN-LINE OP TRANSLATION TABLES ------------------------------------

vars
    false_immediate = false,
    true_immediate,
    nil_immediate
    ;

lvars
    frame_len;

define lconstant wof = nonop fi_*(% WORD_OFFS %) enddefine;

define lconstant init_immediates();
    unless false_immediate then
        perm_const_opnd([false]) -> false_immediate;
        perm_const_opnd([true]) -> true_immediate;
        perm_const_opnd([nil]) -> nil_immediate;
    endunless;
enddefine;

define lconstant sp_offset(index, access);
    lvars index, access;
#_IF DEF STACK_GROWS_UP
    -index -> index;
    if access then index-1 -> index endif;
#_ENDIF
    index.wof
enddefine;

define lconstant sp_index_opnd(index);
    lvars index;
    {% SP, sp_offset(index, true) %}
enddefine;

define caller_return_instr(opnd, upd);
    lvars opnd, upd;
#_IF DEF M_CALLER_RETURN
    {^M_CALLER_RETURN ^upd ^opnd}
#_ELSE
    lvars cret = sp_index_opnd(field_##("SF_RETURN_ADDR") + frame_len);
    if upd then
        {^M_MOVE ^opnd ^cret}
    else
        {^M_MOVE ^cret ^opnd}
    endif
#_ENDIF
enddefine;

define lconstant caller_sp_instr();
    {% M_PTR_ADD_OFFS, t_WORD, sp_offset(frame_len, false), SP, -_USP %}
enddefine;

define lconstant subv_instr(upd, base_fld_name, index);
    lvars upd, base_fld_name, index;
    {% OP_SUBV, upd, T_WORD, (field_##(base_fld_name) fi_+ index).wof,
                        USP_+, USP_+, if upd then USP_+ else -_USP endif %}
enddefine;

define lconstant wfield_instr(upd, fld_name);
    lvars upd, fld_name;
    {% OP_!, upd, "+", false, T_WORD, field_##(fld_name).wof, 0, USP_+,
                                        if upd then USP_+ else -_USP endif %}
enddefine;

lvars procedure Gen_dlocal_context;
;;;
define lconstant Call_gen_dlc(); Gen_dlocal_context() enddefine;

define lconstant cmpkey_instr(key);
    lvars key;
    ;;; key can be an integer specifying flags to test in K_FLAGS
    unless isintegral(key) then
        perm_const_opnd(key, NPA_WEAK_OPTION) -> key
    endunless;
    {% M_CMPKEY, key, USP_+ % EQ ?}
enddefine;


lconstant procedure (
    call_synonyms = newassoc([
        [==             \^_eq]
        [/==            \^_neq]
        [fi_<           \^_pslt]
        [fi_<=          \^_pslteq]
        [fi_>           \^_psgr]
        [fi_>=          \^_psgreq]
        [isinteger      \^_isinteger]
        [iscompound     \^_iscompound]
        [issimple       \^_issimple]
        [not            \^_not]
        [fi_+           \^_padd]
        [fi_-           \^_psub]
        [fi_*           \^_pmult]
        [fi_//          \^_pdiv]
        [fi_||          \^_por]
        [fi_&&          \^_pand]
        [dup            \^_dupstk]
        [fast_subscrv   \^_subsv]
        [fast_frozval   \^_subsfroz]
        [fsub_b         \^_subss]
        [datakey        \^_datakey]
        [prolog_assign  \^_prolog_assign]

        % applist(SUBROUTINE_OPTIMISE_LIST,
                    procedure(entry);
                        lvars entry;
                        [%  entry(1), "\^_" <> allbutfirst(1,entry(4)) %]
                    endprocedure)
        %

        ;;; tested for explicitly in pas_CALL
;;;     [$-lisp$-Lisp_setstacklength \^_setstklen]
        ]),


    inline_procs = newassoc([
    [ +             [ {^OP_+- + ^USP_+} ]]
    [ -             [ {^OP_+- - ^USP_+} ]]
    [ \^_add        [ {^M_ADD ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^_sub        [ {^M_SUB ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^_mult       [ {^M_MULT ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^_biclear    [ {^M_BIC ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^_biset      [ {^M_BIS ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^_bimask     [ {^M_BIM ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^_shift      [ {^M_ASH ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^_negate     [ {^M_NEG ^USP_+ ^ -_USP} ]]
    [ \^_logcom     [ {^M_LOGCOM ^USP_+ ^ -_USP} ]]
    [ \^_padd       [ {^M_PADD ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^_psub       [ {^M_PSUB ^USP_+ ^USP_+ ^ -_USP} ]]
    [ erase         [ {^M_ERASE ^USP_+} ]]
    [ \^_useras     [ {^M_ADD ^USP_+ ^USP ^USP} ]]
    [ \^_dupstk     [ {^M_MOVE ^i_USP ^ -_USP} ]]
    [ \^_ptr_sub    [ {^M_PTR_SUB ^USP_+ ^USP_+ ^USP_+ ^ -_USP} ]]

    [ \^_stklength  %   procedure;
                            {% M_PTR_SUB, t_WORD, USP,
                                perm_var_opnd([\^_userhi]), -_USP %}
                        endprocedure %]

    [ stacklength   %   procedure;
                            lvars n = WORD_OFFS, pow;
                            [%  "\^_stklength",
                                if is_power2(n) ->> pow then
                                    {% M_ASH, -pow, USP_+, -_USP %}
                                else
                                    {% M_MOVE, n, -_USP %},
                                    {% M_CALLSUB, perm_const_opnd([\^_divq]) %}
                                endif,
                                "\^_pint"
                            %]
                        endprocedure %]

    [ lisp_true     %   procedure;
                            lvars lab = genjumplab(false);
                            [% {% M_CMP, false_immediate, i_USP, "NEQ", lab %},
                               {% M_MOVE, nil_immediate, i_USP %},
                               label_instr(lab)
                            %]
                        endprocedure %]

    [ \^_setstklen  [ {^OP_SETSTKLEN ^USP_+ ^USP_+} ]]
    [ \^_user_sp    [ {^M_MOVE ^USP ^ -_USP} ]]
    [ \^_sp         [ {^M_MOVE ^SP ^ -_USP} ]]
    [ \^_caller_sp  % caller_sp_instr %]
    [ \^_sp_flush           [ \^_sp ]]
    [ \^_caller_sp_flush    [ \^_caller_sp ]]
    [ \^_caller_return      % caller_return_instr(% -_USP, false%) %]

    [ \^_dlocal_abexit_return % Call_gen_dlc(%true%) %]
    [ dlocal_context % Call_gen_dlc(%"dlocal_context"%) %]
    [ dlocal_process % Call_gen_dlc(%"dlocal_process"%) %]
    [ fast_apply    [ {^OP_FASTCALL ^USP_+} ]]
    [ chain         [ {^OP_CHAIN ^USP_+} ]]
    [ fast_chain    [ {^OP_FASTCHAIN ^USP_+} ]]
    [ \^_chainfrom_caller
                    [ {^OP_CHAIN_CALLER ^USP_+} ]]
    [ \^_fast_chainfrom_caller
                    [ {^OP_FASTCHAIN_CALLER ^USP_+} ]]
    [ \^_srchain    [ {^OP_CHAINSUB ^USP_+} ]]

    [ \^__!_+_ind   [ {^OP_! ^false + ^true  ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^__!_-_ind   [ {^OP_! ^false - ^true  ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^__!_+_off   [ {^OP_! ^false + ^false ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^__!_-_off   [ {^OP_! ^false - ^false ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^__@_+_ind   [ {^OP_@ ^false + ^true  ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^__@_-_ind   [ {^OP_@ ^false - ^true  ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^__@_+_off   [ {^OP_@ ^false + ^false ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^__@_-_off   [ {^OP_@ ^false - ^false ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^__@@_+_ind  [ {^OP_@ ^false + ^true  ^USP_+ ^USP_+ ^USP_+   0   ^ -_USP} ]]
    [ \^__@@_-_ind  [ {^OP_@ ^false - ^true  ^USP_+ ^USP_+ ^USP_+   0   ^ -_USP} ]]
    [ \^__@@_+_off  [ {^OP_@ ^false + ^false ^USP_+ ^USP_+ ^USP_+   0   ^ -_USP} ]]
    [ \^__@@_-_off  [ {^OP_@ ^false - ^false ^USP_+ ^USP_+ ^USP_+   0   ^ -_USP} ]]
    [ \^__!++       [ {^OP_--!++ ^false + ^USP_+ ^USP_+ ^ -_USP ^ -_USP} ]]
    [ \^__!--       [ {^OP_--!++ ^false - ^USP_+ ^USP_+ ^ -_USP ^ -_USP} ]]
    [ \^__@++       [ {^OP_--@++ + ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^__@--       [ {^OP_--@++ - ^USP_+ ^USP_+ ^ -_USP} ]]
    [ \^__@@++      [ {^OP_--@++ + ^USP_+   0   ^ -_USP} ]]
    [ \^__@@--      [ {^OP_--@++ - ^USP_+   0   ^ -_USP} ]]

    [ fast_word_string  % wfield_instr(% false, "W_STRING" %) %]
    [ fast_cont     % wfield_instr(% false, "RF_CONT" %) %]
    [ fast_idval    % wfield_instr(% false, "ID_VALOF" %) %]
    [ fast_front    % wfield_instr(% false, "P_FRONT" %) %]
    [ fast_back     % wfield_instr(% false, "P_BACK" %) %]
    [ fast_destpair [ {^OP_FDEST ^USP_+ ^ -_USP ^ -_USP} ]]

    [ fast_prolog_functor   % wfield_instr(% false, "PGT_FUNCTOR" %) %]
    [ fast_prolog_arg       % subv_instr(% false, "PGT_ARGS", 0 %) %]

    [ \^_subsv      % subv_instr(% false, "V_WORDS", 0 %) %]
    [ \^_subsv0     % subv_instr(% false, "V_WORDS", 1 %) %]
    [ \^_subsfroz   % subv_instr(% false, "PD_CLOS_FROZVALS", 0 %) %]

    [ \^_bitfield   [ {^M_MOVEbit ^USP_+ ^USP_+ ^USP_+ ^ -_USP} ]]

    [ fast_prolog_arity
                    %   procedure;
                            [%  wfield_instr(false, "V_LENGTH"),
                                {^M_SUB 1 ^USP_+ ^ -_USP},
                                "\^_pint"
                            %]
                        endprocedure %]

    [ \^_plog_trail_push
                    % procedure();
                        lvars id = perm_var_opnd([\^_plog_trail_sp]);
                        {^OP_--!++ % true, "+", T_WORD, id, USP_+, id %}
                      endprocedure %]

    ;;; plus the machine-dependent ones -- see genproc.p
    ] nc_<> mc_inline_procs_list),


    inline_upd_procs = newassoc([
    [ \^_user_sp        [ {^M_MOVE ^USP_+ ^USP} ]]
    [ \^_caller_return  % caller_return_instr(% USP_+, true%) %]

    [ dlocal_context % Call_gen_dlc(%false%) %]
    [ dlocal_process % Call_gen_dlc(%false%) %]

    [ fast_apply    [{^OP_FASTUCALL ^USP_+} ]]

    [ \^__!_+_ind   [{^OP_! ^true + ^true  ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^USP_+}]]
    [ \^__!_-_ind   [{^OP_! ^true - ^true  ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^USP_+}]]
    [ \^__!_+_off   [{^OP_! ^true + ^false ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^USP_+}]]
    [ \^__!_-_off   [{^OP_! ^true - ^false ^USP_+ ^USP_+ ^USP_+ ^USP_+ ^USP_+}]]
    [ \^__!++       [{^OP_--!++ ^true + ^USP_+ ^USP_+ ^USP_+ ^ -_USP}]]
    [ \^__!--       [{^OP_--!++ ^true - ^USP_+ ^USP_+ ^USP_+ ^ -_USP}]]

    [ fast_cont     % wfield_instr(% true, "RF_CONT" %) %]
    [ fast_idval    % wfield_instr(% true, "ID_VALOF" %) %]
    [ fast_front    % wfield_instr(% true, "P_FRONT" %) %]
    [ fast_back     % wfield_instr(% true, "P_BACK" %) %]

    [ fast_prolog_functor   % wfield_instr(% true, "PGT_FUNCTOR" %) %]
    [ fast_prolog_arg       % subv_instr(% true, "PGT_ARGS", 0 %) %]

    [ \^_subsv      % subv_instr(% true, "V_WORDS", 0 %) %]
    [ \^_subsv0     % subv_instr(% true, "V_WORDS", 1 %) %]
    [ \^_subsfroz   % subv_instr(% true, "PD_CLOS_FROZVALS", 0 %) %]

    [ \^_bitfield   [ {^M_UPDbit ^USP_+ ^USP_+ ^USP_+ ^USP_+} ]]

    ]),

    );


constant procedure (

            ;;; also used in m_optimise.p
    inline_conditions = newassoc([
    [ \^_not        NOT]
    [ \^_bitst      {^M_BIT  ^USP_+ ^USP_+ NEQ ?}   ]
    [ \^_zero       {^M_TEST ^USP_+ EQ  ?}          ]
    [ \^_nonzero    {^M_TEST ^USP_+ NEQ ?}          ]
    [ \^_neg        {^M_TEST ^USP_+ NEG ?}          ]
    [ \^_nonneg     {^M_TEST ^USP_+ POS ?}          ]

    [ \^_eq         {^M_CMP  ^USP_+ ^USP_+  EQ   ?}]
    [ \^_neq        {^M_CMP  ^USP_+ ^USP_+  NEQ  ?}]

    [ \^_gr         {^M_CMP  ^USP_+ ^USP_+  ULT  ?}]
    [ \^_greq       {^M_CMP  ^USP_+ ^USP_+  ULEQ ?}]
    [ \^_lt         {^M_CMP  ^USP_+ ^USP_+  UGT  ?}]
    [ \^_lteq       {^M_CMP  ^USP_+ ^USP_+  UGEQ ?}]

    [ \^_sgr        {^M_CMP  ^USP_+ ^USP_+  LT   ?}]
    [ \^_sgreq      {^M_CMP  ^USP_+ ^USP_+  LEQ  ?}]
    [ \^_slt        {^M_CMP  ^USP_+ ^USP_+  GT   ?}]
    [ \^_slteq      {^M_CMP  ^USP_+ ^USP_+  GEQ  ?}]

    [ \^_psgr       {^M_PCMP  ^USP_+ ^USP_+  LT   ?}]
    [ \^_psgreq     {^M_PCMP  ^USP_+ ^USP_+  LEQ  ?}]
    [ \^_pslt       {^M_PCMP  ^USP_+ ^USP_+  GT   ?}]
    [ \^_pslteq     {^M_PCMP  ^USP_+ ^USP_+  GEQ  ?}]
    [ \^_padd_testovf {^M_PADD_TEST ^USP_+ ^USP_+ NOVF ?}]
    [ \^_psub_testovf {^M_PSUB_TEST ^USP_+ ^USP_+ NOVF ?}]

    [ \^_ptr_gr     {^M_PTR_CMP  ^USP_+ ^USP_+ ^USP_+  ULT  ?}]
    [ \^_ptr_greq   {^M_PTR_CMP  ^USP_+ ^USP_+ ^USP_+  ULEQ ?}]
    [ \^_ptr_lt     {^M_PTR_CMP  ^USP_+ ^USP_+ ^USP_+  UGT  ?}]
    [ \^_ptr_lteq   {^M_PTR_CMP  ^USP_+ ^USP_+ ^USP_+  UGEQ ?}]

    [ \^_has_structkey {^M_CMPKEY  ^USP_+ ^USP_+ EQ ?}]

    [ isstring      % cmpkey_instr(%M_K_STRING%) % ]
    [ isdstring     % cmpkey_instr(%M_K_DSTRING%) % ]
    [ isident       % cmpkey_instr(%[ident_key]%) % ]
    [ isword        % cmpkey_instr(%[word_key]%) % ]
    [ isprocedure   % cmpkey_instr(%[procedure_key]%) % ]
    [ isvector      % cmpkey_instr(%[vector_key]%) % ]
    [ ispair        % cmpkey_instr(%[pair_key]%) % ]
    [ isboolean     % cmpkey_instr(%[boolean_key]%) % ]
    [ isprocess     % cmpkey_instr(%[process_key]%) % ]
    [ isprologvar   % cmpkey_instr(%[prologvar_key]%) % ]
    [ isbiginteger  % cmpkey_instr(%[biginteger_key]%) % ]
    [ iskey         % cmpkey_instr(%[key_key]%) % ]
    [ isdevice      % cmpkey_instr(%[device_key]%) % ]

    ;;; plus the machine-dependent ones -- see genproc.p
    ] nc_<> mc_inline_conditions_list),

    );

global constant procedure (
    negate_test = newassoc([
        [EQ   NEQ]  [NEQ   EQ]
        [LT   GEQ]  [LEQ   GT]  [GT   LEQ]  [GEQ   LT]
        [ULT UGEQ]  [ULEQ UGT]  [UGT ULEQ]  [UGEQ ULT]
        [NEG  POS]  [POS  NEG]
        [OVF NOVF]  [NOVF OVF]
        ]),

    commute_test = newassoc([
        [EQ   EQ]  [NEQ   NEQ]
        [LT   GT]  [LEQ   GEQ]  [GT   LT]  [GEQ   LEQ]
        [ULT UGT]  [ULEQ UGEQ]  [UGT ULT]  [UGEQ ULEQ]
        ]),

    );

;;; -------------------------------------------------------------------------

lconstant
    pas_generate_p  = newactproperty([], 32, false, true,
                            procedure();
                                mishap(->, 1, 'UNKNOWN VM INSTRUCTION (generate)')
                            endprocedure);

lconstant procedure
    copyable_closure = newproperty([], 8, false, "tmparg");

    ;;; Subscripts for dlocal expression entry vectors
lconstant macro (DLX_VARS = 1, DLX_GET = 2, DLX_PUT = 3, DLX_ACTID = 4);

lvars
    vm_codelist,
    vm_instr,
    pdr_flags,
    ;


    ;;; generate code for an lblock
define lconstant gen_lblock_code(lblock_instr);
    lvars s_list, f_list, id, clos_inits, lblock_instr, s_len;
    explode(lblock_instr) -> clos_inits -> f_list -> s_list -> ;
    listlength(s_list) -> s_len;

    ;;; code for identifiers initialised to run-time idents
    if f_list /== [] then
        ;;; typed required, and maybe untyped
        push_str(f_list);
        push_str(listlength(f_list)+s_len); ;;; total number needed
        call_constp([Sys Cons_rt_idents]);
        applist(s_list <> rev(f_list), pop_qid)     ;;; pop 'em the right way
    elseif s_list /== [] then
        ;;; only untyped -- use quicker procedure
        push_str(s_len);                    ;;; the number needed
        call_constp([Sys Cons_rt_idents_untyped]);
        applist(s_list, pop_qid)
    endif;

    ;;; code to initialise local lvars to cache lexical closures
    for id in clos_inits do m_move(false.getstr, id.getid_u) endfor
enddefine;

define lconstant trans_vmcode(vm_codelist);
    dlocal vm_codelist, vm_instr;
    until vm_codelist == [] do
        if isvector(f_hd(vm_codelist) ->> vm_instr) then
            pas_generate_p(f_subv(1, vm_instr))()
        else
            if ispair(vm_instr) then
                ;;; front is a vector containing code to be inserted BEFORE
                ;;; label in back (used by -do_inline_conditions-)
                trans_vmcode(f_subv(1, fast_destpair(vm_instr) -> vm_instr))
            endif;
            m_label(vm_instr)
        endif;
        f_tl(vm_codelist) -> vm_codelist
    enduntil
enddefine;


define m_translate(create_sf_instr, Ndlocals, frame_len, nlgoto_var,
                    rtid_args, lblock_instr, dloc_rtid_list, dlexpr_list,
                    pdr_flags, pdr_entry_type, codelist)
                -> (m_codelist, exit_lab, pdr_flags);

    lvars   pair, Ndlocals, nlgoto_var, rtid_args, lblock_instr, codelist,
            create_sf_instr, pdr_entry_type
        ;
    dlvars  lx, dlexpr_list, dloc_rtid_list, exit_lab, norm_exit_lab,
            dlexpr_index, dlexpr_save, dlexpr_proc, somedlexpr = false,
            mystackmark, stackerr_lab
        ;
    dlocal  frame_len, pdr_flags, m_codelist, m_codelist_end, m_last_call_pair,
            m_last_call_name,
        ;
    lconstant
            NORMAL = 1, ABNORMEXIT = 2, SUSPEND = 3, RESUME = 4,
        ;

    define lconstant inv_dlocal_context =
        mishap(%0, 'INVALID USE OF dlocal_context, ETC'%)
    enddefine;

    dlocal Gen_dlocal_context = inv_dlocal_context;

    define lconstant translxcode(el, context);
        lvars instr, lab, clist, el;
        dlvars context;
        for clist on lx(el) do
            unless isvector(f_hd(clist) ->> instr) then
                genlab() ->> lab -> f_hd(clist);
                islabel(instr) -> islabel(lab)
            endunless
        endfor;

        define dlocal Gen_dlocal_context(which);
            lvars which;
            unless which then
                ;;; produce error for update
                inv_dlocal_context()
            endunless;
            [ {%M_MOVE, if which == "dlocal_context" then
                            context.getstr
                        elseif which == "dlocal_process" then
                            dlexpr_proc
                        else
                            ;;; getting abnormal_exit_return
                            dlexpr_save
                        endif, -_USP%}]
        enddefine;

        trans_vmcode(lx(el))
    enddefine;

    define lconstant lx_optim(el);
        lvars el;
        pas_optimise(lx(el) nc_<> [{pas_END}], []) -> lx(el)
    enddefine;

    define lconstant push_lxvars(); applist(lx(DLX_VARS), push_id) enddefine;

    define lconstant pop_lxvars(); applist(rev(lx(DLX_VARS)), pop_id) enddefine;

    define lconstant set_lx_index(i);
        lvars i;
        m_move(i.getstr, dlexpr_index)
    enddefine;

    define lconstant gen_trailing_code();
        lvars n, lab, suslab, rsmlab;

        define lconstant switch_code(p, switch_var, proc);
            lvars n, lab0 = genjumplab(false), lablist = [], switch,
                procedure p, switch_var, proc;

            if proc then
                ;;; save current dlexpr_proc on stack and set to this proc
                m_move(dlexpr_proc, -_USP);
                m_move(CHAIN_REG, dlexpr_proc)
            endif;
            push_str(mystackmark);                          ;;; stackmark
            if (listlength(dlexpr_list) ->> n) == 1 then
                m_place(M_PCMP, 0.getstr, switch_var, "EQ", lab0, 5);
                p(1, lab0)
            else
                ;;; switch on the index to restore the dlexprs saved
                m_placei({^M_BRANCH_ON ^switch_var 0 ^true} ->> switch);
                m_place(M_BRANCH, lab0, 2);     ;;; goto elselab if none to run
                for lx in rev(dlexpr_list) do
                    m_label(dup(genjumplab(false)) :: lablist -> lablist);
                    p(n, lab0);
                    n fi_- 1 -> n
                endfor;
                lablist -> switch(3);   ;;; label list, lab 1 - lab N
            endif;

            m_label(lab0);              ;;; elselab
            ;;; code to check mystackmark still in place
            m_place(M_CMP, mystackmark.getstr, USP_+, "NEQ", stackerr_lab, 5);
            if proc then
                ;;; set CHAIN_REG from dlexpr_proc, restore latter from stack
                m_move(dlexpr_proc, CHAIN_REG);
                m_move(USP_+, dlexpr_proc)
            endif
        enddefine;

        define lconstant swap_rtids();
            lvars pair;
            ;;; code to swap dlocal rt id values
            for pair in dloc_rtid_list do
                ;;; rtid, save -> rtid -> save
                push_id(back(pair)),
                m_move(front(pair).getid, back(pair).getid_u),
                pop_id(front(pair))
            endfor
        enddefine;

        ;;; Abnormal exit is IMMEDIATELY preceded by two standard-length
        ;;; branch instructions of length L that go to the suspend code and
        ;;; the resume code. These are then accessible as
        ;;;         PD_EXIT - 2*L  and  PD_EXIT - L
        m_place(M_BRANCH_std, genjumplab(false) ->> suslab, 2);
        m_place(M_BRANCH_std, genjumplab(false) ->> rsmlab, 2);
        m_label(genjumplab(false) ->> exit_lab);    ;;; abnormal exit is here
        if somedlexpr then
            ;;; exit code saves the frigged return in dlexpr_save and puts the
            ;;; proper return back, then switches on the index for the dlexprs,
            ;;; after which it resaves the proper caller return in
            ;;; CHAIN_REG and puts the frigged one back
            ;;; (then goes to normal exit)
            m_placei(caller_return_instr(dlexpr_save, false));  ;;; get
            m_placei(caller_return_instr(CHAIN_REG, true));     ;;; set
            switch_code(procedure(n, endlab);
                            lvars n, endlab;
                            set_lx_index(n fi_- 1),
                            push_lxvars(), translxcode(DLX_PUT, ABNORMEXIT)
                        endprocedure, dlexpr_index, false);
            m_placei(caller_return_instr(CHAIN_REG, false));    ;;; get
            m_placei(caller_return_instr(dlexpr_save, true))    ;;; set
        endif;
        m_place(M_BRANCH, norm_exit_lab, 2);            ;;; goto normal exit

        ;;; code to be run by process suspend
        m_label(suslab);
        if somedlexpr then
            ;;; code saves the value of dlexpr index on the stack while
            ;;; running the expressions, and then pops it into dlexpr save (so
            ;;; that the resume code does upto that number and no more).
            m_move(dlexpr_index, -_USP);
            switch_code(procedure(n, endlab);
                            lvars n, endlab;
                            set_lx_index(n fi_- 1), translxcode(DLX_GET, SUSPEND),
                            push_lxvars(), translxcode(DLX_PUT, SUSPEND),
                            pop_lxvars()
                        endprocedure, dlexpr_index, true);
            m_move(USP_+, dlexpr_save)
        endif;
        swap_rtids();
        m_place(M_CHAINSUB,
                perm_const_opnd([\^_swap_out_continue], NPA_WEAK_OPTION),
                2);

        ;;; code to be run by process resume
        m_label(rsmlab);
        swap_rtids();
        if somedlexpr then
            ;;; do dlexprs upto index saved in dlexpr_save
            switch_code(procedure(n, endlab);
                            lvars i = 1, n, endlab;
                            for lx in dlexpr_list do
                                push_lxvars(), translxcode(DLX_GET, RESUME),
                                pop_lxvars(), translxcode(DLX_PUT, RESUME),
                                set_lx_index(i);
                                quitif(i == n);
                                i fi_+ 1 -> i
                            endfor,
                            if n /== 1 then m_place(M_BRANCH, endlab, 2) endif
                        endprocedure, dlexpr_save, true)
        endif;
        m_place(M_CHAINSUB,
                perm_const_opnd([\^_swap_in_continue], NPA_WEAK_OPTION),
                2);
        if somedlexpr then
            m_label(stackerr_lab), call_constp([Sys Dlexpr_stackerr])
        endif
    enddefine;      /* gen_trailing_code */

    init_immediates();

    ;;; label of normal exit code
    genjumplab(false) ->> exit_lab -> norm_exit_lab;

    if f_tl(codelist) == [] and dlexpr_list == [] and rtid_args == [] then
        ;;; presumably identfn!
        [^(label_instr(exit_lab)) {^M_RETURN} {^M_END}] -> m_codelist;
        return
    endif;

    ;;; generate M-code

    ;;; prologue code to create stack frame (M_CREATE_SF)
    [^create_sf_instr] ->> m_codelist -> m_codelist_end;

    if dlexpr_list /== [] then
        true -> somedlexpr;
        ;;; first 3 elements of dlexpr_list are 2 nonpop and 1 pop lvars
        ;;; allocated for dlexpr index, dlexpr save and dlexpr proc
        (f_dest(dlexpr_list) -> dlexpr_list).getid -> dlexpr_index;
        (f_dest(dlexpr_list) -> dlexpr_list).getid -> dlexpr_save;
        (f_dest(dlexpr_list) -> dlexpr_list).getid -> dlexpr_proc;
        ;;; init dlexpr index to 0 (must be done before any interrupt checking)
        set_lx_index(0);

        genjumplab(false) -> stackerr_lab;
        cons_free_struct("STACKMARK",
            {% syspop\:_int(0), datakey(popstackmark) %} ) -> mystackmark
    endif;

    ;;; Code for extra run-time ident args passed for non-locals,
    ;;; followed by code to save any that are dlocals
    applist(rtid_args, pop_qid);
    for pair in dloc_rtid_list do
        ;;; front is save var, back is rtid -- save in save var
        m_move(back(pair).getid, front(pair).getid_u)
    endfor;

    if pdr_entry_type == 2 then
        ;;; This a prolog procedure (i.e. using PLOG_SAVE/RESTORE), so we use
        ;;; _prolog_save_check to save PLGSV values in the stack frame (which
        ;;; MUST be done before any GCs can happen inside this procedure).
        m_place(M_CALLSUB, perm_const_opnd([\^_prolog_save_check]), 2)
    elseif pdr_entry_type == 1 then
        ;;; call normal check subroutine
        m_place(M_CALLSUB, perm_const_opnd([\^_checkall]), 2)
    endif;

    if nlgoto_var then
        ;;; code to initialise the local idval of the var to the next
        ;;; generated integer, which will uniquely identify this call
        call_constp([Sys Gen_stack_frame_num]), pop_id(nlgoto_var)
    endif;

    ;;; code for the outermost lblock
    if lblock_instr then gen_lblock_code(lblock_instr) endif;

    ;;; add a label at the end of initial code so code scheduling
    ;;; optimisations can't mix up it up with the main code
    genjumplab(false) :: codelist -> codelist;

    if somedlexpr then
        lvars lx_n = 0;
        ;;; dlexpr code for normal entry
        for lx in dlexpr_list do
            lx_n fi_+ 1 -> lx_n;
            lx_optim(DLX_GET);
            translxcode(DLX_GET, NORMAL), pop_lxvars(), set_lx_index(lx_n)
        endfor;
        ;;; process codelist
        trans_vmcode(codelist);
        ;;; dlexpr code for normal exit -- ensure it starts with a label so
        ;;; code optimisation can't mix up it up with the main code
        m_label(genjumplab(false));
        for lx in rev(dlexpr_list) do
            lx_n fi_- 1 -> lx_n;
            lx_optim(DLX_PUT);
            set_lx_index(lx_n), push_lxvars(), translxcode(DLX_PUT, NORMAL)
        endfor
    else
        ;;; process codelist
        trans_vmcode(codelist)
    endif;


    ;;; epilogue code

    m_label(norm_exit_lab);         ;;; label of normal exit code
    ;;; code to restore dlocal rtids
    for pair in dloc_rtid_list do
        ;;; front is save var, back is rtid -- restore
        m_move(front(pair).getid, back(pair).getid_u)
    endfor;
    m_place(M_UNWIND_SF, 1);        ;;; unwind stack frame
    m_place(M_RETURN, 1);           ;;; return from procedure

    if somedlexpr or dloc_rtid_list /== [] then
        ;;; special code for dlexprs and local rtids
        ;;; flag tell process suspend and resume to run code
        pdr_flags || M_PD_PROC_DLEXPR_CODE -> pdr_flags;
        gen_trailing_code()
    endif;

    ;;; finally, optimise the M-code
    m_place(M_END, 1);              ;;; end of codelist
    m_optimise(m_codelist, exit_lab, Ndlocals, pdr_flags) -> m_codelist
enddefine;


    ;;; Expand the inline procedure (list of instructions).  If an
    ;;; "instruction" is a word, look it up and expand that too. else place
    ;;; it (should be a vector).
    ;;; DOESN'T CHECK FOR RECURSIVE DEFINITIONS. an item following a "->" gets
    ;;; looked up in the updater table.
define lconstant do_inline_proc(list);
    lvars list, instr, more;
    if isprocedure(list) then
        unless islist(list() ->> list) then list :: [] -> list endunless
    endif;
    until list == [] do
        dest(list) -> list -> instr;
        if isvector(instr) then
            m_placei(copy(instr))
        else
            if instr == "->" then
                inline_upd_procs(dest(list) -> list ->> instr)
            else
                inline_procs(instr)
            endif -> more;
            unless more then
                mishap(instr, 1, 'LOOKUP FAILURE ON INLINE PROCEDURE')
            endunless;
            do_inline_proc(more)
        endif
    enduntil
enddefine;

define lconstant next_instr();
    lvars nexti;
    f_hd(f_tl(vm_codelist)) -> nexti;
    if isvector(nexti) then f_subv(1, nexti) else "LABEL" endif, nexti
enddefine;

define lconstant remove_next_call_of(pname);
    lvars nexti, id, pname;
    if (next_instr() -> nexti) == "pas_CALL"
    and isident(f_subv(2,nexti) ->> id) == "perm"
    and get_ident_token(id) == pname then
        f_tl(f_tl(vm_codelist)) -> f_tl(vm_codelist);   ;;; remove next instr
        true
    else
        false
    endif
enddefine;

define lconstant pair_label(lab);
    lvars lab;
    ;;; pair is a code insertion before label in back (codelist in front)
    if ispair(f_front(lab) ->> lab) then f_back(lab) else lab endif
enddefine;

define lconstant conditions(bool);
    lvars bool;
    m_place(M_CMP, false_immediate, if bool then i_USP else USP_+ endif,
            if f_subv(3,vm_instr) then "NEQ" else "EQ" endif,
            pair_label(f_subv(2,vm_instr)), 5);
    if bool then erase_top() endif
enddefine;

define lconstant do_inline_conditions(ivec);
    lvars   list, next, nextop, ivec, n, lab1, lab2, test, v, pair,
            following_not;

    define lconstant push_bool(bool);
        lvars bool;
        {pas_PUSH %identof_path(if bool then [true] else [false] endif)%}
    enddefine;

    define lconstant lab_at(clist);
        lvars clist;
        if isvector(f_hd(clist)) then
            genjumplab(false) :: clist
        else
            clist
        endif
    enddefine;

    remove_next_call_of("not") -> following_not;

    f_tl(vm_codelist) -> list;
    f_hd(list) -> next;
    isvector(next) and f_subv(1,next) -> nextop;

    if nextop == "pas_ERASE" and isvector(ivec) and f_subv(1,ivec) == M_TEST
    then
        ;;; erasing result of single arg test -- do nothing
        return
    endif;

    unless nextop == "pas_IF" then
        if nextop == "pas_BOOL"
        ;;; not if backward label
        and islabel(pair_label(f_subv(2,next) ->> lab1)) &&=_0 LAB_PROC_INNER
        then
            ;;; add code to produce boolean result on stack
            unless ispair(f_hd(lab1) ->> pair) then
                ;;; insert bool push code before lab1
                ;;; (code inserted by trans_vmcode)
                initv(3) -> v;
                lab_at([% push_bool(true), {pas_END} %]) ->> lab2 -> f_subv(3,v);
                lab_at([% push_bool(false), {pas_GOTO ^lab1} %])
                                    nc_<> lab2 ->> lab2 -> f_subv(2,v);
                {pas_GOTO ^lab1} :: lab2 -> f_subv(1,v);
                conspair(v, pair) ->> pair -> f_hd(lab1)
            endunless;
            f_front(pair) -> v;
            if f_subv(3,next) then f_subv(3,v) else f_subv(2,v) endif
                                                        -> f_subv(2,next);
            "pas_IF" -> f_subv(1,next)
        else
            lab_at(list) -> lab1;
            if ispair(f_hd(lab1) ->> pair) then
                ;;; inserted bool push code follows
                f_front(pair) -> v;
                f_tl(f_subv(1,v)) -> f_subv(1,v);   ;;; remove GOTO around
                {pas_IF %f_subv(3,v), true%} :: lab1
            else
                lab_at(push_bool(true) :: lab1) -> lab2;
                [%  {pas_IF ^lab2 ^true},
                    push_bool(false),
                    {pas_GOTO ^lab1}
                %] nc_<> lab2
            endif ->> list -> f_tl(vm_codelist);
            f_hd(list) -> next
        endif
    endunless;

    f_subv(3, next) -> test;
    if following_not then not(test) ->> test -> f_subv(3, next) endif;

    if ivec == "NOT" then
        ;;; _not --- remove the call of not and reverse the IF test
        next ->> vm_instr -> f_hd(vm_codelist);
        f_tl(list) -> f_tl(vm_codelist);
        not(test) -> f_subv(3, next);   ;;; reverse test
        chain(pas_generate_p("pas_IF"))
    endif;

    list -> vm_codelist;
    if isprocedure(ivec) then ivec() else copy(ivec) endif -> ivec;
    datalength(ivec) -> n;
    pair_label(f_subv(2,next)) -> f_subv(n,ivec);   ;;; the label
    unless test then
        ;;; test is IFNOT
        n fi_- 1 -> n;
        negate_test(f_subv(n,ivec)) -> f_subv(n,ivec)
    endunless;
    m_placei(ivec)
enddefine;


;;; --- SYNTAX FOR GENERATOR PROCEDURES -----------------------------------

define :define_form lconstant instr;
    lvars idname = readitem();
    [define lconstant ^idname ^^proglist] -> proglist;
    pop11_comp_expr();
    sys_current_val(idname) -> pas_generate_p(idname)
enddefine;


;;; --- VM OPCODE DEFINITIONS FOR GENERATING STRUCTURES -------------------


define :instr pas_LBLOCK();
    gen_lblock_code(vm_instr)
enddefine;

define :instr pas_PUSH();
    lvars nextop;
    next_instr() -> -> nextop;
    if nextop == "pas_CALLS" or nextop == "pas_UCALLS" then
        ;;; optimise a PUSH followed by a (U)CALLS to a (U)CALL
        f_tl(f_tl(vm_codelist)) -> f_tl(vm_codelist);   ;;; remove next instr
        if nextop == "pas_CALLS" then
            "pas_CALL"
        else
            "pas_UCALL"
        endif ->> f_subv(1, vm_instr);
        chain(pas_generate_p())
    endif;
    push_id(f_subv(2, vm_instr))
enddefine;

define :instr pas_PUSHQ();
    lvars item = f_subv(2, vm_instr);
    if isbiginteger(item) and remove_next_call_of("\^_int") then
        syspop\:_int(item) ->> item -> f_subv(2, vm_instr)
    endif;
    push_str(item)
enddefine;

define :instr pas_PUSHS();
    m_move(i_USP, -_USP)
enddefine;

define :instr pas_SWAP();
    lvars i = f_subv(2, vm_instr), j = f_subv(3, vm_instr);
    if i > j then i, j -> i -> j endif;
    m_place(OP_SWAP, i+2, repeat j times USP_+ endrepeat, j+2)
enddefine;

define :instr pas_IDENT();
    push_qid(f_subv(2, vm_instr))
enddefine;

define :instr pas_POP();
    pop_id(f_subv(2, vm_instr))
enddefine;

define :instr pas_ERASE();
    erase_top()
enddefine;

define :instr pas_CALL();
    lvars id = f_subv(2, vm_instr), word, x, i, id_token;
    if isident(id) == "perm" then
        get_ident_token(id) -> id_token;
        unless call_synonyms(id_token) ->> word then
            if fast_word_string(id_token) = '$-lisp$-Lisp_setstacklength' then
                "\^_setstklen"
            else
                id_token
            endif -> word
        endunless;

        if word == "\^_pint" then
            if (next_instr() -> x ->> i) == "pas_TYPE_CHECK"
            and f_subv(2,x) == integer_key then
                f_tl(f_tl(vm_codelist)) -> f_tl(vm_codelist);   ;;; remove
                next_instr() -> x -> i
            endif;
            if i == "pas_GO_ON" then
                ;;; turn the go_on into a go_on_int
                "pas_GO_ON_INT" -> f_subv(1, x);
                return
            endif

        elseif word == "\^_plog_trail_push" then
            M_PD_DOES_PLOG_PUSH || pdr_flags -> pdr_flags

        elseif word == "\^_prolog_assign"
        and m_codelist_end == m_last_call_pair
        and ((m_last_call_name ->> x) == "\^_conspair"
            or x == "consprologterm")
        then
            f_hd(m_codelist_end) -> i;
            if x == "consprologterm" then
                M_CALL -> f_subv(1, i);
                perm_constp_opnd([Sys Plog Assign_term], false)
            else
                perm_const_opnd([\^_prolog_assign_pair])
            endif -> f_subv(2, i);
            false -> m_last_call_name;
            return
        endif;

        if inline_procs(word) ->> x then
            do_inline_proc(x);
            return
        elseif inline_conditions(word) ->> x then
            do_inline_conditions(x);
            return
        elseif word /== id_token then
            get_perm_ident(word) ->> id -> f_subv(2, vm_instr)
        endif
    else
        false -> word
    endif;

    m_place(if nonpop_ident(id) then M_CALLSUB else OP_CALL endif,
                                                            id.getid, 2);
    if word then
        word -> m_last_call_name;
        m_codelist_end -> m_last_call_pair
    endif
enddefine;

define :instr pas_CALLQ();
    m_place(OP_CALL, f_subv(2, vm_instr).getstr, 2)
enddefine;

define :instr pas_CALLS();
    m_place(OP_CALL, USP_+, 2);
enddefine;

define :instr pas_UCALL();
    lvars id = f_subv(2, vm_instr), word, opnd, id_token;
    if isident(id) == "perm" then
        get_ident_token(id) -> id_token;
        unless call_synonyms(id_token) ->> word then
            id_token -> word
        endunless;
        if inline_upd_procs(word) ->> opnd then
            do_inline_proc(opnd);
            return
        elseif word /== id_token then
            get_perm_ident(word) ->> id -> f_subv(2, vm_instr)
        endif
    else
        "?" -> word
    endif;

    get_ident_opnd(id, true, true) -> opnd;
    unless nonpop_ident(id) then
        m_place(OP_UCALL, opnd, 2)
    elseif isref(opnd) then     ;;; i.e. it's constant
        m_place(M_CALLSUB, consref(updlabof(fast_cont(opnd), true)), 2)
    else
        mishap(word, 1, 'UPDATER CALL OF NON-CONSTANT SUBROUTINE')
    endunless
enddefine;

define :instr pas_UCALLQ();
    m_place(OP_UCALL, f_subv(2, vm_instr).getstr, 2)
enddefine;

define :instr pas_UCALLS();
    m_place(OP_UCALL, USP_+, 2)
enddefine;

define :instr pas_IF();
    conditions(false)
enddefine;

define :instr pas_BOOL();
    conditions(true)
enddefine;

define :instr pas_GOTO();
    lvars l, lab = f_subv(2, vm_instr);
    ;;; remove any code upto the next label
    f_tl(vm_codelist) -> l;
    while isvector(f_hd(l)) and f_subv(1,f_hd(l)) /== "pas_END" do
        f_tl(l) -> l
    endwhile;
    l -> f_tl(vm_codelist);
    unless lab == l then
        m_place(M_BRANCH, pair_label(lab), 2)
    endunless
enddefine;

define :instr pas_NL_GOTO();
    lvars id;
    push_str(f_subv(2, vm_instr));      ;;; push target procedure label
    if f_subv(3, vm_instr) ->> id then
        ;;; nlgoto lvar needed
        push_qid(id);               ;;; push the number from the id (rtid)
        call_constp([Sys Non_local_goto_id])
    else
        ;;; the nlgoto lvar isn't needed -- the exit is just to the first
        ;;; occurrence of the target procedure
        ;;; chain Non_local_goto
        m_place(OP_CHAIN, perm_constp_opnd([Sys Non_local_goto], false), 2)
    endif
enddefine;

define :instr pas_GO_ON();
    lvars   elselab = f_subv(3, vm_instr), base = 1,
            labs = maplist(f_subv(2,vm_instr), pair_label);
    if datalength(vm_instr) == 4 then f_subv(4,vm_instr) -> base endif;
#_IF DEF BRANCH_ON_ALLOWS_BASE
    m_place(M_BRANCH_ON, USP_+, labs, elselab and true, base, 5);
#_ELSE
    ;;; assumes base is 1
    if base /== 1 then m_place(M_PADD, popint(1-base), USP_+, -_USP, 4) endif;
    m_place(M_BRANCH_ON, USP_+, labs, elselab and true, 4);
#_ENDIF
    if elselab then               ;;; else label
        m_place(M_BRANCH, pair_label(elselab), 2);
    else
        call_constp([Sys Go_on_outrange])
    endif
enddefine;

define :instr pas_GO_ON_INT();
    lvars   elselab = f_subv(3, vm_instr), base = 1,
            labs = maplist(f_subv(2,vm_instr), pair_label);
    unless elselab then
        mishap(0, 'go_on MUST HAVE AN else LABEL')
    endunless;
    if datalength(vm_instr) == 4 then f_subv(4,vm_instr) -> base endif;
#_IF DEF BRANCH_ON_ALLOWS_BASE
    m_place(M_BRANCH_ON_INT, USP_+, labs, base, 4);
#_ELSE
    ;;; assumes base is 1
    if base /== 1 then m_place(M_ADD, 1-base, USP_+, -_USP, 4) endif;
    m_place(M_BRANCH_ON_INT, USP_+, labs, 3);
#_ENDIF
    m_place(M_BRANCH, pair_label(elselab), 2)
enddefine;

define :instr pas_LEX_CLOSURE();
    lvars clos_template = f_subv(2, vm_instr);
    true -> copyable_closure(clos_template);
    push_str(clos_template);            ;;; push the closure template
    call_constp([Sys Cons_lex_closure])
enddefine;

define :instr pas_NOOP();
enddefine;

    /*  Check for interrupts/userstack overflow */
define :instr pas_CHECK();
    lvars lab1, lab2;
    genjumplab(false) -> lab1;
    genjumplab(false) -> lab2;
    m_place(M_TEST, perm_var_opnd([\^_trap]), "NEQ", lab1, 4);
    m_place(M_CMP, perm_var_opnd([\^_userlim]), USP, "ULEQ", lab2, 5);
    m_label(lab1);
    m_place(M_CALLSUB, perm_const_opnd([\^_checkall]), 2);
    m_label(lab2)
enddefine;

    /*  Type-check item on top of stack */
define :instr pas_TYPE_CHECK();
    ;;; type in instruction is currently either integer_key or procedure_key
    m_place(OP_TYPE_CHECK, f_subv(2, vm_instr), USP_+, 3)
enddefine;


;;; --- PROLOG -----------------------------------------------------------

    ;;; {pas_PLOG_RESTORE}
define :instr pas_PLOG_RESTORE();
    m_place(M_CALLSUB, perm_const_opnd([\^_prolog_restore]), 2)
enddefine;

    ;;; {pas_PLOG_RESTART}
define :instr pas_PLOG_RESTART();
    m_place(M_CALLSUB, perm_const_opnd(
                        if pdr_flags &&/=_0 M_PD_PLOG_CHOICE then
                            [\^_prolog_save_check]
                        else
                            ;;; doesn't actually use SAVE/RESTORE
                            [\^_checkplogall]
                        endif), 2)
enddefine;

    ;;; {pas_PLOG_IFNOT_ATOM <ifnot_label> <ref <atom> or false> <id or false>}
define :instr pas_PLOG_IFNOT_ATOM();
    lvars id, atomref, ifnot_lab;
    explode(vm_instr) -> id -> atomref -> ifnot_lab -> ;
    m_place(M_CALLSUB,
            perm_const_opnd([\^_prolog_unify_atom]),    ;;; takes args in regs
            if atomref then cont(atomref).getstr else USP_+ endif,
            if id then id.getid else USP_+ endif,
            4);
    m_place(M_PLOG_IFNOT_ATOM, pair_label(ifnot_lab), 2)
enddefine;

    ;;; {pas_PLOG_TERM_SWITCH <fail_label> <var_label> <term>}
    ;;; fall thru if arg is a pair/term
define :instr pas_PLOG_TERM_SWITCH();
    lvars term, var_lab, fail_lab;
    explode(vm_instr) -> term -> var_lab -> fail_lab -> ;
    if ispair(term) then
        m_place(M_CALLSUB, perm_const_opnd([\^_prolog_pair_switch]), USP_+, 3)
    else
        m_place(M_CALLSUB,
                perm_const_opnd([\^_prolog_term_switch]),
                popint(datalength(term)),
                prolog_functor(term).getstr,
                USP_+,
                5)
    endif;
    m_place(M_PLOG_TERM_SWITCH, pair_label(fail_lab), pair_label(var_lab),
                                                                -_USP, 4)
enddefine;


;;; --- FIELD ACCESSING ------------------------------------------------

    ;;; bits in exptr argument
lconstant macro (
    MODE_LEVEL              = 2:1e8 - 1,    ;;; mask for access level
    MODE_FIXED_EXPTR        = 2:1e8,        ;;; return fixed rec for exptr
    MODE_ADDR_MODE          = 2:1e9,        ;;; address access
    MODE_STRUCT_FIXED_EXPTR = 2:1e10,       ;;; fixed exptr for struct
    );

lvars doing_exacc_field_offset = false;

    ;;; {pas_FIELD <field num> <spec> <checking> <exptr> <update>}
define :instr pas_FIELD();
    lvars   spec, n, type, offs, spec_list, upd, exptr, tcode, vtype,
            nbits, path, vecsub, upd, acc_inst, code_start, checking,
            checkcode, acc_p, conv_p, addr_mode, extern, exlevels, exoffs,
            array_size, p, prec_exptr, rec_id, subs_id, fltsingle;

    lconstant invalid_update = 'INVALID EXTERNAL UPDATE OPERATION';

    define lconstant PUSHQ(item);
        lvars item;
        {pas_PUSHQ ^item}
    enddefine;

    define lconstant CALL(word);
        lvars word;
        {pas_CALL ^(get_perm_ident(nonpop_trans_vm(word)))}
    enddefine;

    define lconstant get_acc_conv_p(spec, type);
        lvars spec, type;
        if isintegral(type) and type &&/=_0 t_IS_STRUCT
        and exptr &&/=_0 MODE_STRUCT_FIXED_EXPTR then
            exptr fi_|| MODE_FIXED_EXPTR -> exptr
        endif;
        if datalength(spec) == FIELD_VEC_LEN then
            dup([])
        else
            spec(FIELD_ACC_P), spec(FIELD_CONV_P)
        endif -> (acc_p, conv_p)
    enddefine;

    define lconstant call_p_instr(p, upd, place);
        lvars p, upd, place;
        if upd then
            if updater(p) ->> upd then
                upd -> p
            else
                mishap(p, 1, 'FIELD ACCESS/CONVERSION PROCEDURE HAS NO UPDATER')
            endif
        endif;
        returnif(p == identfn or p = #_< identfn(%%) >_#);
        (OP_CALL, p.getstr, 2);
        if place then m_place() else consvector() endif
    enddefine;

    define lconstant fixed_exptr();
        acc_p /== [] or conv_p /== [] or exptr &&/=_0 MODE_FIXED_EXPTR
    enddefine;

    define lconstant run_access_p();
        lvars l;
        ;;; run access procedures on result, updater of last if update mode
        fast_for l on acc_p do
            call_p_instr(f_hd(l), upd and f_tl(l) == [], true)
        endfor
    enddefine;

    define lconstant run_conv_p();
        lvars p;
        ;;; run conversion procedures
        fast_for p in conv_p do call_p_instr(p, false, true) endfor
    enddefine;

    ;;; external ptr result -- construct external_ptr rec unless conversion
    ;;; procedure specified, in which case return a fixed one to save
    ;;; garbage.
    define lconstant exptr_result(run_acc_p);
        lvars run_acc_p;
        returnif(doing_exacc_field_offset);
        ;;; 2nd arg is false to cons a new pointer, true to use fixed one
        m_place(OP_MAKE_EXPTR, fixed_exptr(),
                                getstr(%writeable consexternal_ptr()%), 3);
        if run_acc_p then chain(run_access_p) endif
    enddefine;


    explode(vm_instr), if datalength(vm_instr) == 6 then false, false endif
                -> (, n, spec, checking, exptr, upd, rec_id, subs_id);
    unless exptr then 0 -> exptr endunless;
    pop_struct_spec(spec, exptr && MODE_LEVEL)(STRUC_FIELD_LIST) -> spec_list;
    exptr &&/=_0 MODE_ADDR_MODE -> addr_mode;
    false -> vecsub;

    if exptr &&/=_0 MODE_LEVEL ->> extern then
        ;;; external access
        spec_list(POP_EXPTR_FIELD) -> spec;
        spec(FIELD_OFFSET) ->> offs -> exoffs;
        ;;; allow for extra levels of indirection
        (exptr && MODE_LEVEL) - 1 -> exlevels;

        spec(FIELD_TYPE_SPEC) -> type;
        if addr_mode and not(isref(type)) then
            ;;; accessing pointer value (type must be an integer)
            consref(spec) -> type
        endif;
        if isref(type) then
            ;;; external pointer access -- get pointer field first
            exlevels+1 -> exlevels;
            0 -> offs;
            cont(type) -> spec;
            spec(FIELD_TYPE_SPEC) -> type;
            if ispair(type) then
                ;;; array
                type -> spec
            elseif isvector(type) and type(VECDESC_TYPE) == "struct" then
                ;;; structure
                type(STRUC_FIELD_LIST) -> spec_list;
                false -> spec
            elseif isvector(type) and type(VECDESC_TYPE) == "function" then
                ;;; function call
                if upd then mishap(0, invalid_update) endif;
                "function" -> extern;
                type(FUNC_NARGS) -> n;
                type(FUNC_FLTSINGLE) -> fltsingle;
                if type(FUNC_RESULT_SPEC) ->> spec then
                    ;;; access result
                    spec(FIELD_TYPE_SPEC) -> type;
                    spec(FIELD_OFFSET) -> offs  ;;; may be nonzero
                endif
            endif
        ;;; else pointer value itself being accessed
        endif

    elseif islist(spec) then
        ;;; pop record
        false -> spec
    else
        ;;; pop vector
        spec_list(POP_VECTOR_FIELD) -> spec;
        spec(FIELD_OFFSET) -> offs;
        spec(FIELD_TYPE_SPEC) -> spec   ;;; will be a pair for an array
    endif;

    if ispair(spec) then
        ;;; array access
        cont(back(spec)) -> array_size;
        front(spec) -> spec;        ;;; is an array field
        spec(FIELD_TYPE_SPEC) -> type;
        if n then n else true endif -> vecsub
    elseunless spec or extern == "function" then
        ;;; structure access -- pick up appropriate field
        get_ident_field_spec(n, spec_list) -> spec;
        spec(FIELD_OFFSET) -> offs;
        spec(FIELD_TYPE_SPEC) -> type
    endif;

    false -> prec_exptr;
    if f_subv(1, f_hd(m_codelist_end)) == OP_MAKE_EXPTR then
        f_hd(m_codelist_end) -> prec_exptr;
        true -> f_subv(2,prec_exptr);   ;;; stop it consing a new pointer
    endif;

    if checking then
        ;;; code to check argument(s)
        if extern then
            if vecsub then
                unless isinteger(vecsub) then
                    if array_size < HUGE then
                        ;;; check subscript against max length
                        push_str(array_size);
                        call_constp([Sys Checkr_exptrclass_subscr])
                    else
                        ;;; just check subscript > 0
                        call_constp([Sys Checkr_exptrclass_subscr0])
                    endif
                endunless;
                false -> prec_exptr
            elseif extern == "function" and not(n) then
                ;;; check nargs to variadic external call
                call_constp([Sys Checkr_exptrclass_nargs]);
                false -> prec_exptr
            elseunless prec_exptr then
                ;;; just check for external ptr class
                call_constp([Sys Checkr_exptrclass])
            endif
        elseif iskey(checking) then
            ;;; pop record/vector
            push_str(checking);
            call_constp(if vecsub then
                            [Sys Checkr_vec_subscr]
                        else
                            [Sys Checkr_record]
                        endif)
        endif
    endif;

    m_codelist_end -> code_start;

    if extern == "function" then
        if n then
            ;;; fixed nargs
            push_str(n);
            [Sys Call_extern]
        else
            ;;; variadic
            [Sys Call_extern_nargs]
        endif -> p;
        push_str(syspop\:_int(fltsingle));
        call_constp(p);
        unless spec then
            ;;; no result
            erase_top();    ;;; erase result from _call_external
            return
        endunless

    elseif exlevels /== 0 and not(doing_exacc_field_offset) then
        ;;; plant external accesses
        if isintegral(type) and type &&/=_0 t_IS_STRUCT
        and type && t_BASE_TYPE == t_WORD and offs == 0
        and not(upd or addr_mode or vecsub) and exlevels == 1
        and (get_acc_conv_p(spec, type), fixed_exptr())
        then
            ;;; just use the original pointer
            run_access_p(); run_conv_p();
            return
        elseif prec_exptr and not(upd) then
            false -> f_subv(3,prec_exptr);      ;;; make it a no-op
            exlevels-1 -> exlevels;
            0 -> exoffs
        endif;
        fast_repeat exlevels times
            m_place(OP_!, false, "+", false, T_WORD, exoffs, 0, USP_+, -_USP, 9);
            0 -> exoffs
        endrepeat
    endif;

    if isref(type) then T_WORD else type endif -> tcode;
    if addr_mode then tcode || #_< t_ADDR_MODE || t_IS_STRUCT >_# -> tcode endif;

    if vecsub then
        {% OP_SUBV, false, tcode, offs, USP_+,
            if isinteger(vecsub) then popint(vecsub) else USP_+ endif, -_USP %}
    else
        {% OP_!, false, "+", false, tcode, offs, 0, USP_+, -_USP %}
    endif -> acc_inst;

    if tcode && t_BASE_TYPE /== t_WORD and not(doing_exacc_field_offset) then
        [% convert_pointer_code(false, T_WORD, tcode, mishap, PUSHQ, CALL),
           {pas_END}
        %] .trans_vmcode
    endif;

    unless addr_mode then
        while isref(type) do
            m_placei(acc_inst);
            cont(type) -> spec;
            spec(FIELD_TYPE_SPEC) -> type;
            if isref(type) then T_WORD else type endif -> tcode;
            {% OP_!, false, "+", false, tcode, spec(FIELD_OFFSET),
                        0, USP_+, -_USP %} -> acc_inst
        endwhile
    endunless;

    m_placei(acc_inst);     ;;; accesses the field/field address

    tcode && tv_VAL_TYPE -> vtype;
    get_acc_conv_p(spec, type);

    if upd then
        ;;; updating
        if tcode &&/=_0 t_IS_STRUCT and acc_p == [] then
            mishap(0, invalid_update)
        endif;
        [] -> checkcode;
        unless upd = "noconvp" then
            ;;; run updaters of conversion procedures backwards
            [% fast_for p in rev(conv_p) do call_p_instr(p, true, false)
            endfor %] -> checkcode
        endunless;

        if tcode &&=_0 t_ADDR_MODE then
            if vtype == tv_UNSIGNED or vtype == tv_SIGNED then
                ;;; generate check/conversion on integer field ranges
                t_bitoffset(tcode, false) -> nbits;
                if vtype == tv_UNSIGNED then
                    if nbits fi_> POPINT_BITS then
                        [Sys Pint_->_uint]
                    else
                        [Sys Simpint_->_uint]
                    endif
                else
                    nbits fi_- 1 -> nbits;
                    if nbits fi_> POPINT_BITS and tcode &&=_0 t_PINT then
                        [Sys Pint_->_sint]
                    else
                        [Sys Simpint_->_sint]
                    endif
                endif -> path;
                checkcode nc_<>
                    [ {% M_MOVE, 1<<nbits-1, -_USP %}   ;;; _rangemask
                      {% M_CALL, perm_constp_opnd(path, false) %}
                    ]       -> checkcode
            elseif vtype == tv_EXVAL or (vtype == tv_EXPTR and acc_p == [])
            then
                if vtype == tv_EXVAL then [Sys Fld Exval_val]
                else [Sys Checkr_exptrclass_ptr]
                endif -> p;
                checkcode nc_<> [{% M_CALL, perm_constp_opnd(p, false) %}]
                                    -> checkcode
            endif
        endif;

        if checkcode /== [] then
            ;;; add checking/conversion code before everything else
            if rec_id then
                [%  {%M_MOVE, USP_+, rec_id.getid_u%},
                    if vecsub == true then {%M_MOVE, USP_+, subs_id.getid_u%} endif
                %]
                nc_<> checkcode nc_<>
                [%  if vecsub == true then {%M_MOVE, subs_id.getid, -_USP%} endif,
                    {%M_MOVE, rec_id.getid, -_USP%}
                %]
            else
                [ {% OP_UFIELD, consref(checkcode),
                        if vecsub == true then
                            USP_+, USP_+
                        else
                            USP_+
                        endif %}
                ]
            endif nc_<> f_tl(code_start) -> f_tl(code_start)
        endif;
    endif;

    if tcode &&/=_0 t_IS_STRUCT then
        ;;; external 'compound' field or address mode -- get its address
        exptr_result(not(addr_mode))

    elseif tcode &&/=_0 t_ADDR_MODE then
        ;;; use access/update procedure on address of field
        lvars btype = tcode && t_BASE_TYPE;
        if vtype == tv_FLOAT then
            if tcode &&/=_0 t_FLOAT_C_SINGLE then
                [Sys Fld Float_val_s_C]
            elseif btype == t_DOUBLE then
                [Sys Fld Float_val_d]
            else
                [Sys Fld Float_val_s]
            endif
;;;     elseif vtype == tv_EXVAL then
;;;         [Sys Fld Exval_val]
        elseif btype == t_DOUBLE and vtype == tv_SIGNED then
            [Sys Fld Double_val_s]
        elseif btype == t_DOUBLE and vtype == tv_UNSIGNED then
            [Sys Fld Double_val_u]
        else
            mishap(0, 'pas_FIELD: INVALID ADDRESS-MODE FIELD')
        endif -> path;
        if upd then ucall_constp else call_constp endif(path);

    elseif vtype == tv_FLOAT then
        mishap(0, 'pas_FIELD: INVALID FLOATING-POINT FIELD')

    elseif upd and acc_p == [] then
        ;;; update field
        true -> acc_inst(2);
        USP_+ -> acc_inst(datalength(acc_inst))

    elseif vtype == tv_EXPTR or vtype == tv_EXVAL then
        exptr_result(vtype == tv_EXPTR)

    elseif vtype == tv_FULL then
        if extern then
            ;;; check values coming out of external full fields to
            ;;; be pucker pop objects
            call_constp([Sys Fld Full_val_extern])
        endif
    else
        ;;; integer field
        t_bitoffset(tcode, false) -> nbits;
        if tcode &&=_0 t_PINT
        and nbits fi_> #_< POPINT_BITS+1 >_#
        or (vtype == tv_UNSIGNED and nbits fi_> POPINT_BITS) then
            call_constp(if vtype == tv_UNSIGNED then
                            [Sys Uint_->_pint]
                        else
                            [Sys Sint_->_pint]
                        endif)
        else
            do_inline_proc([\^_pint])
        endif
    endif;

    unless upd or addr_mode then run_conv_p() endunless
enddefine;

    ;;; Called when interpreting pas_FIELD -- if array offset, subscript
    ;;; is on the stack
define $-Popas$-exacc_field_offset(vm_instr) -> ptr;
    lvars   tcode, offs, vecsub, ptr, acc_inst;
    dlocal  vm_instr, m_codelist, m_codelist_end,
            doing_exacc_field_offset = true;
    lconstant codepair = writeable [{pas_NOOP}];
    codepair ->> m_codelist -> m_codelist_end;
    pas_generate_p("pas_FIELD")();
    f_hd(f_tl(codepair)) -> acc_inst;
    if acc_inst(1) == OP_SUBV then
        explode(acc_inst) -> (, , tcode, offs, , vecsub, );
        unless vecsub == USP_+ then mcint(vecsub) endunless -> vecsub;
        offs + (vecsub-1)*t_offset(tcode, type_error)
    else
        acc_inst(6)
    endif -> offs;
    consexternal_ptr() -> ptr;
    offs -> exacc ^int ptr
enddefine;


;;; --- PSUEDO-INSTRUCTIONS FOR SPECIAL PROCEDURES -----------------------

define :instr pas_PUSH_PINDEX();
    m_move(pdr_index_opnd(f_subv(2, vm_instr)), -_USP)
enddefine;

define :instr pas_END();
enddefine;


;;; --- GENERATE PROCEDURES & CLOSURES ETC ----------------------------------

    ;;; Generate common part of procedure/closure header
define lconstant gencommonhead(p, pprops, upd, p_len, flags, nargs);
    lvars p, p_len, pprops, upd, flags, nargs;

    setseg(p, false);
    asm_outword(pprops,                 ;;; PD_PROPS
                perm_const_lab([procedure_key]),
                2);
    outlab(current_pdr_label);          ;;; label
    asm_outword(current_pdr_exec_label, ;;; PD_EXECUTE
                upd,                    ;;; PD_UPDATER
                2);
    asm_outint(p_len, 1);               ;;; PD_LENGTH
    asm_outbyte(flags,                  ;;; PD_FLAGS
                nargs,                  ;;; PD_NARGS
                2)
enddefine;



define lconstant gen_procedure( all_dlocals,
                                lex_locals,
                                nlgoto_var,
                                rtid_args,
                                lblock_instr,
                                pdr_nl_labels,
                                dloc_rtid_list,
                                dlexpr_list,
                                pdr_entry_type,
                                codelist,
                                flags,
                                struct_tab,
                                p);

    lvars   x, id, flags, p, lex_locals, all_dlocals, codelist, reg_spec,
            reg_locals, pd_frame_len, pdr_len, exit_lab, plab,
            pdr_nl_labels, pop_dlocals, nonpop_dlocals, nfsave_id,
            Ndlocals, Nregs, Npopregs, len, struct_tab,
            Nstkvars, Npopstkvars, rtid_args, lblock_instr, dlexpr_list,
            dloc_rtid_list, nlgoto_var, pdr_entry_type, pprops, upd,
            localise_pop_regs = false, localise_nonpop_regs = false,
            actid_dlexprs = [], flags2 = 0, procedure gencode;

    dlocal  operand_type = newproperty([], 8, 0, false),
            current_pdr_label, current_pdr_exec_label;

    lconstant no_reg_marker = '';

    ;;; procedure label and start-of-code label
    struct_label_prop(p) -> current_pdr_label;
    execlabof(current_pdr_label, true) -> current_pdr_exec_label;

    if dlexpr_list /== [] then
        ;;; weed out a separate list of dlocal active vars (that aren't just
        ;;; lextokens)
        [%  for x in dlexpr_list do
#_IF pop_internal_version >= 145300
                ;;; new system puts the active id as the 4th element
                nextif(datalength(x) /== DLX_ACTID
                        or isident(x(DLX_ACTID)) == "lextoken");
#_ELSE
                ;;; temporary hack that recognises perm active vars (which is
                ;;; good enough for system code, and avoids a 'rebooting'
                ;;; process with corepop11 on all systems)
                define lconstant get_callid(dlx, el, op);
                    lvars dlx, el, op, clist = dlx(el), instr;
                    listlength(clist) == 1
                    and subscrv(1,hd(clist) ->> instr) == op
                    and subscrv(2,instr)
                enddefine;
                get_callid(x, DLX_GET, "pas_CALL") -> id;
                nextunless(id and isactive(id)
                            and isident(id) /== "lextoken"
                            and get_callid(x, DLX_PUT, "pas_UCALL") == id);
                consvector(explode(x), id, DLX_ACTID) -> x;
#_ENDIF
                ;;; ensure their save lvars go in stack frame slots not regs
                for id in x(DLX_VARS) do
                    no_reg_marker -> fast_idval(id)
                endfor;
                x
            endfor
        %] -> actid_dlexprs;

        ;;; 2 extra nonpop and 1 extra pop lvars required if procedure
        ;;; has dlocal expressions (dlexpr index, dlexpr save, dlexpr proc)
        [% new_lex_id(true), new_lex_id(true), new_lex_id(false) %] -> id;
        ;;; save them on the front of the dlexpr list
        id <> dlexpr_list -> dlexpr_list;
        ;;; splice them in at the end of the outer-level lvars
        lvars l = lex_locals, pair;
        if l /== [] and isident(f_front(l)) then
            while (f_back(l) ->> pair) /== [] and isident(f_front(pair))
            do
                pair -> l
            endwhile;
            id nc_<> pair -> f_back(l)
        else
            id nc_<> l -> lex_locals
        endif
    endif;

    0 -> Nstkvars;

    if pdr_entry_type == 2 then
        ;;; Prolog procedure -- Allow for the 3 special prolog nonpop lvars
        ;;; (SF_PLGSV_NEXT_VAR, SF_PLGSV_CONTN_TOP, SF_PLGSV_TRAIL_SP)
#_IF DEF SPARC
        ;;; Allocate dummy ids to use up the first 3 nonpop registers
        [% repeat 3 times new_lex_id(true) endrepeat %]
                                        nc_<> lex_locals -> lex_locals;
#_ELSE
        ;;; Don't want them in registers, just skip over 3 stack frame
        ;;; slots
        3 -> Nstkvars;
#_ENDIF
        ;;; Add the one dynamic prolog save variable
        identof_path([\^_plog_save_contn_sp]) :: all_dlocals -> all_dlocals;
        ;;; set prolog procedure in flags
        M_PD_PLOG_CHOICE fi_|| flags -> flags
    endif;

    ;;; Split dlocal identifiers into pop/nonpop
    [] ->> pop_dlocals -> nonpop_dlocals;
    test_identof_path([Sys \^_nextfree_save]) -> nfsave_id;
    fast_for id in all_dlocals do
        if id == localise_pop_reg_id then
            ;;; dummy to force all pop registers to be local
            true -> localise_pop_regs;
            nextloop
        elseif id == localise_nonpop_reg_id then
            ;;; dummy to force all nonpop registers to be local
            true -> localise_nonpop_regs;
            nextloop
        endif;
        ;;; get identifier label
        if note_perm_ident(id, IDT_GEN_IDENT) ->> x then
            if id == nfsave_id then
                M_PD_SAVES_NEXTFREE fi_|| flags -> flags
            endif;
            ;;; so we don't set IDT_GEN_FULL_ID
            identlabel(x)
        else
            gen_lex_ident(id, true)
        endif,
        if nonpop_ident(id) then
            () :: nonpop_dlocals -> nonpop_dlocals
        else
            () :: pop_dlocals -> pop_dlocals
        endif
    endfor;

#_IF DEF SPARC
    ;;; nonpop ones must come first to get the order right in the stack frame
    nonpop_dlocals <> pop_dlocals -> all_dlocals;   ;;; nonpop ones come first
#_ELSE
    ;;; pop ones must come first to get the order right in the stack frame
    pop_dlocals <> nonpop_dlocals -> all_dlocals;   ;;; pop ones come first
#_ENDIF
    listlength(all_dlocals) -> Ndlocals;



    ;;; Allocate local lvars to registers or stack frame offsets,
    ;;; and set up translation in their idvals.

    dlvars ##_SF_LOCALS = field_##("SF_LOCALS");

    define lconstant alloc_lvars(idlist, stk_index, reglist, doing_pop)
                                                                -> max_index;
        lvars   id, r, list, reglist, stk_index, max_index = stk_index,
                doing_pop, idlist, type_code, unit_bitsize, indx, wlen;
        fast_for id in idlist do
            if isident(id) then
                nextif(nonpop_ident(id) == doing_pop);
                if not(doing_pop) and (lstackmem_data(id) ->> type_code) then
                    destpair(type_code) -> (type_code, unit_bitsize);
                    ((t_bitoffset(type_code,false)+WORD_BITS-1) div WORD_BITS)
                                            -> wlen;    ;;; no words occupied
                    stk_index -> indx;
#_IF DOUBLE_ALIGN_BITS > WORD_ALIGN_BITS
                    if type_code && t_BASE_TYPE == t_DOUBLE then
                        ;;; adjust index for position of SF_LOCALS_D
                        ;;; if necessary
                        if testbit(##_SF_LOCALS, 0) then indx-1 -> indx endif;
                        ;;; then double align
                        if testbit(indx, 0) then
                            indx+1 -> indx;
                            stk_index+1 -> stk_index
                        endif
                    endif;
#_ENDIF
#_IF DEF STACK_GROWS_UP
                    indx+wlen -> indx;
#_ENDIF
                    (indx*WORD_BITS) / unit_bitsize;
                    stk_index + wlen ->> stk_index -> max_index
                elseif reglist == [] or fast_idval(id) == no_reg_marker then
                    ;;; allocate to a stack frame cell
                    sp_index_opnd(##_SF_LOCALS fi_+ stk_index);
                    stk_index fi_+ 1 ->> stk_index -> max_index
                else
                    ;;; allocate to a register
                    f_dest(reglist) -> (r, reglist);
                    unless fast_lmember(r, reg_locals) then
                        r :: reg_locals -> reg_locals
                    endunless;
                    reglabel(r)
                endif -> fast_idval(id)
            else
                ;;; nested lblock list
                max(alloc_lvars(id, stk_index, reglist, doing_pop), max_index)
                                                    -> max_index
            endif
        endfor
    enddefine;

    define lconstant init_local_regs(reglist, all_local) -> l;
        lvars r, reglist, all_local, l = [];
#_IF not(DEF SPARC)                         ;;; always [] for SPARC
        if all_local then tl(reglist) -> l endif;
        for r in hd(reglist) do
            ;;; always local
            unless fast_lmember(r, l) then r :: l -> l endunless
        endfor
#_ENDIF
    enddefine;

    ;;; Nonpop on-stack vars come first in the stack frame, so we
    ;;; do these first.
    init_local_regs(nonpop_registers, localise_nonpop_regs) -> reg_locals;
    alloc_lvars(lex_locals, Nstkvars, tl(nonpop_registers), false) -> Nstkvars;
    listlength(reg_locals) -> Nregs;        ;;; save number of nonpop regs

    ;;; then the pop ones
    init_local_regs(pop_registers, localise_pop_regs) <> reg_locals
                                                -> reg_locals;
    dup(alloc_lvars(lex_locals, Nstkvars, tl(pop_registers), true))
                                fi_- Nstkvars -> Npopstkvars -> Nstkvars;
    dup(listlength(reg_locals)) fi_- Nregs -> Npopregs -> Nregs;

    ;;; values for procedure header
    lvars pd_num_pstk_vars, pd_gc_offset_len, pd_gc_scan_len;
#_IF DEF SPARC
    ;;; PD_NUM_PSTK_VARS contains the number of pop registers used
    Npopregs -> pd_num_pstk_vars;
    ##_SF_LOCALS fi_+ Nstkvars -> len;  ;;; len to start of dlocals
    ;;; PD_GC_OFFSET_LEN is the length to the first pop onstack lvar
    len fi_- Npopstkvars -> pd_gc_offset_len;
    ;;; PD_GC_SCAN_LEN is the number of pop onstack lvars + pop dlocals
    Npopstkvars fi_+ listlength(pop_dlocals) -> pd_gc_scan_len;
    len fi_+ Ndlocals -> pd_frame_len;
#_ELSE
    ;;; PD_NUM_PSTK_VARS contains the number of pop onstack lvars
    Npopstkvars -> pd_num_pstk_vars;
    ##_SF_LOCALS fi_+ Nstkvars -> len;  ;;; len to start of dlocals
    ;;; PD_GC_OFFSET_LEN is the length to the first pop dlocal
    len fi_+ listlength(nonpop_dlocals) -> pd_gc_offset_len;
    ;;; PD_GC_SCAN_LEN is the number of pop dlocals + pop registers
    listlength(pop_dlocals) fi_+ Npopregs -> pd_gc_scan_len;
    len fi_+ Ndlocals fi_+ Nregs fi_- field_##("SF_RETURN_ADDR")
                                                    -> pd_frame_len;
#_ENDIF

#_IF DEF STACK_ALIGN_BITS
    ;;; stack frame must be aligned on a multi-word boundary
    lconstant STACK_ALIGN_WORDS = STACK_ALIGN_BITS div WORD_BITS;

    define lconstant correct_popstk(idlist, offs);
        lvars id, idlist, offs;
        fast_for id in idlist do
            if isident(id) then
                nextif(nonpop_ident(id) or isword(fast_idval(id)->>id));
                f_subv(2, id) fi_+ offs -> f_subv(2, id);
            else
                correct_popstk(id, offs);
            endif;
        endfor;
    enddefine;

    lvars pad = pd_frame_len fi_rem STACK_ALIGN_WORDS;
    unless pad == 0 then
        STACK_ALIGN_WORDS fi_- pad -> pad;
        ;;; pad out the stack frame by adding that number of non-pop
        ;;; on-stack lvars
        Nstkvars fi_+ pad -> Nstkvars;
        ;;; adjust values for procedure header
        pd_frame_len fi_+ pad -> pd_frame_len;
        pd_gc_offset_len fi_+ pad -> pd_gc_offset_len;
        ;;; adjust offsets for pop on-stack lvars
        unless Npopstkvars == 0 then
            correct_popstk(lex_locals, sp_offset(pad, false));
        endunless;
    endunless;
#_ENDIF

    if pd_frame_len > MAX_STACK_FRAME_SIZE then
        mishap(p, 1, 'PROCEDURE STACK FRAME TOO LARGE (too many lvars)')
    endif;


    if actid_dlexprs /== [] then
        ;;; set up dlocal active var data following the ordinary dlocals in
        ;;; the structure table (used by Dlocal_frame_offset)

        define lconstant Stack_dlocal_actid_data(actid_dlexprs);
            lvars dlx, actid_dlexprs, v, i, index_data, index, stksub;
            repeat
                0;                  ;;; dummy where index_data will go
                1 -> stksub;
                3 -> i;             ;;; skip count bits
                0 -> index_data;
                repeat
                    fast_destpair(actid_dlexprs) -> (dlx, actid_dlexprs);
                    dlx(DLX_VARS) -> v;
                    if v == [] then
                        1 -> index  ;;; dummy for 0 multiplicity
                    else
                        f_hd(v) -> v;       ;;; sp offset operand in idval
                        ;;; word index into the stack frame of the first
                        ;;; save lvar
                        subscrv(2,fast_idval(v)) / WORD_OFFS -> index;
#_IF DEF STACK_GROWS_UP
                        -(index+1) -> index;
#_ENDIF
                    endif;
                    (index << i) || index_data + 1 -> index_data;
                    i + BYTE_BITS -> i;
                    dlx(DLX_ACTID);     ;;; stack the active identifier
                    stksub+1 -> stksub;
                    quitif(actid_dlexprs == []) (2);
                    quitif(i > #_<POPINT_BITS-BYTE_BITS>_#)
                endrepeat;
                index_data -> subscr_stack(stksub)  ;;; replace the dummy
            endrepeat;
            ;;; replace the dummy, but this time with -1 in the top bits to
            ;;; indicate this is the last index word
            (-1<<i) || index_data -> subscr_stack(stksub)
        enddefine;

        [% Stack_dlocal_actid_data(actid_dlexprs) %] -> actid_dlexprs;
        flags2 || M_PD2_HAS_DLOCAL_ACTIVE -> flags2
    endif;


    ;;; First do optimisations that can be done within the scope
    ;;; of POP virtual machine instructions. This also allocates symbolic
    ;;; labels to non-redundant label pairs.
    pas_optimise(codelist, pdr_nl_labels) -> codelist;

    ;;; generate pdprops and updater fields
    genstructure(pdprops(p)) -> pprops;
    genstructure(updater(p)) -> upd;

    ;;; complete literal table starting at PD_TABLE
    all_dlocals <> maplist(actid_dlexprs nc_<> struct_tab, genstructure)
                                -> struct_tab;

    0 -> reg_spec;      ;;; default reg_spec (filled in by M_CREATE_SF)
    lvars sf_instr = {%M_CREATE_SF, reg_locals, Npopregs, Nstkvars,
                        Npopstkvars, all_dlocals, ident reg_spec%};

    ;;; Translate to M-code and optimise.
    m_translate(sf_instr, Ndlocals, pd_frame_len, nlgoto_var,
                rtid_args, lblock_instr, dloc_rtid_list, dlexpr_list,
                flags, pdr_entry_type, codelist)
            -> (codelist, exit_lab, flags);


    ;;; Call machine-dependent code generator -- returns a procedure
    ;;; to output the code and the expression to be put in the PD_LENGTH
    ;;; field, given the header size in words as arg.
    mc_code_generator(codelist, field_##("PD_TABLE") - field_##("POPBASE")
                        + listlength(struct_tab)) -> (gencode, pdr_len);

    ;;; output procedure header
    gencommonhead(p, pprops, upd, pdr_len, flags, pdnargs(p));

    asm_outbyte(flags2,                     ;;; PD_FLAGS2
                0,                          ;;; PD_SPARE (currently unused)
                2);

    asm_outshort(reg_spec, 1);              ;;; PD_REGMASK
#_IF DEF FRAME_LEN_16BIT
        asm_outshort(0, 1);
        asm_outshort
#_ELSE
#_IF DEF FRAME_LEN_32BIT
        asm_outshort(0, 1);
        asm_outint(0, 1);
        asm_outint
#_ELSE
        asm_outbyte
#_ENDIF
#_ENDIF
    (
            ;;; stack frame length in words
            pd_frame_len,                   ;;; PD_FRAME_LEN
            ;;; see above
            pd_gc_offset_len,               ;;; PD_GC_OFFSET_LEN
            ;;; see above
            pd_gc_scan_len,                 ;;; PD_GC_SCAN_LEN
            ;;; no of stack lvars
            Nstkvars,                       ;;; PD_NUM_STK_VARS
            ;;; see above
            pd_num_pstk_vars,               ;;; PD_NUM_PSTK_VARS
            ;;; no of dynamic locals
            Ndlocals,                       ;;; PD_NLOCALS
            6);

    asm_outword(exit_lab, 1);               ;;; address of exit code

    ;;; generate literal table starting at PD_TABLE
    fast_for x in struct_tab do asm_outword(x, 1) endfor;

    ;;; generate code body
    gencode()
enddefine;      /* gen_procedure */


define $-Popas$-generate_gen_procedure(p);
    lvars p;
    chain(explode(gen_procedure_args(p)), p, gen_procedure)
enddefine;

define $-Popas$-generate_closure(clos);
    lvars   clos, pdpart_lab, frozvals, flags, clos_pdpart, u,
            pdr_len, pdpart_opnd, nfroz, nargs, pprops, upd,
            procedure gencode;
    dlocal  current_pdr_label, current_pdr_exec_label;

    init_immediates();

    if isproperty(clos) then
        ;;; ensure properties can be copied
        true ->> copyable_closure(clos) -> copyable_closure(updater(clos));
        ;;; can't use datalist to get property frozvals!
        M_PD_CLOS_PROPERTY, frozval(1,clos) :: []
    elseif isundef(clos) then
        M_PD_CLOS_UNDEF, datalist(clos)     ;;; special for Popc
    else
        0, datalist(clos)
    endif -> (flags, frozvals);
    if isclosure(clos) == 1 then
        ;;; protected
        flags || M_PD_CLOS_PROTECT -> flags
    endif;

    ;;; closure label and start-of-code label
    struct_label_prop(clos) -> current_pdr_label;
    execlabof(current_pdr_label, true) -> current_pdr_exec_label;

    ;;; generate pdpart
    pdpart(clos) -> clos_pdpart;
    genstructure(clos_pdpart) -> pdpart_lab;

    if isundef(updater(clos_pdpart) ->> u) and not(struct_label_prop(u))
    and updater(partapply(clos_pdpart, frozvals)) = updater(clos) then
        false -> updater(clos)
    endif;

    ;;; generate frozvals
    maplist(frozvals,   procedure() -> i;
                            lvars f = genstructure(), i;
                            unless _intval(f) ->> i then f -> i endunless
                        endprocedure) -> frozvals;

    ;;; generate pdprops and updater fields
    genstructure(pdprops(clos)) -> pprops;
    genstructure(updater(clos)) -> upd;

    ;;; Call machine-dependent code generator -- returns a procedure
    ;;; to output the code
    if is_writeable(clos) or copyable_closure(clos) then
        false
    else
        consref(pdpart_lab)
    endif -> pdpart_opnd;

    listlength(frozvals) -> nfroz;

    ;;; Call machine-dependent code generator on codelist -- returns a
    ;;; procedure to output the code and the expression to be put in the
    ;;; PD_LENGTH field, given the header size in words as arg.
    mc_code_generator([{%M_CLOSURE, frozvals, pdpart_opnd%} {^M_END}],
                        field_##("PD_CLOS_FROZVALS") - field_##("POPBASE")
                            + nfroz) -> pdr_len -> gencode;

    if (pdnargs(clos) ->> nargs) == pdnargs(clos_pdpart)-nfroz then
        ;;; PD_NARGS field of 16:FF means compute pdnargs as
        ;;; pdnargs(pdpart) - nfroz
        16:FF -> nargs
    endif;

    ;;; now output code for closure - header first.
    gencommonhead(clos, pprops, upd, pdr_len, flags fi_|| M_PD_CLOSURE, nargs);

    asm_outshort(nfroz, 1);             ;;; PD_CLOS_NFROZ
    asm_outword(pdpart_lab, 1);         ;;; PD_CLOS_PDPART

    ;;; generate frozval table
    applist(frozvals, #_< asm_outword(%1%) >_#);    ;;; frozvals

    ;;; finally, generate code body
    gencode()
enddefine;

endsection;     /* M_trans */

endsection;     /* $-Popas */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 16 1996
        Changed pas_GO_ON(_INT) to accept 4-element instruction specifying
        base integer corresponding to 1st label, and to add this to
        M_BRANCH_ON instructions if BRANCH_ON_ALLOWS_BASE is defined.
--- John Gibson, Oct 11 1996
        Changed m_translate to add a dummy label before the start of a
        procedure's main code (so code scheduling optimisations can't
        mix it up with the initial code).
--- John Gibson, Sep 18 1995
        Removed code in gen_procedure which assigned offset expressions
        to procedure_label labels (genstruct.p now plants the actual
        label in the PLAB_OFFSET field)
--- John Gibson, Aug 11 1995
        Replaced some calls of f_hd with f_front (label in an instruction
        may be a pair, not a list).
--- John Gibson, May 27 1995
        # Changed gen_procedure to generate dlocal active var data in the
          procedure header
        # Replaced short PD_SPARE field with 2 byte fields PD_FLAGS2, PD_SPARE
--- John Gibson, Apr  8 1995
        Changed procedure header layout
--- John Gibson, Feb  9 1995
        Changed gen_procedure to call label_- on code offset labs after
        procedure is generated, i.e. after planting the labels
--- John Gibson, Oct 28 1994
        Made gen_procedure and generate_closure generate the pdprops and
        updater fields before calling mc_code_generator (i.e. so that all
        structures are generated before calling the latter).
--- John Gibson, Oct 20 1994
        Made gen_procedure recognise dlocal'ed dummy variables
        \<ALL_POP_REGISTERS\> and _\<ALL_NONPOP_REGISTERS\> as meaning
        make all the corresponding registers local (ignored for SPARC)
--- John Gibson, Oct  6 1994
        Replaced call of can_defer_opnd with CAN_DEFER_OPND macro.
--- John Gibson, Aug 31 1994
        Removed pas_AR*RAY_SUB (no longer needed)
--- John Gibson, Jun  1 1994
        Changed alloc_lvars to deal with lstackmem offset lvars
--- Robert John Duncan, Mar 22 1994
        Changed gen_procedure to align stack frames on a boundary specified
        by STACK_ALIGN_BITS (if defined).
--- John Gibson, May  3 1993
        Made pas_FIELD work for external function call ....
--- John Gibson, Dec 15 1992
        Made generate_closure ensure that property closures are always
        copyable.
--- John Gibson, Oct  9 1992
        Added exacc_field_offset
--- John Gibson, Sep 23 1992
        Changed PD_CLOS_NFROZ to be short instead of byte
--- John Gibson, Sep  1 1992
        Altered external call to use fltsingle arg
--- John Gibson, Jul 21 1992
        Version 14.21 changes
--- John Gibson, Jan  2 1992
        Changed -cmpkey_instr- to allow for integer specifying flags in
        K_FLAGS, and changed optimisation for -isstring- to be test for
        M_K_STRING.
--- John Gibson, Nov 10 1990
        Added inline routine _dlocal_abexit_return
--- John Gibson, Jan 11 1990
        Version 13.7 for new pointers.
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, May  5 1989
        Version 13.6402 changes
--- John Gibson, Apr 26 1989
        Version 13.64 changes
--- John Gibson, Feb 13 1989
        Impreoved interface to genproc.p
--- John Gibson, Jan 29 1989
        New version of popc
--- John Gibson, Nov 23 1988
        Changes to cope with lexical blocks in the VM.
--- John Gibson, Jul 15 1988
        Changes for SPARC, etc.
--- John Gibson, Feb 21 1988
        Use of all subroutines now recorded on .w output
--- John Gibson, Jan 17 1988
        Changes for coping with sections, weakrefs, new format for assembler
        files, etc, etc.
--- John Gibson, Nov 17 1987
        Crrected use of -M_CMP- to compare -switch_var- in procedure
        -switch_code- with -M_PCMP-.
--- John Gibson, Oct 31 1987
        Corrected bug in -do_inline_conditions- which caused incorrect code to
        generated for calls of -not- inside dlocal expressions
 */
