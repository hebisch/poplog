/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/syscomp/m_optimise.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                        M-CODE OPTIMISATION

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas$-M_trans;

constant
        procedure (OP_!, OP_@, work_addr_reg, caller_return_instr,
        inline_conditions)
    ;

vars
        procedure (operand_type)
    ;

define cons_access_opnd(base, offs, type);
    lvars base, offs, type;
    if isstring(base) then
        if offs /== 0 then
            base label_+ offs -> base;
            0 -> offs
        endif;
        returnif(type == T_WORD) (base)
    endif;
#_IF DEF USE_NEW_M_OPERANDS
    consvector(base, offs, if type == T_WORD then 2 else type, 3 endif)
#_ELSE
    consvector(base, offs, 2)
#_ENDIF
enddefine;

#_IF DEF SPECIAL_VAR_BLOCK_REG and DEF GEN_SVB_CONSTANTS
define perm_const_of_svb_opnd = newassoc([]) enddefine;
#_ENDIF

define lconstant get_const_opnd(path, upd, needp) -> opnd;
    lvars path, upd, needp, lab, opnd, offs, ref_opnd;
#_IF DEF SPECIAL_VAR_BLOCK_REG and DEF GEN_SVB_CONSTANTS
    define lconstant cache = newproperty([], 8, false, "perm") enddefine;
    dlocal perm_const_svb_check = not(upd);
#_ENDIF
    perm_const_lab(path, needp) -> lab;
    if upd then
        updlabof(lab, true) -> lab
    else
        returnif(_intval(lab) ->> opnd)
    endif;
#_IF DEF SPECIAL_VAR_BLOCK_REG and DEF GEN_SVB_CONSTANTS
    consref(lab) ->> opnd -> ref_opnd;
    if perm_const_svb_check ->> offs then
        ;;; in _special_var_block -- value is offset from start
        unless cache(offs) ->> opnd then
            cons_access_opnd(SPECIAL_VAR_BLOCK_REG, offs, T_WORD) ->> opnd
                                                    -> cache(offs);
            ref_opnd -> perm_const_of_svb_opnd(opnd)
        endunless
    endif;
#_ELSE
    consref(lab) -> opnd;
#_ENDIF
    if execlabof(lab, false) and isproperty(operand_type) then
        "procedure" -> operand_type(opnd)
    endif
enddefine;

define perm_const_opnd = get_const_opnd(% false, false %) enddefine;
define perm_constp_opnd = get_const_opnd(% true %) enddefine;


lconstant procedure (
    arithop1    =
        newassoc([[^M_LOGCOM ^(nonop ~~)] [^M_NEG ^negate]]),

    arithop2    =
        newassoc([  [^M_ADD  [c ^(nonop +)    0]]
                    [^M_BIS  [c ^(nonop ||)   0]]
                    [^M_PADD [c ^false        0]]
                    [^M_MULT [c ^(nonop *)    1]]
                    [^M_SUB  [n ^(nonop -)    0]]
                    [^M_BIC  [n ^(nonop &&~~) 0]]
                    [^M_PSUB [n ^false        0]]
                    [^M_ASH  [n ^(nonop <<)   0]]
                    [^M_BIM  [c ^(nonop &&)   -1]]
                ]),
    );

lvars
    m_codelist, org_m_codelist,
    pdr_exit_lab,
    pdr_Nlocals,
    pdr_flags,
    ;


define lconstant get_access_type(type_code) -> acctype;
    lvars type_code, acctype;
    lconstant
        valid_types = [^T_BYTE ^T_SHORT ^T_INT ^T_INT_DOUBLE ^T_WORD],
        clearmask = tv_VAL_TYPE || (t_OTHER_FLAGS&&~~t_WORDRANGE_DOUBLE);

    type_code &&~~ clearmask -> acctype;

    if fast_lmember(acctype, valid_types) then
        if acctype && t_BASE_TYPE < t_WORD then
            acctype || (type_code && tv_SIGNED) -> acctype
        endif
    else
        type_error(0, 'non-accessible type')
    endif
enddefine;

define lconstant get_bit_opcode(type_code, upd);
    lvars type_code, upd;
    if t_offset(type_code, type_error) <= INT_BITS then
        if upd then
            M_UPDbit
        elseif type_code && tv_VAL_TYPE == tv_SIGNED then
            M_MOVEsbit
        else
            M_MOVEbit
        endif
    else
        type_error(0, 'non-accessible type')
    endif
enddefine;

define lconstant removeinstr();
    f_dest(f_tl(m_codelist)) -> f_tl(m_codelist) -> f_hd(m_codelist)
enddefine;

define lconstant removenext();
    f_tl(f_tl(m_codelist)) -> f_tl(m_codelist)
enddefine;

define lconstant addinstr(inst);
    lvars inst;
    f_hd(m_codelist);
    inst -> f_hd(m_codelist);
    :: f_tl(m_codelist) ->> f_tl(m_codelist) -> m_codelist
enddefine;

define lconstant move_instr(src, dst);
    lvars src, dst;
    consvector(M_MOVE, src, dst, 3)
enddefine;

define lconstant addmove = move_instr <> addinstr enddefine;

define immediate_operand(opnd);
    lvars opnd;
    if isintegral(opnd) then
        opnd
    elseif isref(opnd) then
        fast_cont(opnd)
    else
        false
    endif
enddefine;

define reg_in_operand(opnd);
    lvars opnd;
    if regnumber(opnd) then
        opnd
    else
        if ispair(opnd) then fast_front(opnd) -> opnd endif;
        if isvector(opnd) and not(isstring(f_subv(1,opnd) ->> opnd)) then
            opnd
        else
            false
        endif
    endif
enddefine;

define lconstant findsr(inst);
    lvars n, inst;
    fast_for n from 2 to datalength(inst) do
        returnif(reg_in_operand(f_subv(n,inst)) == USP) (n)
    endfor;
    false
enddefine;

define auto_operand(opnd);
    lvars opnd;
    if ispair(opnd) then fast_front(opnd) -> opnd endif;
    isvector(opnd) and isboolean(f_subv(2,opnd))
enddefine;

define lconstant auto_check(fsub, tsub, inst, opnd);
    lvars n, fsub, tsub, reg, iopnd, auto, inst, opnd;
    returnunless((reg_in_operand(opnd) ->> reg) and autoidreg(reg)) (true);
    auto_operand(opnd) -> auto;
    fast_for n from fsub to tsub do
        f_subv(n, inst) -> iopnd;
        unless reg_in_operand(iopnd) == reg then
            nextloop
        elseif auto or auto_operand(iopnd) then
            return(false)
        endunless
    endfor;
    true
enddefine;


#_IF DEF USE_NEW_M_OPERANDS
define :inline CAN_DEFER_OPND(opnd, offs, acctype, upd);
    can_defer_opnd(opnd, offs, acctype, upd)
enddefine;
#_ELSE
define :inline CAN_DEFER_OPND(opnd, offs, acctype, upd);
    can_defer_opnd(opnd, offs, upd)
enddefine;
#_ENDIF


define lconstant typed_move_instr(x, y, upd, acctype);
    lvars x, y, upd, acctype, op;
#_IF DEF USE_NEW_M_OPERANDS
    M_MOVE -> op;       ;;; operand is already typed
#_ELSE
    lconstant type_m_opcode = newassoc([
                    [^T_BYTE        {^M_MOVEb     ^M_UPDb} ]
                    [^T_SGN_BYTE    {^M_MOVEsb    ^M_UPDb} ]
                    [^T_SHORT       {^M_MOVEs     ^M_UPDs} ]
                    [^T_SGN_SHORT   {^M_MOVEss    ^M_UPDs} ]
                    [^T_INT         {^M_MOVEi     ^M_UPDi} ]
                    [^T_SGN_INT     {^M_MOVEsi    ^M_UPDi} ]
                    [^T_WORD        {^M_MOVE      ^M_MOVE} ]
                    [^T_INT_DOUBLE  {^M_MOVE      ^M_MOVE} ]
                    ]);
;;;        printf(acctype, 'typed_move_instr: acctype = %p\n');
    type_m_opcode(acctype) -> op;
    if upd then f_subv(2,op) else f_subv(1,op) endif -> op;
#_ENDIF
    consvector(op, if upd then y, x else x, y endif, 3)
enddefine;


define lconstant try_imm_+-(x, y, pdr);
    lvars x, y, ix, iy, procedure pdr;
    if y == 0 then
        x
    elseif x == 0 and pdr == nonop + then
        y
    elseif (immediate_operand(x) ->> ix) and (immediate_operand(y) ->> iy) then
        if isintegral(x) and isintegral(y) then
            pdr(x, y)
        else
            consref(if pdr == nonop + then ix label_+ iy
                    else ix label_- iy
                    endif)
        endif
    else
        false
    endif
enddefine;


lconstant procedure (postmove, do_premove_next);

define lconstant get_moves(m_codelist, mustconvert) -> (got_one, m_codelist);
    dlocal  m_codelist;
    lvars   x, y, opcode, z, l, inst, nextop, mustconvert, op_pdr,
            nextinst, got_one = false;

    f_hd(m_codelist) -> inst;
    f_subv(1, inst) -> opcode;
;;;        printf(opcode, inst, 'get_moves if: \n inst = %p\n opcode = %p\n');
    if isstring(pdprops(opcode) ->> x) and f_subs(1, x) == `%` then
        ;;; pseudo-op
        opcode(m_codelist, mustconvert) -> m_codelist;
        opcode -> x;
        f_hd(m_codelist) -> inst;
        f_subv(1, inst) -> opcode;
        returnif(opcode == x);      ;;; didn't convert
        true -> got_one
    endif;
;;;        printf('get_moves endif\n');

    if (arithop1(opcode) ->> l) and isintegral(inst(2)) then
        l(inst(2)) -> y, inst(3) -> z
    elseif (arithop2(opcode) ->> l) then
        explode(inst) -> (, x, y, z);
        if l(1) == "c" and (x = z or isintegral(y)) then
            x -> f_subv(3, inst), y -> f_subv(2, inst);
            x, y -> x -> y
        endif;
        f_hd(f_tl(m_codelist)) -> nextinst;     ;;; next instruction
        f_subv(1, nextinst) -> nextop;
        l(2) -> op_pdr;
        if opcode == M_ADD or opcode == M_SUB then
            if try_imm_+-(y, x, op_pdr) ->> l then
                l -> y
            elseunless isinteger(x) then
                return
            else
                ;;; see if can optimise index/displace for field access
                ;;; where index/displace has constant added/subtracted,
                ;;; e.g. x!FOO[_s _add _1]
                if (nextop == OP_! or nextop == OP_@)
                and z == -_USP and f_subv(7, nextinst) == USP_+ then
                    ;;; must be adding constant to the index/displace
                    ;;; add to base dis
                    if f_subv(3, nextinst) == "-" then -x -> x endif;
                    if f_subv(4, nextinst) then     ;;; test index scale
                        x * t_offset(f_subv(5, nextinst), type_error) -> x
                    endif;
                    try_imm_+-(f_subv(6, nextinst), x, op_pdr)
                                                        -> f_subv(6, nextinst)
                    ;;; then change this instr to move other operand
                else
                    if x fi_< 0 then
                        -x -> f_subv(2, inst);
                        if opcode == M_ADD then M_SUB else M_ADD endif
                                        -> f_subv(1, inst)
                    endif;
                    return
                endif
            endif
        elseif op_pdr and isintegral(x) and isintegral(y) then
            apply(y, x, op_pdr) -> y
        elseif l(3) == x then
            ;;; just move y
        elseif opcode == M_ASH then
            returnunless(isinteger(x) and z == -_USP);
            if nextop == OP_@ and f_subv(7,nextinst) == USP_+ then
                get_moves(f_tl(m_codelist), true) -> (, f_tl(m_codelist));
                f_hd(f_tl(m_codelist)) -> nextinst;
                f_subv(1,nextinst) -> nextop
            endif;
            returnunless(nextop == M_ASH and f_subv(3,nextinst) == USP_+
                            and isinteger(f_subv(2,nextinst)->>l) );
            if x fi_>= 0 or l fi_<= 0 then
                if (x fi_+ l ->> x) == 0 then
                    f_subv(4,nextinst) -> z;
                    removenext()
                else
                    x -> f_subv(2, nextinst)
                endif
                ;;; drop thru for move
            elseif (-x ->> x) fi_< l then
                M_BIC -> f_subv(1,inst);
                (1 << x) fi_- 1 -> f_subv(2,inst);
                l fi_- x -> f_subv(2,nextinst);
                return
            else
                l fi_- x -> f_subv(2,inst);
                M_BIC -> f_subv(1,nextinst);
                (1 << l) fi_- 1 -> f_subv(2,nextinst);
                returnif(x /== l)       ;;; else do move
            endif
        elseif opcode == M_BIC then
            if (nextop == M_BIS or nextop == M_ADD or nextop == M_ASH)
            and isinteger(x) and z == -_USP
            and isinteger(f_subv(2,nextinst)->>l)
            and f_subv(3, nextinst) == USP_+
            then
                if nextop == M_ASH then
                    returnunless(l fi_< 0 and x fi_<< l == 0)
                    ;;; else just move y
                elseif (x==l or (nextop==M_BIS and x fi_&&~~ l == 0)) then
                    M_BIS -> f_subv(1, nextinst)    ;;; then move y
                else
                    return
                endif
            else
                return
            endif
        else
            return
        endif
    elseif opcode == M_PTR_ADD_OFFS or opcode == M_PTR_SUB_OFFS then
        explode(inst) -> (, /* type */, x, y, z);
        if opcode == M_PTR_ADD_OFFS then nonop + else nonop - endif -> op_pdr;
        if try_imm_+-(y, x, op_pdr) ->> l then
            l -> y
        else
            f_hd(f_tl(m_codelist)) -> nextinst;     ;;; next instruction
            f_subv(1, nextinst) -> nextop;
            if (nextop == OP_! or nextop == OP_@)
            and z == -_USP and f_subv(8, nextinst) == USP_+
            and immediate_operand(x) then
                ;;; add to base offset
                try_imm_+-(f_subv(6, nextinst), x, op_pdr)
                                                        -> f_subv(6, nextinst)
                ;;; then change this instr to move other operand
            else
                if isinteger(x) and x fi_< 0 then
                    -x -> f_subv(3, inst);
                    if opcode == M_PTR_ADD_OFFS then
                        M_PTR_SUB_OFFS
                    else
                        M_PTR_ADD_OFFS
                    endif -> f_subv(1, inst);
                endif;
                return
            endif
        endif
    else
        return
    endif;

    ;;; come here if arith move got
    move_instr(y, z) -> f_hd(m_codelist);
    true -> got_one
enddefine;

define lconstant premove(m_codelist, push_precedes);
    lvars n, nxinst, inst, op2, opcode, push_precedes;
    dlocal m_codelist;

    define lconstant replace_pop_in_next(opnd);
        lvars n, nxinst, opnd;
        f_hd(f_tl(m_codelist)) -> nxinst;
        findsr(nxinst) -> n;
        if n and f_subv(n, nxinst) == USP_+ ;;; i.e. is stack pop
        and auto_check(2, n fi_- 1, nxinst, opnd)
        then
            opnd -> f_subv(n, nxinst);      ;;; replace pop with opnd
            true
        else
            false
        endif;
    enddefine;

    f_hd(m_codelist) -> inst;
    returnunless(f_subv(1, inst) == M_MOVE);
    premove -> f_subv(1, inst);         ;;; prevent further action by premove
    f_subv(2, inst) -> op2;
    if f_subv(3, inst) == -_USP then
        ;;; pushing to stack
        do_premove_next(false, true);       ;;; optimise ahead
        if replace_pop_in_next(op2) then
            removeinstr();                  ;;; remove move
            f_hd(m_codelist) -> inst;
            f_subv(1, inst) -> opcode;
            if opcode == M_ERASE and not(auto_operand(op2)) then
                removeinstr();
                chain(m_codelist, push_precedes, premove)
            elseif op2 == i_USP and opcode == premove
            and regnumber(f_subv(3,inst) ->> op2)
            and replace_pop_in_next(op2)
            then
                ;;; i.e. {M_MOVE i_USP, reg} {OP ... USP_+, ...}
                ;;; goes to {M_MOVE USP_+, reg} {OP ... reg, ...}
                ;;; using the reg instead of the stack
                USP_+ -> f_subv(2, inst)
            endif
        endif
    elseif op2 == USP_+ then
        ;;; popping it
        do_premove_next(false, false);      ;;; optimise ahead
        f_hd(f_tl(m_codelist)) -> nxinst;
        if f_subv(1,nxinst)==premove and f_subv(3,nxinst) == -_USP
        and f_subv(2,nxinst) = (f_subv(3,inst) ->> op2)
        and not(push_precedes and regnumber(op2))
        and not(auto_operand(op2))
        then
            ;;; save the repush
            i_USP -> f_subv(2,inst);
            removenext()
        endif
    endif
enddefine;

define lconstant do_premove(m_codelist, mustconvert, push_precedes)
                                                    -> m_codelist;
    lvars mustconvert, push_precedes;
    dlocal m_codelist;
    repeat
;;;                printf('premove\n');
        premove(m_codelist, push_precedes);
;;;                printf('postmove\n');
        postmove(m_codelist, false);
;;;                printf('get_moves\n');
        quitunless(get_moves(m_codelist, mustconvert) -> m_codelist)
    endrepeat;
    if mustconvert then postmove(m_codelist, true) endif
enddefine;

define lconstant do_premove_next(mustconvert, push_precedes);
    lvars mustconvert, push_precedes;
    do_premove(f_tl(m_codelist), mustconvert, push_precedes) ->
enddefine;

define lconstant postmove(m_codelist, mustconvert);
    lvars l, n, inst, nxinst, mustconvert;
    dlocal m_codelist;
    f_hd(m_codelist) -> inst;
    datalength(inst) -> l;
    returnif(f_tl(m_codelist) == []);
    while f_subv(l, inst) == -_USP do
        do_premove_next(mustconvert, true);
        f_hd(f_tl(m_codelist)) -> nxinst;
        if f_subv(1,nxinst) == premove and f_subv(2,nxinst) == USP_+ then
            f_subv(3,nxinst) -> f_subv(l,inst);
            removenext();           ;;; remove move
            if l fi_> 2 then l fi_- 1 -> l; nextloop endif
        endif;
        quitloop
    endwhile
enddefine;

define lconstant finalimprove(inst) -> inst;
    lvars inst, l, n, m, nxinst, x, op;
    if (f_subv(1, inst) ->> op) == premove then
        M_MOVE -> f_subv(1, inst)
    elseif op == M_BIT then
        f_subv(4, inst) -> x;   ;;; test
        if (f_subv(2, inst) ->> n) == 0 then
            ;;; testing against 0 -- result is always 0
            if x == "EQ" then
                ;;; always branches
                {% M_BRANCH, f_subv(5, inst) %}
            else
                ;;; never branches -- remove instruction
                false
            endif -> inst
        elseif n = #_< 1 << (WORD_BITS-1) >_# then
            ;;; testing sign bit -- use M_TEST instead
            {%  M_TEST, f_subv(3, inst),
                if x == "EQ" then "POS" else "NEG" endif,
                f_subv(5, inst) %} -> inst
        endif;
        return
    endif;

    datalength(inst) -> l;
    returnunless(f_subv(l, inst) == -_USP);
    f_hd(f_tl(m_codelist)) -> nxinst;
    if (findsr(nxinst) ->> n) and f_subv(n, nxinst) == USP_+ then
        ;;; use a register instead
        f_subv(1, nxinst) -> x;
        if arithop2(x) or x == M_PTR_ADD_OFFS or x == M_PTR_SUB_OFFS then
            datalength(nxinst) -> m;
            f_subv(m, nxinst) -> x;             ;;; destination operand
            m fi_- 2 -> m;
            if m == n then m fi_+ 1 -> m endif;
            if regnumber(x) and reg_in_operand(f_subv(m, nxinst)) /== x then
                x
            else
                WK_REG
            endif
        else
            WK_REG
        endif ->> x ->> f_subv(l, inst) -> f_subv(n, nxinst);

        if (arithop2(f_subv(1, inst)) ->> m) and m(1) == "c"
        and f_subv(2, inst) = x then
            ;;; commutative
            f_subv(3, inst) -> f_subv(2, inst);
            x -> f_subv(3, inst)
        endif;

    elseif l fi_> 2 then
        l fi_- 1 -> n;          ;;; examine next to last opnd
        if f_subv(n, inst) == USP_+  then
            i_USP ->> f_subv(n, inst) -> f_subv(l, inst);
#_IF DEF i_USP_+
        elseif f_subv(n, inst) == i_USP_+  then
            ii_USP -> f_subv(n, inst); i_USP -> f_subv(l, inst);
#_ENDIF
        endif
    endif;
enddefine;

define m_optimise(m_codelist, pdr_exit_lab, pdr_Nlocals, pdr_flags)
                                                        -> org_m_codelist;
    dlocal m_codelist, org_m_codelist, pdr_exit_lab, pdr_Nlocals, pdr_flags;
    lvars i;
    m_codelist -> org_m_codelist;
    until m_codelist == [] do
;;;                printf(hd(m_codelist), 'm_optimise: %p\n');
        if f_subv(1, f_hd(m_codelist)) /== M_LABEL then
            do_premove(m_codelist, true, false) -> m_codelist;
            if finalimprove(f_hd(m_codelist)) ->> i then
                i -> f_hd(m_codelist)
            else
                removeinstr();
                nextloop
            endif
        endif;
        f_tl(m_codelist) -> m_codelist
    enduntil
enddefine;



;;; --- PSEUDO OPS USED IN M-CODE GENERATION ---------------------------------

define lconstant replace_current(list);
    lvars list, t = f_tl(m_codelist);
    if list == [] then
        f_dest(t)
    else
        f_dest(list) nc_<> t
    endif -> (f_hd(m_codelist), f_tl(m_codelist))
enddefine;

define lconstant repush_instrs(fsub, tsub, inst);
    lvars n, r, tsub, fsub, inst;
    fast_for n from fsub by -1 to tsub do
        unless (f_subv(n, inst) ->> r) == USP_+ then
            move_instr(r, -_USP)
        endunless
    endfor
enddefine;

define lconstant scale_index_instr(scale, index, out);
    lvars scale, index, out, pow;
    {% if is_power2(scale) ->> pow then
            M_ASH, pow
        else
            M_MULT, scale
        endif, index, out %}
enddefine;

define lconstant ptr_add_sub_inst(+_or_-, type_code, offs, addr, dst);
    lvars +_or_-, type, type_code, offs, addr, dst;
    if (type_code && t_BASE_TYPE ->> type) == t_BIT then
        mishap(0, 'ILLEGAL BIT POINTER OPERATION')
    endif;
    {%  if +_or_- == "+" then M_PTR_ADD_OFFS
        else M_PTR_SUB_OFFS
        endif, type, offs, addr, dst%}
enddefine;

define OP_!(m_codelist, mustconvert) -> m_codelist with_props '%OP_!';
    lvars   addr, out, opcode, offs, offs_index, type_code, upd, indexscale,
            size, mustconvert, x, +_or_-, offs_opcode, acctype, wkreg;

    dlocal  m_codelist;

    define lconstant add_to_wk_addr(+_or_-, offs, acctype);
        lvars +_or_-, offs, acctype;
        addinstr(ptr_add_sub_inst(+_or_-, acctype, offs, addr,
                                    work_addr_reg(upd) ->> addr))
    enddefine;

    define lconstant get_offs_opnd(offs, acctype);
        lvars offs, acctype, ioffs, opnd;
        if immediate_operand(offs) ->> ioffs then
            if CAN_DEFER_OPND(addr, ioffs, acctype, upd) ->> opnd then
                opnd -> addr
            else
                addmove(addr, work_addr_reg(upd) ->> opnd);
                cons_access_opnd(opnd, ioffs, acctype) -> addr
            endif
        else
            add_to_wk_addr("+", offs, acctype); ;;; -> addr
            cons_access_opnd(addr, 0, acctype) -> addr
        endif
    enddefine;

    explode(f_hd(m_codelist)) -> (, upd, +_or_-, indexscale, type_code,
                                    offs, offs_index, addr, out);

    unless mustconvert then
        returnif(addr == USP_+ or upd and out == USP_+)
    endunless;

    if type_code &&/=_0 t_ADDR_MODE then
        chain(m_codelist, mustconvert, OP_@)
    endif;

    t_offset(type_code, type_error) -> size;
    if indexscale then size else 1 endif -> indexscale;
    if type_code && t_BASE_TYPE == t_BIT then
        ;;; bitfield
        get_bit_opcode(type_code, upd) -> opcode;
        [%  unless offs_index == 0 then
                if indexscale /== 1 then
                    scale_index_instr(indexscale, offs_index, -_USP);
                    USP_+ -> offs_index
                endif;
                if +_or_- == "+" then M_ADD else M_SUB endif -> offs_opcode;
                {^offs_opcode ^offs_index ^offs ^ -_USP};
                USP_+ -> offs
            endunless;
            {^opcode ^size ^offs ^addr ^out}
        %] . replace_current
    else
        ;;; non bitfield
;;;                printf('OP_!\n');
        get_access_type(type_code) -> acctype;
        if offs_index /== 0 then
            if indexscale /== 1 then
                ;;; offs_index is an index, needs scaling
                if isinteger(offs_index) then
                    offs_index * indexscale -> offs_index
                else
                    work_addr_reg(upd) -> wkreg;
                    if reg_in_operand(addr) == wkreg then -_USP -> wkreg endif;
                    addinstr(scale_index_instr(indexscale, offs_index, wkreg));
                    if wkreg == -_USP then USP_+ else wkreg endif -> offs_index
                endif
            endif;
            if try_imm_+-(offs, offs_index, valof(+_or_-)) ->> x then
                x -> offs
            else
                ;;; add it to the address
                add_to_wk_addr(+_or_-, offs_index, acctype)     ;;; -> addr
            endif
        endif;
        get_offs_opnd(offs, acctype);       ;;; -> addr
        typed_move_instr(addr, out, upd, acctype) -> f_hd(m_codelist)
    endif
enddefine;

define OP_@(m_codelist, mustconvert) -> m_codelist with_props '%OP_@';
    lvars   addr, out, type_code, dis, dis_index, indexscale, mustconvert,
            size, +_or_-;

    dlocal  m_codelist;

    define lconstant add_dis_index(+_or_-, dis, dis_index, out);
        lvars +_or_-, dis, dis_index, out;
        if dis_index == 0 then
            dis
        elseif dis == 0 and +_or_- == "+" then
            dis_index
        else
            return( {%if +_or_- == "+" then M_ADD else M_SUB endif,
                                            dis_index, dis, out%} )
        endif -> dis;
        unless dis == USP_+ and out = -_USP then
            move_instr(dis, out)
        endunless
    enddefine;

    explode(f_hd(m_codelist)) -> (, , +_or_-, indexscale, type_code,
                                    dis, dis_index, addr, out);
    unless mustconvert then
        returnif(addr == USP_+ or dis_index == USP_+)
    endunless;
    if indexscale then t_offset(type_code, type_error) else 1 endif
                                                        -> indexscale;
    [%  if indexscale /== 1 and dis_index /== 0 then
            scale_index_instr(indexscale, dis_index, -_USP);
            USP_+ -> dis_index
        endif;
        if addr == 0 then
            add_dis_index(+_or_-, dis, dis_index, out)
        else
            add_dis_index(+_or_-, dis_index, dis, -_USP);
            ptr_add_sub_inst(+_or_-, type_code, USP_+, addr, out)
        endif
    %] . replace_current
enddefine;

define OP_--!++(m_codelist, mustconvert) -> m_codelist with_props '%OP_--!++';
    lvars   inst, acctype, size, upd, op, reg, addr, y, z, change, incr,
            samepoint, mustconvert, type_code;

    dlocal  m_codelist;

    explode(f_hd(m_codelist)) -> z -> y -> addr -> type_code -> incr -> upd ->;
    unless mustconvert then
        returnif(addr == USP_+ or z == -_USP or upd and y == USP_+)
    endunless;
    incr == "+" -> incr;
    t_offset(type_code, type_error) -> size;
    get_access_type(type_code) -> acctype;
    false -> change;
    addr = z -> samepoint;
    if samepoint and autoidreg(addr) then
        cons_access_opnd(addr, incr, acctype)
    elseif samepoint and (CAN_DEFER_OPND(addr, 0, acctype, upd) ->> op) then
        size -> change, op
    else
        ;;; not same
        if autoidreg(z) then
            z                   ;;; use output reg for inc/dec
        else
            true -> change;
            work_addr_reg(upd)  ;;; use another reg
        endif -> reg;
        addmove(addr, reg);
        cons_access_opnd(reg, incr, acctype)
    endif -> op;
    typed_move_instr(op, y, upd, acctype) -> inst;
    if change then
        if isinteger(change) then
            if incr then
                addinstr(inst);
                ptr_add_sub_inst("+", type_code, size, z, z)
            else
                addinstr(ptr_add_sub_inst("-", type_code, size, z, z));
                inst;
            endif;
        else
            addinstr(inst);
            move_instr(reg, z)
        endif;
    else
        inst
    endif -> f_hd(m_codelist)
enddefine;

define OP_--@++(m_codelist, mustconvert) -> m_codelist with_props '%OP_--@++';
    lvars type_code, addr, +_or_-, out, mustconvert;
    dlocal m_codelist;
    explode(f_hd(m_codelist)) -> out -> addr -> type_code -> +_or_- ->;
    unless mustconvert then
        returnif(addr == USP_+ or type_code == USP_+)
    endunless;
    ptr_add_sub_inst(+_or_-, type_code, t_offset(type_code, type_error),
                                addr, out) -> f_hd(m_codelist)
enddefine;

define OP_FDEST(m_codelist, mustconvert) -> m_codelist with_props '%OP_FDEST';
    dlocal  m_codelist;
    lvars   (, addr, outf, outb) = explode(f_hd(m_codelist)), mustconvert;

    unless mustconvert then returnif(addr == USP_+) endunless;
    unless regnumber(addr) and addr /== outf then
        addmove(addr, work_addr_reg(false) ->> addr)
    endunless;
    [%  move_instr(cons_access_opnd(addr, field_##("P_FRONT") fi_* WORD_OFFS,
                                        T_WORD), outf),
        move_instr(cons_access_opnd(addr, field_##("P_BACK") fi_* WORD_OFFS,
                                        T_WORD), outb)
    %] . replace_current
enddefine;

define OP_SUBV(m_codelist, mustconvert) -> m_codelist with_props '%OP_SUBV';
    lvars   addr, out, opcode, offs, subs, type_code, upd, size, mustconvert,
            wk_addr, type, isaddr, acctype;
    dlocal  m_codelist;

    explode(f_hd(m_codelist)) -> (, upd, type_code, offs, addr, subs, out);

    unless mustconvert then
        returnif(addr == USP_+ or subs == USP_+ or (upd and out == USP_+))
    endunless;

    type_code && t_BASE_TYPE -> type;
    t_offset(type_code, type_error) -> size;
    offs-size -> offs;          ;;; accounts for subscript starting at 1

    if isinteger(subs) then
        offs + mcint(subs)*size -> offs;
        {^OP_! ^upd + ^false ^type_code ^offs 0 ^addr ^out} -> f_hd(m_codelist);
        chain(m_codelist, mustconvert, OP_!)
    endif;

    type_code &&/=_0 t_ADDR_MODE -> isaddr;
    work_addr_reg(upd) -> wk_addr;
    if addr == USP_+ then addmove(addr, wk_addr ->> addr) endif;

    [%  if subs /== USP_+ then move_instr(subs, -_USP) endif;
        cvt_pop_subscript(size)+offs -> offs;   ;;; leaves instrs on stack
        if type == t_BIT then
            ;;; bitfield
            get_bit_opcode(type_code, upd) -> opcode;
            {^M_ADD ^offs ^USP_+ ^ -_USP};
            {^opcode ^size ^USP_+ ^addr ^out}
        else
            ;;; non bitfield
            {^M_PTR_ADD_OFFS ^type ^USP_+ ^addr ^wk_addr};
            if isaddr then
                ;;; produce field address (can't be update)
                {^M_PTR_ADD_OFFS ^type ^offs ^wk_addr ^out}
            else
                get_access_type(type_code) -> acctype;
                cons_access_opnd(wk_addr, offs, acctype) -> addr;
                typed_move_instr(addr, out, upd, acctype)
            endif
        endif
    %] . replace_current
enddefine;

define OP_UFIELD(m_codelist, mustconvert) -> m_codelist with_props '%OP_UFIELD';
    dlocal m_codelist;
    lvars r, i, j, n, inst, offs, stack_opnd, mustconvert;
    f_hd(m_codelist) -> inst;
    datalength(inst) -> n;

    unless mustconvert then
        returnif(f_subv(n, inst) == USP_+)
    endunless;

    fast_for i from 3 to n do
        reg_in_operand(f_subv(i, inst)) -> r;
        quitif(r==USP or r==WK_REG or r==WK_ADDR_REG_1 or r==WK_ADDR_REG_2)
    endfast_for;
    (n fi_- i fi_+ 1) fi_* WORD_OFFS -> offs;

    [%  if offs /== 0 then
            repush_instrs(n, i, inst);
            cons_access_opnd(USP, offs, T_WORD) -> stack_opnd;
            move_instr(stack_opnd, -_USP)
        endif;
        dl(cont(f_subv(2, inst)));
        if offs /== 0 then move_instr(USP_+, stack_opnd) endif;
        repush_instrs(i-1, 3, inst)
    %] . replace_current
enddefine;

define OP_MAKE_EXPTR(m_codelist, mustconvert) -> m_codelist with_props '%OP_MAKE_EXPTR';
    dlocal m_codelist;
    lvars mustconvert, fixed, instr = f_hd(m_codelist);
    unless f_subv(3,instr) then
        removeinstr();
        return
    endunless;
    [%  if f_subv(2,instr) then
            f_subv(3,instr)() -> fixed;
            move_instr(USP_+, cont(fixed));
            move_instr(fixed, -_USP)
        else
            {% M_CALL, perm_const_opnd([Sys Cons_extern_ptr]) %};
        endif
    %] . replace_current
enddefine;


define lconstant do_call(m_codelist, mustconvert, upd, fast) -> m_codelist;
    dlocal m_codelist;
    lvars opnd, instr, subr, proc, upd, fast, mustconvert;
    f_hd(m_codelist) -> instr;
    f_subv(2, instr) -> opnd;
    returnunless(mustconvert or opnd /== USP_+);

    operand_type(opnd) == "procedure" -> proc;
    false -> subr;
    if proc or fast then
        if isref(opnd) then
            ;;; constant
            unless proc then
                mishap(cont(opnd),1,'COMPILING FAST_APPLY OF NON-PROCEDURE CONSTANT')
            endunless;
            if upd then
                consref(updlabof(fast_cont(opnd), true)) -> opnd
            endif
        elseif upd then
            [\^_popuncenter] -> subr
        endif
    else
        if upd then [\^_popuenter] else [\^_popenter] endif -> subr
    endif;

    if subr then
        ;;; takes arg in a reg
        {% M_CALLSUB, perm_const_opnd(subr), opnd %} -> f_hd(m_codelist)
    else
        opnd -> f_subv(2, instr);
        M_CALL -> f_subv(1, instr)
    endif
enddefine;

constant procedure (
    OP_CALL     = do_call(%false, false%),
    OP_FASTCALL = do_call(%false, true%),
    OP_UCALL    = do_call(%true, false%),
    OP_FASTUCALL= do_call(%true, true%),
    );
'%OP_CALL'      -> pdprops(OP_CALL);
'%OP_FASTCALL'  -> pdprops(OP_FASTCALL);
'%OP_UCALL'     -> pdprops(OP_UCALL);
'%OP_FASTUCALL' -> pdprops(OP_FASTUCALL);


define lconstant do_chain(m_codelist, mustconvert, from_caller, fast) -> m_codelist;
    lvars   opnd, instr, const, proc, fast, from_caller, unwind,
            mustconvert;
    dlocal  m_codelist;
    f_hd(m_codelist) -> instr;
    f_subv(2, instr) -> opnd;
    returnunless(mustconvert or opnd /== USP_+);

    isref(opnd) -> const;
    operand_type(opnd) == "procedure" -> proc;
    if proc then
        true -> fast
    elseif const then
        mishap(cont(opnd), 1, 'COMPILING CHAIN OR FAST_CHAIN TO NON-PROCEDURE CONSTANT');
    endif;
    if fast and pdr_Nlocals == 0 and pdr_flags &&=_0 M_PD_PROC_DLEXPR_CODE then
        ;;; do it all in-line if no dynamic locals
        perm_const_opnd([\^_unwind_frame]) -> unwind;

#_IF DEF ALWAYS_USE_CHAIN_REG
        if from_caller then
            ;;; must move the operand to the stack, because the
            ;;; locals will have been restored (and CHAIN_REG isn't assumed
            ;;; to survive a call of _unwind_frame).
            if opnd /== USP_+ then
                addmove(opnd, -_USP), USP_+ -> opnd
            endif;
            ;;; must also move address of _unwind_frame into CHAIN_REG
            addmove(unwind, CHAIN_REG ->> unwind)
        elseif opnd /== USP_+ then
            ;;; must move the operand to CHAIN_REG, because the
            ;;; locals will have been restored
            addmove(opnd, CHAIN_REG ->> opnd)
        endif;
        addinstr({^M_UNWIND_SF});

#_ELSE
        if f_tl(org_m_codelist) == m_codelist then
            ;;; follows immediately after M_CREATE_SF (as in -fast_chain-)
            instr -> f_hd(org_m_codelist);
            [ {% M_LABEL, pdr_exit_lab %} {^M_END} ] -> f_tl(org_m_codelist);
            org_m_codelist -> m_codelist
        else
            unless const or opnd == USP_+ then
                if from_caller then
                    ;;; must move the operand to the stack, because the
                    ;;; locals will have been restored (and CHAIN_REG isn't
                    ;;; assumed to survive a call of _unwind_frame).
                    addmove(opnd, -_USP), USP_+ -> opnd
                else
                    ;;; must move the operand to CHAIN_REG, because the
                    ;;; locals will have been restored
                    addmove(opnd, CHAIN_REG ->> opnd)
                endif
            endunless;
            addinstr({^M_UNWIND_SF})
        endif;
#_ENDIF

        if from_caller then
            addinstr({^M_CALLSUB ^unwind});     ;;; leaves return in CHAIN_REG
#_IF DEF UNWIND_FRAME_SAVES_PB
            ;;; _unwind_frame also saves the procedure base reg on the stack
            addmove(USP_+, PROCEDURE_BASE_REG);
#_ENDIF
            {^M_CALL_WITH_RETURN ^opnd ^CHAIN_REG} -> f_hd(m_codelist)
        else
            opnd -> f_subv(2, instr);
            M_CHAIN -> f_subv(1, instr)
        endif

    else
        ;;; do it by frigging caller's return address
        ;;; and leaving normally
        if opnd /== USP_+ then addmove(opnd, -_USP) endif;
        if from_caller then
            perm_const_opnd(if fast then [\^_sysncchain_caller]
                            else [\^_syschain_caller]
                            endif)
        else
            ;;; save caller's return in CHAIN_REG
            addinstr(caller_return_instr(CHAIN_REG, false));
            perm_const_opnd(if fast then [\^_sysncchain]
                            else [\^_syschain]
                            endif)
        endif;
        ;;; set routine address as new caller return
        addinstr(caller_return_instr((), true));
        ;;; then change this instruction to a BRANCH to abnormal exit code
        pdr_exit_lab -> f_subv(2, instr);
        M_BRANCH -> f_subv(1, instr)
    endif
enddefine;

constant procedure (
    OP_CHAIN            = do_chain(%false, false%),
    OP_FASTCHAIN        = do_chain(%false, true%),
    OP_CHAIN_CALLER     = do_chain(%true, false%),
    OP_FASTCHAIN_CALLER = do_chain(%true, true%),
    );
'%OP_CHAIN'             -> pdprops(OP_CHAIN);
'%OP_FASTCHAIN'         -> pdprops(OP_FASTCHAIN);
'%OP_CHAIN_CALLER'      -> pdprops(OP_CHAIN_CALLER);
'%OP_FASTCHAIN_CALLER'  -> pdprops(OP_FASTCHAIN_CALLER);


define OP_CHAINSUB(m_codelist, mustconvert) -> m_codelist with_props '%OP_CHAINSUB';
    lvars opnd, instr, mustconvert;
    dlocal m_codelist;
    f_hd(m_codelist) -> instr;
    f_subv(2, instr) -> opnd;
    returnunless(mustconvert or opnd /== USP_+);

#_IF DEF ALWAYS_USE_CHAIN_REG
    ;;; must move operand to CHAIN_REG, because the
    ;;; base address reg will have been restored
    addmove(opnd, CHAIN_REG ->> f_subv(2, instr));
#_ELSE
    unless isref(opnd) then
        ;;; not constant -- must move operand to CHAIN_REG, because the
        ;;; locals will have been restored
        addmove(opnd, CHAIN_REG ->> f_subv(2, instr))
    endunless;
#_ENDIF
    addinstr({^M_UNWIND_SF});
    M_CHAINSUB -> f_subv(1, instr)
enddefine;

define OP_+-(m_codelist, mustconvert) -> m_codelist with_props '%OP_+-';
    lvars opnd, instr = f_hd(m_codelist), p, mustconvert;
    dlocal m_codelist;
    f_subv(3, instr) -> opnd;
    returnunless(mustconvert or opnd /== USP_+);
    f_subv(2, instr) -> p;
    if opnd == popint(1) then
        if p == "+" then [Sys +_1] else [Sys -_1] endif
    else
        addmove(opnd, -_USP);
        if p == "+" then [+] else [-] endif
    endif -> p;
    {% M_CALL, perm_const_opnd(p) %} -> f_hd(m_codelist)
enddefine;

define OP_SETSTKLEN(m_codelist, mustconvert) -> m_codelist with_props '%OP_SETSTKLEN';
    lvars nr, instr = f_hd(m_codelist), mustconvert;
    dlocal m_codelist;
    f_subv(2, instr) -> nr;     ;;; popint number of results
    returnunless(mustconvert or nr /== USP_+);

    if isinteger(nr) then
        ;;; do it with inline code
        M_SETSTKLEN -> f_subv(1, instr);
        mcint(nr) fi_* WORD_OFFS -> f_subv(2, instr)    ;;; offs size of results
    else
        ;;; call subroutine
        addmove(f_subv(3,instr), -_USP);
        addmove(nr, -_USP);
        {% M_CALLSUB, perm_const_opnd([\^_setstklen]) %} -> f_hd(m_codelist)
    endif
enddefine;

define OP_SWAP(m_codelist, mustconvert) -> m_codelist with_props '%OP_SWAP';
    lvars n, n2, n1, instr = f_hd(m_codelist), opnd1, opnd2, i, mustconvert;
    dlocal m_codelist;

    define lconstant stack_nth(n);
        lvars n;
        {% USP, n fi_* WORD_OFFS %}
    enddefine;

    datalength(instr) -> n2;
    returnunless(mustconvert or f_subv(n2, instr) /== USP_+);
    fast_for n from n2 by -1 to 3 do
        quitif(f_subv(n,instr) /== USP_+)
    endfast_for;
    f_subv(2, instr) -> n1;
    f_subv(n1, instr) -> opnd1;
    [%  if n >= n1 and auto_check(n1+1, n, instr, opnd1)
        and reg_in_operand(opnd1) /== USP then
            f_subv(n, instr) -> opnd2;
            if n == n2 and auto_check(n1, n-1, instr, opnd2)
            and reg_in_operand(opnd2) /== USP then
                opnd2 -> f_subv(n1, instr);
                opnd1 -> f_subv(n, instr)
            else
                repush_instrs(n, n1+1, instr);
                n2-n1 -> i;
                move_instr(stack_nth(i-1), -_USP);
                move_instr(opnd1, stack_nth(i));
                n1-1 -> n
            endif
        else
            if n >= n1 then repush_instrs(n, n1, instr), n1-1 -> n endif;
            stack_nth(n1-n-1) -> opnd1;
            stack_nth(n2-n-1) -> opnd2;
            move_instr(opnd1, WK_REG);
            move_instr(opnd2, opnd1);
            move_instr(WK_REG, opnd2);
        endif;
        repush_instrs(n, 3, instr)
    %]. replace_current
enddefine;

define OP_TYPE_CHECK(m_codelist, mustconvert) -> m_codelist with_props '%OP_TYPE_CHECK';
    lvars opnd, isreg, errp, isp, ivec, lab, instr = f_hd(m_codelist),
        mustconvert;
    dlocal m_codelist;
    returnunless(mustconvert or f_subv(3, instr) /== USP_+);
    f_subv(3,instr) -> opnd;
    if f_subv(2, instr) == procedure_key then
        ;;; precedes pop into procedure var
        if operand_type(opnd) == "procedure" then
            move_instr(opnd, -_USP) -> f_hd(m_codelist);
            return
        endif;
        "isprocedure", [Sys Inline_checkr_procedure]
    else
        ;;; integer -- precedes GO_ON
        "\^_isinteger", [Sys Inline_checkr_integer]
    endif -> errp -> isp;
    inline_conditions(isp) -> ivec;
    if isprocedure(ivec) then ivec() else copy(ivec) endif -> ivec;
    regnumber(opnd) -> isreg;
    genjumplab(false) ->> lab -> f_subv(datalength(ivec),ivec);
    [%  if opnd /== USP_+ then move_instr(opnd, -_USP) endif;
        if isreg then
            ivec, move_instr(opnd, -_USP)
        else
            move_instr(i_USP, -_USP), ivec
        endif;
        {% M_CALL, perm_constp_opnd(errp,false) %};
        {% M_LABEL, lab %};
        if isreg then move_instr(opnd, -_USP) endif
    %] . replace_current
enddefine;



endsection;     /* $-Popas$-M_trans */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 10 1997
        Fixed bug in OP_! (work_addr_reg was being used for scaled offset
        when already in use for the pointer operand).
--- John Gibson, Mar  3 1995
        Changed get_access_type to allow for T_INT_DOUBLE
--- John Gibson, Nov  3 1994
        Improvements to get_moves
--- John Gibson, Oct  7 1994
        # Added generation of new typed mem access operands for short/byte
          (instead of M_MOVEx instructions, etc), dependent on
          USE_NEW_M_OPERANDS being set true by genproc.p.
        # Added support for constants in _special_var_block, dependent on
          GEN_SVB_CONSTANTS  being set true
--- John Gibson, Jul 21 1992
        Version 14.21 changes
--- John Gibson, Dec  5 1991
        Fixed bug in -do_chain- where chain instr follows immediately
        after M_CREATE_SF
--- John Gibson, Jan 19 1990
        Correction to optimisation in -get_moves-
--- John Gibson, Oct  6 1989
        Fixed bug in OP_FDEST
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, May  5 1989
        Version 13.6402 changes
--- John Gibson, Apr 26 1989
        Version 13.64 changes
--- John Gibson, Jan 29 1989
        New version of popc
--- John Gibson, Jul 15 1988
        Changes for Sun-4 port
--- John Gibson, Feb 21 1988
        Use of all subroutines now recorded on .w output
--- John Gibson, Jan 17 1988
        Changes for coping with sections, weakrefs, new format for assembler
        files, etc, etc.
 */
