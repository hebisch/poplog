/*
 > File:        $usepop/src/syscomp/genproc_arm.p
 > Purpose:     Compiles M-Code to ARM assembler (for R-pi, etc.)
 */


#_INCLUDE 'common.ph'

section $-Popas$-M_trans;

global constant procedure (
    immediate_operand,
    auto_operand,
    reg_in_operand,
    commute_test,
    negate_test,
    perm_const_opnd,
);

global vars
    current_pdr_label,
    current_pdr_exec_label,
;


    /****************************************************************
    * This file is contained entirely in section Genproc, and must  *
    *   define all the exports to section M_trans listed below.     *
    ****************************************************************/

section Genproc =>

        /*  M-opcode Procedures */

        M_ADD
        M_ASH
        M_BIC
        M_BIM
        M_BIS
        M_BIT
        M_BRANCH
        M_BRANCH_std
        M_BRANCH_ON
        M_BRANCH_ON_INT
        M_CALL
        M_CALL_WITH_RETURN
        M_CALLSUB
        M_CHAIN
        M_CHAINSUB
        M_CLOSURE
        M_CMP
        M_CMPKEY
        M_CREATE_SF
        M_END
        M_ERASE
        M_LABEL
        M_LOGCOM
        M_MOVE
        ;;; No longer needed
        ;;; M_MOVEb
        M_MOVEbit
        ;;; No longer needed
        ;;; M_MOVEi    
        ;;; M_MOVEs
        ;;; M_MOVEsb
        M_MOVEsbit
        ;;; No longer needed
        ;;; M_MOVEsi
        ;;; M_MOVEss
        M_MULT
        M_NEG
        M_PADD
        M_PADD_TEST
        M_PCMP
        M_PLOG_IFNOT_ATOM
        M_PLOG_TERM_SWITCH
        M_PSUB
        M_PSUB_TEST
        M_PTR_ADD_OFFS
        M_PTR_CMP
        M_PTR_SUB
        M_PTR_SUB_OFFS
        M_RETURN
        M_SETSTKLEN
        M_SUB
        M_TEST
        M_UNWIND_SF
        ;;; No longer needed
        ;;; M_UPDb
        M_UPDbit
        ;;; No longer needed
        ;;; M_UPDi
        M_UPDs

        /*  Registers */

        SP
        USP
        USP_+
        -_USP
        i_USP
;;;     i_USP_+         ;;; these two are optional
;;;     ii_USP
        WK_ADDR_REG_1
        WK_ADDR_REG_2
        WK_REG
        CHAIN_REG

        /*  Register Lists */

        nonpop_registers
        pop_registers

        /*  Register Procedures */

        reglabel
        regnumber
        autoidreg

        /*  M-code Mapping of Subroutines */

        mc_inline_conditions_list
        mc_inline_procs_list

        /*  Procedures Needed by -m_optimise- */

        cvt_pop_subscript
        can_defer_opnd
        pdr_index_opnd

        /*  Procedure to Generate Code */

        mc_code_generator
        USE_NEW_M_OPERANDS
;

constant macro USE_NEW_M_OPERANDS = true;


;;; === REGISTER USAGE ================================================

/*
   r0  - work/arg            r1 - work/arg
   r2  - work/arg            r3 - work/arg/chain
   r4  - non pop lvar        r5 - non pop lvar ??? work ???
   r6  - non pop lvar        r7 - non pop lvar
   r8  - non pop lvar        r9  - pop lvar
   r10 - user stack pointer  r11 - base
   r12 - work                r13 - stack pointer
   r14 - link register/work  r15 - program counter

*/

lconstant

    ;;; ARM register names and their usage

    R0 = "r0",    ;;; WK_REG/work reg ???/arg_reg_0
    R1 = "r1",    ;;; principal work reg/arg_reg_1
    R2 = "r2",    ;;; CHAIN_REG/arg_reg_2
    R3 = "r3",    ;;; WK_ADDR_REG_1
    R4 = "r4",    ;;; 
    R5 = "r5",    ;;; secondary work reg
    R9  = "r9",
    R10 = "r10",  ;;; USP
    R11 = "r11",  ;;; PB
    R12 = "r12",  ;;; WK_ADDR_REG_2
    R13 = "sp",    ;;; SP/r13
    LR = "lr",     ;;; LR/r14
;

constant

    ;;; POPC register operands

    SP = R13,

    ;;; USP: user stack pointer

    USP = R10,

    ;;; WK_REG: used by "m_optimise" to eliminate user stack pushes and
    ;;; pops between successive M-code instructions.  Must be preserved
    ;;; by M_MOVE.  Any instruction should handle WK_REG as source
    ;;; or destination.

    WK_REG = R0,

    ;;; CHAIN_REG: used to save procedure operands for in-line chaining
    ;;; and to save return addresses for out-of-line chaining through
    ;;; the subroutines "_syschain" and "_sysncchain". It must not be
    ;;; touched by M_UNWIND_SF.

    CHAIN_REG = R2,

    ;;; WK_ADDR_REG_1, WK_ADDR_REG_2: used for building field-access
    ;;; operands of the form {reg offset} when an existing operand
    ;;; cannot be deferred directly (can_defer_operand having returned
    ;;; <false> for it). REG_1 is used for source operands, REG_2 for
    ;;; destination operands.

    WK_ADDR_REG_1 = R3,
    WK_ADDR_REG_2 = R12,

    ;;; USP_+ : pop from the user stack
    ;;; -_USP : push on the user stack
    ;;; i_USP : top of user stack

    USP_+ = {^USP ^true},
    -_USP = {^USP ^false},
    i_USP = {^USP 0},

    ;;; i_USP_+ : not supported
    ;;; ii_USP  : not supported

    ;;; Lists of pop/non-pop registers for register locals

    ;;; pop_registers = [[] 9 10],
    pop_registers = [[]],
    ;;; nonpop_registers = [[] 6 7 8],
    nonpop_registers = [[]],
;

;;; regnumber:
;;;     maps register names to numbers as used in the "reg" field of
;;;     instructions

define regnumber = newassoc([]) enddefine;

;;; reglabel:
;;;     the inverse mapping

define reglabel = newassoc([]); enddefine;

procedure();
        lvars n, l;
        for n from 0 to 15 do
                if n = 13 then "sp"
                elseif n = 14 then "lr"
                elseif n = 15 then "pc"
                else
                    consword('r' >< n)
                endif -> l;
                n -> regnumber(l);
                l -> reglabel(n);
        endfor
endprocedure();

;;; Local register operands:

lconstant

    ;;; PB: procedure base register.
    ;;; This points to the start of the current procedure record, allowing
    ;;; access to values there.

    PB = R11,
;

;;; autoidreg:
;;;     indicates whether a register supports auto-indirection. All do
;;;     on the ARM.

identof("regnumber") -> identof("autoidreg");


;;; === M-CODE OPERANDS ===============================================

;;; isimm, immval:
;;;     an immediate operand can be an integer, standing for itself, or
;;;     a reference to a string, standing for an immediate symbol or
;;;     expression.

lconstant macro isimm = "immediate_operand";

define lconstant immval(opd);
    lvars opd;
    if isintegral(opd) then opd else cont(opd) endif;
enddefine;

define lconstant immrep(x);
    lvars x;
    if isintegral(x) then x else consref(x) endif;
enddefine;

;;; isreg:
;;;     a register operand is a word, the register name. The property
;;;     regnumber is used as a recogniser for legal register names.

lconstant macro isreg = "regnumber";

;;; isabs:
;;;     an absolute operand is a string, standing for an absolute symbol
;;;     or expression. A register based/indexed operand is a vector,
;;;     with the most general form:
;;;         {^base ^disp ^index ^scale}
;;;     where base and index are registers, disp is an immediate
;;;     displacement and scale is an index scale factor (1,2,4 or 8).
;;;     The index and scale components are usually omitted, and are
;;;     never generated by the VM compiler: they're used here for
;;;     occasional optimisations.

lconstant macro isabs = "isstring";

define lconstant is_small_disp(disp);
    lvars disp;
    isinteger(disp) and disp > -64 and disp < 63;
enddefine;

;;; can_defer_opnd:
;;;     takes an M-code operand plus displacement as arguments and
;;;     returns either a deferred version of the operand or <false> if
;;;     it's already a memory operand and so can't be deferred further.
;;;     Used in "m_optimise" to generate field access and update
;;;     operands. Whenever this returns <false>, one of the work address
;;;     registers has to be used for an intermediate load and
;;;     indirection.

define can_defer_opnd(opd, dis, acctype, upd);
    lvars opd, dis, upd;
    if isreg(opd) and is_small_disp(dis) and acctype == T_WORD then
        ;;; register becomes register indirect
        {^opd ^dis};
/*
    elseif isreg(opd) and isreg(dis) and acctype == T_WORD then
        ;;; register becomes register indirect
        {^opd ^dis};
*/
    elseif isref(opd) and isinteger(dis) and acctype == T_WORD then
        ;;; immediate symbol becomes absolute expression
        asm_expr(fast_cont(opd), "+", dis);
    else
        false;
    endif;
enddefine;

define lconstant wof = nonop fi_*(% WORD_OFFS %) enddefine;

;;; pdr_index_opnd:
;;;     used by "m_trans.p" for creating operands to push/call values
;;;     from procedure headers (not used in closures).

define pdr_index_opnd(fld_index);
    lvars fld_index;
    {% PB, fld_index*WORD_OFFS %}
enddefine;


;;; == TRANSLATION TO ASSEMBLY CODE =======================================

lvars
    m_instr,
        ;;; current M-code instruction
    last_instr,
        ;;; last assembly-code instruction planted
    new_literals,
    lit_offset,
;

;;; plant:
;;;     add an assembly-code instruction to the code list

define lconstant plant(/* opcode, operands, ..., n */);
    conspair(consvector(), []) ->> f_tl(last_instr) -> last_instr;
enddefine;

;;; asmXXX:
;;;     assembly-code instructions

define lconstant asm_emit(/* opcode, operands, ..., n*/);
    ;;; add instruction to the code list
    plant(/* opcode, operands, ..., n*/);
enddefine;

define lconstant asmALIGN();
    asm_emit("align", 1);
enddefine;

define lconstant asmLABEL(lab);
    lvars lab;
    asm_emit("label", lab, 2);
enddefine;

define lconstant position_or_add(opd, lst);
    lvars opd, lst, lst0 = lst, lst1, n = 0;
    returnif(lst == [])(0, [^opd]);
    while not(lst == []) do
        returnif(fast_front(lst) = opd) (n, lst0);
        fast_back(lst) -> lst1;
        n fi_+ 1 -> n;
        if lst1 == [] then
            [^opd] -> fast_back(lst);
            return(n, lst0);
        endif;
        lst1 -> lst;
    endwhile;
enddefine;

define lconstant get_literal_addr(lit);
    lvars lit, disp, tmp;
    position_or_add(lit, new_literals) -> (disp, new_literals);
    return('[' >< PB >< ', #' >< (4*(disp + lit_offset)) >< ']');
enddefine;

define lconstant load_literal(lit, tmp);
    lvars lit, tmp;
    asm_emit("ldr", tmp, get_literal_addr(lit), 3);
enddefine;

define lconstant get_addressable_op(opd, tmp);
    lvars opd, disp, opd1, type;
    returnif(isreg(opd))(opd);
    if isvector(opd) then
        if datalength(opd) = 1 then
            return( '[' >< f_subv(1, opd) >< ']');
        elseif datalength(opd) >= 2 then
            f_subv(2, opd) -> disp;
            f_subv(1, opd) -> opd1;
            if disp = 0 then
                    ;;; Already OK
            elseif is_small_disp(disp) then
                    opd1 >< ', #' >< disp -> opd1;
            elseif isboolean(disp) then
                '4' -> tmp;
                if datalength(opd) == 3 then
                    f_subv(3, opd) && t_BASE_TYPE -> type;
                    if type == t_INT then '4'
                    elseif type == t_SHORT then '2'
                    elseif type == t_BYTE then '1'
                    else
                        mishap(opd, 1, 'Unhandled operand type');
                    endif -> tmp;
                endif;
                if disp then
                    '[' >< opd1 >< '], #' >< tmp 
                else
                    '[' >< opd1 >< ', #-' >< tmp >< ']!'
                endif -> opd1;
                return(opd1);
            elseif isinteger(disp) then
                ;;; FIXME: Implement real register allocation
                load_literal(disp, LR);
                opd1 >< ', ' >< LR -> opd1;
            else
                mishap(opd, 1, 'Unhandled operand in get_addressable_op');
            endif;
            return('[' >< opd1 >< ']');
        endif;            
        mishap(opd, 1, 'Unhandled operand in get_addressable_op');
    endif;
    if isinteger(opd) or isbiginteger(opd) then
        mishap(opd, 1, 'Want address of literal');
        ;;; load_literal_addr(opd, tmp);
        ;;; return('[' >< tmp >< ']');
    endif;
    ;;; FIXME: check if this really works
    if isref(opd) then
        ;;; printf(opd, 'has rep opd: %p\n');
        fast_cont(opd) -> opd1;
        return(get_literal_addr(opd1));
    elseif isstring(opd) then
        ;;; printf(opd, 'has string opd: %p\n');
        load_literal(opd, tmp);
        return('[' >< tmp >< ']');
        ;;; return(get_literal_addr(opd));
    endif;
    mishap(opd, 1, 'Unhandled operand in get_addressable_op');
enddefine;

define lconstant load_to_reg(opd, tmp);
    lvars opd, tmp, opd1, opcode, type, n;
    returnif(isreg(opd))(opd);
    if isinteger(opd) or isbiginteger(opd) then
        if is_small_disp(opd) then
            asm_emit("mov", tmp, '#' >< opd, 3);
            return(tmp);
        else
            load_literal(opd, tmp);
            return(tmp);
        endif;
    endif;
    get_addressable_op(opd, tmp) -> opd1;
    "ldr" -> opcode;
    if isvector(opd) and datalength(opd) == 3 then
        ;;; printf(opd, 'Typed operand %p\n');
        f_subv(3, opd) && t_BASE_TYPE -> type;
        ;;; printf(type, 'Base type: %p\n');
        if type == t_INT then "ldr"
        elseif type == t_SHORT then "ldrh"
        elseif type == t_BYTE then "ldrb"
        else
            mishap(opd, 1, 'Unhandled operand type');
        endif -> opcode;
    endif;
    asm_emit(opcode, tmp, opd1, 3);
    if opcode /== "ldr" then
        f_subv(3, opd) -> type;
        if (type && tv_SIGNED) /== 0 then
            type && t_BASE_TYPE -> type;
            if type == t_SHORT then 16 else 24 endif -> n;
            asm_emit("mov", tmp, tmp, 'asl #' >< n, 4);
            asm_emit("mov", tmp, tmp, 'asr #' >< n, 4);
        endif;
    endif;
    return(tmp);
enddefine;

define lconstant gen_reg_store(src, dst, tmp);
    lvars src, dst, tmp, dst1, type, opcode;
    get_addressable_op(dst, tmp) -> dst1;
    "str" -> opcode;
    if isvector(dst) and datalength(dst) == 3 then
        f_subv(3, dst) && t_BASE_TYPE -> type;
        if type == t_INT then "str"
        elseif type == t_SHORT then "strh"
        elseif type == t_BYTE then "strb"
        else
            mishap(dst, 1, 'Unhandled operand type');
        endif -> opcode;
    endif;
    asm_emit(opcode, src, dst1, 3);
enddefine;

define lconstant push_operand(opd);
    lvars opd;
    load_to_reg(opd, R1) -> opd;
    asm_emit("str", opd, '[sp, #-4]!', 3);
enddefine;

define lconstant pop_operand(opd);
    lvars opd, reg;
    if isreg(opd) then opd else R1 endif -> reg;
    asm_emit("ldr", reg, '[sp], #4', 3);
    returnif(opd == reg);
    gen_reg_store(reg, opd, R5);
enddefine;

define lconstant gen_transfer(opcode, target);
    lvars target, opcode;
    if isreg(target) then
        if opcode == "bl" then "blx" -> opcode; endif;
        if opcode == "b" then "bx" -> opcode; endif;
    endif;
    asm_emit(opcode, target, 2);
enddefine;

;;; testop:
;;;     maps M-code condition codes to assembler opcode extension

define lconstant testop =
    newassoc([
        [EQ     eq]
        [NEQ    ne]
        [LT     lt]
        [LEQ    le]
        [GT     gt]
        [GEQ    ge]
        [ULT    cc]
        [ULEQ   ls]
        [UGT    hi]
        [UGEQ   cs]
        [NEG    mi]
        [POS    pl]
        [OVF    vs]
        [NOVF   vc]
    ]);
enddefine;

define lconstant get_jump_addr(lab);
    ;;; FIXME: really implement
    if isstring(lab) then
        lab;
    else
        mishap(lab, 1, 'get_jump_addr unimplemented');
    endif;
enddefine;

define lconstant gen_branch(opcode, lab);
    get_jump_addr(lab) -> lab;
    asm_emit(opcode, lab, 2);
enddefine;

/*
 *  Data Movement
 */

define gen_move(src, dst);
    lvars  src, dst;
    returnif(src = dst or src == USP_+ and dst == -_USP);
    if isreg(dst) then
        load_to_reg(src, dst) -> src;
        returnif(src = dst);
        ;;; printf(src, 'calling asm_emit, src = %p\n');
        asm_emit("mov", dst, src, 3);
        ;;; printf('gen_move returning');
    else
        ;;; FIXME: Do it in general
        ;;; Special case to support
        ;;;   M_MOVE r10 {r10 <false>}
        if src == USP and dst == -_USP then
            asm_emit("mov", R1, src, 3);
            R1 -> src;
        else
            load_to_reg(src, R1) -> src;
        endif;
        ;;; NOTE: I_SWAP assumes that WK_REG = R0 will survive
        ;;; M_MOVE with arguments on user stack.  This is OK, since
        ;;; gen_reg_store does not need temporary when
        ;;; dst is on user stack.
        gen_reg_store(src, dst, R0);
    endif;
enddefine;

define M_MOVE();
    lvars (, src, dst) = explode(m_instr);
    gen_move(src, dst);
enddefine;

;;; gen_bfield:
;;;     extract a bitfield from a structure using assembly code routines
;;;     "_bfield" and "_sbfield"

define lconstant gen_bfield(routine);
    lvars routine, (, size, offs, src, dst) = explode(m_instr);
    gen_move(size, R0);
    gen_move(offs, R1);
    gen_move(src, R2);
    gen_transfer("bl", symlabel(routine));
    gen_move(R0, dst);
enddefine;

define M_MOVEbit  = gen_bfield(% "\^_bfield"  %) enddefine;
define M_MOVEsbit = gen_bfield(% "\^_sbfield" %) enddefine;

define M_UPDbit();
    lvars (, size, offs, dst, src) = explode(m_instr);
    gen_move(src, -_USP);
    gen_move(size, R0);
    gen_move(offs, R1);
    gen_move(dst, R2);
    gen_transfer("bl", symlabel("\^_ubfield"));
enddefine;


/*
 *  Basic operations
 */

define is_int_opd(src);
    isintegral(src) and is_small_disp(src);
enddefine;

define get_operand2(src);
    lvars src;
    if is_int_opd(src) then '#' >< src else load_to_reg(src, R5) endif;
enddefine;

define get_operands(src1, src2);
    lvars src1, src2;
    ;;; printf(src1, 'get_operands(%p, ');
    ;;; printf(src2, '%p)\n');
    load_to_reg(src1, R1);
    get_operand2(src2);
enddefine;

;;; Like get_operands, but user is supposed to switch order
;;; of operand (we need to perform loads in source order,
;;; to preserve order of side effects).
define get_operands_r(src1, src2);
    lvars src1, src2;
    get_operand2(src1);
    load_to_reg(src2, R1);
enddefine;

;;; gen_op_2:
;;;     plants code for a unary operation 'opcode' of the form:
;;;         dst := op(src)

define lconstant gen_op_2(src, dst, opcode);
    lvars src, dst, asm_op, dreg, op1;
    get_operand2(src) -> op1;
    if isreg(dst) then
        dst -> dreg;
        false -> dst;
    else
        R1 -> dreg;
    endif;
    asm_emit(opcode, dreg, op1, 3);
    if dst then
        gen_reg_store(dreg, dst, R5);
    endif;
enddefine;

;;; gen_op_3:
;;;     plants general 3 address binary operation 'opcode':
;;;         dst := src1 op src2

define lconstant gen_op_3(src1, src2, dst, opcode);
    lvars src1, src2, dst, opcode, op1, op2, dreg;
    get_operands_r(src1, src2) -> (op2, op1);
    if isreg(dst) then
        dst -> dreg;
        false -> dst;
    else
        R1 -> dreg;
    endif;
    asm_emit(opcode, dreg, op1, op2, 4);
    if dst then
        gen_reg_store(dreg, dst, R5);
    endif;
enddefine;

;;; gen_op_commute:
;;;     plants code for a commutative binary operation 'opcode':
;;;         dst := src1 op src2

define lconstant gen_op_commute(src1, src2, dst, opcode);
    lvars src1, src2, dst, opcode;
    if is_int_opd(src2) then
        (src1, src2) -> (src2, src1)
    endif;
    gen_op_3(src1, src2, dst, opcode);
enddefine;

;;; m_op_*: 
;;;     translate 2- and 3-operand M-code arithmetic/logical instructions
;;;     on machine integers;  calls corresponding gen_... operation.

define lconstant m_op_2(opcode);
    lvars opcode, (, src, dst) = explode(m_instr);
    gen_op_2(src, dst, opcode);
enddefine;

define lconstant m_op_commute(opcode);
    lvars opcode,
          (, src1, src2, dst) = explode(m_instr);
    gen_op_commute(src1, src2, dst, opcode);
enddefine;

define lconstant m_op_3(opcode);
    lvars opcode,
          (, src1, src2, dst) = explode(m_instr);
    gen_op_3(src1, src2, dst, opcode);
enddefine;

;;; m_parith, m_parith_test:
;;;     plant code for an addition or subtraction of pop integers.
;;;     This means clearing the bottom two bits of the first operand and
;;;     then doing an ordinary machine integer operation.
;;;     The testing version pushes the result on the user stack and
;;;     plants a branch conditional on the result.

define lconstant m_parith(opcode);
    lvars opcode, (, src1, src2, dst) = explode(m_instr);
    if isintegral(src1) then
        src1 - 3 -> src1;
    else
        load_to_reg(src1, R5) -> src1;
        asm_emit("sub", R5, src1, 3, 4);
        R5 -> src1;
    endif;
    gen_op_3(src1, src2, dst, opcode);
enddefine;

define lconstant m_parith_test(opcode);
    lvars gen_p, (, src1, src2, test, lab) = explode(m_instr);
    if isintegral(src1) then
        src1 - 3 -> src1;
    else
        load_to_reg(src1, R5) -> src1;
        asm_emit("sub", R5, src1, 3, 4);
        R5 -> src1;
    endif;
    gen_op_3(src1, src2, -_USP, opcode);
    gen_branch('b' >< testop(test), lab);
enddefine;

;;; ptr_arith:
;;;     plants code for an operation on pointers. Pointers are just
;;;     machine integers, so their code-planting procedures can be used
;;;     directly. The type field of the instruction is ignored.

define lconstant m_ptr_op_3(opcode);
    lvars gen_p, (, /*type*/, offs, ptr, dst) = explode(m_instr);
    gen_op_3(offs, ptr, dst, opcode);
enddefine;

define lconstant m_ptr_op_commute(opcode);
    lvars gen_p, (, /*type*/, offs, ptr, dst) = explode(m_instr);
    gen_op_commute(offs, ptr, dst, opcode);
enddefine;

define M_ADD    = m_op_commute(% "add"  %) enddefine;
define M_SUB    = m_op_3(% "sub"  %) enddefine;
define M_BIC    = m_op_3(% "bic" %) enddefine;
define M_BIS    = m_op_commute(% "orr"  %) enddefine;
define M_BIM    = m_op_commute(% "and"  %) enddefine;
define M_LOGCOM = m_op_2(% "mvn"  %) enddefine;


;;; different than m_op_commute because we need both arguments
;;; in registers
define M_MULT();
    lvars (, src1, src2, dst) = explode(m_instr), dreg;
    load_to_reg(src1, R1) -> src1;
    load_to_reg(src2, R5) -> src2;
    if isreg(dst) then
        dst -> dreg;
        false -> dst;
    else
        R1 -> dreg;
    endif;
    /* NOTE: ARM v5 theoretically can not produce result of
       multiplication in one of source registers.  But
       this is OK in v6 and probaly also in existing v5 */
    if dreg == src1 then
        (src1, src2) -> (src2, src1)
    endif;
    asm_emit("mul", dreg, src1, src2, 4);
    if dst then
        gen_reg_store(dreg, dst, R5);
    endif;
enddefine;

define M_NEG();
    lvars (, src, dst) = explode(m_instr);
    gen_op_3(0, src, dst, "rsb");
enddefine;

/*
Syntax:         M_PADD src1 src2 dest

Description:    Add POP integer contents of -src1- to POP integer contents
                of -src2- and put POP integer result in -dest-.

Operation:      dest:pint = src2:pint + src1:pint

Notes:          With normal POP integer representation and machine arithmetic:
                dest = src2 + (src1 - 0x3)
*/
define M_PADD = m_parith(% "add" %) enddefine;
define M_PSUB = m_parith(% "sub" %) enddefine;

/*
M_PADD_TEST                                       Add POP Integers With Test

Syntax:         M_PADD_TEST src1 src2 cond label

Description:    Add POP integer contents of -src1- to POP integer contents
                of -src2- and push the POP integer result on the stack. If
                the -cond- is true then branch to the -label- else continue.

Operation:      push (src2:pint + src1:pint) on user stack
                if cond then PC = label

Notes:          Calculation as for M_PADD.  Test is always NOVF.
*/
define M_PADD_TEST = m_parith_test(% "adds" %) enddefine;

/* Like M_PADD_TEST, but subtract */
define M_PSUB_TEST = m_parith_test(% "subs" %) enddefine;

define M_PTR_ADD_OFFS = m_ptr_op_commute(% "add" %) enddefine;
define M_PTR_SUB_OFFS = m_ptr_op_3(% "sub" %) enddefine;
define M_PTR_SUB      = m_ptr_op_3(% "sub" %) enddefine;

;;; M_ASH:
;;;     performs an arithmetic shift of src2 by an amount src1, leaving
;;;     result in dst. The shift may be right or left depending on the
;;;     sign of src1.

define M_ASH();
    lvars (, src1, src2, dst) = explode(m_instr),
          dreg;
    if isreg(dst) then
        dst -> dreg;
        false -> dst;
    else
        R1 -> dreg;
    endif;
    if isintegral(src1) then
        load_to_reg(src2, R1) -> src2;
        if src1 < -31 then -31 -> src1 endif;
        if src1 > 31 then
            asm_emit("mov", dreg, '#0', 3);
        else
            if src1 < 0 then
                asm_emit("mov", dreg, src2, 'asr #'>< -src1, 4);
            else
                asm_emit("mov", dreg, src2, 'asl #'>< src1, 4);
            endif;
        endif;
    else
        load_to_reg(src1, R5) -> src1;
        load_to_reg(src2, R1) -> src2;
        asm_emit("cmp", src1, '#0', 3);
        asm_emit("movge", dreg, src2, 'asl ' >< src1, 4);
        asm_emit("rsb", R5, src1, '#0', 4);
        asm_emit("movlt", dreg, src2, 'asr ' >< R5, 4);
    endif;
    if dst then
        gen_reg_store(dreg, dst, R5);
    endif;
enddefine;


/*
 *  Branches and Tests
 */

;;; gen_test_or_cmp:
;;;     plants code to test/compare src1 against src2, and jump to lab
;;;     on test. The test may be CMP (src2 - src1) or TEST (src1 &&
;;;     src2) determined by cmp_or_test; the test will be one of the M-code
;;;     test codes EQ, NEQ etc.

define lconstant gen_test_or_cmp(src1, src2, test, lab, cmp_or_test);
    lvars src1, src2, test, lab, cmp_or_test, op1, op2;
    get_operands(src1, src2) -> (op1, op2);
    asm_emit(cmp_or_test, op1, op2, 3);
    gen_branch('b' >< testop(test), lab);
enddefine;

define lconstant gen_cmp  = gen_test_or_cmp(% "cmp" %) enddefine;
define lconstant gen_test = gen_test_or_cmp(% "tst" %) enddefine;

;;; M_BIT:
;;;     tests an operand against a bit mask

define M_BIT();
    lvars (, mask, src, test, lab) = explode(m_instr);
    gen_test(src, mask, test, lab);
enddefine;

;;; M_TEST:
;;;     compares an operand with zero

define M_TEST();
    lvars (, src, test, lab) = explode(m_instr);
    gen_cmp(src, 0, test, lab);
enddefine;

;;; M_CMP:
;;;     compares two machine integers

define M_CMP();
    lvars (, src1, src2, test, lab) = explode(m_instr);
    gen_cmp(src1, src2, test, lab);
enddefine;

;;; M_PCMP:
;;;     compares two POP integers (same as M_CMP)

define M_PCMP();
    lvars (, src1, src2, test, lab) = explode(m_instr);
    gen_cmp(src1, src2, test, lab);
enddefine;

;;; M_PTR_CMP:
;;;     compares two pointers; this is the same as comparing machine
;;;     integers regardless of the pointer type.

define M_PTR_CMP();
    lvars (, /*type*/, src1, src2, test, lab) = explode(m_instr);
    gen_cmp(src1, src2, test, lab);
enddefine;

;;; M_CMPKEY:
;;;     compares the key of an item with a given key

define M_CMPKEY();
    lvars (, key, src, test, lab) = explode(m_instr);
    ;;; test for a simple item first
    lvars tlab = if test == "EQ" then genlab() else lab endif;
    load_to_reg(src, R5) -> src;
    gen_test(src, 1, "NEQ", tlab);
    ;;; item is compound: get its key
    {% src, field_##("KEY").wof %} -> src;
    if isintegral(key) then
        ;;; testing flag(s) nonzero in K_FLAGS field:
        ;;; get key to register
        gen_move(src, R5 ->> src);
        ;;; test the flags
        gen_test(key, {% src, field_##("K_FLAGS").wof %}, negate_test(test), lab);
    else
        ;;; test for specific key
        gen_cmp(key, src, test, lab);
    endif;
    if test == "EQ" then asmLABEL(tlab) endif;
enddefine;

;;; gen_switch:
;;;     plants a computed goto on the integer src: this may be a system
;;;     integer or a POP integer depending on the flag sysint.
;;;     labs is a list of the labels to jump to, counted from 1.
;;;     If -else_case- is <true>, the instruction is followed by a default
;;;     case for a value out of range; if <false>, there is an error case
;;;     following which expects the out-of-range value on the stack.

define lconstant gen_switch(src, labs, else_case, sysint);
    lvars src, labs, else_case, sysint;
    lvars else_lab = genlab();
    lvars ncases = listlength(labs);
    load_to_reg(src, R1) -> src;
    lvars sreg = src;
    ;;; printf('clear pop bits\n');
    ;;; clear pop bits when POP integer
    if not(sysint) then
        asm_emit("sub", R0, src, 3, 4);
        R0 -> sreg;
    endif;
    ;;; Check it's in range: an unsigned comparison takes care of both the
    ;;; too large and too small cases.
    lvars opd2;
    ;;; printf('calling get_operand2\n');
    get_operand2(if sysint then ncases else popint(ncases) - 3 endif) -> opd2;
    asm_emit("cmp", sreg, opd2, 3);
    ;;; If in range, use it as an index into a jump table. The table is
    ;;; just a sequence of word labels. Table starts 8 bytes after
    ;;; indexing instruction.  Since pc register contains address of
    ;;; current instruction + 8 we can just use it as table address.
    ;;; For indexing we need to shift sysint, but can use corrected
    ;;; POP integer as is.
    asm_emit("ldrls", "pc", '[pc, ' >< sreg ><
             if sysint then ', asl #2]' else ']' endif, 3);
    asm_emit("b", else_lab, 2);
    ;;; Plant the table; it begins with -else_lab- to account for the 0 case
    asmALIGN();
    lvars lab;
    asm_emit("long", else_lab, explode(labs), ncases + 2);
    ;;; Plant the else case
    asmLABEL(else_lab);
    if not(else_case) then
        asm_emit("str", src, '[r10, #-4]!', 3);
    endif;
enddefine;

;;; M_LABEL <label>:
;;;     plants a label

define M_LABEL();
    asmLABEL(m_instr(2));
enddefine;

;;; M_BRANCH <label>:
;;;     unconditional jump to label

define M_BRANCH();
    gen_branch("b", m_instr(2));
enddefine;

;;; M_BRANCH_std:
;;;     same as M_BRANCH, but guarantees to produce an instruction of a
;;;     fixed size.  Since ARM instructions have fixed length it
;;;     is really the same...

define M_BRANCH_std();
    gen_branch("b", m_instr(2));
enddefine;

;;; M_BRANCH_ON:
;;;     computed goto on a POP integer.

define M_BRANCH_ON();
    lvars (, src, labs, else_case) = explode(m_instr);
    gen_switch(src, labs, else_case, false);
enddefine;

;;; M_BRANCH_ON_INT:
;;;     computed goto on a system integer. This will always have an else
;;;     case.

define M_BRANCH_ON_INT();
    lvars (, src, labs) = explode(m_instr);
    gen_switch(src, labs, true, true);
enddefine;


/*
 *  Procedure Call and Return
 */

;;; get_exec_opd:
;;;     computes an operand which can be used as the target of a call or an
;;;     unconditional jump to execute the procedure opd. Such an operand
;;;     has an extra level of indirection over that already present in opd.
;;;     If opd is a POP procedure, the computed operand must refer to its
;;;     execute address.

define lconstant get_exec_opd(opd, is_pop_pdr);
    lvars opd, is_pop_pdr, tmp;
    if isimm(opd) then
        ;;; it must be the immediate label of a system procedure
        if is_pop_pdr then execlabof(cont(opd), true) else cont(opd) endif;
    else
        if is_pop_pdr then
            load_to_reg(opd, R0) -> tmp;
            if tmp /== R0 then
                asm_emit("mov", R0, tmp, 3);
                R0 -> tmp;
            endif;
            asm_emit("ldr", R1, '[' >< R0 >< ', #' ><
                                  field_##("PD_EXECUTE").wof >< ']', 3);
            R1
        else
            load_to_reg(opd, R5)
        endif;
    endif;
enddefine;

define lconstant gen_call_or_chain(opd, opcode, is_pop_pdr);
    lvars opd, opcode, is_pop_pdr;
    ;;; printf(opd, 'gen_call_or_chain(%p, ');
    ;;; printf(opcode, '%p, ');
    ;;; printf(is_pop_pdr, '%p)\n');
    lvars target = get_exec_opd(opd, is_pop_pdr);
;;;    if isimm(opd) and opcode == "bx" then
;;;        "b" -> opcode;
;;;    endif;
;;;    if not(isimm(opd)) and opcode == "bl" then
;;;        "blx" -> opcode;
;;;    endif;
    gen_transfer(opcode, target);
enddefine;

define gen_chain(opd, is_pop_pdr);
    lvars opd, is_pop_pdr;
    ;;; Put our return address back in LR
    ;;; asm_emit("ldr", LR, '[sp], #4', 3);
    ;;; Jump
    gen_call_or_chain(opd, "b", is_pop_pdr);
enddefine;

define lconstant m_chain(is_pop_pdr);
    lvars is_pop_pdri;
    gen_chain(m_instr(2), is_pop_pdr);
enddefine;

define lconstant m_call(is_pop_pdr);
    lvars is_pop_pdr;
    gen_call_or_chain(m_instr(2), "bl", is_pop_pdr);
enddefine;

define M_CALL  = m_call(% true %) enddefine;
define M_CHAIN = m_chain(% true %) enddefine;

define M_CALL_WITH_RETURN();
    lvars tmp;
    ;;; Set return address in LR
    load_to_reg(m_instr(3), LR) -> tmp;
    if tmp /== LR then
        asm_emit("mov", LR, tmp, 3);
    endif;
    ;;; jump to the procedure
    gen_call_or_chain(m_instr(2), "b", true);
enddefine;

;;; {M_CALLSUB <subroutine_opd> <args ...>}
;;;     call subroutine, passing arguments (0-3) in registers.
;;;     Subroutine will always be constant when arguments are present

define M_CALLSUB();
    lvars l = datalength(m_instr);
    if l == 6 then    gen_move(m_instr(3),   R3) endif; ;;; arg_reg_3
    if l fi_>= 5 then gen_move(m_instr(l - 2), R2) endif; ;;; arg_reg_2
    if l fi_>= 4 then gen_move(m_instr(l - 1), R1) endif; ;;; arg_reg_1
    if l fi_>= 3 then gen_move(m_instr(l),   R0) endif; ;;; arg_reg_0
    gen_transfer("bl", get_exec_opd(m_instr(2), false));
enddefine;

define M_CHAINSUB = m_chain(% false %) enddefine;

define M_RETURN(); asm_emit("bx", LR, 2); enddefine;


/*
 *  Procedure Entry and Exit
 */

lblock

lvars

    ;;; These variables are set by M_CREATE_SF and used by M_UNWIND_SF

    ;;; Names of dynamic local variables
    dlocal_labs,
    reg_spec,
    ;;; Number of on-stack vars
    Nstkvars,
    Nregs,
;

;;; {M_CREATE_SF <reg_locals> <Npopreg> <Nstkvars> <Npopstkvars>
;;;             <dlocal_labs> <ident reg_spec>}
;;;     plant code to construct procedure stack frame

define M_CREATE_SF();
    lconstant popint_zero = popint(0);
    lvars reg_spec_id, Npopregs, Npopstkvars, reg_locals, n, regmask,
          tmp, j;

    explode(m_instr) -> reg_spec_id -> dlocal_labs -> Npopstkvars
        -> Nstkvars -> Npopregs -> reg_locals -> ;

    ;;; FIXME: compute sensible value
    0 -> idval(reg_spec_id);

    listlength(reg_locals) -> Nregs;

    0 -> regmask;
    fast_for n in reg_locals do regmask || (1 << n) -> regmask endfast_for;

    ;;; Save registers
    '{' -> reg_spec;
    for n from 0 to 12 do
        if regmask &&/=_0 (1 << n) then
            reg_spec >< 'r' >< n >< ', ' -> reg_spec;
        endif;
    endfor;
    reg_spec >< 'lr}' -> reg_spec;
    asm_emit("stmfd", 'sp!', reg_spec, 3);

    ;;; Setup PB
    asm_emit("ldr", PB, '[pc, #-16]', 3);

    ;;; printf(dlocal_labs, 'M_CREATE_SF: Push dynamic locals %p\n');
    ;;; Push dynamic locals
    applist(dlocal_labs, push_operand);
    ;;; printf('Pushed dynamic locals\n');
    ;;; Clear POP registers and allocate POP variables
    false -> tmp;
    1 -> j;
    if Npopregs > 0 then
        for n from 12 by -1 to 0 do
            if regmask &&/=_0 (1 << n) then
                if tmp then 
                    gen_move(tmp, reglabel(n));
                else
                    reglabel(n) -> tmp;
                    gen_move(popint_zero, reglabel(n));
                endif;
                j + 1 -> j;
                quitif(j > Npopregs);
            endif;
        endfor;
    endif;
    ;;; Allocate POP on-stack lvars (initialised to zero)
    if not(tmp) and Npopstkvars > 0 then
        reglabel(0) -> tmp;
        gen_move(popint_zero, reglabel(0));
    endif;
    repeat Npopstkvars times push_operand(tmp) endrepeat;
    ;;; Allocate non-POP on-stack lvars (uninitialised)
    if Nstkvars /== Npopstkvars then
        gen_op_3((Nstkvars - Npopstkvars) * 4, SP, SP, "sub");
    endif;
    ;;; Push the owner address
    ;;; printf('Push the owner address\n');
    push_operand(PB);
enddefine;

;;; {M_UNWIND_SF}
;;;     plant code to unwind a procedure stack frame

define M_UNWIND_SF();
    ;;; Remove owner address and on-stack vars (POP and non-POP)
    gen_op_3((Nstkvars + 1) * 4, SP, SP, "add");
    ;;; Pop dynamic locals
    applist(rev(dlocal_labs), pop_operand);
        
    ;;; restore registers
    asm_emit("ldmfd", 'sp!', reg_spec, 3);
    ;;; Restore procedure base register from previous owner address
    gen_move({^SP}, PB);
enddefine;

endlblock;

;;; {M_END}
;;;     end a procedure

define M_END();
enddefine;


/*
 *  Special instructions
 */

;;; {M_CLOSURE <frozvals> <pdpart opnd>}
;;;     plant closure code

define M_CLOSURE();
    lvars (, frozvals, pdpart_opd) = explode(m_instr);
    lvars nfroz = listlength(frozvals);
    ;;; mishap(frozvals, pdpart_opd, 2, 'Unimplemented M_CLOSURE');
;;;    printf(current_pdr_label, 'current_pdr_label = %p\n');
    lvars lab = genlab();
    ;;; Get closure address into r0
    asm_emit("ldr", R0, lab, 3);
    if nfroz fi_> 16 then
        ;;; for more than 16 frozvals, call Exec_closure
        gen_move(R0, -_USP);
        perm_const_opnd([Sys Exec_closure]) -> pdpart_opd;
    else
        ;;; push the frozvals
        lconstant frozval_offset = field_##("PD_CLOS_FROZVALS").wof;
        lvars i = 0;
/*
        while i + 4 < nfroz do
            asm_emit("ldr", R4, '[r0, #' >< frozval_offset + i*4 >< ']', 3);
            asm_emit("ldr", R3, '[r0, #' >< frozval_offset +
                                            (i + 1)*4 >< ']', 3);
            asm_emit("ldr", R2, '[r0, #' >< frozval_offset +
                                             (i + 2)*4 >< ']', 3);
            asm_emit("ldr", R1, '[r0, #' >< frozval_offset +
                                             (i + 3)*4 >< ']', 3);
            i + 4 -> i;
            asm_emit("stmfd", 'r10!', '{r1-r4}', 3);
        endwhile;
*/
        for i from 0 to nfroz - 1 do
            asm_emit("ldr", R1, '[r0, #' >< (frozval_offset + i*4) >< ']', 3);
            asm_emit("str", R1, '[r10, #-4]!', 3)
        endfor;
        if not(pdpart_opd) then
            {% R0, field_##("PD_CLOS_PDPART").wof %} -> pdpart_opd;
        endif;
    endif;
    gen_chain(pdpart_opd, true);
    asmLABEL(lab);
    asm_emit("long", current_pdr_label, 2);
enddefine;

;;; {M_PLOG_IFNOT_ATOM <ifnot_lab>}
;;;     test result of _prolog_unify_atom

define M_PLOG_IFNOT_ATOM();
    mishap(0, 'Unimplemented M_PLOG_IFNOT_ATOM');
enddefine;

;;; {M_PLOG_TERM_SWITCH <fail_lab> <var_lab> <dst>}
;;;     test result from _prolog_pair_switch/_prolog_term_switch
;;;     If EQ, move R0 (dereferenced result) to <dst>

define M_PLOG_TERM_SWITCH();
    mishap(0, 'Unimplemented M_PLOG_TERM_SWITCH');
enddefine;

;;; {M_SETSTKLEN <offset of stack increase> <popint saved stklen opnd>}
;;;     adjust the number of results returned by a Lisp function.
;;;     <offset> is always a constant integer

define M_SETSTKLEN();
    lvars (, offs, sl) = explode(m_instr), wreg;
    ;;; compute desired user stack length as saved length plus offset;
    ;;; subtract 3 to account for popint bits
    load_to_reg(sl, R0) -> wreg;
    if offs == 0 then
        asm_emit("sub", R1, wreg, 3, 4);
    else
        gen_op_commute("add", wreg, offs - 3, R1);
    endif;
    ;;; compute desired stack pointer in R0
    load_to_reg(identlabel("\^_userhi"), R0);
    asm_emit("sub", R0, R0, R1);
    ;;; compare desired and actual user stack pointers, if equal, jump to end,
    lvars lab = genlab();
    gen_cmp(R0, USP, "EQ", lab);
    ;;; otherwise call "setstklen_diff" to fix
    gen_transfer("bl", symlabel("\^_setstklen_diff"));
    asmLABEL(lab);
enddefine;

;;; M_ERASE:
;;;     pop to a register; have to do the move, in case the address is
;;;     invalid (e.g., stack empty)

define M_ERASE();
    gen_move(m_instr(2), R1);
enddefine;


/*
 *  Generate assembly code
 */

define lconstant generate(codelist, hdr_len) -> (ilist, new_literals);
    lvars codelist, hdr_len, ilist;
    dlocal  m_instr, last_instr,
            new_literals = [], lit_offset = hdr_len - 2;
    conspair({#}, []) ->> ilist -> last_instr;
    asmLABEL(current_pdr_exec_label);
    for m_instr in codelist do
#_IF DEF M_DEBUG
        ;;; add comment to assembly code listing
        lvars len;
        "#", destvector(m_instr) -> len;
        pdprops(subscr_stack(len)) -> subscr_stack(len);
        asm_emit(len fi_+ 1);
#_ENDIF
        lvars opcode = f_subv(1, m_instr);
        if isprocedure(opcode) then
            fast_apply(opcode);
        else
            mishap(opcode, 1, 'UNKNOWN M-OPCODE');
        endif;
    endfor;
enddefine;


;;; === CODE OUTPUT ===================================================

;;; outopd, outinst:
;;;     write out an operand/instruction. These differ considerably
;;;     depending on the assembler type

define lconstant outopnd(opd);
    lvars opd;
    if ispair(opd) then
        mishap(opd, 1, 'outopnd: unhandled operand\n');
    endif;
    if isreg(opd) then
        asmf_printf(opd, '%p');
    elseif isimm(opd) then
        asmf_printf(immval(opd), '#%p');
    elseif isabs(opd) then
        asmf_printf(opd, '%p');
    elseif isvector(opd) then
        mishap(opd, 1, 'outopnd: unhandled operand\n');
    else
        mishap(opd, 1, 'ILLEGAL OPERAND');
    endif;
enddefine;

;;; outinst:
;;;     writes out an instruction

define lconstant outinst(instr);
    lvars instr;
    lconstant COMMENT = `@`;
    lvars opcode = f_subv(1, instr);
    ;;; printf(instr, 'outinst(%p)\n');
    if opcode == "label" then
        outlab(f_subv(2, instr));
    elseif opcode == "align" then
        asm_align_word();
    elseif opcode == "long" then
        asm_outword(destvector(instr) fi_- 1) -> ;
    else
        lvars i, n = datalength(instr);
        if opcode == "#" then
            asmf_printf(COMMENT, '\t%c');
            for i from 2 to n do
                asmf_printf(f_subv(i, instr), '\s%p');
            endfor;
        else
            asmf_printf(opcode, '\t%p\t');
            unless n == 1 then
                outopnd(f_subv(2, instr));
                for i from 3 to n do
                    asmf_printf(',\s');
                    outopnd(f_subv(i, instr));
                endfor;
            endunless;
        endif;
        asmf_charout(`\n`);
    endif;
enddefine;


;;; === GENERATING PROCEDURE AND CLOSURE CODE =========================

;;; mc_code_generator:
;;;     generates assembler code for a procedure/closure.
;;;     It returns:
;;;         - a label, which will be set to the procedure size in words;
;;;         - a procedure to output the generated assembly code.
;;;     This is called from "m_trans".
;;;     The global variables
;;;         current_pdr_label, current_pdr_exec_label
;;;     contain the current procedure's label and start-of-code label

define mc_code_generator(codelist, hdr_len) -> (gencode, pdr_len);
    lconstant procedure gencode;
    lvars codelist, hdr_len, pdr_len, new_lits,
          cur_pdr_label = current_pdr_label;

    ;;; printf(codelist, 'mc_code_generator, codelist = %p\n');

    ;;; Translate M-code to assembler
    generate(codelist, hdr_len) -> (codelist, new_lits) ;

    ;;; Create a label for the procedure length
    genlab() -> pdr_len;

    ;;; Create the code-output procedure
    define lconstant gencode();
        lvars lit, endlab;
        ;;; Output literals
        fast_for lit in new_lits do
            asm_outword(lit, 1)
        endfor;
        ;;; Output address of procedure record
        asm_outword(cur_pdr_label, 1);
        ;;; Output the code
        applist(codelist, outinst);
        ;;; Align on a longword boundary
        asm_align_word();
        ;;; Plant an end label
        outlab(genlab() ->> endlab);
        ;;; Define pdr_len as the size in words of the procedure
        outlabset(pdr_len,
                  asm_pdr_len(hdr_len, current_pdr_exec_label, endlab));
    enddefine;
enddefine;


;;; === OTHER DEFINITIONS NEEDED BY "m_trans.p" ==========================


constant

    ;;; M-code tables for machine-dependent in-line subroutines.
    ;;; These are added to the corresponding properties in m_trans.p

    mc_inline_procs_list = [
        [ \^_ptr_to_offs  [{^M_ERASE ^USP_+}]]
        [ \^_offs_to_ptr  [{^M_ERASE ^USP_+}]]
        [ \^_int          [{^M_ASH -2 ^USP_+ ^ -_USP}]]
        [ \^_pint         [{^M_ASH 2 ^USP_+ ^ -_USP}
                            {^M_ADD 3 ^USP_+ ^ -_USP}]]
        [ \^_por          [{^M_BIS ^USP_+ ^USP_+ ^ -_USP}]]
        [ \^_pand         [{^M_BIM ^USP_+ ^USP_+ ^ -_USP}]]
        [ \^_mksimple     [{^M_ADD 1 ^USP_+ ^ -_USP}]]
        [ \^_mkcompound   [{^M_SUB 1 ^USP_+ ^ -_USP}]]
        [ \^_mksimple2    [{^M_ADD 3 ^USP_+ ^ -_USP}]]
        [ \^_mkcompound2  [{^M_SUB 3 ^USP_+ ^ -_USP}]]
    ],

    mc_inline_conditions_list = [
        [ \^_iscompound   {^M_BIT  2:01 ^USP_+ EQ  ?}]
        [ \^_issimple     {^M_BIT  2:01 ^USP_+ NEQ ?}]
        [ \^_issimple2    {^M_BIT  2:10 ^USP_+ NEQ ?}]
        [ \^_isinteger    {^M_BIT  2:10 ^USP_+ NEQ ?}]
        [ \^_isaddress    {^M_BIT  2:11 ^USP_+ EQ  ?}]
    ],
;

    /*  Procedure to convert a pop integer subscript to an appropriate offset
        for the data type being accessed, used by OP_SUBV in m_trans.p to
        compile code for fast_subscrv, vectorclass field accesses, etc.
        scale is the scale for the data type involved; the results are
        the M-code instructions (if any) necessary to convert the subscript
        on top of the stack to an offset, plus a constant correction to be
        added.
    */
define cvt_pop_subscript(scale);
    lvars pow, scale;
    if is_power2(scale) ->> pow then
        ;;; pow-2 accounts for popint being shifted left 2
        unless (pow-2 ->> pow) == 0 then
            {^M_ASH ^pow ^USP_+ ^ -_USP}
        endunless,
        -(popint(0) << pow)     ;;; additive correction to remove popint bits
    else
        ;;; just convert to sysint and multiply
        {^M_ASH -2 ^USP_+ ^ -_USP},     ;;; _int()
        {^M_MULT ^scale ^USP_+ ^ -_USP},
        0                       ;;; no correction necessary
    endif
enddefine;


endsection;     /* Genproc */

endsection;     /* $-Popas$-M_trans */

