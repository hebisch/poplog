/*
 > File:        $usepop/src/syscomp/riscv/genproc.p
 > Purpose:     Compiles M-Code to Risc-V 64-bit assembler
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
        M_UPDs  ;;; apparently unused

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
   x0     - zero                x1 - link register/work
   x2/sp  - stack pointer       x3/gp -
   x4/tp  -                     x5 -
   x6/t1  - work                x7/t2 - work
   x8/s0  - user stack pointer  x9/s1 - procedure base
   x10/a0 - work/arg0           x11/a1 - work/arg1
   x12/a2 - work/arg2           x13/a3 - WK_ADDR_REG_1
   x14/a4 - WK_ADDR_REG_2       x15/a5 - work
   x16/a6 - work                x17/a7 - work
   x18/s2 - non pop lvar        x19/s3 - non pop lvar
   x20/s4 - non pop lvar        x21/s5 - non pop lvar
   x22/s6 - non pop lvar        x23/s7 - pop lvar
   x24/s8 - pop lvar            x25/s9 - pop lvar
   x26/s10 - pop lvar           x27/s11 - pop lvar
   x28/t3 -                     x29/t4 -
   x30/t5 -                     x31/t4 -

*/

lconstant

    ;;; Risc-V register names and their usage

    A0 = "a0",    ;;; WK_REG/work reg ???/arg_reg_0
    A1 = "a1",    ;;; principal work reg/arg_reg_1
    A2 = "a2",    ;;; CHAIN_REG/arg_reg_2
    A3 = "a3",    ;;; WK_ADDR_REG_1
    A4 = "a4",    ;;; WK_ADDR_REG_2
    A5 = "a5",    ;;; secondary work reg
    S0 = "s0",    ;;; USP
    S1 = "s1",    ;;; PB
    LR = "ra",    ;;; link register
;

constant

    SP = "sp",    ;;; stack pointer

    ;;; POPC register operands

    ;;; USP: user stack pointer

    USP = S0,

    ;;; WK_REG: used by "m_optimise" to eliminate user stack pushes and
    ;;; pops between successive M-code instructions.  Must be preserved
    ;;; by M_MOVE.  Any instruction should handle WK_REG as source
    ;;; or destination.

    WK_REG = A0,

    ;;; CHAIN_REG: used to save procedure operands for in-line chaining
    ;;; and to save return addresses for out-of-line chaining through
    ;;; the subroutines "_syschain" and "_sysncchain". It must not be
    ;;; touched by M_UNWIND_SF.

    CHAIN_REG = A2,

    ;;; WK_ADDR_REG_1, WK_ADDR_REG_2: used for building field-access
    ;;; operands of the form {reg offset} when an existing operand
    ;;; cannot be deferred directly (can_defer_operand having returned
    ;;; <false> for it). REG_1 is used for source operands, REG_2 for
    ;;; destination operands.

    WK_ADDR_REG_1 = A3,
    WK_ADDR_REG_2 = A4,

    ;;; USP_+ : pop from the user stack
    ;;; -_USP : push on the user stack
    ;;; i_USP : top of user stack

    USP_+ = {^USP ^true},
    -_USP = {^USP ^false},
    i_USP = {^USP 0},

    ;;; i_USP_+ : not supported
    ;;; ii_USP  : not supported

    ;;; Lists of pop/non-pop registers for register locals

    ;;; Poplog assumes that calls preserve register locals.  Since
    ;;; we currently call C for runtime support we only can use
    ;;; registers preserved by C.
    pop_registers = [[] 23 24 25 26 27],
    ;;; pop_registers = [[]],
    nonpop_registers = [[] 18 19 20 21 22],
    ;;; nonpop_registers = [[]],
;

;;; reglabel:
;;;     maps register numbers to names as used in the "reg" field of
;;;     instructions

define reglabel = newassoc([
    [0 x0]  [1 ^LR] [2 ^SP] [3 x3] [4 x4] [5 t0]
    [6 t1]  [7 t2]  [8 ^S0] [9 ^S1]
    [10 a0] [11 a1] [12 a2] [13 a3] [14 a4] [15 a5] [16 a6] [17 a7]
    [18 s2] [19 s3] [20 s4] [21 s5] [22 s6] [23 s7]
    [24 s8] [25 s9] [26 s10] [27 s11]
    [28 t3] [29 t4] [30 t5] [31 t6]
    ])
enddefine;

;;; regnumber:
;;;     the inverse mapping

define regnumber = newassoc([]); enddefine;

procedure();
        lvars n;
        for n from 0 to 31 do
                n -> regnumber(reglabel(n));
        endfor
endprocedure();

;;; Local register operands:

lconstant

    ;;; PB: procedure base register.
    ;;; This points to the start of the current procedure record, allowing
    ;;; access to values there.

    PB = S1,
;

;;; autoidreg:
;;;     indicates whether a register supports auto-increment.  None
;;;     on the Risc-V.  But since only little can be defered, we
;;;     probably can handle auto-increment better than generic
;;;     optimizer.

/*
define autoidreg() with_nargs 1;
    ->, false;
enddefine;
*/
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

define lconstant is_small_int(disp);
    isintegral(disp) and disp > -2048 and disp < 2048;
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
    if isreg(opd) and is_small_int(dis) and acctype == T_WORD then
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
    return((8*(disp + lit_offset)) >< '(' >< PB >< ')');
enddefine;

define lconstant load_literal(lit, tmp);
    lvars lit, tmp;
    asm_emit("ld", tmp, get_literal_addr(lit), 3);
enddefine;

define lconstant get_autoindex(opd);
    lvars base, dir, unit, type;
    if isvector(opd) and datalength(opd) >= 2 and
                         (isboolean(f_subv(2, opd) ->> dir)) then
        '8' -> unit;
        if datalength(opd) == 3 then
            f_subv(3, opd) && t_BASE_TYPE -> type;
            if type == t_DOUBLE then '8'
            elseif type == t_INT then '4'
            elseif type == t_SHORT then '2'
            elseif type == t_BYTE then '1'
            else
                mishap(opd, 1, 'Unhandled operand type');
            endif -> unit;
        endif;
        (unit, dir, f_subv(1, opd))
    else
        (0, false, false);
    endif
enddefine;

define lconstant get_addressable_op(opd, tmp);
    lvars opd, disp, opd1, type;
    returnif(isreg(opd))(opd);
    if isvector(opd) then
        if datalength(opd) = 1 then
            return( '(' >< f_subv(1, opd) >< ')');
        elseif datalength(opd) >= 2 then
            f_subv(2, opd) -> disp;
            f_subv(1, opd) -> opd1;
            if disp = 0 then
                    '(' >< opd1 >< ')' -> opd1;
            elseif is_small_int(disp) then
                    disp >< '(' >< opd1 >< ')' -> opd1;
            elseif isboolean(disp) then
                mishap(opd, 1,
                       'get_addressable_op: autoincrement not handled\n');
            elseif isinteger(disp) then
                ;;; FIXME: Implement real register allocation
                ;;; mishap(opd, 1,
                ;;;       'get_addressable_op: displacement out of range\n');
                load_literal(disp, LR);
                asm_emit("add", LR, LR, opd1, 4);
                '(' >< LR >< ')' -> opd1;
            else
                mishap(opd, 1, 'Unhandled operand in get_addressable_op');
            endif;
            return(opd1);
        endif;
        mishap(opd, 1, 'Unhandled operand in get_addressable_op');
    endif;
    if isinteger(opd) or isbiginteger(opd) then
        mishap(opd, 1, 'Want address of literal');
    endif;
    if isref(opd) then
        ;;; printf(opd, 'has rep opd: %p\n');
        fast_cont(opd) -> opd1;
        return(get_literal_addr(opd1));
    elseif isstring(opd) then
        ;;; printf(opd, 'has string opd: %p\n');
        load_literal(opd, tmp);
        return('(' >< tmp >< ')');
        ;;; return(get_literal_addr(opd));
    endif;
    mishap(opd, 1, 'Unhandled operand in get_addressable_op');
enddefine;

define lconstant load_to_reg(opd, tmp);
    lvars opd, tmp, opd1, opcode, type, n, signed,
          unit, dir, base;
    returnif(isreg(opd))(opd);
    if isinteger(opd) or isbiginteger(opd) then
        if -1024 <= opd and opd <= 1023 then
            asm_emit("li", tmp, '' >< opd, 3);
            return(tmp);
        elseif -2147481600 <= opd and opd < 2147481600 then
            opd && 16:FFF -> opd1;
            if opd1 >= 16:7FF then
                opd1 - 16:1000 -> opd1; 
            endif;
            ;;; Here assembler wants unsigned number
            16:FFFFFFFF && (opd - opd1) -> opd;
            asm_emit("lui", tmp, '' >< (opd >> 12), 3);
            if opd1 /= 0 then
                asm_emit("addi", tmp, tmp, '' >< opd1, 4);
            endif;
            return(tmp);
        else
            load_literal(opd, tmp);
            return(tmp);
        endif;
    endif;
    get_autoindex(opd) -> (unit, dir, base);
    if base then
        ;;; printf(unit, dir, base, 'autoindexing: unit=%p, dir=%p, base=%p\n');
        if not(isreg(base)) then
            mishap(opd, 1, 'Autoindexing but no register\n');
        endif;
        if not(dir) then
            asm_emit("addi", base, base, '-' >< unit, 4);
        endif;
        '(' >< base >< ')'
    else
        ;;; printf(opd, 'load_to_reg: strange operand %p\n');
        get_addressable_op(opd, tmp);
    endif -> opd1;
    "ld" -> opcode;
    if isvector(opd) and datalength(opd) == 3 then
        ;;; printf(opd, 'Typed operand %p\n');
        f_subv(3, opd) && t_BASE_TYPE -> type;
        ;;; printf(type, 'Base type: %p\n');
        if (type && tv_SIGNED) /== 0 then
            if type == t_DOUBLE then "ld"
            elseif type == t_INT then "lw"
            elseif type == t_SHORT then "lh"
            elseif type == t_BYTE then "lb"
            else
                mishap(opd, 1, 'Unhandled operand type')
            endif
        else
            if type == t_DOUBLE then "ld" 
            elseif type == t_INT then "lwu"
            elseif type == t_SHORT then "lhu"
            elseif type == t_BYTE then "lbu"
            else
                mishap(opd, 1, 'Unhandled operand type')
            endif
        endif -> opcode;
    endif;
    asm_emit(opcode, tmp, opd1, 3);
    if base and dir then
        asm_emit("addi", base, base, unit, 4);
    endif;
    return(tmp);
enddefine;

define lconstant gen_reg_store(src, dst, tmp);
    lvars src, dst, tmp, dst1, type, opcode, unit, dir, base;
    ;;; printf(src, dst, tmp, 'gen_reg_store(%p, %p, %p)\n');
    get_autoindex(dst) -> (unit, dir, base);
    if base then
        ;;; printf(unit, dir, base, 'autoindexing: unit=%p, dir=%p, base=%p\n');
        if not(isreg(base)) then
            mishap(dst, 1, 'Autoindexing but no register\n');
        endif;
        if not(dir) then
            asm_emit("addi", base, base, '-' >< unit, 4);
        endif;
        '(' >< base >< ')'
    else
        ;;; printf(dst, 'gen_reg_store: strange operand %p\n');
        get_addressable_op(dst, tmp);
    endif -> dst1;
    "sd" -> opcode;
    if isvector(dst) and datalength(dst) == 3 then
        f_subv(3, dst) && t_BASE_TYPE -> type;
        if type == t_DOUBLE then "sd"
        elseif type == t_INT then "sw"
        elseif type == t_SHORT then "sh"
        elseif type == t_BYTE then "sb"
        else
            mishap(dst, 1, 'Unhandled operand type');
        endif -> opcode;
    endif;
    asm_emit(opcode, src, dst1, 3);
    if base and dir then
        asm_emit("addi", base, base, unit, 4);
    endif;
enddefine;

define lconstant gen_transfer(opcode, target);
    lvars target, opcode;
    if isreg(target) then
        if opcode == "call" then "jalr" -> opcode; endif;
        if opcode == "b" then "br" -> opcode; endif;
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
        [GEQ    geu]
        [ULT    ltu]
        [ULEQ   leu]
        [UGT    gtu]
        [UGEQ   geu]
        [NEG    lt]
        [POS    gt]
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
        asm_emit("mv", dst, src, 3);
        ;;; printf('gen_move returning');
    else
        ;;; FIXME: Do it in general
        ;;; Special case to support
        ;;;   M_MOVE r10 {r10 <false>}
        if src == USP and dst == -_USP then
            asm_emit("mv", A1, src, 3);
            A1 -> src;
        else
            load_to_reg(src, A1) -> src;
        endif;
        ;;; NOTE: I_SWAP assumes that WK_REG = A0 will survive
        ;;; M_MOVE with arguments on user stack.  This is OK, since
        ;;; gen_reg_store does not need temporary when
        ;;; dst is on user stack.
        gen_reg_store(src, dst, A0);
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
    gen_move(size, A0);
    gen_move(offs, A1);
    gen_move(src, A2);
    gen_transfer("call", symlabel(routine));
    gen_move(A0, dst);
enddefine;

define M_MOVEbit  = gen_bfield(% "\^_bfield"  %) enddefine;
define M_MOVEsbit = gen_bfield(% "\^_sbfield" %) enddefine;

define M_UPDbit();
    lvars (, size, offs, dst, src) = explode(m_instr);
    gen_move(src, -_USP);
    gen_move(size, A0);
    gen_move(offs, A1);
    gen_move(dst, A2);
    gen_transfer("call", symlabel("\^_ubfield"));
enddefine;


/*
 *  Basic operations
 */

define is_int_opd(src, is_add);
    is_add and is_small_int(src)
enddefine;

define get_operand2_for_op(src, is_add);
    if is_int_opd(src, is_add) then src else load_to_reg(src, A5) endif;
enddefine;

define get_operands(src1, src2, is_add);
    lvars src1, src2;
    ;;; printf(src1, 'get_operands(%p, ');
    ;;; printf(src2, '%p)\n');
    load_to_reg(src1, A1);
    get_operand2_for_op(src2, is_add);
enddefine;

;;; Like get_operands, but user is supposed to switch order
;;; of operand (we need to perform loads in source order,
;;; to preserve order of side effects).
define get_operands_r(src1, src2, is_add);
    lvars src1, src2;
    get_operand2_for_op(src1, is_add);
    load_to_reg(src2, A1);
enddefine;

;;; gen_op_2:
;;;     plants code for a unary operation 'opcode' of the form:
;;;         dst := op(src)

define lconstant gen_op_2(src, dst, opcode);
    lvars src, dst, asm_op, dreg, op1;
    load_to_reg(src, A1) -> op1;
    if isreg(dst) then
        dst -> dreg;
        false -> dst;
    else
        A1 -> dreg;
    endif;
    asm_emit(opcode, dreg, op1, 3);
    if dst then
        gen_reg_store(dreg, dst, A5);
    endif;
enddefine;

;;; gen_op_3:
;;;     plants general 3 address binary operation 'opcode':
;;;         dst := src1 op src2

define lconstant gen_op_3(src1, src2, dst, is_add, opcode);
    lvars src1, src2, dst, opcode, op1, op2, dreg;
    if opcode == "sub" and isinteger(src1) then
        "add" -> opcode;
        -src1 -> src1;
    endif;
    get_operands_r(src1, src2, is_add) -> (op2, op1);
    if isreg(dst) then
        dst -> dreg;
        false -> dst;
    else
        A1 -> dreg;
    endif;
    if isreg(op2) then
        asm_emit(opcode, dreg, op1, op2, 4);
    else
        asm_emit(opcode >< 'i', dreg, op1, op2, 4);
    endif;
    if dst then
        gen_reg_store(dreg, dst, A5);
    endif;
enddefine;

;;; gen_op_commute:
;;;     plants code for a commutative binary operation 'opcode':
;;;         dst := src1 op src2

define lconstant gen_op_commute(src1, src2, dst, is_add, opcode);
    lvars src1, src2, dst, opcode;
    if is_int_opd(src2, is_add) then
        (src1, src2) -> (src2, src1)
    endif;
    gen_op_3(src1, src2, dst, is_add, opcode);
enddefine;

;;; m_op_*:
;;;     translate 2- and 3-operand M-code arithmetic/logical instructions
;;;     on machine integers;  calls corresponding gen_... operation.

define lconstant m_op_2(opcode);
    lvars opcode, (, src, dst) = explode(m_instr);
    gen_op_2(src, dst, opcode);
enddefine;

define lconstant m_op_commute(opcode, is_add);
    lvars opcode,
          (, src1, src2, dst) = explode(m_instr);
    gen_op_commute(src1, src2, dst, is_add, opcode);
enddefine;

define lconstant m_op_3(opcode, is_add);
    lvars opcode,
          (, src1, src2, dst) = explode(m_instr);
    gen_op_3(src1, src2, dst, is_add, opcode);
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
        load_to_reg(src1, A5) -> src1;
        asm_emit("addi", A5, src1, -3, 4);
        A5 -> src1;
    endif;
    gen_op_3(src1, src2, dst, true, opcode);
enddefine;

define lconstant m_parith_test(opcode);
    ;;; mishap(0, 'm_parith_test is unimplemented\n');
    lvars gen_p, (, src1, src2, test, lab) = explode(m_instr);
    if test /== "NOVF" then
        mishap(test, 1, 'm_parith_test: unexpected test\n');
    endif;
    if isintegral(src1) then
        src1 - 3 -> src1;
        load_to_reg(src1, A5) -> src1;
    else
        load_to_reg(src1, A5) -> src1;
        asm_emit("addi", A5, src1, -3, 4);
        A5 -> src1;
    endif;
    load_to_reg(src2, A1) -> src2;
    if opcode == "add" then
        asm_emit("add", "t0", src1, src2, 4);
        gen_reg_store("t0", -_USP, "t1");
        ;;; Overflow means that signs of src1 and src2 are the same
        ;;; but sign of result is different.

        ;;; After two xor-s below sign bit of t1 is one if and only if
        ;;; src1 and src2 have the same signs
        asm_emit("xor", "t1", src1, src2, 4);
        asm_emit("xor", "t1", "t1", "x0", 4);
        ;;; After xor below sign bit of t0 is zero if and only if src2 and
        ;;; the sum in machine arithmetic have the same signs
        asm_emit("xor", "t0", "t0", src2, 4);
        asm_emit("and", "t0", "t1", "t0", 4);
        asm_emit("blt", "t0", "x0", lab, 4);
    else
        asm_emit("sub", "t0", src2, src1, 4);
        gen_reg_store("t0", -_USP, "t1");
        ;;; Overflow means that signs of src1 and src2 are different
        ;;; but sign of the result is different than sign of src2
        ;;; After xor below sign bit of t1 is one if and only if
        ;;; src1 and src2 have different signs
        asm_emit("xor", "t1", src1, src2, 4);
        ;;; After xor below sign bit of t0 is zero if and only if src2 and
        ;;; the sum in machine arithmetic have the same signs
        asm_emit("xor", "t0", "t0", src2, 4);
        asm_emit("and", "t0", "t1", "t0", 4);
        asm_emit("blt", "t0", "x0", lab, 4);
    endif;
enddefine;

;;; ptr_arith:
;;;     plants code for an operation on pointers. Pointers are just
;;;     machine integers, so their code-planting procedures can be used
;;;     directly. The type field of the instruction is ignored.

define lconstant m_ptr_op_3(opcode);
    lvars gen_p, (, /*type*/, offs, ptr, dst) = explode(m_instr);
    gen_op_3(offs, ptr, dst, true, opcode);
enddefine;

define lconstant m_ptr_op_commute(opcode);
    lvars gen_p, (, /*type*/, offs, ptr, dst) = explode(m_instr);
    gen_op_commute(offs, ptr, dst, true, opcode);
enddefine;

define M_ADD    = m_op_commute(% "add", true %) enddefine;
define M_SUB    = m_op_3(% "sub", true %) enddefine;
define M_BIS    = m_op_commute(% "or", true %) enddefine;
define M_BIM    = m_op_commute(% "and", true %) enddefine;
define M_LOGCOM = m_op_2(% "not"  %) enddefine;

define M_BIC();
    lvars (, src1, src2, dst) = explode(m_instr);
    lvars dreg = A1;
    load_to_reg(src1, A1) -> src1;
    if isreg(dst) then
        dst -> dreg;
        false -> dst;
    endif;
    asm_emit("not", A1, src1, 3);
    if is_int_opd(src2, true) then
        asm_emit("and", dreg, A1, '' >< src2, 4);
    else
        asm_emit("and", dreg, A1, load_to_reg(src2, A5), 4);
    endif;
    if dst then
        gen_reg_store(dreg, dst, A5);
    endif;
enddefine;


;;; different than m_op_commute because we need both arguments
;;; in registers
define M_MULT();
    lvars (, src1, src2, dst) = explode(m_instr), dreg;
    load_to_reg(src1, A1) -> src1;
    load_to_reg(src2, A5) -> src2;
    if isreg(dst) then
        dst -> dreg;
        false -> dst;
    else
        A1 -> dreg;
    endif;
    asm_emit("mul", dreg, src1, src2, 4);
    if dst then
        gen_reg_store(dreg, dst, A5);
    endif;
enddefine;

define M_NEG();
    lvars (, src, dst) = explode(m_instr);
    gen_op_2(src, dst, "neg");
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
define M_PADD_TEST = m_parith_test(% "add" %) enddefine;

/* Like M_PADD_TEST, but subtract */
define M_PSUB_TEST = m_parith_test(% "sub" %) enddefine;

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
        A1 -> dreg;
    endif;
    if isintegral(src1) then
        load_to_reg(src2, A1) -> src2;
        if src1 < -63 then -63 -> src1 endif;
        if src1 > 63 then
            asm_emit("li", dreg, '0', 3);
        else
            if src1 < 0 then
                asm_emit("srai", dreg, src2, '' >< -src1, 4);
            else
                asm_emit("slli", dreg, src2, '' >< src1, 4);
            endif;
        endif;
    else
        load_to_reg(src1, A5) -> src1;
        load_to_reg(src2, A1) -> src2;
        asm_emit("bltz", src1, '1f', 3);
        asm_emit("sll", dreg, src2, src1, 4);
        asm_emit("j", '2f', 2);
        asm_emit("llabel", '1', 2);
        asm_emit("neg", A5, A5, 3);
        asm_emit("sra", dreg, src2, A5, 4);
        asm_emit("llabel", '2', 2);
    endif;
    if dst then
        gen_reg_store(dreg, dst, A5);
    endif;
enddefine;


/*
 *  Branches and Tests
 */

;;; gen_cmp:
;;;     plants code to compare src1 against src2, and jump to lab
;;;     on test. The test may be CMP (src2 - src1) or TEST (src1 &&
;;;     src2) determined by cmp_or_test; the test will be one of the M-code
;;;     test codes EQ, NEQ etc.

define lconstant gen_cmp(src1, src2, test, lab);
    lvars src1, src2, test, lab, cmp_or_test, op1, op2;
    get_operands(src1, src2, false) -> (op1, op2);
    asm_emit('b' >< testop(test), op1, op2, lab, 4);
enddefine;


define lconstant gen_test(src, mask, test, lab);
    gen_op_commute(mask, src, A5, true, "and");
    lvars opcode = if test = "EQ" then "beq" else "bne" endif;
    asm_emit(opcode, A5, "x0", lab, 4);
enddefine;

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
    load_to_reg(src, A5) -> src;
    gen_test(src, 1, "NEQ", tlab);
    ;;; item is compound: get its key
    {% src, field_##("KEY").wof %} -> src;
    if isintegral(key) then
        ;;; testing flag(s) nonzero in K_FLAGS field:
        ;;; get key to register
        gen_move(src, A5 ->> src);
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
    lvars table_start = genlab();
    lvars ncases = listlength(labs);
    load_to_reg(src, A1) -> src;
    lvars sreg = src;
    ;;; printf('clear pop bits\n');
    ;;; clear pop bits when POP integer
    if not(sysint) then
        asm_emit("addi", A0, src, -3, 4);
        asm_emit("sra", A0, A0, 2, 4);
        A0 -> sreg;
    endif;
    lvars opd2;
    load_to_reg(ncases, A5) -> opd2;
    ;;; Load table start to ra
    asm_emit("auipc", LR, '%hi(' >< table_start >< ')', 3);
    ;;; Check it's in range: an unsigned comparison takes care of both the
    ;;; too large and too small cases.
    asm_emit("bgt", sreg, opd2, '1f', 4);
    ;;; If in range, use opd2 as an index into a jump table. The table is
    ;;; just a sequence of word labels. Table start in ra was computed above.
    asm_emit("sll", sreg, sreg, 3, 4);
    asm_emit("add", LR, LR, sreg, 4);
    asm_emit("llabel", '1', 2);
    asm_emit("ld", A0, '%lo(' >< table_start >< ')(' >< LR ><')', 3);
    asm_emit("jr", A0, 2);
    ;;; Plant the table; it begins with -else_lab- to account for the 0 case
    asmALIGN();
    asmLABEL(table_start);
    asm_emit("long", else_lab, explode(labs), ncases + 2);
    ;;; Plant the else case
    asmLABEL(else_lab);
    if not(else_case) then
        asm_emit("addi", USP, USP, -8, 4);
        asm_emit("sd", src, USP, 3);
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
    gen_branch("j", m_instr(2));
enddefine;

;;; M_BRANCH_std:
;;;     same as M_BRANCH, but guarantees to produce an instruction of a
;;;     fixed size.  We use 4 byte alignment to get size 4.

define M_BRANCH_std();
    asm_emit("aligni", 1);
    gen_branch("j", m_instr(2));
    asm_emit("aligni", 1);
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
            load_to_reg(opd, A0) -> tmp;
            if tmp /== A0 then
                asm_emit("mv", A0, tmp, 3);
                A0 -> tmp;
            endif;
            asm_emit("ld", A1, field_##("PD_EXECUTE").wof >< '(a0)', 3);
            A1
        else
            load_to_reg(opd, A5)
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
    gen_call_or_chain(opd, "j", is_pop_pdr);
enddefine;

define lconstant m_chain(is_pop_pdr);
    lvars is_pop_pdri;
    gen_chain(m_instr(2), is_pop_pdr);
enddefine;

define lconstant m_call(is_pop_pdr);
    lvars is_pop_pdr;
    gen_call_or_chain(m_instr(2), "call", is_pop_pdr);
enddefine;

define M_CALL  = m_call(% true %) enddefine;
define M_CHAIN = m_chain(% true %) enddefine;

define M_CALL_WITH_RETURN();
    lvars tmp;
    ;;; Set return address in LR
    load_to_reg(m_instr(3), LR) -> tmp;
    if tmp /== LR then
        asm_emit("mv", LR, tmp, 3);
    endif;
    ;;; jump to the procedure
    gen_call_or_chain(m_instr(2), "j", true);
enddefine;

;;; {M_CALLSUB <subroutine_opd> <args ...>}
;;;     call subroutine, passing arguments (0-3) in registers.
;;;     Subroutine will always be constant when arguments are present

define M_CALLSUB();
    lvars l = datalength(m_instr);
    if l == 6 then    gen_move(m_instr(3),   A3) endif; ;;; arg_reg_3
    if l fi_>= 5 then gen_move(m_instr(l - 2), A2) endif; ;;; arg_reg_2
    if l fi_>= 4 then gen_move(m_instr(l - 1), A1) endif; ;;; arg_reg_1
    if l fi_>= 3 then gen_move(m_instr(l),   A0) endif; ;;; arg_reg_0
    gen_transfer("call", get_exec_opd(m_instr(2), false));
enddefine;

define M_CHAINSUB = m_chain(% false %) enddefine;

define M_RETURN(); asm_emit("jr", LR, 2); enddefine;


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
    frame_size,
    frame_ind,
    regmask,
;

define lconstant push_operand(opd);
    lvars opd;
    load_to_reg(opd, A1) -> opd;
    asm_emit("sd", opd, 8*frame_ind >< '(sp)', 3);
    frame_ind - 1 -> frame_ind;
enddefine;

define lconstant pop_operand(opd);
    lvars opd, reg;
    if isreg(opd) then opd else A1 endif -> reg;
    asm_emit("ld", reg, 8*frame_ind >< '(sp)', 3);
    frame_ind + 1 -> frame_ind;
    returnif(opd == reg);
    gen_reg_store(reg, opd, A5);
enddefine;

;;; {M_CREATE_SF <reg_locals> <Npopreg> <Nstkvars> <Npopstkvars>
;;;             <dlocal_labs> <ident reg_spec>}
;;;     plant code to construct procedure stack frame

define M_CREATE_SF();
    lconstant popint_zero = popint(0);
    lvars reg_spec_id, Npopregs, Npopstkvars, reg_locals, n,
          tmp, j, k;

    explode(m_instr) -> reg_spec_id -> dlocal_labs -> Npopstkvars
        -> Nstkvars -> Npopregs -> reg_locals -> ;

    listlength(reg_locals) -> Nregs;

    0 -> regmask;
    fast_for n in reg_locals do regmask || (1 << n) -> regmask endfast_for;

    regmask -> idval(reg_spec_id);

    ;;; Compute size of the stack frame
    ;;;  return address + registers + dlocals + stack_vars + owner address
    1 + Nregs + listlength(dlocal_labs) + Nstkvars + 1 -> frame_size;
    if not(frame_size mod 2 = 0) then
       mishap(frame_size, 1, 'M_CREATE_SF: odd frame size\n');
    endif;

    ;;; Setup PB
    asm_emit("lui", PB, '%hi(' >< current_pdr_label >< ')', 3);
    asm_emit("addi", PB, '%lo(' >< current_pdr_label >< ')', 3);

    ;;; Allocate stack frame and push the owner address
    if frame_size < 256 then
       asm_emit("addi", "sp", "sp", '-' >< (8*frame_size), 4);
    else
       ;;; need to change sp in stages, but our fames are not that
       ;;; big
       mishap(8*frame_size, 1, 'stack frame too big\n');
    endif;
    asm_emit("sd", PB, '0(sp)', 3);
    ;;; Save registers
    frame_size - 1 -> frame_ind;
    push_operand(LR);

    for n from 31 by -1 to 3 do
        if regmask &&/=_0 (1 << n) then
            push_operand(reglabel(n))
        endif;
    endfor;

    ;;; printf(dlocal_labs, 'M_CREATE_SF: Push dynamic locals %p\n');
    ;;; Push dynamic locals
    applist(dlocal_labs, push_operand);
    ;;; printf('Pushed dynamic locals\n');
    ;;; Clear POP registers and allocate POP variables
    false -> tmp;
    1 -> j;
    if Npopregs > 0 then
        for n from 31 by -1 to 3 do
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
        frame_ind - (Nstkvars - Npopstkvars) -> frame_ind;
    endif;
enddefine;

;;; {M_UNWIND_SF}
;;;     plant code to unwind a procedure stack frame

define M_UNWIND_SF();
    lvars n, tmp;
    ;;; Remove owner address and on-stack vars (POP and non-POP)
    Nstkvars + 1 -> frame_ind;
    ;;; Pop dynamic locals
    applist(rev(dlocal_labs), pop_operand);

    ;;; restore registers
    for n from 3 to 31 do
        if regmask &&/=_0 (1 << n) then
            pop_operand(reglabel(n))
        endif;
    endfor;
    pop_operand(LR);
    ;;; Dealocate frame and restore procedure base register from
    ;;; previous frame
    asm_emit("add", "sp", "sp", '' >< (8*frame_size), 4);
    asm_emit("ld", PB, '(sp)', 3);
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
    lvars lab = current_pdr_label;
    ;;; Get closure address into A0
    asm_emit("auipc", A0, '%hi(' >< lab >< ')', 3);
    asm_emit("addi", A0, A0, '%lo(' >< lab >< ')', 4);
    if nfroz fi_> 16 then
        ;;; for more than 16 frozvals, call Exec_closure
        gen_move(A0, -_USP);
        perm_const_opnd([Sys Exec_closure]) -> pdpart_opd;
    else
        ;;; push the frozvals
        lconstant frozval_offset = field_##("PD_CLOS_FROZVALS").wof;
        lvars i = 0;
/*
        while i + 4 < nfroz do
            asm_emit("ldr", A4, '[r0, #' >< frozval_offset + i*4 >< ']', 3);
            asm_emit("ldr", A3, '[r0, #' >< frozval_offset +
                                            (i + 1)*4 >< ']', 3);
            asm_emit("ldr", A2, '[r0, #' >< frozval_offset +
                                             (i + 2)*4 >< ']', 3);
            asm_emit("ldr", A1, '[r0, #' >< frozval_offset +
                                             (i + 3)*4 >< ']', 3);
            i + 4 -> i;
            asm_emit("stmfd", 'r10!', '{r1-r4}', 3);
        endwhile;
*/
        for i from 0 to nfroz - 1 do
            asm_emit("ld", A1, (frozval_offset + i*8) >< '(a0)', 3);
            asm_emit("addi", USP, USP, -8, 4);
            asm_emit("sd", A1, '(' >< USP >< ')', 3)
        endfor;
        if not(pdpart_opd) then
            {% A0, field_##("PD_CLOS_PDPART").wof %} -> pdpart_opd;
        endif;
    endif;
    gen_chain(pdpart_opd, true);
    ;;; asmLABEL(lab);
    ;;; asm_emit("long", current_pdr_label, 2);
enddefine;

;;; {M_PLOG_IFNOT_ATOM <ifnot_lab>}
;;;     test result of _prolog_unify_atom

define M_PLOG_IFNOT_ATOM();
    mishap(0, 'Unimplemented M_PLOG_IFNOT_ATOM');
enddefine;

;;; {M_PLOG_TERM_SWITCH <fail_lab> <var_lab> <dst>}
;;;     test result from _prolog_pair_switch/_prolog_term_switch
;;;     If EQ, move A0 (dereferenced result) to <dst>

define M_PLOG_TERM_SWITCH();
    mishap(0, 'Unimplemented M_PLOG_TERM_SWITCH');
enddefine;

;;; {M_SETSTKLEN <offset of stack increase> <popint saved stklen opnd>}
;;;     adjust the number of results returned by a Lisp function.
;;;     <offset> is always a constant integer

define M_SETSTKLEN();
    mishap(0, 'Unimplemented M_SETSTKLEN');
    lvars (, offs, sl) = explode(m_instr), wreg;
    ;;; compute desired user stack length as saved length plus offset;
    ;;; subtract 3 to account for popint bits
    load_to_reg(sl, A0) -> wreg;
    if offs == 0 then
        asm_emit("sub", A1, wreg, 3, 4);
    else
        gen_op_commute("add", wreg, offs - 3, A1);
    endif;
    ;;; compute desired stack pointer in A0
    load_to_reg(identlabel("\^_userhi"), A0);
    asm_emit("sub", A0, A0, A1);
    ;;; compare desired and actual user stack pointers, if equal, jump to end,
    lvars lab = genlab();
    gen_cmp(A0, USP, "EQ", lab);
    ;;; otherwise call "setstklen_diff" to fix
    gen_transfer("call", symlabel("\^_setstklen_diff"));
    asmLABEL(lab);
enddefine;

;;; M_ERASE:
;;;     pop to a register; have to do the move, in case the address is
;;;     invalid (e.g., stack empty)

define M_ERASE();
    gen_move(m_instr(2), A1);
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
        asmf_printf(immval(opd), '%p');
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
    lconstant COMMENT = '#';
    lvars opcode = f_subv(1, instr);
    ;;; printf(instr, 'outinst(%p)\n');
    if opcode == "label" then
        outlab(f_subv(2, instr));
    elseif opcode == "llabel" then
        asm_outlab(f_subv(2, instr));
    elseif opcode == "align" then
        asm_align_word();
    elseif opcode == "aligni" then
        asm_align_int();
    elseif opcode == "long" then
        asm_outword(destvector(instr) fi_- 1) -> ;
    else
        lvars i, n = datalength(instr);
        if opcode == "#" then
            asmf_printf(COMMENT, '\t%s');
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
    lvars codelist, hdr_len, pdr_len, new_lits;

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

