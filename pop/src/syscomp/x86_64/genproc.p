/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:        C.80386/src/syscomp/genproc.p
 > Purpose:     Compiles M-Code to Intel 80x86 assembler (for PC, etc.)
 > Author:      Robert Duncan, Aug 26 1988 (see revisions)
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
        M_MOVEb
        M_MOVEbit
                M_MOVEi
        M_MOVEs
        M_MOVEsb
        M_MOVEsbit
                M_MOVEsi
        M_MOVEss
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
        M_UPDb
        M_UPDbit
                M_UPDi
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
;


;;; === REGISTER USAGE ================================================

/*
   rax - work                rcx - work
   rdx - chain               rbx - pop user stack
   rsp - stack               rbp - base
   rsi - work                rdi - work
   r8  - non pop lvar        r9  - helper for immediate constants
   r10 - non pop lvar        r11 - non pop lvar
   r12 - non pop lvar        r13 - pop lvar
   r14 - pop lvar            r15 - pop lvar

*/

lconstant

    ;;; 80x86 register names and their usage

    EAX = "rax",    ;;; WK_REG/principal work reg/arg_reg_0
    ECX = "rcx",    ;;; secondary work reg/arg_reg_1
    EDX = "rdx",    ;;; CHAIN_REG/arg_reg_2
    EBX = "rbx",    ;;; USP
    ESP = "rsp",    ;;; SP
    EBP = "rbp",    ;;; PB (procedure base reg)
    ESI = "rsi",    ;;; WK_ADDR_REG_1
    EDI = "rdi",    ;;; WK_ADDR_REG_2
        R9  = "r9",
;

constant

    ;;; POPC register operands

    ;;; SP: call stack pointer

    SP = ESP,

    ;;; USP: user stack pointer

    USP = EBX,

    ;;; WK_REG: used by "m_optimise" to eliminate user stack pushes and
    ;;; pops between successive M-code instructions.

    WK_REG = EAX,

    ;;; CHAIN_REG: used to save procedure operands for in-line chaining
    ;;; and to save return addresses for out-of-line chaining through
    ;;; the subroutines "_syschain" and "_sysncchain". It must not be
    ;;; touched by M_UNWIND_SF.

    CHAIN_REG = EDX,

    ;;; WK_ADDR_REG_1, WK_ADDR_REG_2: used for building field-access
    ;;; operands of the form {reg offset} when an existing operand
    ;;; cannot be deferred directly (can_defer_operand having returned
    ;;; <false> for it). REG_1 is used for source operands, REG_2 for
    ;;; destination operands.

    WK_ADDR_REG_1 = ESI,
    WK_ADDR_REG_2 = EDI,

    ;;; USP_+ : pop from the user stack
    ;;; -_USP : push on the user stack
    ;;; i_USP : top of user stack

    USP_+ = {^USP ^true},
    -_USP = {^USP ^false},
    i_USP = {^USP 0},

    ;;; i_USP_+ : not supported
    ;;; ii_USP  : not supported

    ;;; Lists of pop/non-pop registers for register locals
    ;;; (none on this machine)

        pop_registers = [[] 13 14 15],
        nonpop_registers = [[] 8 10 11 12],
;

;;; regnumber:
;;;     maps register names to numbers as used in the "reg" field of
;;;     instructions

define regnumber =
    newassoc([
        [^EAX   0]
        [^ECX   1]
        [^EDX   2]
        [^EBX   3]
        [^ESP   4]
        [^EBP   5]
        [^ESI   6]
        [^EDI   7]
    ])
enddefine;

;;; reglabel:
;;;     the inverse mapping

define reglabel =
    newassoc([
        [0  ^EAX]
        [1  ^ECX]
        [2  ^EDX]
        [3  ^EBX]
        [4  ^ESP]
        [5  ^EBP]
        [6  ^ESI]
        [7  ^EDI]
    ])
enddefine;

define int_reglabel = newassoc([]); enddefine;

define word_reglabel = newassoc([]); enddefine;

define byte_reglabel = newassoc([]); enddefine;

procedure();
        lvars n, l, li, lw, lb;
        for n from 0 to 7 do
            reglabel(n) -> l;
            consword(`e`, l(2), l(3), 3) -> li;
            allbutfirst(1, l) -> lw;
            if n < 4 then
                consword(lw(1), `l`, 2)
            else
                consword(lw(1), lw(2), `l`, 3)
            endif -> lb;
            li ->  int_reglabel(n);
            lw -> word_reglabel(n);
            lb -> byte_reglabel(n);
        endfor;
        for n from 8 to 15 do
                consword('r' >< n) -> l;
                consword('r' >< n >< 'd') -> li;
                consword('r' >< n >< 'w') -> lw;
                consword('r' >< n >< 'b') -> lb;
                n -> regnumber(l);
                l -> reglabel(n);
                li -> int_reglabel(n);
                lw -> word_reglabel(n);
                lb -> byte_reglabel(n);
        endfor
endprocedure();

;;; Local register operands:

lconstant

    ;;; PB: procedure base register.
    ;;; This points to the start of the current procedure record, making up
    ;;; for the lack of PC-relative addressing.

    PB = EBP,
;

;;; wkreg:
;;;     <true> if opd is a work register

define lconstant wkreg(opd);
    lvars opd;
    opd == EAX or opd == ECX;
enddefine;

;;; altwkreg:
;;;     returns a work register not used in the given operand. This is
;;;     needed because the first work reg EAX is also POPC's WK_REG

define lconstant altwkreg(/* opd */) with_nargs 1;
    if reg_in_operand(/* opd */) == EAX then ECX else EAX endif;
enddefine;

;;; autoidreg:
;;;     indicates whether a register supports auto-indirection. None do
;;;     on the x86 (but some auto-indirect operands will still be
;;;     generated by the VM compiler for the USP and WK_ADDR registers:
;;;     these have to be eliminated by the adjust routine below).

define autoidreg() with_nargs 1;
    ->, false;
enddefine;


;;; === M-CODE OPERANDS ===============================================

;;; isimm, immval, immrep:
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
;;;     regnumber is used as a recogniser for legal register names. NB:
;;;     this only works for the 32-bit names; some may appear in the
;;;     final code transformed to their 8- or 16-bit variants.

lconstant macro isreg = "regnumber";

;;; ismem, isabs:
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

define lconstant ismem(opd);
    lvars opd;
    isvector(opd) or isabs(opd);
enddefine;

;;; can_defer_opnd:
;;;     takes an M-code operand plus displacement as arguments and
;;;     returns either a deferred version of the operand or <false> if
;;;     it's already a memory operand and so can't be deferred further.
;;;     Used in "m_optimise" to generate field access and update
;;;     operands. Whenever this returns <false>, one of the work address
;;;     registers has to be used for an intermediate load and
;;;     indirection.

define can_defer_opnd(opd, dis, upd);
    lvars opd, dis, upd;
    if isreg(opd) then
        ;;; register becomes register indirect
        {^opd ^dis};
    elseif isref(opd) then
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
    last_adjusted,
        ;;; last assembly-code operand adjusted
    USP_adjust,
    WK1_adjust,
    WK2_adjust,
        ;;; adjustments to be added to USP and the WK_ADDR registers to
        ;;; account for eliminated auto-indirections
    set_flags,
        ;;; <true> if add/sub operations should set the flags
;

;;; OPCODE, OPERANDS:
;;;     these hide the differences in instruction formats between
;;;     Unix-style and Microsoft/Intel assemblers. Unix style is used in
;;;     this file. The OPERANDS macro is restricted to simple word
;;;     arguments, to prevent problems if the order of arguments is
;;;     changed.

define lconstant macro OPCODE;
    lvars opcode;
    pop11_need_nextreaditem("(") -> ;
    unless isword(readitem() ->> opcode) then
        mishap(opcode, 1, 'WORD NEEDED');
    endunless;
    pop11_need_nextreaditem(")") -> ;
    ;;; map to amd64 opcodes
    define lconstant to_amd64 =
        newproperty([
            [incl       incq]
            [decl       decq]
            [addl       addq]
            [sall       salq]
            [sarl       sarq]
            [testl      testq]
            [cmpl       cmpq]
            [xorl       xorq]
            [movl       movq]
            [imull      imulq]
            [movb       movb]
            [movw       movw]
                        [movd       movl]
            [movzbl     movzbq]
            [movzwl     movzwq]
            [movsbl     movsbq]
            [movswl     movswq]
            [pushl      pushq]
            [popl       popq]
            [leal       leaq]
            [addl       addq]
            [subl       subq]
            [negl       negq]
            [andl       andq]
            [orl        orq]
            [notl       notq]
        ], 32, false, "perm");
    enddefine;
    to_amd64(opcode) or opcode -> opcode;
    """, opcode, """;
enddefine;

define lconstant macro OPERANDS;
    lvars opd, n = 0;
    pop11_need_nextreaditem("(") -> ;
    repeat
        unless isword(readitem() ->> opd) then
            mishap(opd, 1, 'WORD NEEDED');
        endunless;
        opd, n fi_+ 1 -> n;
    quitif(pop11_need_nextreaditem([),]) == ")");
    endrepeat;
    if n == 2 then
        lvars (x, y) = ();
#_IF DEF MASM
        (x, y) -> (y, x);
#_ENDIF
        x, ",", y;
    elseif n == 3 then
        lvars (x, y, z) = ();
#_IF DEF MASM
        (x, y, z) -> (z, y, x);
#_ENDIF
        x, ",", y, ",", z;
    endif;
enddefine;

#_IF DEF MASM

;;; TypedOperand:
;;;     memory operands need to be tagged with their sizes

defclass TypedOperand {
    typed_operand_size,
    typed_operand_value
};

#_ENDIF

;;; plant:
;;;     add an assembly-code instruction to the code list

define lconstant plant(/* opcode, operands, ..., n */);
    conspair(consvector(), []) ->> f_tl(last_instr) -> last_instr;
enddefine;

;;; chop:
;;;     delete last assembly-code instruction from the code list

define lconstant chop();
    #_< {# #} >_# -> f_hd(last_instr);
enddefine;

;;; auto_adjust:
;;;     access/update the current adjustment for a register

define lconstant auto_adjust(opd);
    lvars opd;
    if opd == USP then
        USP_adjust;
    elseif opd == WK_ADDR_REG_1 then
        WK1_adjust;
    elseif opd == WK_ADDR_REG_2 then
        WK2_adjust;
    else
        false;
    endif;
enddefine;
;;;
define updaterof auto_adjust(/* offset, */ opd) with_nargs 2;
    lvars opd;
    if opd == USP then
        /* offset */ -> USP_adjust;
    elseif opd == WK_ADDR_REG_1 then
        /* offset */ -> WK1_adjust;
    elseif opd == WK_ADDR_REG_2 then
        /* offset */ -> WK2_adjust;
    else
        mishap(opd, 1, 'ILLEGAL OPERAND FOR AUTO-ADJUSTMENT');
    endif;
enddefine;

;;; flush_adjustment:
;;;     flush the auto-adjustment for a given register. An LEA
;;;     instruction is used, as it doesn't affect the flags and so is
;;;     safe anywhere.

define lconstant flush_adjustment(reg);
    lvars reg, offset;
    if (auto_adjust(reg) ->> offset) /== 0 then
        lvars adj = {^reg ^offset};
        plant(OPCODE(leal), OPERANDS(adj, reg), 3);
        0 -> auto_adjust(reg);
    endif;
enddefine;

;;; flush_adjustments:
;;;     flush the offsets for some registers.
;;;     If all is <false>, then only the work registers are flushed; this
;;;     is done after every instruction.
;;;     If all is <true>, then the user stack offset is flushed too; this
;;;     has to be done before "discontinuities" in the code, i.e before
;;;     labels and control transfers, or wherever the USP is read.

define lconstant flush_adjustments(all);
    lvars all;
    flush_adjustment(WK_ADDR_REG_1);
    flush_adjustment(WK_ADDR_REG_2);
    if all then flush_adjustment(USP) endif;
enddefine;

;;; adjust:
;;;     adjust an M-code operand to remove auto-indirection; delta is the
;;;     operand size in bytes

define lconstant adjust(opd, delta) -> opd;
    lvars opd, delta;
    lvars offset;
    if isvector(opd) then
        lvars reg = f_subv(1, opd);
        if auto_adjust(reg) ->> offset then
            lvars disp = f_subv(2, opd);
            if disp == true then
                ;;; post-increment
                {^reg ^offset} -> opd;
                offset fi_+ delta -> auto_adjust(reg);
            elseif disp == false then
                ;;; pre-decrement
                offset fi_- delta ->> offset -> auto_adjust(reg);
                {^reg ^offset} -> opd;
            elseif offset /== 0 then
                ;;; ordinary indirection
                {% reg, disp + offset %} -> opd;
            endif;
            ;;; remember this, in case it needs patching later
            opd -> last_adjusted;
        endif;
    elseif auto_adjust(opd) ->> offset then
        ;;; operand is a register with a static offset; we need to flush
        ;;; the offset before the register can be referenced
        if last_adjusted and f_subv(1, last_adjusted) == opd then
            ;;; the previous operand of this instruction involves the
            ;;; same register and has already been adjusted; patch it
            ;;; to take account of the offset we're about to add
            f_subv(2, last_adjusted) fi_- offset -> f_subv(2, last_adjusted);
        endif;
        flush_adjustment(opd);
    elseif ispair(opd) then
        ;;; memory operand to a jump
        lvars target = adjust(f_front(opd), delta);
        if isvector(target) then
            if auto_adjust(f_subv(1, target)) ->> offset then
                ;;; base register may change when the offsets are
                ;;; flushed before the jump, so fix it
                copy(target) -> target;
                f_subv(2, target) fi_- offset -> f_subv(2, target);
            endif;
        endif;
        unless target == f_front(opd) then
            conspair(target, f_back(opd)) -> opd;
        endunless;
    elseif isabs(opd) then
        ;;; record symbol reference
        asm_uselab(opd) -> ;
    elseif isref(opd) then
        ;;; record symbol reference
        asm_uselab(fast_cont(opd)) -> ;
    endif;
enddefine;

;;; long_opd, word_opd, byte_opd, near_opd:
;;;     convert an M-code operand to an assembly-code operand of the
;;;     appropriate size, and eliminate any auto-indirections

define lconstant long_opd(opd) -> opd;
    lvars opd;
    adjust(opd, DOUBLE_OFFS) -> opd;
#_IF DEF MASM
    if ismem(opd) then
        consTypedOperand("dword", opd) -> opd;
    endif;
#_ENDIF
enddefine;

define lconstant int_opd(opd) -> opd;
        lvars opd;
        adjust(opd, INT_OFFS) -> opd;
        if isreg(opd) then
                ;;; replace with 32-bit register name
                int_reglabel(regnumber(opd)) -> opd;
                ;;; consword(`e`, opd(2), opd(3), 3) -> opd;
#_IF DEF MASM
        elseif ismem(opd) then
                consTypedOperand("dword", opd) -> opd;
#_ENDIF
        endif;
enddefine;


define lconstant word_opd(opd) -> opd;
    lvars opd;
    adjust(opd, SHORT_OFFS) -> opd;
    if isreg(opd) then
        ;;; replace with 16-bit register name
        ;;; allbutfirst(1, opd) -> opd;
                word_reglabel(regnumber(opd)) -> opd;
#_IF DEF MASM
    elseif ismem(opd) then
        consTypedOperand("word", opd) -> opd;
#_ENDIF
    endif;
enddefine;

define lconstant byte_opd(opd) -> opd;
    lvars opd;
    adjust(opd, BYTE_OFFS) -> opd;
    if isreg(opd) then
        ;;; replace with 8-bit register name (if allowed)
/*
        unless regnumber(opd) < 4 then
            mishap(opd, 1, 'ILLEGAL 8-BIT REGISTER NAME');
        endunless;
        consword(opd(2), `l`, 2) -> opd;
*/
                byte_reglabel(regnumber(opd)) -> opd;
#_IF DEF MASM
    elseif ismem(opd) then
        consTypedOperand("byte", opd) -> opd;
#_ENDIF
    endif;
enddefine;

define lconstant near_opd(opd) -> opd;
    lvars opd;
    adjust(opd, WORD_OFFS) -> opd;
enddefine;

;;; asmXXX:
;;;     assembly-code instructions

define lconstant asm_emit(/* opcode, operands, ..., n*/);
    ;;; add instruction to the code list
    plant(/* opcode, operands, ..., n*/);
    ;;; flush any outstanding adjustments to work registers
    flush_adjustments(false);
    false -> last_adjusted;
enddefine;

define lconstant asmALIGN();
    asm_emit("align", 1);
enddefine;

define lconstant asmLONG(/* datum, ..., */ n);
    lvars n, data = consvector(n);
    asm_emit("long", explode(data), n fi_+ 1);
enddefine;

define lconstant asmLABEL(lab);
    lvars lab;
    flush_adjustments(true);
    asm_emit("label", lab, 2);
enddefine;

define lconstant asmLEAL(src, dst);
    lvars src, dst;
    long_opd(src) -> src;
    long_opd(dst) -> dst;
    ;;; try combining with a previous shift: optimises some indexed
    ;;; accesses, and encoding of pop integers
    lvars instr = f_hd(last_instr);
    if f_subv(1, instr) == OPCODE(sall) then
        lvars (, x, y) = explode(instr);
        OPERANDS(x, y) -> (x, y);
#_IF DEF MASM
        lvars addr = typed_operand_value(src);
#_ELSE
        lconstant macro addr = "src";
#_ENDIF
        if isvector(addr) and datalength(addr) == 2
        and f_subv(1, addr) == y
        and dst == y
        and (x == 1 or x == 2 or x == 3)
        then
            ;;; replace:    sall $imm, %r1
            ;;;             leal [offs](%r1), %r1
            ;;; by:         leal [offs](, %r1, 1<<imm), %r1
            chop();
            {% false, f_subv(2,addr), y, 1<<x %} -> addr;
#_IF DEF MASM
            addr -> typed_operand_value(src);
#_ENDIF
        endif;
    endif;
    asm_emit(OPCODE(leal), OPERANDS(src, dst), 3);
enddefine;

define lconstant asmMOVL(src, dst);
    lvars src, dst;
    long_opd(src) -> src;
    long_opd(dst) -> dst;
    if src == 0 and isreg(dst) then
        asm_emit(OPCODE(xorl), OPERANDS(dst, dst), 3);
    elseif src /= dst then
        ;;; try and improve on a full move by taking advantage of the
        ;;; last instruction
        lvars instr = f_hd(last_instr);
        if f_subv(1, instr) == OPCODE(movl) then
            lvars (, x, y) = explode(instr);
            OPERANDS(x, y) -> (x, y);
            if y = src then
                if x = dst then
                    ;;; this move is redundant
                    return;
#_IF DEF MASM
                elseif isTypedOperand(src) and not(isTypedOperand(x)) then
                    ;;; isTypedOperand <=> ismem
#_ELSE
                elseif ismem(src) and not(ismem(x)) then
#_ENDIF
                    x -> src;
                endif;
            endif;
        endif;
        asm_emit(OPCODE(movl), OPERANDS(src, dst), 3);
    endif;
enddefine;

define lconstant asmMOVD(src, dst);
        lvars src, dst;
        int_opd(src) -> src;
        int_opd(dst) -> dst;
        asm_emit(OPCODE(movd), OPERANDS(src, dst), 3);
enddefine;

define lconstant asmMOVW(src, dst);
    lvars src, dst;
    word_opd(src) -> src;
    word_opd(dst) -> dst;
    asm_emit(OPCODE(movw), OPERANDS(src, dst), 3);
enddefine;

define lconstant asmMOVB(src, dst);
    lvars src, dst;
    byte_opd(src) -> src;
    byte_opd(dst) -> dst;
    asm_emit(OPCODE(movb), OPERANDS(src, dst), 3);
enddefine;

/* Valid only if dst is a register */
define lconstant asmMOVZDL(src, dst);
        lvars src, dst;
        int_opd(src) -> src;
        int_opd(dst) -> dst;
        asm_emit(OPCODE(movd), OPERANDS(src, dst), 3);
enddefine;

define lconstant asmMOVSDL(src, dst);
        lvars src, dst;
        int_opd(src) -> src;
        long_opd(dst) -> dst;
        asm_emit(OPCODE(movslq), OPERANDS(src, dst), 3);
enddefine;

define lconstant asmMOVZWL(src, dst);
    lvars src, dst;
    word_opd(src) -> src;
    long_opd(dst) -> dst;
    asm_emit(OPCODE(movzwl), OPERANDS(src, dst), 3);
enddefine;

define lconstant asmMOVZBL(src, dst);
    lvars src, dst;
    byte_opd(src) -> src;
    long_opd(dst) -> dst;
    asm_emit(OPCODE(movzbl), OPERANDS(src, dst), 3);
enddefine;

define lconstant asmMOVSWL(src, dst);
    lvars src, dst;
    word_opd(src) -> src;
    long_opd(dst) -> dst;
    asm_emit(OPCODE(movswl), OPERANDS(src, dst), 3);
enddefine;

define lconstant asmMOVSBL(src, dst);
    lvars src, dst;
    byte_opd(src) -> src;
    long_opd(dst) -> dst;
    asm_emit(OPCODE(movsbl), OPERANDS(src, dst), 3);
enddefine;

define lconstant asmADDL(src, dst);
    lvars src, dst;
    long_opd(src) -> src;
    long_opd(dst) -> dst;
    if src == 1 and not(set_flags) then
        asm_emit(OPCODE(incl), OPERANDS(dst), 2);
    else
        asm_emit(OPCODE(addl), OPERANDS(src, dst), 3);
    endif;
enddefine;

define lconstant asmSUBL(src, dst);
    lvars src, dst;
    long_opd(src) -> src;
    long_opd(dst) -> dst;
    if src == 1 and not(set_flags) then
        asm_emit(OPCODE(decl), OPERANDS(dst), 2);
    else
        asm_emit(OPCODE(subl), OPERANDS(src, dst), 3);
    endif;
enddefine;

define lconstant asmIMULL(src_1, src_2, dst);
    lvars src_1, src_2, dst;
    long_opd(src_1) -> src_1;
    long_opd(src_2) -> src_2;
    long_opd(dst) -> dst;
    if isimm(src_1) then
        asm_emit(OPCODE(imull), OPERANDS(src_1, src_2, dst), 4);
    elseif src_2 = dst then
        asm_emit(OPCODE(imull), OPERANDS(src_1, dst), 3);
    else
        mishap(src_1, src_2, dst, 3, 'ILLEGAL OPERANDS FOR IMUL');
    endif;
enddefine;

define lconstant asmSALL(src, dst);
    lvars src, dst;
    long_opd(src) -> src;
    long_opd(dst) -> dst;
    if src == 1 and isreg(dst) then
        asm_emit(OPCODE(addl), OPERANDS(dst, dst), 3);
    else
        asm_emit(OPCODE(sall), OPERANDS(src, dst), 3);
    endif;
enddefine;

define lconstant asmCMPL(src, dst);
    lvars src, dst;
    long_opd(src) -> src;
    long_opd(dst) -> dst;
    if src == 0 and isreg(dst) then
        asm_emit(OPCODE(testl), OPERANDS(dst, dst), 3);
    else
        asm_emit(OPCODE(cmpl), OPERANDS(src, dst), 3);
    endif;
enddefine;

define lconstant asmRET();
    flush_adjustments(true);
    asm_emit(OPCODE(ret), 1);
enddefine;

define lconstant asmINSTR_1_L(opd_1, opcode);
    lvars opd_1, opcode;
    long_opd(opd_1) -> opd_1;
    asm_emit(opcode, OPERANDS(opd_1), 2);
enddefine;

define lconstant asmPUSHL = asmINSTR_1_L(% OPCODE(pushl) %) enddefine;
define lconstant asmPOPL  = asmINSTR_1_L(% OPCODE(popl)  %) enddefine;
define lconstant asmNEGL  = asmINSTR_1_L(% OPCODE(negl)  %) enddefine;
define lconstant asmNOTL  = asmINSTR_1_L(% OPCODE(notl)  %) enddefine;

define lconstant asmINSTR_2_L(opd_1, opd_2, opcode);
    lvars opd_1, opd_2, opcode;
    long_opd(opd_1) -> opd_1;
    long_opd(opd_2) -> opd_2;
    asm_emit(opcode, OPERANDS(opd_1, opd_2), 3);
enddefine;

define lconstant asmSARL  = asmINSTR_2_L(% OPCODE(sarl)  %) enddefine;
define lconstant asmANDL  = asmINSTR_2_L(% OPCODE(andl)  %) enddefine;
define lconstant asmORL   = asmINSTR_2_L(% OPCODE(orl)   %) enddefine;
define lconstant asmTESTL = asmINSTR_2_L(% OPCODE(testl) %) enddefine;

define lconstant asmINSTR_JMP(target, opcode);
    lvars target, opcode;
    near_opd(target) -> target;
    flush_adjustments(true);
    asm_emit(opcode, OPERANDS(target), 2);
enddefine;

define lconstant asmJMP(target);
    lvars target;
    ;;; ignore this instruction if it's unreachable
    lvars instr = f_hd(last_instr);
    unless f_subv(1, instr) == OPCODE(jmp) then
        asmINSTR_JMP(target, OPCODE(jmp));
    endunless;
enddefine;

define lconstant asmJE    = asmINSTR_JMP(% OPCODE(je)    %) enddefine;
define lconstant asmJNE   = asmINSTR_JMP(% OPCODE(jne)   %) enddefine;
define lconstant asmJL    = asmINSTR_JMP(% OPCODE(jl)    %) enddefine;
define lconstant asmJLE   = asmINSTR_JMP(% OPCODE(jle)   %) enddefine;
define lconstant asmJG    = asmINSTR_JMP(% OPCODE(jg)    %) enddefine;
define lconstant asmJGE   = asmINSTR_JMP(% OPCODE(jge)   %) enddefine;
define lconstant asmJA    = asmINSTR_JMP(% OPCODE(ja)    %) enddefine;
define lconstant asmJAE   = asmINSTR_JMP(% OPCODE(jae)   %) enddefine;
define lconstant asmJB    = asmINSTR_JMP(% OPCODE(jb)    %) enddefine;
define lconstant asmJBE   = asmINSTR_JMP(% OPCODE(jbe)   %) enddefine;
define lconstant asmJS    = asmINSTR_JMP(% OPCODE(js)    %) enddefine;
define lconstant asmJNS   = asmINSTR_JMP(% OPCODE(jns)   %) enddefine;
define lconstant asmJO    = asmINSTR_JMP(% OPCODE(jo)    %) enddefine;
define lconstant asmJNO   = asmINSTR_JMP(% OPCODE(jno)   %) enddefine;
define lconstant asmCALL  = asmINSTR_JMP(% OPCODE(call)  %) enddefine;

;;; testop:
;;;     maps M-code condition codes to assembler opcodes

define lconstant testop =
    newassoc([
        [EQ     ^asmJE]
        [NEQ    ^asmJNE]
        [LT     ^asmJL]
        [LEQ    ^asmJLE]
        [GT     ^asmJG]
        [GEQ    ^asmJGE]
        [ULT    ^asmJB]
        [ULEQ   ^asmJBE]
        [UGT    ^asmJA]
        [UGEQ   ^asmJAE]
        [NEG    ^asmJS]
        [POS    ^asmJNS]
        [OVF    ^asmJO]
        [NOVF   ^asmJNO]
    ]);
enddefine;

define constant is_large_int(src);
  lvars src;
  if isinteger(src) or isbiginteger(src) then
    if (src < -2147483648) or (src > 2147483647) then
      true
    else
      false
    endif
  else
    false
  endif
enddefine;

/*
 *  Data Movement
 */

define M_MOVE();
    lvars (, src, dst) = explode(m_instr);
    unless src = dst or src == USP_+ and dst == -_USP then
        if ismem(src) and ismem(dst) then
            asmMOVL(src, EAX ->> src);
        endif;
                if is_large_int(src) then
                  asmMOVL(src, EAX ->> src);
                endif;
        asmMOVL(src, dst);
    endunless;
enddefine;

;;; gen_move_to_l:
;;;     move a byte or word from memory to a long destination.
;;;     asm_op determines the type of the source operand and whether
;;;     zero- or sign-extension is to be employed. In any case, the
;;;     immediate destination must be a register.

define lconstant gen_move_to_l(asm_op);
    lvars asm_op, (, src, dst) = explode(m_instr);
    if isreg(dst) then
        asm_op(src, dst);
    else
        asm_op(src, EAX);
        asmMOVL(EAX, dst);
    endif;
enddefine;

;;; gen_move_from_l:
;;;     move a long to a byte or word in memory. The source must be an
;;;     immediate or register operand, and for byte moves only registers
;;;     EAX - EBX will work

define lconstant gen_move_from_l(asm_op);
    lvars asm_op, (, src, dst) = explode(m_instr);
    if ismem(src)
    ;;; or isreg(src) and asm_op == asmMOVB and regnumber(src) fi_>= 4
    then
        asmMOVL(src, EAX ->> src);
    endif;
    asm_op(src, dst);
enddefine;

define M_MOVEb  = gen_move_to_l(% asmMOVZBL %) enddefine;
define M_MOVEsb = gen_move_to_l(% asmMOVSBL %) enddefine;
define M_MOVEs  = gen_move_to_l(% asmMOVZWL %) enddefine;
define M_MOVEss = gen_move_to_l(% asmMOVSWL %) enddefine;
define M_MOVEi  = gen_move_to_l(% asmMOVZDL %) enddefine;
define M_MOVEsi = gen_move_to_l(% asmMOVSDL %) enddefine;
define M_UPDb   = gen_move_from_l(% asmMOVB %) enddefine;
define M_UPDs   = gen_move_from_l(% asmMOVW %) enddefine;
define M_UPDi   = gen_move_from_l(% asmMOVD %) enddefine;

;;; gen_bfield:
;;;     extract a bitfield from a structure using assembly code routines
;;;     "_bfield" and "_sbfield"

define lconstant gen_bfield(routine);
    lvars routine, (, size, offs, src, dst) = explode(m_instr);
    asmMOVL(size, EDX);
    if reg_in_operand(src) == EAX then
        asmMOVL(src, EDI);
        asmMOVL(offs, EAX);
    else
        asmMOVL(offs, EAX);
        asmMOVL(src, EDI);
    endif;
    asmCALL(symlabel(routine));
    asmMOVL(EAX, dst);
enddefine;

define M_MOVEbit  = gen_bfield(% "\^_bfield"  %) enddefine;
define M_MOVEsbit = gen_bfield(% "\^_sbfield" %) enddefine;

define M_UPDbit();
    lvars (, size, offs, dst, src) = explode(m_instr);
    ;;; Push src
    unless src == USP_+ then
        if ismem(src) then
            ;;; Use ECX as temporary in case EAX is occupied
            asmMOVL(src, ECX ->> src);
        endif;
        asmMOVL(src, -_USP);
    endunless;
    ;;; Field size to EDX
    asmMOVL(size, EDX);
    ;;; Bit offset to EAX, destination struct to EDI
    if reg_in_operand(dst) == EAX then
        asmMOVL(dst, EDI);
        asmMOVL(offs, EAX);
    else
        asmMOVL(offs, EAX);
        asmMOVL(dst, EDI);
    endif;
    asmCALL(symlabel("\^_ubfield"));
enddefine;


/*
 *  Basic Arithmetic
 */

;;; gen_lea:
;;;     compute an address from operands src1 and src2 and load it
;;;     to dst. Both sources must be register or immediate operands.

define lconstant gen_lea(src1, src2, dst);
    lvars src1, src2, dst;
    if isreg(src1) then
        if isreg(src2) then ;;; base + index
                    if src2 /== ESP then
            {^src1 0 ^src2};
                    else
                        {^src2 0 ^src1};
                    endif
        else                        ;;; base + displacement
            {% src1, asm_uselab(immval(src2)) %};
        endif;
    else
        if isreg(src2) then ;;; base + displacement
            {% src2, asm_uselab(immval(src1)) %};
        else                        ;;; immediate
            asmMOVL(src1, EAX);
            {% EAX, asm_uselab(immval(src2)) %};
        endif;
    endif;
    asmLEAL((), dst);
enddefine;

;;; gen_op_1:
;;;     plants code for a unary operation of the form:
;;;         dst := op(src)
;;;     The operator will be either NEG or NOT, determined by asm_op.
;;;     Both of these instructions take only a single operand, so if dst
;;;     is not the same as src, an extra move is required.

define lconstant gen_op_1(src, dst, asm_op);
    lvars src, dst, asm_op, tmp;
    if src = dst then
        asm_op(dst);
    else
        if isreg(dst) then
            dst -> tmp;
        elseif wkreg(src) then
            src -> tmp;
        else
            EAX -> tmp;
        endif;
        asmMOVL(src, tmp);
        asm_op(tmp);
        asmMOVL(tmp, dst);
    endif;
enddefine;

define lconstant gen_neg = gen_op_1(% asmNEGL %) enddefine;
define lconstant gen_not = gen_op_1(% asmNOTL %) enddefine;

;;; gen_op_2:
;;;     plants code for a commutative binary operation:
;;;         dst := src2 op src1
;;;     The operator will be one of ADD, AND or OR, determined by
;;;     asm_op. All these instructions take only two operands,
;;;     one of which must not be in memory.
;;;     If the operation is ADD and set_flags is true, then the code
;;;     planted must leave the flags correctly set for a subsequent test.

define lconstant gen_op_2(src1, src2, dst, asm_op);
    lvars src1, src2, dst, asm_op, tmp;
    if src1 = dst then (src1, src2) -> (src2, src1) endif;
    /* {src1 /= dst | src2 = dst} */
    if src2 = dst then
        if ismem(src1) and ismem(dst) then
            asmMOVL(src1, EAX ->> src1);
        endif;
                if is_large_int(src1) then
                  asmMOVL(src1, altwkreg(src2) ->> src1);
                endif;
        asm_op(src1, dst);
    else
            if is_large_int(src1) and is_large_int(src2) then
                   if isreg(dst) then
                     altwkreg(dst) -> tmp;
                     asmMOVL(src1, dst);
                     asmMOVL(src2, tmp);
                     asm_op(tmp, dst);
                   else
                     asmMOVL(src1, EAX);
                     asmMOVL(src2, ECX);
                     asm_op(ECX, EAX);
                     asmMOVL(EAX, dst);
                   endif;
            else
        /* {src1 /= dst & src2 /= dst} */
        if ismem(src1) then (src1, src2) -> (src2, src1) endif;
        /* {~mem(src1) |mem(src2)} */
                if is_large_int(src1) then (src1, src2) -> (src2, src1) endif;

#_IF false

        if isreg(dst) and dst /== reg_in_operand(src1) then
            dst -> tmp;
        elseif wkreg(src2) then
            src2 -> tmp;
        elseif wkreg(src1) then
            src1, src2 -> (src2, src1);
            src2 -> tmp;
        else
            altwkreg(src1) -> tmp;
        endif;
#_ELSE
                R9 -> tmp;
#_ENDIF
        if asm_op == asmADDL and not(set_flags)
        and not(ismem(src1)) and not(ismem(src2)) 
                and not(is_large_int(src2))
        then
            /* {~mem(src1) & ~mem(src2) & reg(tmp)} */
            /* optimise move+add to lea */
/*
                        if src2 == ESP then (src1, src2) -> (src2, src1) endif;
*/
            gen_lea(src1, src2, tmp);
            asmMOVL(tmp, dst);
        else
            /* {src2 = tmp | tmp /= reg_in_operand(src1)} */
            asmMOVL(src2, tmp);
            asm_op(src1, tmp);
            asmMOVL(tmp, dst);
        endif;
    endif;
    endif;
enddefine;

define lconstant gen_add = gen_op_2(% asmADDL %) enddefine;
define lconstant gen_and = gen_op_2(% asmANDL %) enddefine;
define lconstant gen_or  = gen_op_2(% asmORL  %) enddefine;

;;; gen_sub:
;;;     plants code for the operation
;;;         dst := src2 - src1
;;;     When set_flags is true, the code planted must leave the flags set
;;;     correctly for a subsequent test.

;;; global constant _print_addr;
define lconstant gen_sub(src1, src2, dst);
    lvars src1, src2, dst, tmp;
;;;        printf(src1, 'src1 = %p, ');
;;;        printf(src2, 'src2 = %p, ');
;;;        printf(dst, 'dst = %p\n');
;;;           printf('Moja pulapka\n');
;;;        _print_addr(dst) -> _;
    if src2 = dst then
;;;                printf('src2 = dst\n');
        if ismem(src1) and ismem(dst) or 
                    is_large_int(src1) then
;;;                        printf('asmMOVL(src1, EAX ->> src1);\n');
            asmMOVL(src1, EAX ->> src1);
        endif;
;;;                printf('asmSUBL(src1, dst);\n');
        asmSUBL(src1, dst);
    elseif isreg(dst) and src1 == dst then
;;;                printf('isreg(dst) and src1 == dst\n');
                if is_large_int(src2) then
;;;                    printf('asmMOVL(src2, R9 ->> src2);\n');
                    asmMOVL(src2, R9 ->> src2);
                endif;
;;;                printf('asmSUBL(src2, dst);\n');
        asmSUBL(src2, dst);
;;;                printf('asmNEGL(dst);\n');
        asmNEGL(dst);
    else
;;;                printf('else\n');
        if isreg(dst) and dst /== reg_in_operand(src1) then
;;;                        printf('isreg(dst) and dst /== reg_in_operand(src1)\n');
            dst -> tmp;
        elseif wkreg(src2) then
;;;                        printf('wkreg(src2)\n');
            src2 -> tmp;
        else
;;;                        printf('else: altwkreg(src1) -> tmp;\n');
            altwkreg(src1) -> tmp;
        endif;
                if is_large_int(src1) then 
;;;                   printf('asmMOVL(src1, R9 ->> src1);');
                   asmMOVL(src1, R9 ->> src1);
                endif;
        if not(set_flags) and isintegral(src1) and
                   not(is_large_int(src2)) and not(ismem(src2)) then
            /* {isintegral(src1) & ~mem(src2) & reg(tmp)} */
            ;;; negate src1 and optimise to lea
;;;                        printf('gen_lea(src2, -src1, tmp);\n');
            gen_lea(src2, -src1, tmp);
        else
            /* {src2 = tmp | tmp /= reg_in_operand(src1)} */
;;;                        printf('asmMOVL(src2, tmp);\n');
            asmMOVL(src2, tmp);
;;;                        printf('asmSUBL(src1, tmp);\n');
            asmSUBL(src1, tmp);
            if src1 == USP_+ and src2 == USP_+ then
                ;;; the subtraction operation will have gone the wrong way
;;;                                printf('asmNEGL(tmp);\n');
                asmNEGL(tmp);
            endif;
        endif;
;;;                printf('asmMOVL(tmp, dst);\n');
        asmMOVL(tmp, dst);
    endif;
;;;        printf('end of gen_sub\n');
enddefine;

;;; gen_mult:
;;;     plants code for the operation
;;;         dst := src2 * src1
;;;     This is different from gen_op_2 because IMUL requires dst to be
;;;     in a register; there is also an optimised 3-operand form which can
;;;     be used when either source is an immediate quantity.

define lconstant gen_mult(src1, src2, dst);
    lvars src1, src2, dst, tmp;
    if isimm(src1) and isimm(src2) then asmMOVL(src1, EAX ->> src1) endif;
    /* {~imm(src1) | ~imm(src2)} */
    if ismem(src1) then (src1, src2) -> (src2, src1) endif;
    /* {~mem(src1) | mem(src2)} */
    if isreg(dst)
    and not(ismem(src2) and reg_in_operand(src2) == dst)
    then
        dst -> tmp;
    elseif wkreg(src1) then
        src1 -> tmp;
    elseif wkreg(src2) then
        src2 -> tmp;
    else
        altwkreg(src2) -> tmp;
    endif;
    if src1 == tmp then
        asmIMULL(src2, src1, tmp);
    elseif src2 == tmp or isimm(src1) then
        asmIMULL(src1, src2, tmp);
    elseif isimm(src2) then
        asmIMULL(src2, src1, tmp);
    elseif ismem(src1) then
        /* {mem(src1) & mem(src2) & tmp /= reg_in_operand(src2)} */
        asmMOVL(src1, tmp);
        asmIMULL(src2, tmp, tmp);
    else
        /* {reg(src1) & src1 /= tmp} */
        asmMOVL(src2, tmp);
        asmIMULL(src1, tmp, tmp);
    endif;
    asmMOVL(tmp, dst);
enddefine;

;;; gen_shift:
;;;     plants code for a left or right shift operation,
;;;         dst := src2 shift src1
;;;     where src1 will always be an immediate quantity.

define lconstant gen_shift(src1, src2, dst, asm_op);
    lvars src1, src2, dst, asm_op, tmp;
    if src2 = dst then
        asm_op(src1, dst);
    else
        if isreg(dst) then
            dst -> tmp;
        elseif wkreg(src2) then
            src2 -> tmp;
        else
            EAX -> tmp;
        endif;
        asmMOVL(src2, tmp);
        asm_op(src1, tmp);
        asmMOVL(tmp, dst);
    endif;
enddefine;

;;; gen_arith_2, gen_arith_3:
;;;     translate 2- and 3-operand M-code arithmetic/logical instructions
;;;     on machine integers; gen_p is the code-planting instruction for
;;;     the corresponding machine operation.

define lconstant gen_arith_2(gen_p);
    lvars gen_p, (, src, dst) = explode(m_instr);
    gen_p(src, dst);
enddefine;

define lconstant gen_arith_3(gen_p);
    lvars gen_p, (, src1, src2, dst) = explode(m_instr);
    gen_p(src1, src2, dst);
enddefine;

;;; gen_parith, gen_parith_test:
;;;     plant code for an addition or subtraction of pop integers.
;;;     This means clearing the bottom two bits of the first operand and
;;;     then doing an ordinary machine integer operation.
;;;     The testing version pushes the result on the user stack and
;;;     plants a branch conditional on the result.

define lconstant gen_parith(gen_p);
    lvars gen_p, (, src1, src2, dst) = explode(m_instr);
    if isintegral(src1) then
        src1 - 3 -> src1;
    else
        if gen_p == gen_sub and dst == EAX then
            asmMOVL(src1, ECX ->> src1);
        else
            asmMOVL(src1, altwkreg(src2) ->> src1);
        endif;
        asmSUBL(3, src1);
    endif;
    gen_p(src1, src2, dst);
enddefine;

define lconstant gen_parith_test(gen_p);
    lvars gen_p, (, src1, src2, test, lab) = explode(m_instr);
    dlocal set_flags;
    if isintegral(src1) then
        src1 - 3 -> src1;
    else
        asmMOVL(src1, altwkreg(src2) ->> src1);
        asmSUBL(3, src1);
    endif;
    true -> set_flags;
    gen_p(src1, src2, -_USP);
    testop(test)(lab);
enddefine;

;;; gen_ptr_arith:
;;;     plants code for an operation on pointers. Pointers are just
;;;     machine integers, so their code-planting procedures can be used
;;;     directly. The type field of the instruction is ignored.

define lconstant gen_ptr_arith(gen_p);
    lvars gen_p, (, /*type*/, offs, ptr, dst) = explode(m_instr);
    gen_p(offs, ptr, dst);
enddefine;


define M_ADD    = gen_arith_3(% gen_add  %) enddefine;
define M_SUB    = gen_arith_3(% gen_sub  %) enddefine;
define M_MULT   = gen_arith_3(% gen_mult %) enddefine;
define M_NEG    = gen_arith_2(% gen_neg  %) enddefine;
define M_BIS    = gen_arith_3(% gen_or   %) enddefine;
define M_BIM    = gen_arith_3(% gen_and  %) enddefine;
define M_LOGCOM = gen_arith_2(% gen_not  %) enddefine;

define M_PADD = gen_parith(% gen_add %) enddefine;
define M_PSUB = gen_parith(% gen_sub %) enddefine;

define M_PADD_TEST = gen_parith_test(% gen_add %) enddefine;
define M_PSUB_TEST = gen_parith_test(% gen_sub %) enddefine;

define M_PTR_ADD_OFFS = gen_ptr_arith(% gen_add %) enddefine;
define M_PTR_SUB_OFFS = gen_ptr_arith(% gen_sub %) enddefine;
define M_PTR_SUB      = gen_ptr_arith(% gen_sub %) enddefine;

;;; M_BIC:
;;;     clears the bits in src2 which are set in src1 and places the
;;;     result in dst, i.e. performs the operation
;;;         dst := src2 && ~~src1

define M_BIC();
    lvars (, src1, src2, dst) = explode(m_instr);
    if isintegral(src1) then
        ~~src1 -> src1;
    else
        asmMOVL(src1, altwkreg(src2) ->> src1);
        asmNOTL(src1);
    endif;
    gen_and(src1, src2, dst);
enddefine;

;;; M_ASH:
;;;     performs an arithmetic shift of src2 by an amount src1, leaving
;;;     result in dst. The shift may be right or left depending on the
;;;     sign of src1.

define M_ASH();
    lvars (, src1, src2, dst) = explode(m_instr);
    if isintegral(src1) then
        if src1 < 0 then
            gen_shift(-src1, src2, dst, asmSARL);
        else
            gen_shift(src1, src2, dst, asmSALL);
        endif;
    else
        ;;; the amount to shift has to be in ECX
        asmMOVL(src1, ECX);
        ;;; the shift proper is done by subroutine "_m_ash" defined in
        ;;; aarith.s which acts on the value in EAX
        asmMOVL(src2, EAX);
        asmCALL(symlabel("\^_m_ash"));
        asmMOVL(EAX, dst);
    endif;
enddefine;


/*
 *  Branches and Tests
 */

;;; gen_test_or_cmp:
;;;     plants code to test/compare src1 against src2, and jump to lab
;;;     on test. The test may be CMP (src1 - src2) or TEST (src1 &&
;;;     src2) determined by gen_p; the test will be one of the M-code
;;;     test codes EQ, NEQ etc.

define lconstant gen_test_or_cmp(src1, src2, test, lab, gen_p);
    lvars src1, src2, test, lab, gen_p;
;;;        printf(src1, src2, 'gen_test_or_cmp:\n %p\n %p\n');
        if isimm(src1) and isimm(src2) then
           if isinteger(src2) or isbiginteger(src2) then
              if (src2 < -2147483648) or (src2 > 2147483647) then
                asmMOVL(src2, EAX ->> src2);
              else
                asmMOVL(src1, EAX ->> src1);
              endif;
           else
             asmMOVL(src1, EAX ->> src1);
           endif;
        endif;
        if isinteger(src1) or isbiginteger(src1) then
           if (src1 < -2147483648) or (src1 > 2147483647) then
;;;              printf('loading big constant to register\n');
              asmMOVL(src1, altwkreg(src2) ->> src1);
           endif;
        endif;
        if isinteger(src2) or isbiginteger(src2) then
          if src2 < -2147483648 or src2 > 2147483647 then
;;;             printf('loading big constant to register\n');
             asmMOVL(src2, altwkreg(src1) ->> src2);
          endif;
        endif;
    if ismem(src1) and ismem(src2) or isimm(src1) and isimm(src2) then
        asmMOVL(src1, altwkreg(src2) ->> src1);
    endif;

    if isimm(src1) or ismem(src2) then
        ;;; src1 can't be an immediate operand;
        ;;; also, if there's a memory operand in the test it's better
        ;;; (faster) to have it as src1
        (src1, src2) -> (src2, src1);
        commute_test(test) -> test;
    endif;
    gen_p(src2, src1);
    testop(test)(lab);
enddefine;

define lconstant gen_test = gen_test_or_cmp(% asmTESTL %) enddefine;
define lconstant gen_cmp  = gen_test_or_cmp(% asmCMPL  %) enddefine;

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
    ;;; move src to a register to allow for accessing its key
    unless isreg(src) then
        asmMOVL(src, EAX ->> src);
    endunless;
    ;;; test for a simple item first
    lvars tlab = if test == "EQ" then genlab() else lab endif;
    gen_test(1, src, "NEQ", tlab);
    ;;; item is compound: get its key
    {% src, field_##("KEY").wof %} -> src;
    if isintegral(key) then
        ;;; testing flag(s) nonzero in K_FLAGS field:
        ;;; get key to register
        asmMOVL(src, EAX ->> src);
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
    ;;; Move the value into a register for table indexing
    if not(isreg(src)) then
        asmMOVL(src, EAX ->> src);
    endif;
    ;;; Check it's in range: an unsigned comparison takes care of both the
    ;;; too large and too small cases.
    asmCMPL(if sysint then ncases else popint(ncases) endif, src);
    asmJA(else_lab);
    ;;; If in range, use it as an index into a jump table. The table is just
    ;;; a sequence of longword labels. How the indexing is done depends on
    ;;; whether src is a sysint: if so, we want (table_start + src * 4),
    ;;; or if it's a POP integer we want (table_start + src - 3)
    lvars target;
    if sysint then
        {% false, table_start, src, 8 %}
    else
;;;     {% src, asm_expr(table_start, "-", 3) %}
;;;             printf('gen_switch on pop integer');
        {% false, asm_expr(table_start, "-", 6), src, 2 %}
    endif -> target;
    asmJMP(conspair(target, 0));
    ;;; Plant the table; it begins with -else_lab- to account for the 0 case
    asmALIGN();
    asmLABEL(table_start);
    lvars lab;
    for lab in [^else_lab ^^labs] do
        asmLONG(lab, 1);
    endfor;
    ;;; Plant the else case
    asmLABEL(else_lab);
    if not(else_case) then
        asmMOVL(src, -_USP);
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
    asmJMP(m_instr(2));
enddefine;

;;; M_BRANCH_std:
;;;     same as M_BRANCH, but guarantees to produce an instruction of a
;;;     fixed size. That's not directly available from 386 assembler, so
;;;     we simulate it by moving the label to a register (= 7 bytes) and
;;;     then jumping off that

define M_BRANCH_std();
    asmMOVL(immrep(m_instr(2)), EAX);       ;;; movl $L, %eax (7 bytes)
    asmJMP(conspair(EAX, 0));           ;;; jmp *%eax     (2 bytes)
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
            ;;; it will expect its own address in EAX
            unless opd == EAX then
                asmMOVL(opd, EAX);
            endunless;
            {% EAX, field_##("PD_EXECUTE").wof %} -> opd;
        endif;
        conspair(opd, 0);
    endif;
enddefine;

define lconstant gen_call_or_chain(call_op, is_pop_pdr);
    lvars call_op, is_pop_pdr;
    lvars target = get_exec_opd(m_instr(2), is_pop_pdr);
    call_op(target);
enddefine;

define M_CALL  = gen_call_or_chain(% asmCALL, true %) enddefine;
define M_CHAIN = gen_call_or_chain(% asmJMP,  true %) enddefine;

define M_CALL_WITH_RETURN();
    ;;; push given return address
    asmPUSHL(m_instr(3));
    ;;; chain the procedure
    M_CHAIN();
enddefine;

;;; {M_CALLSUB <subroutine_opd> <args ...>}
;;;     call subroutine, passing arguments (0-3) in registers.
;;;     Subroutine will always be constant when arguments are present

define M_CALLSUB();
    lvars l = datalength(m_instr);
    if l == 5 then    asmMOVL(m_instr(3),   EDX) endif; ;;; arg_reg_2
    if l fi_>= 4 then asmMOVL(m_instr(l-1), ECX) endif; ;;; arg_reg_1
    if l fi_>= 3 then asmMOVL(m_instr(l),   EAX) endif; ;;; arg_reg_0
    asmCALL(get_exec_opd(m_instr(2), false));
enddefine;

define M_CHAINSUB = gen_call_or_chain(% asmJMP, false %) enddefine;

define M_RETURN = asmRET(%%) enddefine;


/*
 *  Procedure Entry and Exit
 */

lblock

lvars

    ;;; These variables are set by M_CREATE_SF and used by M_UNWIND_SF

    dlocal_labs,
        ;;; Names of dynamic local variables
        regmask,
    Nstkvars,
        Nregs,
        ;;; Number of on-stack vars
;

;;; {M_CREATE_SF <reg_locals> <Npopreg> <Nstkvars> <Npopstkvars>
;;;             <dlocal_labs> <ident reg_spec>}
;;;     plant code to construct procedure stack frame

define M_CREATE_SF();
    lconstant popint_zero = popint(0);
    lvars reg_spec_id, Npopregs, Npopstkvars, reg_locals, n, k;

    explode(m_instr) -> reg_spec_id -> dlocal_labs -> Npopstkvars
        -> Nstkvars -> Npopregs -> reg_locals -> ;

/*
    ;;; Check there are no register locals and set the register spec to zero
    unless null(reg_locals) then
        mishap(reg_locals, 1, 'UNEXPECTED REGISTER LOCALS');
    endunless;
*/

        listlength(reg_locals) -> Nregs;

        ;;; create register mask from reg locals
        0 -> regmask;
        fast_for n in reg_locals do regmask || (1 << n) -> regmask endfast_for;

        regmask -> idval(reg_spec_id);

        ;;; Save registers
        for n from 8 to 15 do
            if regmask &&/=_0 (1 << n) then
                asmPUSHL(reglabel(n));
            endif;
        endfor;

        ;;; Initialize pop registers to 0
        1 -> k;
        for n from 15 by - 1 to 8 do
            quitif(k > Npopregs);
            if regmask &&/=_0 (1 << n) then
                asmMOVL(popint_zero, reglabel(n));
                k + 1 -> k;
            endif;
        endfor;

    ;;; Set the procedure base register
    asmMOVL(immrep(current_pdr_label), PB);
    ;;; Push dynamic locals
    applist(dlocal_labs, asmPUSHL);
    ;;; Allocate POP on-stack lvars (initialised to zero)
    repeat Npopstkvars times asmPUSHL(popint_zero) endrepeat;
    ;;; Allocate non-POP on-stack lvars (uninitialised)
    if Nstkvars /== Npopstkvars then
        asmSUBL((Nstkvars - Npopstkvars) * 8, SP);
    endif;
    ;;; Push the owner address
    asmPUSHL(PB);
enddefine;

;;; {M_UNWIND_SF}
;;;     plant code to unwind a procedure stack frame

define M_UNWIND_SF();
        lvars n;
    ;;; Remove owner address and on-stack vars (POP and non-POP)
    asmADDL((Nstkvars + 1) * 8, SP);
    ;;; Pop dynamic locals
    applist(rev(dlocal_labs), asmPOPL);
        ;;; restore registers

        for n from 15 by -1 to 8 do
            if regmask &&/=_0 (1 << n) then
                asmPOPL(reglabel(n));
            endif;
        endfor;

    ;;; Restore procedure base register from previous owner address
    asmMOVL({^SP 8}, PB);
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
    if nfroz fi_> 16 then
        ;;; for more than 16 frozvals, call Exec_closure
        asmMOVL(immrep(current_pdr_label), -_USP);
        perm_const_opnd([Sys Exec_closure]) -> pdpart_opd;
    else
        ;;; get closure address in EAX
        asmMOVL(immrep(current_pdr_label), EAX);
        ;;; push the frozvals
        lconstant frozval_offset = field_##("PD_CLOS_FROZVALS").wof - 8;
        lvars i;
        for i to nfroz do
            asmMOVL({% EAX, frozval_offset + i*8 %}, ECX);
            asmMOVL(ECX, -_USP);
        endfor;
        unless pdpart_opd then
            {% EAX, field_##("PD_CLOS_PDPART").wof %} -> pdpart_opd;
        endunless;
    endif;
    {^M_CHAIN ^pdpart_opd} -> m_instr;
    M_CHAIN();
enddefine;

;;; {M_PLOG_IFNOT_ATOM <ifnot_lab>}
;;;     test result of _prolog_unify_atom

define M_PLOG_IFNOT_ATOM();
    asmJNE(m_instr(2));
enddefine;

;;; {M_PLOG_TERM_SWITCH <fail_lab> <var_lab> <dst>}
;;;     test result from _prolog_pair_switch/_prolog_term_switch
;;;     If EQ, move EAX (dereferenced result) to <dst>

define M_PLOG_TERM_SWITCH();
    asmJA(m_instr(3));
    asmJB(m_instr(2));
    asmMOVL(EAX, m_instr(4));
enddefine;

;;; {M_SETSTKLEN <offset of stack increase> <popint saved stklen opnd>}
;;;     adjust the number of results returned by a Lisp function.
;;;     <offset> is always a constant integer

define M_SETSTKLEN();
    lvars (, offs, sl) = explode(m_instr);
    ;;; compute desired stack length as saved stack length plus offset;
    ;;; subtract 3 to account for popint bits in saved stack length
    asmMOVL(sl, EAX);
    if offs == 0 then
        asmSUBL(3, EAX);
    else
        asmADDL(offs-3, EAX);
    endif;
    ;;; compute desired stack pointer in ECX
    asmMOVL(identlabel("\^_userhi"), ECX);
    asmSUBL(EAX, ECX);
    ;;; compare desired and actual stack pointers
    asmCMPL(ECX, USP);
    ;;; if equal, jump to end, otherwise call "setstklen_diff" to fix
    lvars lab = genlab();
    asmJE(lab);
    asmCALL(symlabel("\^_setstklen_diff"));
    asmLABEL(lab);
enddefine;

;;; M_ERASE:
;;;     pop to a register; have to do the move, in case the address is
;;;     invalid (e.g., stack empty)

define M_ERASE();
    asmMOVL(m_instr(2), EAX);
enddefine;


/*
 *  Generate assembly code
 */

define lconstant generate(codelist) -> ilist;
    lvars   codelist, ilist;
    dlocal  m_instr, last_instr, last_adjusted = false, set_flags = false,
            USP_adjust = 0, WK1_adjust = 0, WK2_adjust = 0;
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
#_IF DEF MASM
    if ispair(opd) then
        ;;; branch target other than a label
        f_front(opd) -> opd;
        if ismem(opd) then
            asmf_printf('dword ptr ');
        endif;
    endif;
    if isword(opd) then
        ;;; assume register: can't use isreg, because it won't recognise
        ;;; the byte- and word-length register names
        asmf_printf(opd, '%p');
    elseif isimm(opd) then
        asmf_printf(immval(opd), '%p');
    else
        ;;; memory
        if isTypedOperand(opd) then
            asmf_printf(typed_operand_size(opd), '%p ptr ');
            typed_operand_value(opd) -> opd;
        endif;
        if isabs(opd) then
            asmf_printf(opd, '%p');
        elseif isvector(opd) then
            ;;; register based/indexed
            lvars base = f_subv(1, opd), disp = f_subv(2, opd);
            if isboolean(disp) then
                mishap(opd, 1, 'ILLEGAL AUTO-INDIRECTION');
            elseif isref(disp) then
                fast_cont(disp) -> disp;
            endif;
            asmf_charout(`[`);
            if datalength(opd) == 2 then
                ;;; based
                asmf_printf(base, '%p');
            else
                ;;; indexed
                if base then asmf_printf(base, '%p+') endif;
                asmf_printf(f_subv(3, opd), '%p');
                if datalength(opd) == 4 then
                    ;;; scaled
                    asmf_printf(f_subv(4, opd), '*%p');
                endif;
            endif;
            if isintegral(disp) and disp < 0 then
                asmf_printf(disp, '%p');
            elseunless disp == 0 then
                asmf_printf(disp, '+%p');
            endif;
            asmf_charout(`]`);
        else
            mishap(opd, 1, 'ILLEGAL OPERAND');
        endif;
    endif;
#_ELSE
    if ispair(opd) then
        asmf_charout(`*`);
        f_front(opd) -> opd;
    endif;
    if isword(opd) then
        ;;; assume register: can't use isreg, because it won't recognise
        ;;; the byte- and word-length register names
        asmf_printf(opd, '%%%p');
    elseif isimm(opd) then
        asmf_printf(immval(opd), '$%p');
    elseif isabs(opd) then
        asmf_printf(opd, '%p');
    elseif isvector(opd) then
        ;;; register based/indexed
        lvars disp = f_subv(2, opd);
        if isboolean(disp) then mishap(opd, 1, 'ILLEGAL AUTO-INDIRECTION') endif;
        if isref(disp) then fast_cont(disp) -> disp endif;
        unless disp == 0 then asmf_pr(disp) endunless;
        asmf_charout(`(`);
        if f_subv(1, opd) then asmf_printf(f_subv(1, opd), '%%%p') endif;
        if datalength(opd) > 2 then asmf_printf(f_subv(3, opd), ', %%%p') endif;
        if datalength(opd) > 3 then asmf_printf(f_subv(4, opd), ', %p') endif;
        asmf_charout(`)`);
    else
        mishap(opd, 1, 'ILLEGAL OPERAND');
    endif;
#_ENDIF
enddefine;

;;; outinst:
;;;     writes out an instruction

define lconstant outinst(instr);
    lvars instr;
#_IF DEF MASM
    lconstant COMMENT = `;`;
#_ELSE
    lconstant COMMENT = `/`;
#_ENDIF
    lvars opcode = f_subv(1, instr);
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
    lvars codelist, hdr_len, pdr_len;

    ;;; Translate M-code to assembler
    generate(codelist) -> codelist;

    ;;; Create a label for the procedure length
    genlab() -> pdr_len;

    ;;; Create the code-output procedure
    define lconstant gencode();
        lvars endlab;
        ;;; Output the code
        applist(codelist, outinst);
        ;;; Align on a longword boundary
        asm_align_word();
        ;;; Plant an end label
        outlab(genlab() ->> endlab);
        ;;; Define pdr_len as the size in words of the procedure
        outlabset(pdr_len, asm_pdr_len(hdr_len, current_pdr_exec_label, endlab));
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


/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec  5 1997
        Replaced uses of f_hd applied to pairs with f_front (f_hd didn't
        work with pop_debugging true).
--- Robert Duncan, Jun 16 1997
        Fixed some optimisations for when MASM is set.
--- Robert Duncan, Jul 31 1996
        Removed S*CO mods entirely!
--- Robert John Duncan, Sep 13 1995
        Corrected S*CO mods
--- Poplog System, Jan 18 1995
        Modifications for Linux and S*CO.
--- Robert John Duncan, Sep  5 1994
        Modified to generate either Microsoft/Intel style assembly code
        or Unix style depending on the flag MASM.
--- Robert John Duncan, Sep  1 1994
        Removed M_AR*RAY_SUB (no longer necessary)
--- Robert John Duncan, Jan 26 1994
        Revised to assume a PC/Unix-type system, based on Jon Meyer's
        changes for ATT386; fixed outopnd to recognise byte register names
        (%al, etc.)
--- Robert John Duncan, Oct 15 1992
        Changed M_CLOSURE to call Exec_closure for more than 16 frozvals
--- Robert John Duncan, Jan  9 1992
        Changed M_CMPKEY to allow key arg to be an integer specifying flag(s)
        to be tested in K_FLAGS field.
--- John Gibson, Jan  8 1992
        Added _por and _pand to mc_inline_procs_list
--- Rob Duncan, Feb  5 1990
        Added -hdr_len- arg to -mc_code_generator- and corrected
        procedure length expression for new pointers
--- John Gibson, Jun  7 1989
        Included common.ph
--- Rob Duncan, May 17 1989
        Changed the assembler expression for computing the procedure size
        in -mc_code_generator- to overcome another Symmetry assembler bug
--- John Gibson, May 17 1989
        Changed all references to subroutine names to begin with \^_
        (Ctrl-_) instead of _.
        Replaced all uses of @ and @@ with calls to field_## on the
        name of the field (converted to word offset where necessary).
        -pdr_offset_opnd- replaced by -pdr_index_opnd-, which now takes
        a pop integer index instead of a sysint offset.
--- Rob Duncan, Apr 26 1989
        Added M_SETSTKLEN, M_PLOG_IFNOT_ATOM and M_PLOG_TERM_SWITCH.
        Changed M_CALLSUB to include any register arguments as part of the
        instruction; removed references to ARG_REG_0.
        Changed to use "define <name> = <exp> enddefine" form wherever
        possible and removed explicit setting of pdprops of m-code
        procedures.
--- Rob Duncan, Mar 31 1989
        Minor change to -M_UPDbit- to eliminate a redundant move when
        src == USP_+
--- John Gibson, Mar 23 1989
        -addr_add_pop_subscr- replaced with -cvt_pop_subscript-
--- Rob Duncan, Feb 17 1989
        Added M_ARRAY_SUB
--- Rob Duncan, Feb 15 1989
        Simplified register allocation strategy; added tests for SUN386
--- John Gibson, Feb 13 1989
        Improved interface from m_trans.p:
            Now only one code-generating procedure -mc_code_generator-,
        closure code being dealt with by the M_CLOSURE instruction.
            Additional procedure -pdr_index_opnd- required by m_trans
        for references to words in procedure header data
 */
