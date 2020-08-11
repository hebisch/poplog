/*
   Copyright Waldek Hebisch, you can distribute this file
   under terms of Free Poplog licence.
   File:        src/arm/ass.p
   Purpose:     Run-time assembler for ARM
   Author:      Waldek Hebisch
*/


#_INCLUDE 'declare.ph'
#_INCLUDE 'vmdefs.ph'
#_INCLUDE 'external.ph'

global constant

    ;;; Assembly code subroutines referenced by user procedures

    _popenter,
    _popuenter,
    _popuncenter,
    _checkall,
    _bfield,
    _sbfield,
    _ubfield,
    _setstklen,
    _setstklen_diff,
;

constant
    procedure (
        initintvec,
    )
;

global vars
    _trap,              ;;; interrupt flag
    pop_debugging,      ;;; <false> if optimisation pass wanted
;

section $-Sys$-Vm;

constant

    procedure (

        ;;; VM interface

        Code_pass,
        Drop_I_code,
        Get_procedure,
        Is_register,
        Trans_structure,
    ),
;

vars
    asm_clist,          ;;; list of I-code to be assembled
    asm_instr,          ;;; the current I-code instruction
    asm_struct_list,    ;;; list of items to go in the structure table
    _asm_pass,          ;;; assembly pass counter - false when dropping code
    _asm_drop_ptr,      ;;; pointer to drop code at
    _asm_code_offset,   ;;; offset into executable code (in bytes)
    _Nlocals,           ;;; number of dynamic locals
    _Npopstkvars,       ;;; number of pop on-stack lvars
    _Nstkvars,          ;;; total number of on-stack lvars
   ;;; distance between procedure start of code
    _pdr_offset,
   ;;; buffer for storing literals
   lit_buff,
   ;;; number of used positions in the literal buffer
   _lit_count,
   ;;; start position of literal pools
;
lvars
   literal_pools,
   ;;; end of code (= start of literals)
   ;;; _code_end,
   ;;; position of first instruction needing a literal
   _current_literal_zone,
   ;;; start of current literal pool
   _current_literal_pool,
;

endsection;

;;; ---------------------------------------------------------------------

section $-Sys$-Vm;

lvars
      _regmask,
      _strsize, ;;; size of structure table in bytes
;

lconstant

    ;;; INSTRUCTION ENCODING

    ;;; Opcodes

    _ADD    = _16:E0800000,
              ;;; e0813003        add     r3, r1, r3
    _B_XX   = _16:0A000000,
              ;;;  0a00000e        beq     +0x40
              ;;;  1afffff0        bne     -0x38
    _BLX    = _16:E12FFF30,
              ;;;  e12fff3c        blx     ip
              ;;;  e12fff33        blx     r3
    _BLXNE  = _16:112FFF30,
    _BX    = _16:E12FFF10,
              ;;;  e12fff11        bx      r1
    _CMP    = _16:E1500000,
              ;;; e154000a        cmp     r4, sl
              ;;; e3530000        cmp     r3, #0
              ;;; 
    _LDMFD  = _16:E8BD0000,
    _LDR    = _16:E4100000,
              ;;;    e5bcf4f0        ldr     pc, [ip, #1264]!
              ;;;    e59f3034        ldr     r3, [pc, #52]
              ;;;    e7932102        ldr     r2, [r3, r2, lsl #2]
              ;;;    e7933002        ldr     r3, [r3, r2]
              ;;;    e49a1004        ldr     r1, [sl], #4
              ;;;    e51b2008        ldr     r2, [fp, #-8]
    _MOV    = _16:E1A00000,
              ;;; e1a01007        mov     r1, r7
              ;;; e3a04000        mov     r4, #0
    _MUL    = _16:E0000000,
    _MVN    = _16:E1E00000,
              ;;; e1e01000        mvn     r1, r0
              ;;; e3e00012        mvn     r0, #18
    _RET    = _16:E12FFF1E,
    _RSB    = _16:E0600000,
              ;;; e065a00a        rsb     sl, r5, sl
    _STMFD  = _16:E92D0000,
    _STR    = _16:E4000000,
              ;;; e50b3008        str     r3, [fp, #-8]
              ;;; e52dc004        str ip, [sp, #-4]!
    _STREQ  = _16:4000000,
    _SUB    = _16:E0400000,
    _TST    = _16:E1100000,

    ;;; Helper constants
    _dst_off       = _12,
    _arg1_off      = _16,
    _plus_flag     = _16:800000,
    _auto_indexing = _16:0200000,
    _pre_offset    = _16:1000000,
    _immediate_bit = _16:2000000,

    ;;; Register codes

    _R0    = _2:000,
    _R1    = _2:001,
    _R2    = _2:010,
    _R3    = _2:011,
    _R4    = _2:100,
    _R5    = _2:101,
    _R6    = _2:110,
    _R7    = _2:111,

    _R8     = _2:1000,
    _R9     = _2:1001,
    _R10    = _2:1010,
    _R11    = _2:1011,
    _R12    = _2:1100,
    _R13    = _2:1101,
    _R14    = _2:1110,
    _R15    = _2:1111,

    ;;; Condition codes

    _cc_EQ  = _2:0000,          ;;; equal
    _cc_NE  = _2:0001,          ;;; not equal
    _cc_HI  = _2:1000,          ;;; unsigned greater-than (above)
    _cc_CS  = _2:0010,          ;;; unsigned greater-than-or-equal
    _cc_CC  = _2:0011,          ;;; unsigned less-than (below)
    _cc_LS  = _2:1001,          ;;; unsigned less-than-or-equal
    _cc_GT  = _2:1100,          ;;; signed greater-than
    _cc_GE  = _2:1010,          ;;; signed greater-than-or-equal
    _cc_LT  = _2:1011,          ;;; signed less-than
    _cc_LE  = _2:1101,          ;;; signed less-than-or-equal

    _cc_ALL = _16:E,            ;;; uncondtional

    ;;; Condition-code mask

    _cc_NOT = _2:0001,          ;;; negates a condition

;

lconstant

    ;;; NAMED REGISTERS

    _SP     = _R13,             ;;; system stack pointer
    _USP    = _R10,             ;;; user stack pointer
    _PB     = _R11,             ;;; procedure base register
    _LR     = _R14,             ;;; link register
    _PC     = _R15,             ;;; program counter

    _ARG_REG_0  = _R0,         ;;; for arguments to subroutines
    _CHAIN_REG  = _R2,         ;;; for targets of chaining
;

protected register constant

    ;;; REGISTER IDENTIFIERS
    ;;; (available to the VM)

    ;;; The value is a popint register number shifted left 1
    ;;; plus bit 0 if pop reg

    ;;; arg_reg_0, _1, & _2 :
    ;;; for passing arguments to subroutines. _1 & _2 are (currently) used
    ;;; only by Prolog instructions.
    ;;; NB. arg_reg_0 (EAX) is also used as a working register for complex
    ;;; moves. There should be at most only simple moves to arg_reg_1 & _2
    ;;; between the assignment to arg_reg_0 and the corresponding subroutine
    ;;; call.

    arg_reg_0   = _pint(_ARG_REG_0) << 1,       ;;; R0
    arg_reg_1   = _pint(_R1) << 1,
    arg_reg_2   = _pint(_R2) << 1,

    chain_reg   = _pint(_CHAIN_REG) << 1,       ;;; R2

/*
        pop_reg_A     = _pint(_R13) <<1 || 1,
        pop_reg_B     = _pint(_R14) <<1 || 1,
        pop_reg_C     = _pint(_R15) <<1 || 1,
        nonpop_reg_A = _pint(_R8) <<1,
        nonpop_reg_B = _pint(_R10) <<1,
        nonpop_reg_C = _pint(_R11) <<1,
        nonpop_reg_D = _pint(_R12) <<1,
*/
;

constant

    ;;; REGISTER LVARS

        asm_pop_registers = [[]],

        asm_nonpop_registers = [[]],

/*

        asm_pop_registers = [%[], ident pop_reg_A, ident pop_reg_B,
                                  ident pop_reg_C %],
        asm_nonpop_registers = [%[], ident nonpop_reg_A, ident nonpop_reg_B,
                                 ident nonpop_reg_C, ident nonpop_reg_D %],

        asm_nonpop_registers = [%[], ident nonpop_reg_A, ident nonpop_reg_D %],
*/
;


define Is_address_reg() with_nargs 1;
    Is_register()           ;;; every reg is an address reg
enddefine;


;;; === CODE-PLANTING PROCEDURES ======================================

;;; Drop_w :
;;;     put a word in the procedure record

define lconstant do_drop_w(_long);
    lvars _long;
;;;     lvars _pword = _long;
    unless _asm_pass then
        ;;; only output the data on the last pass
;;;                _extern printf('Drop_l(0x%08x)\n', _long);
#_IF false
                _extern printf('%c', _pword);
                _shift(_pword, _-8) - > _pword;
                _extern printf('%c', _pword);
                _shift(_pword, _-8) - > _pword;
                _extern printf('%c', _pword);
                _shift(_pword, _-8) - > _pword;
                _extern printf('%c', _pword);
                _extern fflush (_0);
#_ENDIF
        _long -> _asm_drop_ptr!(i)++ -> _asm_drop_ptr;
    endunless;
    @@(i){_asm_code_offset}++ -> _asm_code_offset;
enddefine;

define lconstant dump_literals();
/*
    printf(_asm_pass, 'dump_literals, _asm_pass = %p, ');
    printf(literal_pools, 'literal_pools = %p, ');
    printf(_pint(_lit_count), '_lit_count = %p\n');
    printf(_pint(_asm_code_offset), '_asm_code_offset = %p, ');
    printf(_pint(_current_literal_zone), '_current_literal_zone = %p\n');
*/
    if _asm_pass then
        conspair(_pint(_asm_code_offset), literal_pools)
            -> literal_pools;
    endif;
    lvars _n = _0;
    while _n _lt _lit_count do
        do_drop_w(lit_buff!(i)[_n]);
        _n _add _1 -> _n;
    endwhile;
    if _asm_pass then
        _asm_code_offset _add _16:FFF -> _current_literal_pool;
    else
        _int(front(literal_pools)) -> _current_literal_pool;
        back(literal_pools) -> literal_pools;
    endif;
    _0 -> _lit_count;
enddefine;

define drop_w(_long);
    lvars _long;
    ;;; printf(_pint(_asm_code_offset), 'drop_w, _asm_code_offset = %p\n');
    do_drop_w(_long);
    if _lit_count == _0 or
       (_asm_code_offset _sub _current_literal_zone) _slt _1000 then
        return();
    endif;
    ;;; Jump will go _off + 2 words forward from current position
    ;;; we need to skip over single word for jump and _lit_count
    ;;; literal words.  So in balance we subtract 1 from _lit_count.
    lvars _off = _lit_count _sub _1;
    do_drop_w(_B_XX _biset _shift(_cc_ALL, _28) _biset _off);
    dump_literals();
enddefine;
;;;

define drop_op3(_opcode, _reg, _arg1, _arg2);
    drop_w(_opcode _biset _shift(_reg, _dst_off)
                   _biset _shift(_arg1, _arg1_off)
                   _biset _arg2);
enddefine;

define Mem_offset(_off);
    if _off _slt _0 then
        _negate(_off)
    else
        _off _biset _plus_flag
    endif -> _off;
    _off _biset _pre_offset
enddefine;

define drop_mem_imm_off(_opcode, _reg, _base, _off);
    drop_op3(_opcode, _reg, _base, Mem_offset(_off));
enddefine;

define lconstant drop_mem_indexed(_opcode, _reg, _base, _ind);
    drop_op3(_opcode, _reg, _base,
        _pre_offset _biset _immediate_bit _biset _plus_flag _biset _ind);
enddefine;

define try_encode_imm(_imm);
    lvars _shift_cnt = _0;
    if _imm _lt _256 then
        return(_imm _biset _immediate_bit);
    endif;
    if not(_imm _bitst _16:FFFF) then
        _16 -> _shift_cnt;
        _shift(_imm, _-16) _bimask _16:FFFF -> _imm;
    endif;
    if not(_imm _bitst _16:FF) then
        _shift_cnt _add _8  -> _shift_cnt;
        _shift(_imm, _-8) -> _imm;
    endif;
    if not(_imm _bitst _16:F) then
        _shift_cnt _add _4  -> _shift_cnt;
        _shift(_imm, _-4) -> _imm;
    endif;
    if not(_imm _bitst _16:3) then
        _shift_cnt _add _2 -> _shift_cnt;
        _shift(_imm, _-2) -> _imm;
    endif;
    if _imm _greq _256 then
        return(_0);
    endif;
    _shift(_32 _sub _shift_cnt, _7) _biset _imm _biset _immediate_bit;
enddefine;

define encode_imm(_imm);
    lvars _res = try_encode_imm(_imm);
    if _nonzero(_res) then
        return(_res)
    else
        mishap(0, 'encode_imm: bad immediate');
    endif;
enddefine;

define drop_op_imm(_opcode, _dst, _arg1, _imm);
    drop_op3(_opcode, _dst, _arg1, encode_imm(_imm));
enddefine;

define drop_push_reg(_reg, _base);
    drop_op3(_STR, _reg, _base,
             _4 _biset _auto_indexing _biset _pre_offset);
enddefine;

define drop_pop_reg(_reg, _base);
    drop_op3(_LDR, _reg, _base,
             _4 _biset _auto_indexing _biset _plus_flag);
enddefine;

define drop_br_cond(_cc, _off);
    lvars _cc, _off;
    _off _sub _asm_code_offset -> _off;
    _shift(_off _sub _8, _-2) _bimask _16:FFFFFF -> _off;
    drop_w(_B_XX _biset _shift(_cc, _28) _biset _off);
enddefine;

define load_literal(_reg, _imm);
    lvars _tmp = try_encode_imm(_imm);
    if _nonzero(_tmp) then
        drop_op3(_MOV, _reg, _R0, _tmp);
    elseif _nonzero(try_encode_imm(_-1 _sub _imm) ->> _tmp) then
        drop_op3(_MVN, _reg, _R0, _tmp);
    else
        ;;; printf(_pint(_lit_count), 'load_literal, _lit_count = %p, ');
        ;;; printf(_pint(_asm_code_offset), '_asm_code_offset = %p\n');
        if _lit_count == _0 then
            _asm_code_offset -> _current_literal_zone;
        endif;
        if _lit_count _lt _512 then
            lvars _lit_pos = _shift(_lit_count, _2) _add _current_literal_pool,
                  _dist = _lit_pos _sub _asm_code_offset _sub _8;
            if _dist _greq _16:FFF then
                mishap(0, 'load_literal: distance to literal pool too large');
            else
                _imm -> lit_buff!(i)[_lit_count];
                _lit_count _add _1 -> _lit_count;
                drop_mem_imm_off(_LDR, _reg, _PC, _dist);
            endif;
        else
            mishap(0, 'load_literal: too many literals');
        endif;
    endif;
enddefine;

define do_load_or_store(_opcode, _reg, structure, defer, _tmp_reg);
    lvars _str = _int(structure);
    if _neg(_str) then
        ;;; Negated offset for an on-stack lvar, shifted left by 1
        ;;; If bit 0 is set, access is via a ref (another indirection),
        ;;; but this is disabled by defer being <false>
        unless _str _bitst _1 then false -> defer endunless;
        _negate(_shift(_str, _-1)) -> _str;
        drop_mem_imm_off(if defer then _LDR else _opcode endif,
                         if defer then _tmp_reg else _reg endif,
                         _SP, _str);
    else
        if _opcode == _LDR or defer then
            drop_mem_imm_off(_LDR,
                             if defer then _tmp_reg else _reg endif,
                             _PB, @@PD_TABLE{_str});
        else
            mishap(0, 'store to frozen value');
        endif;
    endif;
    if defer then
        drop_mem_imm_off(_opcode, _reg, _tmp_reg, _0);
    endif;
enddefine;

define get_arg(_arg);
    lvars structure;
    asm_instr!INST_ARGS[_arg] -> structure;
    if iscompound(structure) and structure >=@(w) _system_end then
        ;;; replace structure with offset or register ident on pass 0
        Trans_structure(structure) ->> structure -> asm_instr!INST_ARGS[_arg]
    endif;
    structure;
enddefine;

define load_from_arg(_arg, defer, _tmp_reg);
    lvars _arg, defer, _tmp_reg, _reg,
           structure = get_arg(_arg);
    if issimple(structure) then
        do_load_or_store(_LDR, _tmp_reg, structure, defer, _tmp_reg);
        _tmp_reg;
    elseif Is_register(structure) ->> _reg then
        unless defer then
            mishap(0, 'REGISTER USED AS IMMEDIATE OPERAND')
        endunless;
        _int(_reg);
    else
        load_literal(_tmp_reg, structure);
        if defer then
           drop_mem_imm_off(_LDR, _tmp_reg, _tmp_reg, _0);
        endif;
        _tmp_reg;
    endif;
enddefine;

define store_reg_to_arg(_reg, _arg, defer, _tmp_reg);
    lvars _reg, _arg, defer, _tmp_reg,
          structure = get_arg(_arg);
    if issimple(structure) then
        do_load_or_store(_STR, _reg, structure, defer, _tmp_reg);
    elseif Is_register(structure) ->> _arg then
        unless defer then
            mishap(0, 'REGISTER USED AS IMMEDIATE OPERAND')
        endunless;
        drop_op3(_MOV, _int(_arg), _R0, _reg);
    elseif defer then
        load_literal(_tmp_reg, structure);
        drop_mem_imm_off(_STR, _reg, _tmp_reg, _0);
    else
        mishap(0, 'store_reg_to_arg unimplemented');
    endif;
enddefine;

define store_reg(_reg);
    if asm_instr!V_LENGTH == _2 then
        drop_push_reg(_reg, _USP);
    else
        store_reg_to_arg(_reg, _1, true, _R1);
    endif;
enddefine;

define load_fsrc_to_reg(_arg, _reg);
    dlocal asm_instr;
    lvars opd = asm_instr!INST_ARGS[_arg];
    if opd then
        lvars op = opd!INST_OP;
        if op == I_MOVENUM or op == I_MOVEADDR then
            load_literal(_reg, opd!INST_ARGS[_0]);
            _reg;
        else
            opd -> asm_instr;
            load_from_arg(_0, op == I_MOVE, _reg);
        endif;
    else
        drop_pop_reg(_reg, _USP);
        _reg;
    endif;
enddefine;

;;; === TRANSLATING I-CODE INSTRUCTIONS ===============================


;;; I-code data movement instructions:

define I_POP();
    ;;; printf('I_POP\n');
    drop_pop_reg(_R0, _USP);
    store_reg_to_arg(_R0, _0, true, _R1);
enddefine;

define I_POPQ();
    ;;; printf('I_POPQ\n');
    drop_pop_reg(_R0, _USP);
    store_reg_to_arg(_R0, _0, false, _R1);
enddefine;

define I_STORE();
    ;;; printf('I_STORE\n');
    drop_mem_imm_off(_LDR, _R0, _USP, _0);
    store_reg_to_arg(_R0, _0, true, _R1);
enddefine;

define I_MOVE();
    ;;; printf('I_MOVE\n');
    lvars _dest_reg = false, structure;
    ;;; We can not use _R0 for simple move to other registers,
    ;;; as that may destroy arguments of calls to assembler
    ;;; routines
    if asm_instr!V_LENGTH == _3 then
        get_arg(_1) -> structure;
        if Is_register(structure) ->> _dest_reg then
            _int(_dest_reg) -> _dest_reg;
        endif;
    endif;
    lvars _reg = if _dest_reg then _dest_reg else _R0 endif;
    load_from_arg(_0, true, _reg) -> _reg;
    if _reg /== _dest_reg then
        store_reg(_reg);
    endif;
enddefine;

define I_MOVEQ();
    ;;; printf('I_MOVEQ\n');
    lvars _dest_reg = false, structure;
    ;;; We can not use _R0 for simple move to other registers,
    ;;; as that may destroy arguments of calls to assembler
    ;;; routines
    if asm_instr!V_LENGTH == _3 then
        get_arg(_1) -> structure;
        if Is_register(structure) ->> _dest_reg then
            _int(_dest_reg) -> _dest_reg;
        endif;
    endif;
    lvars _reg = if _dest_reg then _dest_reg else _R0 endif;
    load_from_arg(_0, false, _reg) -> _reg;
    if _reg /== _dest_reg then
        store_reg(_reg);
    endif;
enddefine;

define I_MOVES();
    ;;; printf('I_MOVES\n');
    drop_mem_imm_off(_LDR, _R0, _USP, _0);
    drop_push_reg(_R0, _USP);
enddefine;

define Do_move_imm();
    lvars _arg1;
    asm_instr!INST_ARGS[_0] -> _arg1;
    lvars _dest_reg = false, structure;
    ;;; We can not use _R0 for simple move to other registers,
    ;;; as that may destroy arguments of calls to assembler
    ;;; routines
    if asm_instr!V_LENGTH == _3 then
        get_arg(_1) -> structure;
        if Is_register(structure) ->> _dest_reg then
            _int(_dest_reg) -> _dest_reg;
        endif;
    endif;
    lvars _reg = if _dest_reg then _dest_reg else _R0 endif;
    load_literal(_reg, _arg1);
    if _reg /== _dest_reg then
        store_reg(_reg);
    endif;
enddefine;

define I_MOVENUM();
    ;;; printf('I_MOVENUM\n');
    Do_move_imm();
enddefine;

define I_MOVEADDR();
    ;;; printf('I_MOVEADDR\n');
    Do_move_imm();
enddefine;

;;; Move to/from return address slot in stack frame
;;; (same as I_MOVE or I_MOVEADDR)
define I_MOVE_CALLER_RETURN();
    ;;; printf('I_MOVE_CALLER_RETURN\n');
    fast_chain(asm_instr!INST_ARGS[_2])
enddefine;

define I_PUSH_UINT();
    ;;; printf('I_PUSH_UINT\n');
    lvars _arg1;
    Pint_->_uint(asm_instr!INST_ARGS[_0], _-1) -> _arg1;
    load_literal(_R0, _arg1);
    drop_push_reg(_R0, _USP);
enddefine;

define I_ERASE();
    ;;; printf('I_ERASE\n');
    ;;; mishap(0, 'I_ERASE unimplemented');
    drop_op_imm(_ADD, _USP, _USP, _4);
enddefine;

define I_SWAP();
    lvars _i, _j;
    ;;; printf('I_SWAP\n');
    _int(asm_instr!INST_ARGS[_0]) -> _i;
    _int(asm_instr!INST_ARGS[_1]) -> _j;
    if _i _slt _0 or _j _slt _0 then
        printf(_pint(_i), '_i = %p, ');
        printf(_pint(_j), '_j = %p\n');
        mishap(0, 'I_SWAP with negative index')
    endif;
    if _i _slt _1024 and _j _slt _1024 then
        drop_mem_imm_off(_LDR, _R0, _USP, _shift(_i, _2));
        drop_mem_imm_off(_LDR, _R1, _USP, _shift(_j, _2));
        drop_mem_imm_off(_STR, _R0, _USP, _shift(_j, _2));
        drop_mem_imm_off(_STR, _R1, _USP, _shift(_i, _2));
    else
        mishap(0, 'I_SWAP with too large index');
    endif;
enddefine;


/*  Standard Field Access for -conskey-, -sysFIELD_VAL- and -sysSUBSCR-  */

;;; stack_offset:
;;;     checks that -structure- refers to a direct, on-stack lvar
;;;     and returns the appropriate offset as a sysint

define constant stack_offset(/* structure */) with_nargs 1;
    lvars structure;
    if isinteger(Trans_structure(/* structure */) ->> structure)
    and structure fi_< 0
    and not(_int(structure) _bitst _1)
    then
        _negate(_shift(_int(structure), _-1));
    else
        false;
    endif;
enddefine;

;;; I-code field access instructions:

define lconstant deref_exptr(exptr, _reg, _tmp_reg);
    lvars exptr, _reg, _tmp_reg;
    fast_repeat exptr times
        drop_mem_imm_off(_LDR, _tmp_reg, _reg, _0);
        _tmp_reg -> _reg;
    endrepeat
enddefine;

lconstant

    ;;; Type codes for signed fields

    t_SGN_BYTE  = t_BYTE  || t_SIGNED,
    t_SGN_SHORT = t_SHORT || t_SIGNED,
    t_SGN_INT   = t_INT   || t_SIGNED,
    t_SGN_WORD  = t_WORD  || t_SIGNED,

    ;;; Field sizes (in bytes) of the various types

    field_size = list_assoc_val(% [%
        t_BYTE,         1,
        t_SHORT,        2,
        t_INT,          4,
        t_WORD,         4,
        t_DOUBLE,       8,
        t_SGN_BYTE,     1,
        t_SGN_SHORT,    2,
        t_SGN_INT,      4,
        t_SGN_WORD,     4,
    %] %),

    _load_bit = _16:100000,
    _cond_all_bits = _16:E0000000,

    access_op = list_assoc_val(% [%
        t_BYTE,         _pint(_16:5C00000),  ;;; strb
        t_SHORT,        _pint(_16:1C000B0),  ;;; strh
        t_INT,          _pint(_16:5800000),  ;;; str
        t_WORD,         _pint(_16:5800000),  ;;; str
        t_DOUBLE,       0,  ;;; unsupported
        t_SGN_BYTE,     _pint(_16:1C000D0),  ;;; strsb
        t_SGN_SHORT,    _pint(_16:1C000F0),  ;;; strsh
        t_SGN_INT,      _pint(_16:5800000),  ;;; str
        t_SGN_WORD,     _pint(_16:5800000),  ;;; str
    %] %),

    ;;; Index-scale codes for the possible field sizes

    index_scale = list_assoc_val(% [%
        1,  _pint(_0),
        2,  _pint(_1),
        4,  _pint(_2),
        8,  _pint(_3),
    %] %),

;

define lconstant load_structure_addr(structure, exptr);
    lvars structure, _reg = _R0, _disp;
    if not(structure) then
        drop_pop_reg(_reg, _USP);
    elseif stack_offset(structure) ->> _disp then
        drop_mem_imm_off(_LDR, _R0, _SP, _disp);
    elseif Is_register(Trans_structure(structure)) ->> _reg then
        _int(_reg) -> _reg;
    else
        mishap(structure, 1, 'SYSTEM ERROR 1 IN load_field_addr');
    endif;
    if exptr then
        deref_exptr(exptr, _reg, _R0);
        _R0 -> _reg;
    endif;
    _reg;
enddefine;

define load_field_addr(type, structure, _size, offset, exptr);
    lvars type, structure, _size, offset, exptr;
    lvars _reg = load_structure_addr(structure, exptr), _disp;

    if isinteger(offset) then
        ;;; mishap(0, 'isinteger(offset) in load_field_addr');
        ;;; Convert bit offset to bytes
        ##(b){_int(offset)|1} -> _disp;
/*
        lvars _tmp = try_encode_imm(_imm);
        if _nonzero(_tmp) then
            drop_op3(_ADD, _R0, _reg, _tmp);
        elseif _nonzero(try_encode_imm(_negate(_disp)) ->> _tmp) then
            drop_op3(_SUB, _R0, _reg, _tmp);
        else
*/            
    else
        if not(offset) then
            drop_pop_reg(_R1, _USP);
        elseif stack_offset(offset) ->> _disp then
            drop_mem_imm_off(_LDR, _R1, _SP, _disp);
/*
        elseif Is_register(Trans_structure(offset)) ->> _tmp_reg then
            _int(_tmp_reg) -> _tmp_reg;
*/
        else
            mishap(offset, 1, 'SYSTEM ERROR 2 IN load_field_addr');
        endif;
        lvars _n = field_size(type);
        if _size == 1 then
            ;;; Compute a displacement to be added to the index which accounts
            ;;; for the V_BYTES offset (length+key) and for base-1 indexing
            @@V_BYTES-{_int(_n)} -> _disp;
            if _n == 4 then
                ;;; Index-scaling already accounted for, since index is popint
                1 -> _n;
                ;;; Adjust displacement by 3 to account for the popint bits
                _disp _sub _3 -> _disp;
            else
                ;;; mov r1, r1, asr #2
                drop_op3(_MOV, _R1, _R0, _R1 _biset _16:140);
            endif;
        else
            _int(_size fi_* _n) -> _size;
            @@V_BYTES-{_size} -> _disp;
            load_literal(_R12, _size);
            ;;; mov r1, r1, asr #2
            drop_op3(_MOV, _R1, _R0, _R1 _biset _16:140);
            ;;; mul _R1, _R12, _R1
            drop_op3(_MUL, _R0, _R1, _shift(_R1, _8) _biset _16:90 _biset _R12);
            1 -> _n;
        endif;
        drop_op3(_ADD, _R0, _reg,
                  _R1 _biset _shift(_int(index_scale(_n)), _7));
        _R0 -> _reg;
    endif;
    load_literal(_R1, _disp);
    drop_op3(_ADD, _R0, _reg, _R1);
    _R0;
enddefine;

define lconstant do_bit_field(type, _size, structure, offset, upd, exptr);
    lvars _reg = load_structure_addr(structure, exptr), _disp;
    ;;; mishap(0, 'do_bit_field unimplemented');
    if _reg /== _R2 then
        drop_op3(_MOV, _R2, _R0, _reg);
    endif;
    if isinteger(offset) then
        load_literal(_R1, _int(offset));
    else
        if not(offset) then
            drop_pop_reg(_R1, _USP);
        elseif stack_offset(offset) ->> _disp then
            drop_mem_imm_off(_LDR, _R1, _SP, _disp);
/*
        elseif Is_register(Trans_structure(offset)) ->> _tmp_reg then
            _int(_tmp_reg) -> _tmp_reg;
*/
        else
            mishap(0, 'SYSTEM ERROR do_bit_field');
        endif;
        ;;; Convert offset to machine integer
        drop_op3(_MOV, _R1, _R0, _R1 _biset _16:140);
        load_literal(_R12, _int(_size));
        ;;; mul _R1, _R12, _R1
        drop_op3(_MUL, _R0, _R1, _shift(_R1, _8) _biset _16:90 _biset _R12);
        ##(1){@@V_BYTES|b} _sub _int(_size) -> _disp;
        if not(_zero(_disp)) then
            load_literal(_R12, _disp);
            drop_op3(_ADD, _R1, _R1, _R12);
        endif;
    endif;
    load_literal(_R0, _int(_size));
    lvars _routine = if upd then _ubfield
                     elseif type == t_BIT then _bfield
                     else _sbfield endif;
    load_literal(_R12, _routine);
    drop_w(_BLX _biset _R12);
enddefine;

define I_PUSH_FIELD();
    lvars type, _size, structure, offset, cvt, exptr;
    ;;; printf('I_PUSH_FIELD\n');
    explode(asm_instr) -> exptr -> cvt -> offset -> structure ->
                       _size -> type -> ;
    if type fi_&& t_BASE_TYPE == t_BIT then
        do_bit_field(type, _size, structure, offset, false, exptr);
    else
        lvars _reg = load_field_addr(type, structure, _size, offset, exptr);
        drop_op3(_int(access_op(type)) _biset _cond_all_bits 
                 _biset _load_bit, _R0, _reg, _0);
    endif;
    if cvt then
        ;;; convert to popint
        ;;;   mov r0, r0, asl #2
        drop_op3(_MOV, _R0, _R0, _R0 _biset _16:100);
        drop_op_imm(_ADD, _R0, _R0, _3);
    endif;
    drop_push_reg(_R0, _USP);
enddefine;

define I_POP_FIELD();
    lvars type, btype,_size, structure, offset, exptr;
    ;;; printf('I_POP_FIELD\n');
    explode(asm_instr) -> exptr -> offset -> structure -> _size -> type -> ;
    if ((type fi_&& t_BASE_TYPE) ->> btype) == t_BIT then
        do_bit_field(type, _size, structure, offset, true, exptr);
        return();
    endif;
    lvars _reg = load_field_addr(type, structure, _size, offset, exptr);
    drop_pop_reg(_R1, _USP);
    drop_op3(_int(access_op(btype)) _biset _cond_all_bits,
             _R1, _reg, _0);
enddefine;

define I_PUSH_FIELD_ADDR();
    lvars type, size, structure, offset, exptr;
    ;;; printf('I_PUSH_FIELD_ADDR\n');
    explode(asm_instr) -> exptr -> offset -> structure -> size -> type -> ;
    lvars _reg = load_field_addr(type, structure, size, offset, exptr);
    drop_push_reg(_reg, _USP);
enddefine;


/*  Fast Field Access  */

;;; I-code fast field access instructions:

define I_FASTFIELD();
    lvars _offs;
    ;;; printf('I_FASTFIELD\n');
    lvars _reg0 = load_fsrc_to_reg(_1, _R12);
    if asm_instr!INST_ARGS[_0] ->> _offs then
        _int(_offs) -> _offs;
    else
        drop_mem_imm_off(_LDR, _R0, _reg0, @@P_FRONT);
        drop_push_reg(_R0, _USP);
        @@P_BACK -> _offs;
    endif;
    if _offs _slt _1024 and _offs _sgr _-1024 then
        drop_mem_imm_off(_LDR, _R0, _reg0, _offs);
    else
        load_literal(_R1, _offs);
        drop_mem_indexed(_LDR, _R0, _reg0, _R1);
    endif;
    drop_push_reg(_R0, _USP);
enddefine;

define I_UFASTFIELD();
    ;;; printf('I_UFASTFIELD\n');
    lvars _reg0 = load_fsrc_to_reg(_1, _R0);
    drop_pop_reg(_R2, _USP);
    lvars _offs = _int(asm_instr!INST_ARGS[_0]);
    if _offs _slt _1024 and _offs _sgr _-1024 then
        drop_mem_imm_off(_STR, _R2, _reg0, _offs);
    else
        load_literal(_R1, _offs);
        drop_mem_indexed(_STR, _R2, _reg0, _R1);
    endif;
enddefine;

define I_FASTSUBV();
    ;;; printf('I_FASTSUBV\n');
    lvars _offs = _int(asm_instr!INST_ARGS[_0]) _sub 1;
    lvars _reg0 = load_fsrc_to_reg(_1, _R0);
    drop_pop_reg(_R1, _USP);
    if _offs _slt _256 and _offs _sgreq _0 then
        drop_op_imm(_ADD, _R1, _R1, _offs);
    elseif _offs _slt _0 and _offs _sgr _-256 then
        drop_op_imm(_SUB, _R1, _R1, _negate(_offs));
    else
        mishap(0, 'I_FASTSUBV: too large offset');
    endif;
    drop_mem_indexed(_LDR, _R0, _R0, _R1);
    drop_push_reg(_R0, _USP);
enddefine;

define I_UFASTSUBV();
    ;;; printf('I_UFASTSUBV\n');
    lvars _offs = _int(asm_instr!INST_ARGS[_0]) _sub 1;
    lvars _reg0 = load_fsrc_to_reg(_1, _R0);
    drop_pop_reg(_R1, _USP);
    if _offs _slt _256 and _offs _sgreq _0 then
        drop_op_imm(_ADD, _R1, _R1, _offs);
    elseif _offs _slt _0 and _offs _sgr _-256 then
        drop_op_imm(_SUB, _R1, _R1, _negate(_offs));
    else
        mishap(0, 'I_FASTSUBV: too large offset');
    endif;
    drop_pop_reg(_R2, _USP);
    drop_mem_indexed(_STR, _R2, _R0, _R1);
enddefine;


/*  Fast Integer +/-  */

define I_FAST_+-_2();
    lvars opd, _opcode;
    dlocal asm_instr;
    ;;; printf('I_FAST_+-_2\n');
    if asm_instr!INST_ARGS[_0] then _ADD else _SUB endif -> _opcode;
    lvars _reg0 = load_fsrc_to_reg(_1, _R0);
    lvars _reg1 = load_fsrc_to_reg(_2, _R1);
    drop_op_imm(_SUB, _R0, _reg0, _3);
    drop_op3(_opcode, _R0, _reg1, _R0);
    asm_instr!INST_ARGS[_2] -> opd;
    if opd then
        opd -> asm_instr;
        store_reg_to_arg(_R0, _0, true, _R1);
    else
        drop_push_reg(_R0, _USP);
    endif;
enddefine;

define I_FAST_+-_3();
    lvars _opcode;
    ;;; printf('I_FAST_+-_3\n');
    if asm_instr!INST_ARGS[_0] then _ADD else _SUB endif -> _opcode;
    lvars _reg0 = load_fsrc_to_reg(_1, _R0);
    lvars _reg1 = load_fsrc_to_reg(_2, _R1);
    drop_op_imm(_SUB, _R0, _reg0, _3);
    drop_op3(_opcode, _R0, _reg1, _R0);
    if asm_instr!INST_ARGS[_3] then
        store_reg_to_arg(_R0, _3, true, _R1);
    else
        drop_push_reg(_R0, _USP);
    endif;
enddefine;


/*  Branches and Tests  */

;;; I_LABEL:
;;;     called from -Code_pass- when a label is encountered in -asm_clist-

define I_LABEL();
    ;;; printf('I_LABEL\n');
    _pint(_asm_code_offset) -> fast_front(asm_clist);
enddefine;

;;; I_BR_std:
;;;     plant a relative branch instruction of a known size (5 bytes).

define I_BR_std(_broffset, _arg);
    lvars _broffset, _arg;
    ;;; printf('I_BR_std\n');
    drop_br_cond(_cc_ALL, _broffset);
enddefine;

;;; I_BR:
;;;     plant an unconditional relative jump (same as I_BR_std).

define I_BR(_broffset, _arg);
    lvars _broffset, _arg;
    ;;; printf('I_BR\n');
    drop_br_cond(_cc_ALL, _broffset);
enddefine;

;;; I_BRCOND:
;;;     standard conditional branch, used as argument to I_IF_opt etc.

define I_BRCOND(_ifso, _ifnot, _is_so, _broffset, _arg);
    lvars _ifso, _ifnot, _is_so, _broffset, _arg;
    ;;; printf('I_BRCOND\n');
    mishap(0, 'I_BRCOND unimplemented');
enddefine;

define drop_BR_cond(_ifso, _ifnot,  instr);
    if instr!INST_ARGS[_2] == I_BRCOND then
        drop_br_cond(if instr!INST_ARGS[_1] then _ifso else _ifnot endif,
                     _int(fast_front(instr!INST_ARGS[_0])));
    else
        mishap(0, 'Unknown branch instruction in drop_BR_cond');
    endif;
enddefine;

;;; {I_IF_opt ^target ^is_so ^opd}
;;;     the standard test instruction: compares the operand with <false>
;;;     and jumps to -target- if the test succeeds (or fails, depending on
;;;     the flag -is_so-).

define I_IF_opt();
    ;;; printf('I_IF_opt\n');
    lvars _reg0 = load_fsrc_to_reg(_3, _R0);
    load_literal(_R1, false);
    drop_op3(_CMP, _R0, _R1, _reg0);
    drop_BR_cond(_cc_NE, _cc_EQ, asm_instr);
enddefine;

;;; {I_BOOL_opt ^target ^is_so ^opd}
;;;     like I_IF_opt, but used for sysAND and sysOR: if the branch is
;;;     taken, the operand should be left on the stack.

define I_BOOL_opt();
    ;;; printf('I_BOOL_opt\n');
    lvars opd = asm_instr!INST_ARGS[_3];
    if opd then
        Drop_I_code(opd);
    endif;
    ;;; Argument is on user stack
    drop_op3(_LDR, _R0, _USP, _0);
    load_literal(_R1, false);
    drop_op3(_CMP, _R0, _R1, _R0);
    drop_BR_cond(_cc_NE, _cc_EQ, asm_instr);
    drop_op_imm(_ADD, _USP, _USP, _4);
enddefine;

lconstant

    ;;; Condition codes for comparison subroutines
    ;;; The first code is the obvious one, the second is the one to use
    ;;; if the order of the operands has been reversed

    cmp_condition_codes = [%
        nonop _eq,      _pint(_cc_EQ),  _pint(_cc_EQ),
        nonop _neq,     _pint(_cc_NE),  _pint(_cc_NE),
        nonop _sgr,     _pint(_cc_GT),  _pint(_cc_LE),
        nonop _slteq,   _pint(_cc_LE),  _pint(_cc_GT),
        nonop _slt,     _pint(_cc_LT),  _pint(_cc_GE),
        nonop _sgreq,   _pint(_cc_GE),  _pint(_cc_LT),
    %],

;

;;; {I_IF_CMP ^cmp_routine ^opd1 ^opd2 ^I_IF_opt}
;;;     optimised comparison:
;;;     "cmp_routine" is a comparison subroutine - _eq, _slt etc. - meant
;;;     to be applied to "opd1" and "opd2", but optimised here to be an
;;;     in-line CMP. The two operands are "fsrc"-type operands, i.e. either
;;;     MOVE instructions or <false> for top-of-stack. The last argument is
;;;     an I_IF_opt instruction for testing the result of the comparison.
;;;     The sense of the test is: cmp_routine(opd2, opd1), which means
;;;     testing the result of (opd2 - opd1)

define I_IF_CMP();
    ;;; printf('I_IF_CMP\n');
    lvars _reg1 = load_fsrc_to_reg(_1, _R1),
          _reg0 = load_fsrc_to_reg(_2, _R0);
    drop_op3(_CMP, _R0, _reg0, _reg1);
    lvars _cc = _int(fast_front(fast_back(lmember(asm_instr!INST_ARGS[_0],
                                          cmp_condition_codes))));
    drop_BR_cond(_cc, _cc _bixor _cc_NOT, asm_instr!INST_ARGS[_3]);
enddefine;

;;; {I_IF_TAG _routine operand I_IF_opt-instr}
;;;     where _routine is _issimple or _isinteger

define I_IF_TAG();
    lvars _mod, _rm, _disp;
    ;;; printf('I_IF_TAG\n');
    lvars _reg1 = load_fsrc_to_reg(_1, _R1);
    drop_op_imm(_TST, _R0, _reg1, 
        if asm_instr!INST_ARGS[_0] == _issimple then _2:01 else _2:10 endif);
    drop_BR_cond(_cc_NE, _cc_EQ, asm_instr!INST_ARGS[_2]);
enddefine;

;;; {I_SWITCH ^lablist ^elselab ^opd}
;;;     computed goto.

define I_SWITCH();
    lvars lablist, elselab, _ncases, _tabend, _offs;
    ;;; printf('I_SWITCH\n');
    ;;; mishap(0, 'I_SWITCH unimplemented');
    asm_instr!INST_ARGS[_0] -> lablist;
    asm_instr!INST_ARGS[_1] -> elselab;
    listlength(lablist) -> _ncases;

    ;;; Load the argument to R0.
    lvars _reg = load_fsrc_to_reg(_2, _R0);

    ;;; Compare the argument with the number of cases (both popints)
    load_literal(_R1, _ncases);
    drop_op3(_CMP, _R0, _reg, _R1);
    
    ;;; Compute the offset from the start of the procedure code to the end
    ;;; of the jump offset table: the _16 is the length of the four
    ;;; instructions between here and the first entry in the table,
    ;;; the _4 accounts for the extra (0) entry in the table.
    _asm_code_offset _add _16 _add _int(_ncases * 4) _add _4 -> _tabend;

    ;;; Remove popint bits from R0
    drop_op_imm(_SUB, _reg, _reg, _3);

    ;;; If the argument was out of range, jump to after the table
    ;;; The unsigned condition accounts for both arg < 0 and arg > ncases
    drop_br_cond(_cc_HI, _tabend);

    ;;; Use R0 to index int jump offset table.  On ARM PC point 8 bytes
    ;;; after start of current intstruction.  Since the current instruction
    ;;; and following jump take 8 bytes we need no extra offset.
    drop_mem_indexed(_LDR, _R1, _PC, _reg);

    ;;; Jump to PB + R1
    ;;;    add PC, PB, R1
    drop_op3(_ADD, _PC, _PB, _R1);

    ;;; Now plant the offset table:
    ;;; 0 case first (an error, so jumps to after the table) ...
    drop_w(@@PD_TABLE{_strsize _add _tabend});
    ;;; ... then all the given labels
    until lablist == [] do
        fast_front(fast_destpair(lablist) -> lablist) -> _offs;
        drop_w(@@PD_TABLE{_strsize _add _int(_offs)});
    enduntil;

    ;;; After the table: if there was no explicit "else" case, push the
    ;;; argument back on the stack for a following error
    unless elselab then
        ;;; Add back popint bits
        drop_op_imm(_ADD, _reg, _reg, _3);
        drop_push_reg(_reg, _USP);
    endunless;
    ;;; Then fall through
enddefine;

;;; {I_PLOG_IFNOT_ATOM ^fail_label ^I_BRCOND}
;;;     planted after a call to -prolog_unify_atom- from "aprolog.s".
;;;     That will have set flags for EQ if unification succeeded, NEQ if
;;;     failed. If it failed, use the I_BRCOND instruction to jump to
;;;     -fail_label-.

define I_PLOG_IFNOT_ATOM();
    ;;; printf('I_PLOG_IFNOT_ATOM\n');
    if asm_instr!INST_ARGS[_1] /== I_BRCOND then
        mishap(0, 'I_PLOG_IFNOT_ATOM with unexpected arguments');
    endif;
    drop_br_cond(_cc_NE, _int(fast_front(asm_instr!INST_ARGS[_0])));
enddefine;

;;; {I_PLOG_TERM_SWITCH ^fail_label I_BRCOND ^var_label I_BRCOND}
;;;     planted after a call to -prolog_pair(term)_switch- from "aprolog.s".
;;;     That will have set flags to EQ if item was a matching pair(term),
;;;     UGT if item was a variable and ULT otherwise (failure).
;;;     ** NB. This must not alter -arg_reg_0- **

define I_PLOG_TERM_SWITCH();
    ;;; printf('I_PLOG_TERM_SWITCH\n');
    if asm_instr!INST_ARGS[_1] /== I_BRCOND or
       asm_instr!INST_ARGS[_3] /== I_BRCOND then
        mishap(0, 'I_PLOG_TERM_SWITCH with unexpected arguments');
    endif;
    drop_br_cond(_cc_HI, _int(fast_front(asm_instr!INST_ARGS[_2])));
    drop_br_cond(_cc_CC, _int(fast_front(asm_instr!INST_ARGS[_0])));
enddefine;


;;; I-code procedure call instructions:

define load_proc(via_ident);
    lvars via_ident, _reg;
    load_from_arg(_0, via_ident, _R0) -> _reg;
    if not(_reg == _R0) then
        drop_op3(_MOV, _R0, _R0, _reg);
    endif;
enddefine;

define I_CALL();
    ;;; printf('I_CALL\n');
    load_proc(true);
    load_literal(_R1, _popenter);
    drop_w(_BLX _biset _R1);
enddefine;

define I_CALLQ();
    ;;; printf('I_CALLQ\n');
    load_proc(false);
    load_literal(_R1, _popenter);
    drop_w(_BLX _biset _R1);
enddefine;

define I_CALLP();
    ;;; printf('I_CALLP\n');
    load_proc(true);
    drop_mem_imm_off(_LDR, _R1, _R0, _0);
    drop_w(_BLX _biset _R1);
enddefine;

define I_CALLPQ();
    ;;; printf('I_CALLPQ\n');
    load_proc(false);
    drop_mem_imm_off(_LDR, _R1, _R0, _0);
    drop_w(_BLX _biset _R1);
enddefine;

define I_CALLS();
    ;;; printf('I_CALLS\n');
    drop_pop_reg(_R0, _USP);
    load_literal(_R1, _popenter);
    drop_w(_BLX _biset _R1);
enddefine;

define I_CALLPS();
    ;;; printf('I_CALLPS\n');
    drop_pop_reg(_R0, _USP);
    drop_mem_imm_off(_LDR, _R1, _R0, _0);
    drop_w(_BLX _biset _R1);
enddefine;

define I_UCALL();
    ;;; printf('I_UCALL\n');
    load_proc(true);
    load_literal(_R1, _popuenter);
    drop_w(_BLX _biset _R1);
enddefine;

define I_UCALLQ();
    ;;; printf('I_UCALLQ\n');
    load_proc(false);
    load_literal(_R1, _popuenter);
    drop_w(_BLX _biset _R1);
enddefine;

define I_UCALLP();
    ;;; printf('I_UCALLP\n');
    load_proc(true);
    load_literal(_R1, _popuncenter);
    drop_w(_BLX _biset _R1);
enddefine;

define I_UCALLPQ();
    ;;; printf('I_UCALLPQ\n');
    load_proc(false);
    load_literal(_R1, _popuncenter);
    drop_w(_BLX _biset _R1);
enddefine;

define I_UCALLS();
    ;;; printf('I_UCALLS\n');
    drop_pop_reg(_R0, _USP);
    load_literal(_R1, _popuenter);
    drop_w(_BLX _biset _R1);
enddefine;

define I_UCALLPS();
    ;;; printf('I_UCALLPS\n');
    drop_pop_reg(_R0, _USP);
    load_literal(_R1, _popuncenter);
    drop_w(_BLX _biset _R1);
enddefine;

define I_CALLABS();
    ;;; printf('I_CALLABS\n');
    load_literal(_R0, asm_instr!INST_ARGS[_0]);
    drop_mem_imm_off(_LDR, _R1, _R0, @@PD_EXECUTE);
    drop_w(_BLX _biset _R1);
enddefine;

define I_CHAIN_REG();
    ;;; printf('I_CHAIN_REG\n');
    lvars _reg;
    if Is_register(asm_instr!INST_ARGS[_0]) ->> _reg then
        drop_mem_imm_off(_LDR, _PC, _int(_reg), @@PD_EXECUTE);
        ;;; drop_w(_BX _biset _int(_reg));
    else
        mishap(0, 'I_CHAIN_REG without register unimplemented');
    endif;
enddefine;

define I_CALLSUB();
    ;;; printf('I_CALLSUB\n');
    ;;; _R0 is used for passing arguments, so we can not use it
    load_literal(_R12, asm_instr!INST_ARGS[_0]);
    drop_w(_BLX _biset _R12);
enddefine;

define I_CHAINSUB();
    ;;; printf('I_CHAINSUB\n');
    load_literal(_R0, asm_instr!INST_ARGS[_0]);
    drop_w(_BX _biset _R0);
enddefine;

define I_CALLSUB_REG();
    ;;; printf('I_CALLSUB_REG\n');
    lvars _addr, _reg;
    fast_front(asm_instr!INST_ARGS[_0]) -> _addr;
    if Is_register(_addr) ->> _reg then
        drop_w(_BLX _biset _int(_reg));
    else
        ;;; absolute address
        load_literal(_R12, _addr);
        drop_w(_BLX _biset _R12);
    endif;
enddefine;


/*  Procedure Entry and Exit  */

define I_CREATE_SF();
    lvars   _offs, _num, _tmp;

    ;;; printf('I_CREATE_SF\n');
    ;;; Set the procedure base register.
    ;;; sub PB, pc, #_off
    drop_op_imm(_SUB, _PB, _PC, _pdr_offset);
    ;;; save registers
    drop_w(_STMFD _biset _regmask _biset _shift(_1, _LR));

    ;;; Save dynamic locals
    @@PD_TABLE -> _offs;
    fast_repeat _pint(_Nlocals) times
        drop_mem_imm_off(_LDR, _R1, _PB, _offs);
        drop_mem_imm_off(_LDR, _R0, _R1, _0);
        drop_push_reg(_R0,  _SP);
        @@(w){_offs}++ -> _offs;
    endrepeat;

    ;;; FIXME: Do we need to clear POP registers?
    ;;; Clear and allocate POP on-stack lvars
    _R0 -> _tmp;
    if _Npopstkvars _gr _0 then
        drop_op_imm(_MOV, _tmp, _R0, _3);
        fast_repeat _pint(_Npopstkvars) times
            drop_push_reg(_tmp,  _SP);
        endrepeat;
    endif;
   
    ;;; Allocate non-Pop on-stack lvars
    if _Nstkvars _gr _Npopstkvars then
        drop_op_imm(_SUB, _SP, _SP, @@(w)[_Nstkvars _sub _Npopstkvars]);
    endif;

    ;;; Push the owner address:
    ;;;  str PB, [SP, #4]!
    drop_push_reg(_PB, _SP);
enddefine;


define I_UNWIND_SF();
    lvars _offs, _num;

    ;;; printf('I_UNWIND_SF\n');
    ;;; Remove owner address and on-stack lvars
    drop_op_imm(_ADD, _SP, _SP, @@(w)[_Nstkvars _add _1]);

    ;;; Restore dynamic locals
    @@PD_TABLE[_Nlocals] -> _offs;
    fast_repeat _pint(_Nlocals) times
        --@@(w){_offs} -> _offs;
        drop_mem_imm_off(_LDR, _R1, _PB, _offs);
        drop_pop_reg(_R0,  _SP);
        drop_mem_imm_off(_STR, _R0, _R1, _0);
    endrepeat;
    
    ;;; restore registers
    drop_w(_LDMFD _biset _regmask _biset _shift(_1, _LR));

    ;;; Restore procedure base register from the stack
    drop_mem_imm_off(_LDR, _PB, _SP, _0);
enddefine;


define I_RETURN();
    ;;; printf('I_RETURN\n');
    drop_w(_RET);
enddefine;


/*  Miscellaneous  */

;;; {I_STACKLENGTH}
;;;     push the length of the user stack

define I_STACKLENGTH();
    ;;; printf('I_STACKLENGTH\n');
    load_literal(_R1, ident _userhi);
    drop_mem_imm_off(_LDR, _R1, _R1, _0);
    ;;; do_load_or_store(_LDR, _R1, ident _userhi,
    ;;;                 true, _R1);
    drop_op3(_SUB, _R0, _R1, _USP);
    ;;; Add popint bits
    drop_op_imm(_ADD, _R0, _R0, _3);
    drop_push_reg(_R0, _USP);
enddefine;

;;; {I_SETSTACKLENGTH ^saved_stacklength ^nresults}
;;;     check that the stacklength is equal to the saved stacklength plus
;;;     an expected number of results. If not, call -setstklen_diff- to fix.
;;;     This does the same job as -setstklen- from "alisp.s", but avoids the
;;;     subroutine call when the number of results is known in advance.

define I_SETSTACKLENGTH();
    ;;; printf('I_SETSTACKLENGTH\n');
    ;;; mishap(0, 'I_SETSTACKLENGTH unimplemented');
    lvars _nresults;
    if asm_instr!INST_ARGS[_1] ->> _nresults then
        ;;; a known number of results (a popint):
        ;;; expand code for "_setstklen" inline
        lvars _reg = load_fsrc_to_reg(_0, _R2);
        load_literal(_R3, _nresults _sub _6);
        drop_op3(_ADD, _R2, _reg, _R3);
        load_literal(_R3, ident _userhi);
        drop_mem_imm_off(_LDR, _R3, _R3, _0);
        drop_op3(_SUB, _R2, _R3, _R2);
        drop_op3(_CMP, _R0, _USP, _R2);
        ;;; If needed, call _setstklen_diff
        load_literal(_R12, _setstklen_diff);
        drop_w(_BLXNE _biset _R12);
    else
        ;;; general case, simply call _setstklen
        load_literal(_R12, _setstklen);
        drop_w(_BLX _biset _R12);
    endif;
enddefine;

;;; {I_LISP_TRUE}
;;;     replace <false> result on top of stack by nil.

define I_LISP_TRUE();
    ;;; printf('I_LISP_TRUE\n');
    ;;; mishap(0, 'I_LISP_TRUE unimplemented');
    load_literal(_R1, false);
    drop_mem_imm_off(_LDR, _R0, _USP, _0);
    drop_op3(_CMP, _R0, _R1, _R0);
    load_literal(_R1, nil);
    drop_mem_imm_off(_STREQ, _R1, _USP, _0);
enddefine;

;;; {I_CHECK}
;;;     plant checks on backward jumps.

define I_CHECK();
    ;;; printf('I_CHECK\n');
    ;;; FIXME: implement
/*
    lvars _save;
    flush_USP_offset();
    ;;; Check for interrupts:
    ;;; cmpl $0, _trap
    Drop_imm(_ARITHI, _CMPI, _0, (_ABS, _reg_ABS, ident _trap));
    ;;; If non zero, jump to call checkall
    Drop_b(#_< _Jccs _add _cc_NE >_#), Drop_b(_0);  ;;; dummy offset
    _asm_drop_ptr -> _save;
    ;;; Check for userstack overflow:
    ;;; cmpl %USP, _userlim
    Drop_std(_CMP, _USP, (_ABS, _reg_ABS, ident _userlim));
    ;;; If Ok (_userlim <= USP), jump around the call to checkall
    Drop_b(#_< _Jccs _add _cc_BE >_#), Drop_b(_0);  ;;; dummy offset
    unless _asm_pass then
        ;;; backpatch jump offset to here
        ##(b){_asm_drop_ptr, _save} -> _save!(b)[_-1];
    endunless;
    _asm_drop_ptr -> _save;
    ;;; call _checkall
    Drop_call_abs(_checkall);
    unless _asm_pass then
        ;;; backpatch jump offset around call to checkall
        ##(b){_asm_drop_ptr, _save} -> _save!(b)[_-1];
    endunless;
*/
enddefine;


;;; === THE ASSEMBLER =================================================

;;; Do_consprocedure:
;;;     outputs machine code into a procedure record from pop assembler
;;;     input

define Do_consprocedure(codelist, reg_locals) -> pdr;
    lvars codelist, reg_locals, pdr, _code_offset, _size, _reg_spec;
    lvars reg, _buff, _cnt;
    dlocal _regmask, _asm_drop_ptr, _asm_pass, _strsize, _pdr_offset,
           lit_buff = initintvec(512), _lit_count = _0,
           _current_literal_zone = _0, _current_literal_pool = _16:FFF,
           literal_pools = [];

    ;;; construct reg mask from reg_locals
    _0 -> _regmask;
    for reg in reg_locals do
                _shift(_1, _int(Is_register(reg))) _biset _regmask -> _regmask
    endfor;

    _regmask -> _reg_spec;


    ;;; Pass 0 -- calculate instruction offsets
    _0 ->>  _pdr_offset -> _strsize;
    Code_pass(0, codelist) -> _code_offset;
    @@(w)[_int(listlength(asm_struct_list))] -> _strsize;
    ;;; _strsize _add _code_offset _add _shift(_int(lit_count), _2)-> _size;

    ;;; printf(_pint(_lit_count), 'Code_pass 0 done, _lit_count = %p\n');

    ;;; Now calculate total size of procedure and allocate store for it.
    ;;; The procedure record will be returned with the header and structure
    ;;; table already filled in, and with _asm_drop_ptr pointing to the
    ;;; start of the executable code section.
    @@PD_TABLE{_strsize _add _code_offset | b.r} _sub @@POPBASE -> _size;
    _size _add _shift(_lit_count, _2) -> _size;
    Get_procedure(_size, _reg_spec) -> pdr;

    (_asm_drop_ptr _sub pdr) _add _8 -> _pdr_offset;
    _asm_drop_ptr -> _buff;
    conspair(_pint(_code_offset), literal_pools) -> literal_pools;
    rev(literal_pools) -> literal_pools;
    _0 -> _lit_count;
    _int(front(literal_pools)) -> _current_literal_pool;
    back(literal_pools) -> literal_pools;
    ;;; Final pass -- plants the code
    Code_pass(false, codelist) -> _asm_code_offset;
    _asm_drop_ptr _sub _buff -> _cnt;
    lvars _n = _0;
    while _n _lt _lit_count do
        do_drop_w(lit_buff!(i)[_n]);
        _n _add _1 -> _n;
    endwhile;
   
;;;
;;;     Helper to show location of planted code (allow disassembling).
;;;
/*
    printf(_pint(_cnt), 'writing %p bytes ');
    printf(_buff, 'at 0x%x ');
    printf(pdr, ' for 0x%x, ');
    printf(_pint(_lit_count), '%p literals\n');
*/
/*
       _extern printf('writing %ld bytes at 0x%lx for 0x%lx, %ld literals\n',
           _cnt, _buff, pdr, _lit_count) -> _ ;
       _extern fflush(_0) -> _ ;
*/
;;;
;;;     Extra help: dump generated code to a file.
;;;
;;;        _extern write(_7, _buff, _cnt) -> _ ;
;;;        _extern printf('wrote bytes\n') -> _ ;
;;;        _extern fflush(_0) -> _ ;
enddefine;

endsection;     /* $-Sys$-Vm */

