/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:        C.80386/src/ass.p
 > Purpose:     Run-time assembler for Intel 80386
 > Author:      Robert Duncan, Aug 25 1988 (see revisions)
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
;

endsection;

;;; ---------------------------------------------------------------------

section $-Sys$-Vm;

lvars
    _strsize,           ;;; size of structure table in bytes
;


lconstant

    ;;; FLAGS FOR OPERAND ACCESS

    DEFER   = true,             ;;; access through valof
    DIRECT  = false,            ;;; access direct

    ;;; INSTRUCTION ENCODING

    ;;; Prefix bytes

    _WORD   = _16:66,           ;;; 16-bit operand size

    ;;; Opcode bytes

    _ADD    = _16:01,
    _ARITHI = _16:81,           ;;; immediate arithmetic (modified by TTT)
    _BR_IND = _16:FF,           ;;; indirect control transfer (call or jmp)
    _CMP    = _16:39,
    _IMULI  = _16:69,           ;;; signed multiply, R * imm -> R
    _Jcc    = _16:0F,           ;;; conditional jump (long)
    _Jccs   = _16:70,           ;;; conditional jump (short)
    _JMP    = _16:E9,           ;;; jump EIP-relative
    _LEA    = _16:8D,
    _MOV    = _16:89,           ;;; standard R/M -> R/M move
    _MOVI   = _16:C7,           ;;; move Imm -> R/M
    _MOVIs  = _16:B8,           ;;; short (1-byte) Imm -> Reg move
    _MOVX   = _16:0F,           ;;; move and extend (followed by SX or ZX)
    _MOVSX  = _16:BF,           ;;; move and sign-extend R/M-16 -> Reg32
    _MOVZX  = _16:B7,           ;;; move and zero-extend R/M-16 -> Reg32
    _NOP    = _16:90,
    _POP    = _16:8F,
    _PUSH   = _16:FF,
    _PUSHI  = _16:68,
    _PUSHs  = _16:50,           ;;; short (1-byte) form
    _RET    = _16:C3,
    _SHIFTI = _16:C1,           ;;; shift/rotate by imm8 (modified by TTT)
    _SUB    = _16:29,
    _TESTI  = _16:F7,           ;;; test Imm -> R/M
    _TESTIs = _16:A9,           ;;; test Imm -> EAX
    _XCHG   = _16:87,

    ;;; Extended opcode bits (TTT field)

    _ADDI   = _2:000,
    _CALL   = _2:010,
    _CHAIN  = _2:100,           ;;; i.e. indirect jump
    _CMPI   = _2:111,
    _SAR    = _2:111,
    _SUBI   = _2:101,

    ;;; Opcode flags

    _WBIT   = _2:01,            ;;; operand size: set = 32, clear = 8
    _DBIT   = _2:10,            ;;; direction of op: set = in, clear = out
    _SBIT   = _2:10,            ;;; immediate data size: set = 8, clear = 32

    ;;; A useful abbreviation

    _MOV_IN = _MOV _biset _DBIT,

    ;;; MOD fields

    _DISP0  = _int(2:00<<6),    ;;; no displacement: (%r)
    _DISP8  = _int(2:01<<6),    ;;; 8-bit displacement: [Disp8](%r)
    _DISP32 = _int(2:10<<6),    ;;; full displacement: [Disp32](%r)
    _REG    = _int(2:11<<6),    ;;; register only: %r

    _ABS    = _DISP0,           ;;; with _reg_ABS: absolute 32-bit address

    _IMM    = _1,               ;;; dummy code: indicates immediate operand

    ;;; Register (and R/M) codes

    _EAX    = _2:000,
    _ECX    = _2:001,
    _EDX    = _2:010,
    _EBX    = _2:011,
    _ESP    = _2:100,
    _EBP    = _2:101,
    _ESI    = _2:110,
    _EDI    = _2:111,

    _reg_SIB    = _2:100,       ;;; escape to 2-byte form
    _reg_ABS    = _2:101,       ;;; no register: absolute 32-bit address

    ;;; Scale (s) codes for SIB

    _s1     = _int(2:00<<6),    ;;; scale factor of 1
    _s2     = _int(2:01<<6),    ;;; scale factor of 2
    _s4     = _int(2:10<<6),    ;;; scale factor of 4
    _s8     = _int(2:11<<6),    ;;; scale factor of 8

    ;;; Special value for index field of SIB to indicate no index!

    _NOINDEX    = _int(2:100<<3),

    ;;; Condition codes

    _cc_E   = _2:0100,          ;;; equal
    _cc_NE  = _2:0101,          ;;; not equal
    _cc_A   = _2:0111,          ;;; unsigned greater-than (above)
    _cc_AE  = _2:0011,          ;;; unsigned greater-than-or-equal
    _cc_B   = _2:0010,          ;;; unsigned less-than (below)
    _cc_BE  = _2:0110,          ;;; unsigned less-than-or-equal
    _cc_G   = _2:1111,          ;;; signed greater-than
    _cc_GE  = _2:1101,          ;;; signed greater-than-or-equal
    _cc_L   = _2:1100,          ;;; signed less-than
    _cc_LE  = _2:1110,          ;;; signed less-than-or-equal

    ;;; Condition-code mask

    _cc_NOT = _2:0001,          ;;; negates a condition

;

lconstant

    ;;; NAMED REGISTERS

    _SP     = _ESP,             ;;; system stack pointer
    _USP    = _EBX,             ;;; user stack pointer
    _PB     = _EBP,             ;;; procedure base register

    _ARG_REG_0  = _EAX,         ;;; for arguments to subroutines
    _CHAIN_REG  = _EDX,         ;;; for targets of chaining
    _CALLREG    = _EDI,         ;;; for absolute subroutine calls

    _tmp_SRC    = _ESI,         ;;; for double-indirect source operands
    _tmp_DST    = _EDI,         ;;; for double-indirect destination operands

;

protected register constant

    ;;; REGISTER IDENTIFIERS
    ;;; (available to the VM)

    ;;; The value is a popint register number shifted left 1

    ;;; arg_reg_0, _1, & _2 :
    ;;; for passing arguments to subroutines. _1 & _2 are (currently) used
    ;;; only by Prolog instructions.
    ;;; NB. arg_reg_0 (EAX) is also used as a working register for complex
    ;;; moves. There should be at most only simple moves to arg_reg_1 & _2
    ;;; between the assignment to arg_reg_0 and the corresponding subroutine
    ;;; call.

    arg_reg_0   = _pint(_ARG_REG_0) << 1,       ;;; EAX
    arg_reg_1   = _pint(_ECX) << 1,
    arg_reg_2   = _pint(_EDX) << 1,

    chain_reg   = _pint(_CHAIN_REG) << 1,       ;;; EDX

;

constant

    ;;; REGISTER LVARS
    ;;; (there aren't any!)

    asm_pop_registers       = [[]],
    asm_nonpop_registers    = [[]],

;

lvars
    _USP_offset,    ;;; "static offset" for accumulating adjustments to USP
;

lconstant procedure flush_USP_offset;   ;;; forward reference

define Is_address_reg() with_nargs 1;
    Is_register()           ;;; every reg is an address reg
enddefine;


;;; === CODE-PLANTING PROCEDURES ======================================

;;; Drop_b, Drop_w, Drop_l:
;;;     put a byte/word/longword in the procedure record

define Drop_b(_byte);
    lvars _byte;
    unless _asm_pass then
        ;;; only output the data on the last pass
        _byte -> _asm_drop_ptr!(b)++ -> _asm_drop_ptr;
    endunless;
    @@(b){_asm_code_offset}++ -> _asm_code_offset;
enddefine;

define Drop_w(_word);
    lvars _word;
    unless _asm_pass then
        ;;; only output the data on the last pass
        _word -> _asm_drop_ptr!(s)++ -> _asm_drop_ptr;
    endunless;
    @@(s){_asm_code_offset}++ -> _asm_code_offset;
enddefine;

define Drop_l(_long);
    lvars _long;
    unless _asm_pass then
        ;;; only output the data on the last pass
        _long -> _asm_drop_ptr!(l)++ -> _asm_drop_ptr;
    endunless;
    @@(l){_asm_code_offset}++ -> _asm_code_offset;
enddefine;

;;; Drop_short:
;;;     output a short (1-byte) instruction, with format:

;;;         [opcode|reg]
;;;            5     3

define lconstant Drop_short(/* _opcode, _reg */) with_nargs 2;
    Drop_b(nonop _add(/* _opcode, _reg */));
enddefine;

;;; Drop_std:
;;;     output a "standard" instruction, with format:

;;;         [opcode] [mod|reg|r/m] [displacement]
;;;            8           8           0/8/32

;;;     Also handles system stack indirections, which should be of this form
;;;     but need an extra SIB byte.

define lconstant Drop_std(/* _opcode, */ _reg, _mod, _rm, _disp) with_nargs 5;
    lvars _reg, _mod, _rm, _disp;
    Drop_b(/* _opcode */);
    Drop_b(_mod _add _shift(_reg, _3) _add _rm);
    if _rm == _ESP and _mod /== _REG then
        ;;; stack references need an extra byte
        Drop_b(_NOINDEX _add _ESP);
    endif;
    if _mod == _DISP32
    or _mod == _ABS and _rm == _reg_ABS
    then
        Drop_l(_disp);
    elseif _mod == _DISP8 then
        Drop_b(_disp);
    endif;
enddefine;

;;; Drop_extd:
;;;     output an "extended" instruction, with format:

;;;         [opcode] [mod|reg|2:100] [s|index|base] [displacement]
;;;            8            8              8            0/8/32

define lconstant Drop_extd(/* _opcode, */ _reg, _mod, _base, _disp, _index, _s) with_nargs 7;
    lvars _reg, _mod, _base, _disp, _index, _s;
    Drop_b(/* _opcode */);
    Drop_b(_mod _add _shift(_reg, _3) _add _reg_SIB);
    Drop_b(_s _add _shift(_index, _3) _add _base);
    if _mod == _DISP32
    or _mod == _ABS and _base == _reg_ABS
    then
        Drop_l(_disp);
    elseif _mod == _DISP8 then
        Drop_b(_disp);
    endif;
enddefine;

;;; Drop_imm:
;;;     output an operation on immediate data, with format:

;;;         [opcode] [mod|TTT|rm] [displacement] [immediate]
;;;            8           8          0/8/32         8/32

;;;     The opcode given may be adjusted by having its SBIT set when
;;;     the immediate value can be represented in 8 bits.

define lconstant Drop_imm(_opcode, _ttt, _imm, _mod, _rm, _disp);
    lvars _opcode, _ttt, _imm, _mod, _rm, _disp;
    if _-128 _slteq _imm and _imm _slteq _127 then
        Drop_std(_opcode _biset _SBIT, _ttt, _mod, _rm, _disp), Drop_b(_imm);
    else
        Drop_std(_opcode, _ttt, _mod, _rm, _disp), Drop_l(_imm);
    endif;
enddefine;

;;; Drop_move_imm:
;;;     plant an immediate move, which doesn't fit the above format!

define lconstant Drop_move_imm(_imm, _mod, _rm, _disp);
    lvars _imm, _mod, _rm, _disp;
    if _mod == _REG then
        ;;; use a short form of MOV
        Drop_short(_MOVIs, _rm);
    else
        Drop_std(_MOVI, _0, _mod, _rm, _disp);
    endif;
    Drop_l(_imm);
enddefine;

;;; Drop_test_imm:
;;;     plant test against immediate value (different format again)

define lconstant Drop_test_imm(_imm, _mod, _rm, _disp);
    if _mod == _REG and _rm == _EAX then
        ;;; short form
        Drop_b(_TESTIs);
    else
        Drop_std(_TESTI, _0, (_mod, _rm, _disp));
    endif;
    Drop_l(_imm);
enddefine;

;;; Drop_call_indir:
;;;     plant an indirect subroutine call: the target is the absolute
;;;     address at the given effective address.

define lconstant Drop_call_indir(_mod, _rm, _disp);
    lvars _mod, _rm, _disp;
    flush_USP_offset();
    Drop_std(_BR_IND, _CALL, _mod, _rm, _disp);
enddefine;

;;; Drop_call_or_chain_abs:
;;;     plant a call or chain to an absolute address: -mode- may be _CALL
;;;     or _CHAIN.
;;;     This can't be done directly: the address must be loaded to _CALLREG
;;;     and the call done indirect via that.

define lconstant Drop_call_or_chain_abs(_routine, _mode);
    lvars _routine, _mode;
    ;;; Align the user stack
    flush_USP_offset();
    ;;; Move address to _CALLREG
    Drop_move_imm(_routine, (_REG, _CALLREG, false));
    ;;; Go to it
    Drop_std(_BR_IND, _mode, (_REG, _CALLREG, false));
enddefine;

define lconstant Drop_call_abs(/* _routine */) with_nargs 1;
    Drop_call_or_chain_abs(/* _routine, */ _CALL);
enddefine;

define lconstant Drop_chain_abs(/* _routine */) with_nargs 1;
    Drop_call_or_chain_abs(/* _routine, */ _CHAIN);
enddefine;


;;; === COMPUTING EFFECTIVE ADDRESSES =================================

;;; An "effective address" (efa) has three components: (mod, r/m, disp)

;;; Get_imm_efa:
;;;     the effective address of an immediate operand

define lconstant Get_imm_efa(_imm);
    lvars _imm;
    (_IMM, false, _imm);
enddefine;

;;; Get_abs_efa:
;;;     the effective address of an absolute operand

define lconstant Get_abs_efa(_addr);
    lvars _addr;
    (_ABS, _reg_ABS, _addr);
enddefine;

;;; Get_reg_efa:
;;;     the effective address of a register operand

define lconstant Get_reg_efa(_reg);
    lvars _reg;
    (_REG, _reg, false);
enddefine;

;;; Get_regindir_efa:
;;;     the effective address of a register-indirect operand

define lconstant Get_regindir_efa(_reg, _disp);
    lvars _reg, _disp;
    if _zero(_disp) and _reg /== _EBP then
        _DISP0
    elseif _-128 _slteq _disp and _disp _slteq _127 then
        _DISP8
    else
        _DISP32
    endif, _reg, _disp;
enddefine;

;;; Get_regdindir_efa:
;;;     returns the effective address for a (possibly double-) indirect
;;;     operand. If -defer- is <true>, then the extra level of indirection
;;;     is needed and -_tmp_reg- should be used for the necessary
;;;     intermediate load; if -defer- is <false>, the result is an ordinary
;;;     register-indirect efa.

define lconstant Get_regdindir_efa(_reg, _disp, defer, _tmp_reg);
    lvars _reg, _disp, defer, _tmp_reg;
    if defer then
        ;;; movl [disp](%reg), %tmp_reg
        Drop_std(_MOV_IN, _tmp_reg, Get_regindir_efa(_reg, _disp));
        Get_regindir_efa(_tmp_reg, _0);
    else
        Get_regindir_efa(_reg, _disp);
    endif;
enddefine;

;;; Get_structure_efa:
;;;     returns the effective address for a general I-code operand.
;;;     _arg is the argument number of the operand in -asm_instr-;
;;;     <defer> is true if the operand should be dereferenced;
;;;     _tmp_reg is a register to use if a dereference requires an extra
;;;     load.

define lconstant Get_structure_efa(_arg, defer, _tmp_reg);
    lvars structure, defer, _arg, _tmp_reg, _reg;
    asm_instr!INST_ARGS[_arg] -> structure;
    if iscompound(structure) and structure >=@(w) _system_end then
        ;;; replace structure with offset or register ident on pass 1
        Trans_structure(structure) ->> structure -> asm_instr!INST_ARGS[_arg]
    endif;
    if issimple(structure) then
        _int(structure) -> structure;
        if _neg(structure) then
            ;;; Negated offset for an on-stack lvar, shifted left by 1
            ;;; If bit 0 is set, access is via a ref (another indirection),
            ;;; but this is disabled by -defer- being <false>
            unless structure _bitst _1 then false -> defer endunless;
            ;;; Get the positive, unshifted offset
            _negate(_shift(structure, _-1)) -> structure;
            Get_regdindir_efa(_SP, structure, defer, _tmp_reg);
        else
            ;;; Offset from literal table: access via procedure base register
            Get_regdindir_efa(_PB, @@PD_TABLE{structure}, defer, _tmp_reg);
        endif;
    elseif Is_register(structure) ->> _reg then
        unless defer then
            mishap(0, 'REGISTER USED AS IMMEDIATE OPERAND')
        endunless;
        (_REG, _int(_reg), false);
    elseif defer then
        ;;; Absolute
        (_ABS, _reg_ABS, structure);
    else
        ;;; Immediate
        (_IMM, false, structure);
    endif;
enddefine;

;;; Effective addresses for user-stack operands:

define lconstant Get_upush_efa();
    --@@(w){_USP_offset} -> _USP_offset;
    Get_regindir_efa(_USP, _USP_offset);
enddefine;

define lconstant Get_upop_efa();
    Get_regindir_efa(_USP, _USP_offset);
    @@(w){_USP_offset}++ -> _USP_offset;
enddefine;

define lconstant Get_utop_efa();
    Get_regindir_efa(_USP, _USP_offset);
enddefine;

define lconstant Get_usubscr_efa(_i);
    lvars _i;
    Get_regindir_efa(_USP, _USP_offset _add @@(w)[_i]);
enddefine;

;;; flush_USP_offset:
;;;     adjust the userstack pointer to account for any accumulated static
;;;     offset.

define lconstant flush_USP_offset();
    unless _zero(_USP_offset) then
        ;;; leal [_USP_offset](%USP), %USP
        Drop_std(_LEA, _USP, Get_utop_efa());
        _0 -> _USP_offset;
    endunless;
enddefine;


;;; === TRANSLATING I-CODE INSTRUCTIONS ===============================


/*  Data Movement  */

;;; Get_dst_efa:
;;;     get the effective address of the destination of a move instruction.
;;;     If there's no explicit destination, a push on the user stack is
;;;     assumed.

define lconstant Get_dst_efa();
    if asm_instr!V_LENGTH == _2 then
        ;;; destination is a stack push
        Get_upush_efa()
    else
        Get_structure_efa(_1, DEFER, _tmp_DST)
    endif;
enddefine;

;;; Drop_move:
;;;     plant a move from a general source to a general destination.

define lconstant Drop_move(_src_mod, _src_rm, _src_disp, _dst_mod, _dst_rm, _dst_disp);
    lvars _src_mod, _src_rm, _src_disp, _dst_mod, _dst_rm, _dst_disp;
    if _src_mod == _IMM then
        Drop_move_imm(_src_disp, _dst_mod, _dst_rm, _dst_disp);
    elseif _src_mod == _REG then
        ;;; moving Reg -> R/M
        Drop_std(_MOV, _src_rm, _dst_mod, _dst_rm, _dst_disp);
    elseif _dst_mod == _REG then
        ;;; moving Mem -> Reg
        Drop_std(_MOV_IN, _dst_rm, _src_mod, _src_rm, _src_disp);
    else
        ;;; moving Mem -> Mem
        ;;; go through a temporary
        Drop_std(_MOV_IN, _EAX, _src_mod, _src_rm, _src_disp);
        Drop_std(_MOV, _EAX, _dst_mod, _dst_rm, _dst_disp);
    endif;
enddefine;


;;; I-code data movement instructions:

define I_POP();
    Drop_move(Get_upop_efa(), Get_structure_efa(_0, DEFER, _tmp_DST));
enddefine;

define I_POPQ();
    Drop_move(Get_upop_efa(), Get_structure_efa(_0, DIRECT, _tmp_DST));
enddefine;

define I_STORE();
    Drop_move(Get_utop_efa(), Get_structure_efa(_0, DEFER, _tmp_DST));
enddefine;

define I_MOVE();
    Drop_move(Get_structure_efa(_0, DEFER, _tmp_SRC), Get_dst_efa());
enddefine;

define I_MOVEQ();
    Drop_move(Get_structure_efa(_0, DIRECT, _tmp_SRC), Get_dst_efa());
enddefine;

define I_MOVES();
    Drop_std(_MOV_IN, _EAX, Get_utop_efa());
    Drop_std(_MOV, _EAX, Get_upush_efa());
enddefine;

define I_MOVENUM();
    Drop_move_imm(asm_instr!INST_ARGS[_0], Get_dst_efa());
enddefine;

define I_MOVEADDR();
    Drop_move_imm(asm_instr!INST_ARGS[_0], Get_dst_efa());
enddefine;

;;; Move to/from return address slot in stack frame
;;; (same as I_MOVE or I_MOVEADDR)
define I_MOVE_CALLER_RETURN();
    fast_chain(asm_instr!INST_ARGS[_2])     ;;; I_MOVE or I_MOVEADDR
enddefine;

define I_PUSH_UINT();
    Drop_move_imm(Pint_->_uint(asm_instr!INST_ARGS[_0], _-1), Get_upush_efa());
enddefine;

define I_ERASE();
    Get_upop_efa() -> -> -> ;
enddefine;

define I_SWAP();
    lvars _i, _j;
    _int(asm_instr!INST_ARGS[_0]) -> _i;
    _int(asm_instr!INST_ARGS[_1]) -> _j;
    ;;; movl [_i](%USP), %eax
    Drop_std(_MOV_IN, _EAX, Get_usubscr_efa(_i));
    ;;; xchgl %eax, [_j](%USP)
    Drop_std(_XCHG, _EAX, Get_usubscr_efa(_j));
    ;;; movl %eax, [_i](%USP)
    Drop_std(_MOV, _EAX, Get_usubscr_efa(_i));
enddefine;


/*  Standard Field Access for -conskey-, -sysFIELD_VAL- and -sysSUBSCR-  */

lconstant

    ;;; Type codes for signed fields

    t_SGN_BYTE  = t_BYTE  || t_SIGNED,
    t_SGN_SHORT = t_SHORT || t_SIGNED,
    t_SGN_WORD  = t_WORD  || t_SIGNED,

    ;;; Opcodes for accessing fields

    field_access_ops = list_assoc_val(% [%
        t_BYTE,         [% _pint(_MOVX), _pint(_MOVZX _biclear _WBIT) %],
        t_SHORT,        [% _pint(_MOVX), _pint(_MOVZX) %],
        t_WORD,         [% _pint(_MOV_IN) %],
        t_SGN_BYTE,     [% _pint(_MOVX), _pint(_MOVSX _biclear _WBIT) %],
        t_SGN_SHORT,    [% _pint(_MOVX), _pint(_MOVSX) %],
        t_SGN_WORD,     [% _pint(_MOV_IN) %],
    %] %),

    ;;; Opcodes for updating fields

    field_update_ops = list_assoc_val(% [%
        t_BYTE,         [% _pint(_MOV _biclear _WBIT) %],
        t_SHORT,        [% _pint(_WORD), _pint(_MOV) %],
        t_WORD,         [% _pint(_MOV) %],
        t_SGN_BYTE,     [% _pint(_MOV _biclear _WBIT) %],
        t_SGN_SHORT,    [% _pint(_WORD), _pint(_MOV) %],
        t_SGN_WORD,     [% _pint(_MOV) %],
    %] %),

    ;;; Opcodes for computing field adresses

    field_address_ops = [% _pint(_LEA) %],

    ;;; Field sizes (in bytes) of the various types

    field_size = list_assoc_val(% [%
        t_BYTE,         1,
        t_SHORT,        2,
        t_WORD,         4,
        t_DOUBLE,       8,
        t_SGN_BYTE,     1,
        t_SGN_SHORT,    2,
        t_SGN_WORD,     4,
    %] %),

    ;;; Index-scale codes for the possible field sizes

    index_scale = list_assoc_val(% [%
        1,  _pint(_s1),
        2,  _pint(_s2),
        4,  _pint(_s4),
        8,  _pint(_s8),
    %] %),

;

;;; Drop_pfx:
;;;     given a list of opcode bytes, plant all the prefix bytes and
;;;     return the opcode proper.

define lconstant Drop_pfx(opcodes);
    lvars opcodes;
    until (fast_destpair(opcodes) ->> opcodes) == [] do
        Drop_b(_int(/* prefix */));
    enduntil;
    _int(/* opcode */);
enddefine;

;;; stack_offset:
;;;     checks that -structure- refers to a direct, on-stack lvar
;;;     and returns the appropriate offset as a sysint

define lconstant stack_offset(/* structure */) with_nargs 1;
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

;;; Drop_vecsub_mult:
;;;     convert a vector index to an offset, i.e. convert to system integer
;;;     and multiply by field size

define lconstant Drop_vecsub_mult(_reg, _size);
    lvars _reg, _size;
    ;;; sarl $2, %reg
    Drop_std(_SHIFTI, _SAR, (_REG, _reg, false)), Drop_b(_2);
    unless _size == _1 then
        ;;; imull $size, %reg
        Drop_imm(_IMULI, _reg, _size, (_REG, _reg, false));
    endunless;
enddefine;

;;; Drop_exptr_deref:
;;;     deref an external pointer -n- times

define lconstant Drop_exptr_deref(exptr, _reg);
    lvars exptr, _reg;
    fast_repeat exptr times
        Drop_std(_MOV_IN, _reg, Get_regindir_efa(_reg, _0))
    endrepeat
enddefine;

define lconstant Do_bitfield(type, _size, structure, offset, upd, exptr);
    lvars type, structure, offset, upd, exptr, _size, _disp;
    ;;; Get structure address in EDI
    if not(structure) then
        ;;; structure is on top of stack (from -sysFIELD_VAL-)
        Drop_std(_MOV_IN, _EDI, Get_upop_efa());
    elseif stack_offset(structure) ->> _disp then
        ;;; structure is in a stack lvar (from -conskey-)
        Drop_std(_MOV_IN, _EDI, Get_regindir_efa(_SP, _disp));
    else
        mishap(0, 'SYSTEM ERROR 1 IN Do_bitfield');
    endif;
    ;;; If it's an external structure, get the real address
    if exptr then
        Drop_exptr_deref(exptr, _EDI)   ;;; deref exptr times
    endif;
    ;;; Get field size in EDX
    _int(_size) -> _size;
    Drop_move_imm(_size, (_REG, _EDX, false));
    ;;; Get bit offset in EAX
    if isinteger(offset) then
        ;;; Record-type field with constant offset
        _int(offset) -> _disp;
        ;;; movl $disp, %eax
        Drop_move_imm(_disp, (_REG, _EAX, false));
    else
        ;;; Vector-type structure with index in source specified by offset
        if not(offset) then
            ;;; index is on top of stack (from -sysSUBSCR-)
            Drop_std(_MOV_IN, _EAX, Get_upop_efa());
        elseif stack_offset(offset) ->> _disp then
            ;;; index is in a stack lvar (from -conskey-):
            ;;; movl [disp](%SP), %eax
            Drop_std(_MOV_IN, _EAX, Get_regindir_efa(_SP, _disp));
        else
            mishap(0, 'SYSTEM ERROR 2 IN Do_bitfield');
        endif;
        Drop_vecsub_mult(_EAX, _size);
        unless _zero(##(1){@@V_BYTES|b} _sub _size ->> _disp) then
            Drop_imm(_ARITHI, _ADDI, _disp, (_REG, _EAX, false));
        endunless;
    endif;
    ;;; Call appropriate bitfield access/update routine from "amove.s"
    ;;; (but not via _CALLREG (_EDI) which is already occupied!)
    if upd then _ubfield elseif type == t_BIT then _bfield else _sbfield endif,
    Drop_move_imm(/* routine, */ (_REG, _ESI, false));
    Drop_call_indir((_REG, _ESI, false));
enddefine;

define lconstant Do_field(type, structure, _size, offset, defer, upd, exptr);
    lvars   ops, type, structure, offset, defer, upd, exptr,
            _size, _reg, _disp, _n;

    ;;; Get appropriate opcode bytes for type and direction of move
    if upd then
        field_update_ops(type)
    elseif defer then
        field_access_ops(type)
    else
        ;;; only the address wanted
        field_address_ops
    endif -> ops;

    ;;; Get structure into a working address register
    if upd then _EDI else _ESI endif -> _reg;
    if not(structure) then
        ;;; structure is on top of stack (from -sysFIELD_VAL-)
        Drop_std(_MOV_IN, _reg, Get_upop_efa());
    elseif stack_offset(structure) ->> _disp then
        ;;; structure is in a stack lvar (from -conskey-)
        Drop_std(_MOV_IN, _reg, Get_regindir_efa(_SP, _disp));
    else
        mishap(0, 'SYSTEM ERROR 1 IN Do_field');
    endif;
    ;;; If it's an external structure, get the real address
    if exptr then
        Drop_exptr_deref(exptr, _reg)   ;;; deref exptr times
    endif;

    ;;; Load/update field to/from EAX
    if isinteger(offset) then
        ;;; Record-type structure with a constant offset
        ;;; If updating, load new value from stack to EAX
        if upd then Drop_std(_MOV_IN, _EAX, Get_upop_efa()) endif;
        ;;; Convert bit offset to bytes
        ##(b){_int(offset)|1} -> _disp;
        Drop_std(Drop_pfx(ops), _EAX, Get_regindir_efa(_reg, _disp));
    else
        ;;; Vector-type structure with index in source specified by offset
        ;;; Load index to ECX
        if not(offset) then
            ;;; index is on top of stack (from -sysSUBSCR-)
            Drop_std(_MOV_IN, _ECX, Get_upop_efa());
        elseif stack_offset(offset) ->> _disp then
            ;;; index is in a stack lvar (from -conskey-)
            Drop_std(_MOV_IN, _ECX, Get_regindir_efa(_SP, _disp));
        else
            mishap(0, 'SYSTEM ERROR 2 IN Do_field');
        endif;
        ;;; If updating, load new value from stack to EAX
        if upd then Drop_std(_MOV_IN, _EAX, Get_upop_efa()) endif;
        ;;; Get the size in bytes of this field type
        field_size(type) -> _n;
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
                ;;; Turn popint index into sysint and use automatic scaling
                ;;; sarl $2, %ecx
                Drop_std(_SHIFTI, _SAR, (_REG, _ECX, false));
                Drop_b(_2);
            endif;
        else
            Drop_vecsub_mult(_ECX, _int(_size fi_* _n) ->> _size);
            @@V_BYTES-{_size} -> _disp;
            1 -> _n;
        endif;
        ;;; opcode %eax, [disp](%reg, %ecx, index_scale(size))
        Drop_extd(Drop_pfx(ops), _EAX, Get_regindir_efa(_reg, _disp),
            _ECX, _int(index_scale(_n)));
    endif;
enddefine;

;;; I-code field access instructions:

define I_PUSH_FIELD();
    lvars type, size, structure, offset, cvt, exptr;
    explode(asm_instr) -> exptr -> cvt -> offset -> structure -> size -> type -> ;
    ;;; Load field value to EAX
    if type fi_&& t_BASE_TYPE == t_BIT then
        Do_bitfield(type, size, structure, offset, false, exptr);
    else
        Do_field(type, structure, 1, offset, DEFER, false, exptr);
    endif;
    if cvt then
        ;;; Convert EAX to popint:
        ;;; leal [3](, %eax, 4), %eax
        Drop_extd(_LEA, _EAX, Get_abs_efa(_3), _EAX, _s4);
    endif;
    ;;; Push result
    Drop_std(_MOV, _EAX, Get_upush_efa());
enddefine;

define I_POP_FIELD();
    lvars type, size, structure, offset, exptr;
    explode(asm_instr) -> exptr -> offset -> structure -> size -> type -> ;
    if type fi_&& t_BASE_TYPE == t_BIT then
        Do_bitfield(type, size, structure, offset, true, exptr);
    else
        Do_field(type, structure, 1, offset, DEFER, true, exptr);
    endif;
enddefine;

define I_PUSH_FIELD_ADDR();
    lvars type, size, structure, offset, exptr;
    explode(asm_instr) -> exptr -> offset -> structure -> size -> type -> ;
    ;;; Get field address in EAX ...
    Do_field(type, structure, size, offset, DIRECT, false, exptr);
    ;;; ... and push it
    Drop_std(_MOV, _EAX, Get_upush_efa());
enddefine;


/*  Fast Field Access  */

;;; Get_fsrc_efa:
;;;     get the effective address of an operand of a "fast_" procedure or
;;;     optimised instruction. The operand is argument -_arg- of the current
;;;     instruction. It could be a MOVE instruction (representing a push
;;;     not yet done) or <false> if the item is already on the stack.

define lconstant Get_fsrc_efa(_arg, _tmp_reg);
    lvars   op, _arg, _tmp_reg;
    dlocal  asm_instr;
    if asm_instr!INST_ARGS[_arg] ->> asm_instr then
        ;;; source specified by a MOVE instruction
        if (asm_instr!INST_OP ->> op) == I_MOVENUM or op == I_MOVEADDR then
            ;;; immediate data
            (_IMM, false, asm_instr!INST_ARGS[_0]);
        else
            ;;; general move:
            ;;; op == I_MOVE implies DEFER, op == I_MOVEQ implies DIRECT
            Get_structure_efa(_0, op == I_MOVE, _tmp_reg);
        endif;
    else
        ;;; on stack
        Get_upop_efa();
    endif;
enddefine;

;;; Get_fdst_efa:
;;;     returns the efa of the result of a "fast_" procedure.
;;;     This should be a push on the user stack, but it can be optimised
;;;     away if there's an immediately-following POP instruction.

define lconstant Get_fdst_efa();
    lvars   instr;
    dlocal  asm_instr;
    ;;; Look out for a following POP instruction
    fast_front(fast_back(asm_clist)) -> instr;
    if isvector(instr) and instr!INST_OP == I_POP then
        ;;; Erase the instruction
        fast_back(asm_clist) -> asm_clist;
        ;;; Return the efa of the destination of the POP
        instr -> asm_instr;
        Get_structure_efa(_0, DEFER, _tmp_DST);
    else
        ;;; Return the efa of a stack push
        Get_upush_efa();
    endif;
enddefine;

;;; Drop_fsrc:
;;;     move an operand of a "fast_" procedure to a register.

define lconstant Drop_fsrc(_arg, _reg);
    lvars _arg, _reg;
    Drop_move(Get_fsrc_efa(_arg, _ESI), (_REG, _reg, false));
enddefine;


;;; I-code fast field access instructions:

define I_FASTFIELD();
    lvars _offs;
    Drop_fsrc(_1, _ESI);
    if asm_instr!INST_ARGS[_0] ->> _offs then
        _int(_offs) -> _offs;
    else
        ;;; <false> means -fast_destpair-
        Drop_move(Get_regindir_efa(_ESI, @@P_FRONT), Get_upush_efa());
        @@P_BACK -> _offs;
    endif;
    Drop_move(Get_regindir_efa(_ESI, _offs), Get_fdst_efa());
enddefine;

define I_UFASTFIELD();
    Drop_fsrc(_1, _EDI);
    Drop_move(Get_upop_efa(), Get_regindir_efa(_EDI, _int(asm_instr!INST_ARGS[_0])));
enddefine;

define I_FASTSUBV();
    lvars _mod, _rm, _disp, _offs;
    ;;; Structure in ESI
    Drop_fsrc(_1, _ESI);
    ;;; Popint index from stack to ECX
    Drop_std(_MOV_IN, _ECX, Get_upop_efa());
    ;;; Arg 0 is offset to first vector element (as popint)
    ;;; -- subtract popint 1 to account for base 1 subscript and popint bits
    _int(asm_instr!INST_ARGS[_0]) _sub 1 -> _offs;
    if (Get_fdst_efa() -> _disp -> _rm ->> _mod) == _REG then
        ;;; movl [offs](%esi, %ecx), %rm
        Drop_extd(_MOV_IN, _rm, Get_regindir_efa(_ESI, _offs), _ECX, _s1);
    else
        ;;; movl [offs](%esi, %ecx), %eax
        Drop_extd(_MOV_IN, _EAX, Get_regindir_efa(_ESI, _offs), _ECX, _s1);
        ;;; movl %eax, dst
        Drop_std(_MOV, _EAX, (_mod, _rm, _disp));
    endif;
enddefine;

define I_UFASTSUBV();
    lvars _offs;
    ;;; Structure in EDI
    Drop_fsrc(_1, _EDI);
    ;;; Popint index from stack to ECX
    Drop_std(_MOV_IN, _ECX, Get_upop_efa());
    ;;; Arg 0 is offset to first vector element (as popint)
    ;;; -- subtract popint 1 to account for base 1 subscript and popint bits
    _int(asm_instr!INST_ARGS[_0]) _sub 1 -> _offs;
    ;;; New value from stack to EAX
    Drop_std(_MOV_IN, _EAX, Get_upop_efa());
    ;;; movl %eax, [offs](%edi, %ecx)
    Drop_extd(_MOV, _EAX, Get_regindir_efa(_EDI, _offs), _ECX, _s1);
enddefine;


/*  Fast Integer +/-  */

define I_FAST_+-_2();
    lvars   opd, Drop_inst, _plus;
    dlocal  asm_instr;
    asm_instr!INST_ARGS[_0] -> _plus;
    if (asm_instr!INST_ARGS[_1] ->> opd) and opd!INST_OP == I_MOVENUM then
        ;;; Immediate +/-
        _ARITHI,
        if _plus then _ADDI else _SUBI endif,
        opd!INST_ARGS[_0] _biclear _3;
        Drop_imm -> Drop_inst;
    else
        ;;; Move source to EAX and remove popint bits
        Drop_fsrc(_1, _EAX);
        Drop_imm(_ARITHI, _SUBI, _3, (_REG, _EAX, false));
        if _plus then _ADD else _SUB endif, _EAX;
        Drop_std -> Drop_inst;
    endif;
    ;;; Get efa for source/destination operand
    if asm_instr!INST_ARGS[_2] ->> asm_instr then
        Get_structure_efa(_0, DEFER, _tmp_DST)
    else
        ;;; use top of stack
        Get_utop_efa()
    endif,
    Drop_inst(/* opcode, opd, dst_efa */);
enddefine;

define I_FAST_+-_3();
    lvars opd, _plus;
    asm_instr!INST_ARGS[_0] -> _plus;
    if (asm_instr!INST_ARGS[_1] ->> opd) and opd!INST_OP == I_MOVENUM then
        ;;; Immediate +/- :
        ;;; movl arg2, %ecx
        Drop_fsrc(_2, _ECX);
        ;;; add/subl $num, %ecx
        _ARITHI, if _plus then _ADDI else _SUBI endif,
        opd!INST_ARGS[_0] _biclear _3,
        Drop_imm(_REG, _ECX, false);
    else
        ;;; movl arg1, %eax
        Drop_fsrc(_1, _EAX);
        ;;; subl $3, %eax (popint bits)
        Drop_imm(_ARITHI, _SUBI, _3, (_REG, _EAX, false));
        ;;; movl arg2, %ecx
        Drop_fsrc(_2, _ECX);
        ;;; add/subl %eax, %ecx
        if _plus then _ADD else _SUB endif, _EAX,
        Drop_std(_REG, _ECX, false);
    endif;
    ;;; Result now in ECX: move it to the destination
    if asm_instr!INST_ARGS[_3] then
        Drop_std(_MOV, _ECX, Get_structure_efa(_3, DEFER, _tmp_DST));
    else
        Drop_std(_MOV, _ECX, Get_upush_efa());
    endif;
enddefine;


/*  Branches and Tests  */

;;; Drop_jmp_indir:
;;;     output an indirect jump: the target is the absolute address at the
;;;     given effective address

define lconstant Drop_jmp_indir(_mod, _rm, _disp);
    lvars _mod, _rm, _disp;
    flush_USP_offset();
    Drop_std(_BR_IND, _CHAIN, (_mod, _rm, _disp));
enddefine;

;;; Drop_jmp_rel:
;;;     output a relative jump instruction. The argument is an offset from
;;;     the procedure's execute address, so has to be adjusted to be
;;;     relative to the next instruction.
;;;     This always plants a long (5-byte) instruction; there's no attempt
;;;     to optimise short jumps.

define lconstant Drop_jmp_rel(/* _offs */) with_nargs 1;
    flush_USP_offset();
    Drop_b(_JMP);
    Drop_l((/* _offs */ _sub _asm_code_offset) _sub _4);
enddefine;

;;; Drop_jcc:
;;;     output a conditional jump. The offset argument is as above.

define lconstant Drop_jcc(_cc, _offs);
    lvars _cc, _offs;
    flush_USP_offset();
    Drop_b(_Jcc);
    Drop_b(_16:80 _add _cc);
    Drop_l((_offs _sub _asm_code_offset) _sub _4);
enddefine;

;;; I_LABEL:
;;;     called from -Code_pass- when a label is encountered in -asm_clist-

define I_LABEL();
    flush_USP_offset();
    ;;; set front of the label pair to the popint offset from the code start
    _pint(_asm_code_offset) -> fast_front(asm_clist);
enddefine;

;;; I_BR_std:
;;;     plant a relative branch instruction of a known size (5 bytes).

define I_BR_std(_broffset, _arg);
    lvars _broffset, _arg;
    Drop_jmp_rel(_broffset);
    Drop_b(_NOP), Drop_b(_NOP);     ;;; ** Extra 2 bytes to match M-code version
enddefine;

;;; I_BR:
;;;     plant an unconditional relative jump (same as I_BR_std).

define I_BR(_broffset, _arg);
    lvars _broffset, _arg;
    Drop_jmp_rel(_broffset);
enddefine;

;;; I_BRCOND:
;;;     standard conditional branch, used as argument to I_IF_opt etc.

define I_BRCOND(_ifso, _ifnot, _is_so, _broffset, _arg);
    lvars _ifso, _ifnot, _is_so, _broffset, _arg;
    Drop_jcc(if _is_so then _ifso else _ifnot endif, _broffset);
enddefine;

;;; Drop_if:
;;;     call the branch-dropping routine from an argument of a test
;;;     instruction (I_IF_opt or I_BOOL_opt). Currently, this will
;;;     always be I_BRCOND.

define lconstant Drop_if(/* _ifso, _ifnot, */ instr) with_nargs 3;
    lvars instr;
    fast_apply(/* _ifso, _ifnot, */
        instr!INST_ARGS[_1],    ;;; flag selecting appropriate condition
        _int(fast_front(instr!INST_ARGS[_0])),  ;;; branch offset
        _2, instr!INST_ARGS[_2]);   ;;; the I_BRCOND or similar
enddefine;

;;; {I_IF_opt ^target ^is_so ^opd}
;;;     the standard test instruction: compares the operand with <false>
;;;     and jumps to -target- if the test succeeds (or fails, depending on
;;;     the flag -is_so-).

define I_IF_opt();
    lvars _mod, _rm, _disp;
    if (Get_fsrc_efa(_3, _ESI) -> _disp -> _rm ->> _mod) == _IMM then
        Drop_move_imm(_disp, (_REG, _EAX, false));
        (_REG, _EAX, false) -> _disp -> _rm -> _mod;
    endif;
    Drop_imm(_ARITHI, _CMPI, false, (_mod, _rm, _disp));
    Drop_if(_cc_NE, _cc_E, asm_instr);
enddefine;

;;; {I_BOOL_opt ^target ^is_so ^opd}
;;;     like I_IF_opt, but used for sysAND and sysOR: if the branch is
;;;     taken, the operand should be left on the stack.

define I_BOOL_opt();
    lvars opd;
    if asm_instr!INST_ARGS[_3] ->> opd then
        ;;; explicit operand -- push it
        Drop_I_code(opd);
    endif;
    ;;; Operand now on the stack
    ;;; Compare it with <false>
    Drop_imm(_ARITHI, _CMPI, false, Get_utop_efa());
    ;;; Drop the branch
    Drop_if(_cc_NE, _cc_E, asm_instr);
    ;;; If the branch wasn't taken, remove the item from the stack again
    Get_upop_efa() -> -> -> ;
enddefine;

lconstant

    ;;; Condition codes for comparison subroutines
    ;;; The first code is the obvious one, the second is the one to use
    ;;; if the order of the operands has been reversed

    cmp_condition_codes = [%
        nonop _eq,      _pint(_cc_E),   _pint(_cc_E),
        nonop _neq,     _pint(_cc_NE),  _pint(_cc_NE),
        nonop _sgr,     _pint(_cc_G),   _pint(_cc_L),
        nonop _slteq,   _pint(_cc_LE),  _pint(_cc_GE),
        nonop _slt,     _pint(_cc_L),   _pint(_cc_G),
        nonop _sgreq,   _pint(_cc_GE),  _pint(_cc_LE),
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
    lvars commute = false, _mod1, _rm1, _disp1, _mod2, _rm2, _disp2, _cc;
    ;;; Plant a comparison of the two operands
    Get_fsrc_efa(_1, _EDI) -> _disp2 -> _rm2 -> _mod2;
    if (Get_fsrc_efa(_2, _ESI) -> _disp1 -> _rm1 ->> _mod1) == _IMM then
        if _mod2 == _IMM then
            Drop_move_imm(_disp2, (_REG, _EAX, false));
            (_REG, _EAX, false) -> _disp2 -> _rm2 -> _mod2;
        endif;
        Drop_imm(_ARITHI, _CMPI, _disp1, _mod2, _rm2, _disp2);
        true -> commute;
    elseif _mod2 == _IMM then
        Drop_imm(_ARITHI, _CMPI, _disp2, _mod1, _rm1, _disp1);
    elseif _mod1 == _REG then
        Drop_std(_CMP, _rm1, _mod2, _rm2, _disp2);
        true -> commute;
    elseif _mod2 == _REG then
        Drop_std(_CMP, _rm2, _mod1, _rm1, _disp1);
    else
        ;;; memory/memory
        Drop_std(_MOV_IN, _EAX, _mod2, _rm2, _disp2);
        Drop_std(_CMP, _EAX, _mod1, _rm1, _disp1);
    endif;
    ;;; Get the appropriate condition code for the comparison subroutine
    fast_back(lmember(asm_instr!INST_ARGS[_0], cmp_condition_codes)) -> _cc;
    if commute then fast_back(_cc) -> _cc endif;
    _int(fast_front(_cc)) -> _cc;
    ;;; Do the I_IF_opt
    Drop_if(_cc, _cc _bixor _cc_NOT, asm_instr!INST_ARGS[_3]);
enddefine;

;;; {I_IF_TAG _routine operand I_IF_opt-instr}
;;;     where _routine is _issimple or _isinteger

define I_IF_TAG();
    lvars _mod, _rm, _disp;
    if (Get_fsrc_efa(_1, _ESI) -> _disp -> _rm ->> _mod) == _IMM then
        Drop_move_imm(_disp, (_REG, _EAX, false));
        (_REG, _EAX, false) -> (_mod, _rm, _disp);
    endif;
    Drop_test_imm(
        if asm_instr!INST_ARGS[_0] == _issimple then _2:01 else _2:10 endif,
        (_mod, _rm, _disp));
    Drop_if(_cc_NE, _cc_E, asm_instr!INST_ARGS[_2]);
enddefine;

;;; {I_SWITCH ^lablist ^elselab ^opd}
;;;     computed goto.

define I_SWITCH();
    lvars lablist, elselab, _ncases, _tabend, _offs;

    asm_instr!INST_ARGS[_0] -> lablist;
    asm_instr!INST_ARGS[_1] -> elselab;
    listlength(lablist) -> _ncases;

    ;;; Load the argument to EAX, then flush any outstanding adjustments to
    ;;; the user-stack pointer.
    Drop_fsrc(_2, _EAX);
    flush_USP_offset();

    ;;; Compare the argument with the number of cases (both popints)
    Drop_imm(_ARITHI, _CMPI, _ncases, (_REG, _EAX, false));

    ;;; Compute the offset from the start of the procedure code to the end
    ;;; of the jump offset table: the _17 is the length of the four
    ;;; instructions between here and the first entry in the table,
    ;;; the _4 accounts for the extra (0) entry in the table.
    _asm_code_offset _add _17 _add _int(_ncases * 4) _add _4 -> _tabend;

    ;;; If the argument was out of range, jump to after the table
    ;;; The unsigned condition accounts for both arg < 0 and arg > ncases
    Drop_jcc(_cc_A, _tabend);   ;;; 6 bytes

    ;;; Use the argument in EAX as an index into the jump offset table
    ;;; and load the corrseponding offset value into ECX
    ;;; The _11 is the length of the three instructions up to the table start
    ;;; and the _3 converts EAX into a sysint number of bytes.
    ;;; movl [offs](%PB, %eax), %ecx
    @@PD_TABLE{_strsize _add _asm_code_offset _add _11 _sub _3} -> _offs;
    Drop_extd(_MOV_IN, _ECX, (_DISP32, _PB, _offs), _EAX, _s1); ;;; 7 bytes

    ;;; Add PB to ECX to get an absolute address
    ;;; addl %PB, %ecx
    Drop_std(_ADD, _PB, (_REG, _ECX, false));   ;;; 2 bytes

    ;;; Go to it
    ;;; jmp *(%ecx)
    Drop_std(_BR_IND, _CHAIN, (_REG, _ECX, false)); ;;; 2 bytes

    ;;; Now plant the offset table:
    ;;; 0 case first (an error, so jumps to after the table) ...
    Drop_l(@@PD_TABLE{_strsize _add _tabend});
    ;;; ... then all the given labels
    until lablist == [] do
        fast_front(fast_destpair(lablist) -> lablist) -> _offs;
        Drop_l(@@PD_TABLE{_strsize _add _int(_offs)});
    enduntil;

    ;;; After the table: if there was no explicit "else" case, push the
    ;;; argument back on the stack for a following error
    unless elselab then
        Drop_std(_MOV, _EAX, Get_upush_efa());
    endunless;
    ;;; Then fall through
enddefine;

;;; {I_PLOG_IFNOT_ATOM ^fail_label ^I_BRCOND}
;;;     planted after a call to -prolog_unify_atom- from "aprolog.s".
;;;     That will have set flags for EQ if unification succeeded, NEQ if
;;;     failed. If it failed, use the I_BRCOND instruction to jump to
;;;     -fail_label-.

define I_PLOG_IFNOT_ATOM();
    fast_apply(
        _cc_NE, _cc_E,                              ;;; failure conditions
        true,                                       ;;; select the first
        _int(fast_front(asm_instr!INST_ARGS[_0])),  ;;; the jump offset
        _1, asm_instr!INST_ARGS[_1]);               ;;; the BR instruction
enddefine;

;;; {I_PLOG_TERM_SWITCH ^fail_label I_BRCOND ^var_label I_BRCOND}
;;;     planted after a call to -prolog_pair(term)_switch- from "aprolog.s".
;;;     That will have set flags to EQ if item was a matching pair(term),
;;;     UGT if item was a variable and ULT otherwise (failure).
;;;     ** NB. This must not alter -arg_reg_0- **

define I_PLOG_TERM_SWITCH();
    ;;; Branch to -var_label- if item was a variable
    fast_apply(
        _cc_A, _cc_BE,                              ;;; var conditions
        true,                                       ;;; select the first
        _int(fast_front(asm_instr!INST_ARGS[_2])),  ;;; var label offset
        _3, asm_instr!INST_ARGS[_3]);               ;;; the BR instruction
    ;;; Branch to -fail_label- if item didn't match
    fast_apply(
        _cc_B, _cc_AE,                              ;;; fail conditions
        true,                                       ;;; select the first
        _int(fast_front(asm_instr!INST_ARGS[_0])),  ;;; fail label offset
        _1, asm_instr!INST_ARGS[_1]);               ;;; the BR instruction
    ;;; Fall through if item matched - push on arg_reg_0 follows
enddefine;


/*  Procedure Calls  */

define lconstant Drop_call(on_stack, defer, _routine);
    lvars on_stack, defer, _routine;
    ;;; Get procedure in ARG_REG_0:
    if on_stack then
        Get_upop_efa()
    else
        Get_structure_efa(_0, defer, _tmp_SRC)
    endif, Drop_move((), (_REG, _ARG_REG_0, false));
    if _routine then
        ;;; Call via checking routine
        Drop_call_abs(_routine);
    else
        ;;; Call direct:
        ;;; call [PD_EXECUTE](%ARG_REG_0)
        Drop_call_indir(Get_regindir_efa(_ARG_REG_0, @@PD_EXECUTE));
    endif;
enddefine;


;;; I-code procedure call instructions:

define I_CALL();
    Drop_call(false, DEFER, _popenter);
enddefine;

define I_CALLQ();
    Drop_call(false, DIRECT, _popenter);
enddefine;

define I_CALLP();
    Drop_call(false, DEFER, false);
enddefine;

define I_CALLPQ();
    Drop_call(false, DIRECT, false);
enddefine;

define I_CALLS();
    Drop_call(true, DIRECT, _popenter);
enddefine;

define I_CALLPS();
    Drop_call(true, DIRECT, false);
enddefine;

define I_UCALL();
    Drop_call(false, DEFER, _popuenter);
enddefine;

define I_UCALLQ();
    Drop_call(false, DIRECT, _popuenter);
enddefine;

define I_UCALLP();
    Drop_call(false, DEFER, _popuncenter);
enddefine;

define I_UCALLPQ();
    Drop_call(false, DIRECT, _popuncenter);
enddefine;

define I_UCALLS();
    Drop_call(true, DIRECT, _popuenter);
enddefine;

define I_UCALLPS();
    Drop_call(true, DIRECT, _popuncenter);
enddefine;

define I_CALLABS();
    Drop_move_imm(asm_instr!INST_ARGS[_0], (_REG, _ARG_REG_0, false));
    Drop_call_indir(Get_regindir_efa(_ARG_REG_0, @@PD_EXECUTE));
enddefine;

define I_CHAIN_REG();
    ;;; Chain a procedure from a register
    Drop_std(_MOV_IN, _ARG_REG_0,  Get_structure_efa(_0, DEFER, _tmp_SRC));
    Drop_jmp_indir(Get_regindir_efa(_ARG_REG_0, @@PD_EXECUTE));
enddefine;

define I_CALLSUB();
    Drop_call_abs(asm_instr!INST_ARGS[_0]);
enddefine;

define I_CHAINSUB();
    Drop_chain_abs(asm_instr!INST_ARGS[_0]);
enddefine;

define I_CALLSUB_REG();
    lvars _addr, _reg;
    fast_front(asm_instr!INST_ARGS[_0]) -> _addr;
    if Is_register(_addr) ->> _reg then
        Drop_call_indir(_REG, _int(_reg), false);
    else
        ;;; absolute address
        Drop_call_abs(_addr);
    endif;
enddefine;


/*  Procedure Entry and Exit  */

define I_CREATE_SF();
    lvars   _offs;

    ;;; Set the procedure base register.
    ;;; If all is well, the procedure address should be in ARG_REG_0.
    ;;; movl %ARG_REG_0, %PB
    Drop_std(_MOV, _ARG_REG_0, (_REG, _PB, false));

    ;;; Save dynamic locals
    @@PD_TABLE -> _offs;
    fast_repeat _pint(_Nlocals) times
        ;;; movl [_offs](%PB), %eax
        Drop_std(_MOV_IN, _EAX, Get_regindir_efa(_PB, _offs));
        ;;; pushl (%eax)
        Drop_std(_PUSH, _2:110, _DISP0, _EAX, _0);
        @@(w){_offs}++ -> _offs;
    endrepeat;

    ;;; Allocate POP on-stack lvars (initialised to POP zero)
    fast_repeat _pint(_Npopstkvars) times
        ;;; pushl $3
        Drop_b(#_< _PUSHI _biset _SBIT >_#), Drop_b(_3);
    endrepeat;

    ;;; Allocate non-POP on-stack lvars (uninitialised)
    if _Nstkvars /== _Npopstkvars then
        Drop_imm(_ARITHI, _SUBI, @@(w)[_Nstkvars _sub _Npopstkvars],
            (_REG, _SP, false));
    endif;

    ;;; Push the owner address (from PB):
    ;;; pushl %PB
    Drop_short(_PUSHs, _PB);
enddefine;


define I_UNWIND_SF();
    lvars _offs;

    ;;; Remove owner address and on-stack lvars
    Drop_imm(_ARITHI, _ADDI, @@(w)[_Nstkvars _add _1], (_REG, _SP, false));

    ;;; Restore dynamic locals
    @@PD_TABLE[_Nlocals] -> _offs;
    fast_repeat _pint(_Nlocals) times
        --@@(w){_offs} -> _offs;
        ;;; movl [_offs](%PB), %eax
        Drop_std(_MOV_IN, _EAX, Get_regindir_efa(_PB, _offs));
        ;;; popl (%eax)
        Drop_std(_POP, _0, _DISP0, _EAX, _0);
    endrepeat;

    ;;; Restore procedure base register by reaching over return address
    ;;; to get the previous stack-frame owner:
    ;;; movl [4](%SP), %PB
    Drop_std(_MOV_IN, _PB, _DISP8, _SP, _4);
enddefine;


define I_RETURN();
    flush_USP_offset();
    Drop_b(_RET);
enddefine;


/*  Miscellaneous  */

;;; {I_STACKLENGTH}
;;;     push the length of the user stack

define I_STACKLENGTH();
    ;;; movl _userhi, %eax
    Drop_move((_ABS, _reg_ABS, ident _userhi), (_REG, _EAX, false));
    ;;; subl %USP, %eax
    Drop_std(_SUB, _USP, (_REG, _EAX, false));
    ;;; Add in the popint bits and adjust for the stack offset
    ;;; addl $[3 - USP_offset], %eax
    Drop_imm(_ARITHI, _ADDI, _3 _sub _USP_offset, (_REG, _EAX, false));
    ;;; Push EAX
    Drop_std(_MOV, _EAX, Get_upush_efa());
enddefine;

;;; {I_SETSTACKLENGTH ^saved_stacklength ^nresults}
;;;     check that the stacklength is equal to the saved stacklength plus
;;;     an expected number of results. If not, call -setstklen_diff- to fix.
;;;     This does the same job as -setstklen- from "alisp.s", but avoids the
;;;     subroutine call when the number of results is known in advance.

define I_SETSTACKLENGTH();
    lvars _nresults, _save;
    if asm_instr!INST_ARGS[_1] ->> _nresults then
        ;;; a known number of results (a popint):
        ;;; expand code for "_setstklen" inline
        ;;; movl saved_stacklength, %eax
        Drop_fsrc(_0, _EAX);
        ;;; Make sure the stack pointer is properly set
        flush_USP_offset();
        ;;; Add -nresults- to saved stacklength in EAX; the _-6 accounts for
        ;;; the popint bits in the stacklength and nresults
        Drop_imm(_ARITHI, _ADDI, _nresults _sub _6, (_REG, _EAX, false));
        ;;; movl _userhi, %ecx
        Drop_std(_MOV_IN, _ECX, (_ABS, _reg_ABS, ident _userhi));
        ;;; subl %eax, %ecx
        Drop_std(_SUB, _EAX, (_REG, _ECX, false));
        ;;; cmpl %ecx, %USP
        Drop_std(_CMP, _ECX, (_REG, _USP, false));
        ;;; If USP ok, jump around the subroutine call
        ;;; je 1f
        Drop_b(#_< _Jccs _add _cc_E >_#), Drop_b(_0);   ;;; dummy offset
        _asm_drop_ptr -> _save;
        ;;; call c_setstklen_diff
        Drop_call_abs(_setstklen_diff);
        unless _asm_pass then
            ;;; backpatch the conditional jump offset
            ##(b){_asm_drop_ptr, _save} -> _save!(b)[_-1];
        endunless;
    else
        ;;; both stacklength and nresults are on the stack:
        ;;; call c_setstklen
        Drop_call_abs(_setstklen);
    endif;
enddefine;

;;; {I_LISP_TRUE}
;;;     replace <false> result on top of stack by nil.

define I_LISP_TRUE();
    lvars _save;
    Drop_imm(_ARITHI, _CMPI, false, Get_utop_efa());
    Drop_b(#_< _Jccs _add _cc_NE >_#), Drop_b(_0);  ;;; dummy offset
    _asm_drop_ptr -> _save;
    Drop_move_imm(nil, Get_utop_efa());
    unless _asm_pass then
        ;;; backpatch the conditional jump offset
        ##(b){_asm_drop_ptr, _save} -> _save!(b)[_-1];
    endunless;
enddefine;

;;; {I_CHECK}
;;;     plant checks on backward jumps.

define I_CHECK();
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
enddefine;


;;; === THE ASSEMBLER =================================================

;;; Do_consprocedure:
;;;     outputs machine code into a procedure record from pop assembler
;;;     input

define Do_consprocedure(codelist, reg_locals) -> pdr;
    lvars   codelist, reg_locals, pdr, _code_offset, _size, _reg_spec;
    dlocal  _asm_drop_ptr, _asm_pass, _strsize;


    ;;; Check there are no register locals and set _reg_spec to 0
    unless null(reg_locals) then
        mishap(reg_locals, 1, 'UNEXPECTED REGISTER LOCALS');
    endunless;
    _0 -> _reg_spec;

    ;;; Pass 1 -- calculate instruction offsets
    _0 ->> _USP_offset -> _strsize;
    Code_pass(0, codelist) -> _code_offset;
    @@(w)[_int(listlength(asm_struct_list))] -> _strsize;
    _strsize _add _code_offset -> _size;

    ;;; Pass 2 -- for adjusting offsets in big procedures
    ;;; Not applicable to the 80386, where everything over 127 bytes is "big"
    ;;; Code_pass(1, codelist) -> _code_offset;

    ;;; Pass 3 -- for optimisations of jump offsets etc.
    ;;; Not made use of (yet)
    ;;; unless pop_debugging then
    ;;;     Code_pass(2, codelist) -> _code_offset;
    ;;; endunless;

    ;;; Now calculate total size of procedure and allocate store for it.
    ;;; The procedure record will be returned with the header and structure
    ;;; table already filled in, and with _asm_drop_ptr pointing to the
    ;;; start of the executable code section.
    @@PD_TABLE{_strsize _add _code_offset | b.r} _sub @@POPBASE -> _size;
    Get_procedure(_size, _reg_spec) -> pdr;

    ;;; Final pass -- plants the code
    Code_pass(false, codelist) -> ;
enddefine;

endsection;     /* $-Sys$-Vm */


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Sep 30 1996
        Added I_IF_TAG for issimple/isinteger tests
--- Robert John Duncan, Dec 15 1994
        Corrections to Do_field and Drop_vecsub_mult for accessing compound
        data (i.e. size /== 1)
--- Simon Nichols, Sep  6 1990
        -size- argument to field instructions now in units of type size;
        changed -Do_field- to detect size/==1 case for subscripting
        external array of structures/arrays etc.
--- John Gibson, Mar 21 1990
        Changed field access procedures so that when -exptr- argument
        is true, it's an integer specifying deref first that many times.
--- John Gibson, Dec 10 1989
        Change to I_(U)FASTSUBV for new offset value in instruction, plus
        change to -Do_consprocedure- for new pop pointers (size has
        @@POPBASE subtracted).
--- John Gibson, Jun 30 1989
        Added I_CALLPS and I_UCALLPS (for -fast_apply-)
--- John Gibson, Jun  4 1989
        Replaced references to pop_optimise with not(pop_debugging)
--- John Gibson, Apr 30 1989
        Put into section $-Sys$-Vm, and removed use of 'nonexported'.
--- Rob Duncan, Sep 20 1988
    Modified -Do_field- and -Do_bitfield- to accept -offset- <false> as
    meaning vector subscript on stack (for new -sysSUBSCR-) and to take
    extra argument -ext- for planting code for an external field access.
    I_PUSH_FIELD, I_POP_FIELD and I_PUSH_FIELD_ADDR pass the extra argument
    on from -asm_instr-.
    Also modified -Do_bitfield- to do away with call to "bfield_sub" by
    inlining the offset conversion as this adds no space overhead
 */
