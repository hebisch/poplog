/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/vmdefs.ph
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;----------------- VIRTUAL MACHINE DEFINITIONS --------------------------


    ;;; load definitions for vm control flags
#_INCLUDE '../lib/include/vm_flags.ph'


lconstant macro (

    ;;; Flags and shift values for simulated stack state "_simstack"
    ;;; Each 5-bit field in _simstack represents a stack position, so e.g.
    ;;; we shift it left 5 to push something, right 5 to pop something.
    ;;; Currrently, the 5-bit code in each position has only three values,
    ;;; viz 0, ANY or XPUSH, but could be extended to typing information

    _S_XPUSH        = _1,           ;;; was an explicit variable/literal push
    _S_TYPE         = _int(15<<1),  ;;; mask for type
    _S_ANY          = _int(1<<1),   ;;; untyped
    _S_PDR          = _int(2<<1),   ;;; type procedure

    _S_CLEAR_OVERFLOW   = _int(2**(5*5)-1), ;;; mask to clear top bits when shifting left
    _S_PUSH         = _5,           ;;; shift to push 1
    _S_POP1         = _-5,          ;;; shift to pop 1
    _S_POP2         = _-10,         ;;; shift to pop 2
    _S_POP3         = _-15,         ;;; shift to pop 3

    _S_NONE         = _0,           ;;; nothing on the stack
    _S_ONE          = _int(1<<0),   ;;; minimum value for 1 thing on the stack
    _S_TWO          = _int(1<<5),   ;;;                   2 things
    _S_THREE        = _int(1<<10),  ;;;                   3 things


    _PUSH_simstack      = [_shift(_simstack _bimask _S_CLEAR_OVERFLOW, _S_PUSH)],
    _PUSH_ANY_simstack  = [_PUSH_simstack _biset _S_ANY],
    _POP_simstack       = [(_shift(_simstack, _S_POP1) _biclear _S_XPUSH)],
    _POP2_simstack      = [(_shift(_simstack, _S_POP2) _biclear _S_XPUSH)],
    _POP3_simstack      = [(_shift(_simstack, _S_POP3) _biclear _S_XPUSH)],
    _REP_ANY_simstack   = [_shift(_POP_simstack _bimask _S_CLEAR_OVERFLOW,
                                            _S_PUSH) _biset _S_ANY],

    ;;; Code in the front of label pairs (stored as a pop integer, negative
    ;;; so it can be distinguished from the positive offset from I_LABEL
    ;;; that eventually replaces it).
    M_LAB_LEVEL         = 2:1e8-1,      ;;; mask for VM level number
    M_LAB_REFCOUNT      = 2:1e16-2:1e8, ;;; mask for reference count
    M_LAB_REF1          = 2:1e8,        ;;; 1 in reference count
    M_LAB_REFMAX        = 2:1e15,       ;;; top bit in reference count
    M_LAB_PLANTED       = 2:1e16,       ;;; label planted (with sysLABEL, etc)
    M_LAB_DLABEL        = 2:1e17,       ;;; sysDLABEL label
    M_LAB_HAS_CHECK     = 2:1e18,       ;;; I_CHECK planted after label
    M_LAB_INITIAL       = -2:1e24,      ;;; bits to make value negative

    ;;; flags for PCR_FLAGS
    M_PCR_PUSHED            = 2:1e0,    ;;; if pushed
    M_PCR_PLOG_TRAIL_CHECK  = 2:1e1,    ;;; if requires check for prolog trail overflow
    M_PCR_USES_NON_LOCALS   = 2:1e2,    ;;; if uses any non-local lvars
    M_PCR_RETURNED          = 2:1e3,    ;;; if returned by sysENDPROCEDURE
    M_PCR_RECURSING         = 2:1e4,    ;;; if being processed recursively
    M_PCR_ENTRY_CHECK       = 2:1e5,    ;;; if requires entry check code
    M_PCR_NONEXEC_LBLOCK    = 2:1e6,    ;;; represents a nonexecuting lblock
    M_PCR_EVALUATE          = 2:1e7,    ;;; execute after assembling
    M_PCR_EVALUATED         = 2:1e8,    ;;; executed


    ;;; See Np_literal_offset in vm_conspdr.p
    _NONPOP_LIT             = _2:1e28,

    ;;; Values for type arg to sysCONSTRUCT
    SCON_LIST               = 1,
    SCON_VECONS             = 2,
    SCON_VECTOR             = 3,
    SCON_RECORD             = 4,
    );



    ;;; Procedure Compilation Record (a closure)
struct PCR
  { CLOSURE_COMMON_FIELDS           ;;; upto PD_CLOS_PDPART, excl PD_CLOS_FROZVALS
    full    PCR_FLAGS,              ;;; flags
            PCR_PCR_ASSOC,          ;;; assoc list of procedures -> pcrs

            PCR_LBLOCK,             ;;; outer lblock record for this procedure
            PCR_CURR_LBLOCK,        ;;; record for current lblock
            PCR_UNRESOLVED_LIST,    ;;; list of unresolved procedures/lconstants

            PCR_DYN_VARS,           ;;; list of local dynamic identifiers
            PCR_CODE_LIST,          ;;; code list
            PCR_CODE_END,           ;;; end pair of code list
            PCR_CODE_PENULT,        ;;; penultimate pair of code list
            PCR_REPROCESS_INSTRS,   ;;; codelist pairs of instrs to re-process
            PCR_DLEXPR_LIST,        ;;; list of dlocal expressions

            PCR_LABEL_COUNT,        ;;; count of labels gone to and not planted
            PCR_NON_LOCAL_LABELS,   ;;; non-local label list

            PCR_PDR_REC,            ;;; procedure record when assembled
            PCR_CLOS_TEMPLATE,      ;;; template for lexical closure
            PCR_RTID_ARG_HOMES,     ;;; home idents for non-local rtid args
            PCR_CLOS_LVAR,          ;;; lvar id used to store run-time closure
            PCR_LITERAL_CACHE,      ;;; assoc list of literals for caching

            PCR_PLOG_PDR,           ;;; true if uses PLOG_SAVE/RESTORE
            PCR_PLOG_FAIL_INSTR,    ;;; last IFNOT_ATOM/TERM_SWITCH after a SAVE

            PCR_OUTER_PCR,          ;;; outer pcr
            PCR_SELF;               ;;; this pcr -- MUST BE LAST
  };

    ;;; Lexical block record (a vector)
struct LBLOCK
  { word    V_LENGTH;
    full    KEY,
>->         LBLK_LEVEL,             ;;; level within procedure (0 at top)
            LBLK_INNER_BLOCKS,      ;;; list of nested lblocks
            LBLK_OUTER_BLOCK,       ;;; outer lblock
            LBLK_LVAR_ENV,          ;;; assoc list of home lvars to local representative
            LBLK_LEX_ASSOC,         ;;; assoc list of tokens to lex identifiers
            LBLK_TMP_LVARS,         ;;; list of unused temporary lvar names
            LBLK_SYMBOL_LABELS,     ;;; assoc list of symbol labels to label pairs
            LBLK_INSTR,             ;;; codelist instruction at start of lblock
            LBLK_VM_FLAGS;          ;;; local value of -pop_vm_flags-
  };

    ;;; Instructions planted in the VM codelist (vectors)
struct VM_INSTRUCTION
  { word    V_LENGTH;
    full    KEY,
>->         INST_OP,        ;;; operation procedure (or procedure name)
            INST_ARGS[];    ;;; 0 or more arguments
  };

    ;;; Dlocal expression entry (a vector)
struct DLEXPR
  { word    V_LENGTH;
    full    KEY,
>->         DLX_VARS,       ;;; list of save lvar idents
            DLX_GET_CODE,   ;;; access codelist
            DLX_PUT_CODE,   ;;; update codelist
            ;;; next only for active variable
            DLX_ACTID;      ;;; active variable id
  };


constant
        procedure (Sys$-cons_assoc, Sys$-dest_assoc, Sys$-New_lextoken)
    ;

vars
        pop_vm_flags, pop_new_lvar_list, pop_syntax_only
    ;

section $-Sys$-Vm;

constant
        procedure (Cons_inst, Plant, Unplant, Is_pcr, Newlab, Newlab_ref1,
        Deref_lab, Moveq_op, Garbage_inst, Incr_lab_refcount,
        Cons_pushq, Cons_goto, Cons_if, Cons_bool,
        rI_MOVEQ_LEX, rI_CALLQ_LEX, rI_UCALLQ_LEX, I_POPQ,
        I_MOVE, I_MOVEQ, I_MOVEADDR, I_MOVENUM, I_MOVES, I_POP, I_STORE,
        I_CALLABS, I_CALL, I_CALLPQ, I_CALLP, I_CALLQ, I_CALLS, I_CALLSUB,
        I_UCALL, I_UCALLPQ, I_UCALLP, I_UCALLQ, I_UCALLS, I_NOT, I_GO_ON,
        I_IF, I_BOOL, I_NBOOL, I_GOTO, I_BR, I_BRCOND, I_CHAINPS, I_CHAINS,
        I_ERASE
        )
    ;

vars
        vm_current_pcr, vm_pcr_stack, vm_current_lblk, vm_lex_push_counts,
        vm_code_end, vm_code_penult, vm_lex_assoc,
        vm_dvar_list, vm_reprocess_instrs, vm_label_count, _vm_level,
        asm_lvar_env, asm_instr, asm_clist,
        _simstack
    ;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 27 1996
        Revised M_LAB values to include a reference count.
--- John Gibson, Jan  7 1990
        Changes for new pointers.
--- John Gibson, May 15 1989
        Moved _M_LEX_... flags to ident.ph
--- John Gibson, Apr 30 1989
        All VM stuff now section $-Sys$-Vm.
--- John Gibson, Apr 27 1989
        Added M_LAB_HAS_CHECK
--- John Gibson, Mar  1 1989
        Added M_PCR_RECURSING
--- John Gibson, Nov 23 1988
        Changes for lexical blocks.
--- John Gibson, Jul 26 1988
        Change to fields in PCR record
--- John Gibson, Feb 28 1988
        Lconstant'ed macro defs
 */
