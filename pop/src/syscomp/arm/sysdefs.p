
section;

global constant macro (

    POPC_SYSDEFS_LOADED = true,


;;; === SYSTEM NAME (PC) ==============================================


    MACHINE = [[pc]],

    PC = true,
    ARM_LINUX = true,


;;; === PROCESSOR (INTEL 80x86) =======================================


    PROCESSOR = [[arm]],      ;;; or similar

    ;;; Values for machine and C data types are defined in mcdata.p,
    ;;; and can be overidden here if necessary

    WORD_BITS       = 32,

    POPINT_BITS     = 29,

    SHORT_ALIGN_BITS = 16,       ;;; alignment in bits for short access
    INT_ALIGN_BITS   = 32,       ;;; alignment in bits for int access
    DOUBLE_ALIGN_BITS = 64,      ;;; alignment in bits for double access

    STRUCT_SHORT_ALIGN_BITS = 16,   ;;; bit alignment for short
    STRUCT_INT_ALIGN_BITS   = 32,   ;;; bit alignment for int
    STRUCT_DOUBLE_ALIGN_BITS = 64,  ;;; bit alignment for double

    ;;; Stack alignment in bits
    STACK_ALIGN_BITS = 64,

    CODE_POINTER_TYPE = "byte", ;;; type of pointer to machine code
    BIT_POINTER_TYPE = "byte",  ;;; type of pointer for bitfield access

    ;;; FRAME_LEN_32BIT = true,


;;; === OPERATING SYSTEM (UNIX BSD) ==================================


    UNIX = true,
    BERKELEY = 4.3,
    LINUX = 2.0,
    LINUX_ELF = true,
    POSIX1 = 198808,            ;;; probably later than this ...
    OPERATING_SYSTEM = [[unix linux ^LINUX elf posix {^POSIX1}]],

    SHARED_LIBRARIES = true,

    BSD_MMAP  = true,     ;;; has -mmap- and -mprotect- facilities
    BSD_MPROTECT  = true,

    VPAGE_OFFS = 4096,    ;;; word address offset of a virtual page

    ;;; LOWEST_ADDRESS:
    ;;; LOWEST_ADDRESS = 0,

    ;;; UNIX_USRSTACK:
    ;;;     Fake, on Linux we need to estimate stack locaction at
    ;;;     runtime.

    UNIX_USRSTACK  = 16:FC000000,

    ;;; Procedures to get and set the memory break and return the REAL end of
    ;;; memory. (We always need the real end to ensure that the end of the
    ;;; user stack is always at the true end of memory, so that user stack
    ;;; underflow produces a memory access violation.)

    GET_REAL_BREAK =
        [procedure(); _extern sbrk(_0)@(b.r->vpage) endprocedure],

    SET_REAL_BREAK =
        [procedure(_break) -> _break;
            lvars _break = _break@(w.r->vpage);
            if _extern brk(_break@(w->b)) == _-1 then
                _-1 -> _break
            endif
        endprocedure],

    ;;; Flush the instruction cache
    CACHEFLUSH = [
        procedure(_ptr, _nbytes);
            lvars _ptr, _nbytes;
            ;;; __clear_cache is a Linux function
            _extern __clear_cache(_ptr, _ptr _add _nbytes) -> ;
        endprocedure
    ],

;;; === OTHER =========================================================

    ;;; ANSI C returns floats as single, not double
    ANSI_C = true,
    C_FLOAT_RESULT_SINGLE = true,

    ;;; list of procedures to be optimised as subroutine calls
    ;;; format of entries is
    ;;;     [<pdr name> <nargs> <nresults> <subroutine name>]

    SUBROUTINE_OPTIMISE_LIST =
        [[
            [prolog_newvar  0 1 _prolog_newvar]
            [datakey        1 1 _datakey]
            [prolog_deref   1 1 _prolog_deref]
            [conspair       2 1 _conspair]
            [fi_>>          2 1 _rshift]
        ]],

    ;;; Old-style I_PUSH/POP_FIELD(_ADDR) instructions in ass.p
    OLD_FIELD_INSTRUCTIONS = true,

    ;;; Include M-code listing in assembly language files

    M_DEBUG = true,

);

endsection;     /* $- */

