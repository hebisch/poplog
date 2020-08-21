/* --- Copyright University of Sussex 2005. All rights reserved. ----------
 > File:        S.pcunix/src/syscomp/sysdefs_linux_elf.p
 > Purpose:     Definitions for machine & operating system (PC/Linux ELF)
 > Author:      Robert Duncan, Oct 31 1988 (see revisions)
 */


section;

global constant macro (

    POPC_SYSDEFS_LOADED = true,


;;; === SYSTEM NAME (PC) ==============================================


    MACHINE = [[pc]],

    PC = true,
    X86_LINUX = true,


;;; === PROCESSOR (INTEL 80x86) =======================================


    PROCESSOR = [[80386]],      ;;; or similar

    ;;; Values for machine and C data types are defined in mcdata.p,
    ;;; and can be overidden here if necessary

    WORD_BITS       = 64,

;;; POPINT_BITS     = 62,
    POPINT_BITS     = 61,

    SHORT_ALIGN_BITS = 8,       ;;; alignment in bits for short access
    INT_ALIGN_BITS   = 8,       ;;; alignment in bits for int access
    DOUBLE_ALIGN_BITS = 8,      ;;; alignment in bits for double access

    STRUCT_SHORT_ALIGN_BITS = 16,   ;;; bit alignment for short
    STRUCT_INT_ALIGN_BITS   = 32,   ;;; bit alignment for int
    STRUCT_DOUBLE_ALIGN_BITS = 64,  ;;; bit alignment for double

    ;;; Stack alignment in bits
    STACK_ALIGN_BITS = 128,

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

    BSD_MMAP        = true,      ;;; has -mmap- and -mprotect- facilities
    BSD_MPROTECT    = true,

    VPAGE_OFFS = 4096,      ;;; word address offset of a virtual page

    ;;; LOWEST_ADDRESS:
    LOWEST_ADDRESS = 0,

    ;;; UNIX_USRSTACK:
    ;;;     ????

    UNIX_USRSTACK  = 16:FC0000000,

    ;;; Procedures to get and set the memory break and return the REAL end of
    ;;; memory. (We always need the real end to ensure that the end of the
    ;;; user stack is always at the true end of memory, so that user stack
    ;;; underflow produces a memory access violation.)

    GET_REAL_BREAK =
        [procedure(); _extern sbrk(_0)@(b.r->vpage) endprocedure],

    SET_REAL_BREAK =
        [procedure(_break) -> _break;
            lvars _break = _break@(w.r->vpage);
            if _extern[SE] brk(_break@(w->b)) == _-1 then
                _-1 -> _break
            endif
        endprocedure],


;;; === OTHER =========================================================

    ;;; ANSI C returns floats as single, not double
    ANSI_C = true,
    ;;; AMD64 returns a float as 32-bits in XMM0
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
        ]],

    ;;; Old-style I_PUSH/POP_FIELD(_ADDR) instructions in ass.p
    OLD_FIELD_INSTRUCTIONS = true,

    ;;; Include M-code listing in assembly language files

    M_DEBUG = true,

    ;;; Result of external call may need sign extension

    SIGN_EXTEND_EXTERN = true,

);

endsection;     /* $- */



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Mar 13 2005
        Patch for AMD64 poplog provided by Waldek Hebisch
--- Aaron Sloman, Jan  6 2005
    Changed to set       C_FLOAT_RESULT_SINGLE = false,
    to fix bug in invocations of external programs returning single
    float results. See also $popsrc/fields.p where it provied necessary to alter
    Convert_func_*spec
--- John Gibson, Apr  9 1999
        Changed LINUX to 2.0
--- John Gibson, Jan 21 1999
        Added X86_LINUX
--- Robert Duncan, May 12 1997
        Added OLD_FIELD_INSTRUCTIONS. Removed is* procedures from
        SUBROUTINE_OPTIMISE_LIST (no longer required).
--- Robert Duncan, Apr 25 1996
        New for Linux ELF
--- Integral Solutions Ltd, May 12 1995
        No longer sets TERMINFO for Linux.
--- John Gibson, Mar 21 1995
        Standard machine data type definitions are now in mcdata.p
--- John Gibson, Feb 28 1995
        Added POSIX1 and "posix" entry in OPERATING_SYSTEM, removed H*ERTZ
--- Poplog System, Jan 18 1995 (Julian Clinton)
        Now defaulted to Linux (Berkley-ish with some SVR4).
--- Robert John Duncan, Jan 26 1994
        Modified from Sun386 (now defunct) for PC running SVR4
        (some things left unchecked for now ...)
--- John Gibson, Oct 22 1992
        Changed P*OPC to POPC_SYSDEFS_LOADED
--- Robert John Duncan, Jun 23 1992
        Added BSD_VFORK
--- John Gibson, Dec 11 1990
        SUNOS 4.1
--- Rob Duncan, Aug 31 1989
        Put in value for UNIX_USRSTACK
--- John Gibson, Aug 31 1989
        Added comment for UNIX_USRSTACK, but have no way of finding value.
--- John Gibson, Aug 24 1989
        Removed S+IGNALS
--- Rob Duncan, Apr  3 1989
        Changed OPERATING_SYSTEM definition to include "unix" as well as
        "sunos"
 */
