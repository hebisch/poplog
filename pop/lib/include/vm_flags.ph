/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/include/vm_flags.ph
 >  Purpose:        Definitions of virtual machine control flags
 >                  (These are bits in the integer variable -pop_vm_flags-)
 >  Author:         John Gibson, Jun 30 1985 (see revisions)
 >  Documentation:  REF * VMCODE, REF * IDENT
 >  Related Files:  SRC * VMPLANT.P, LIB COMPILE_MODE
 */

#_TERMIN_IF DEF VM_FLAGS_INCLUDED

;;; N.B. compile_mode is a POP-11 syntax construct for compile-time
;;; manipulation of the flags defined in this file; the 'compile_mode:vm'
;;; option corresponding to SETTING each flag appears on the right after
;;; its definition.

;;; Note also that pop_vm_flags is localised to each lexical block,
;;; procedure or compilation stream. It is locally set to zero when compiling
;;; libraries (i.e. by subsystem_libcompile).

section;

iconstant macro (

    VM_MIX_NONLOCAL_AND_LOCAL_LEX   = 2:1e0,        ;;; +mixlex
            /*  If set, specifies that a procedure may access a lexical
                identifier non-locally, and then later re-declare it as local
                so that subsequent uses reference the local identifier.
                Otherwise, this is disallowed (POP-11 always has this clear).
            */

    VM_DISCOUNT_LEX_PROC_PUSHES     = 2:1e1,        ;;; +dislpp
            /*  While set, a sysPUSHQ instruction for a procedure that uses
                lexical variables non-locally (as well as a sysPUSH for a
                lexical constant containing such a procedure) will be
                discounted from the point of view of considering the procedure
                to have been 'pushed' (as described in the 'Implementation of
                Lexical Scoping' section of REF *VMCODE). This flag also stops
                calls of sysIDENT on lexical variable making them 'type-3'
                variables (again, only while the flag is set). (POP-11 always
                has this clear.)
            */

    VM_NOPROT_LVARS                 = 2:1e2,        ;;; -lexprt
            /*  If set, an error is NOT signalled if a protected permanent
                identifier is declared lexically (by sysLVARS, sysDLVARS,
                sysLCONSTANT).
            */

    VM_NO_BACK_JUMP_CHECKS          = 2:1e3,        ;;; -bjmpch
            /*  Normally, jump instructions such as sysGOTO, sysIFSO,
                sysIFNOT, etc, will plant extra code before a backward jump
                (i.e. one that references a label already planted with
                sysLABEL), where the extra code checks for interrupts and for
                user stack overflow. While this flag is set, the checking code
                is omitted.
                    N.B. This flag is effective only when pop_debugging
                is false.
            */

    VM_NO_CHECK_GO_ON_INT           = 2:1e4,        ;;; -goonch
            /*  While set, sysGO_ON will not plant code to check that the item
                on top of the stack is an integer. Used extensively by Clisp.
                    N.B. This flag is effective only when pop_debugging
                is false.
            */

    VM_NO_PDR_ENTRY_CHECKS          = 2:1e5,        ;;; -pentch
            /*  Normally, procedures have extra code planted at the beginning
                to check for interrupts, user stack overflow and call stack
                (i.e. recursion) overflow; when this flag is set (at the time
                of the sysENDPROCEDURE that terminates a procedure), the
                checking code is omitted.
                    N.B. This flag is effective only when pop_debugging
                is false.
            */

    VM_NO_TYPED_VAR_CHECKS          = 2:1e6,        ;;; -typech
            /*  Setting this flag disables the code that normally gets planted
                to check that run-time assignments to typed variables are
                assigning the correct type of object to the variable.
                (Currently, the only kind of typed variable is
                procedure-type.)
                    N.B. This flag is effective only when pop_debugging
                is false.
            */

    VM_NOPROT_PVARS                 = 2:1e7,        ;;; -prmprt
            /*  If set, an error is NOT signalled if a protected permanent
                identifier is redeclared (by sysSYNTAX, ident_declare, etc).
            */

    VM_PERM_FIXED_DECLARE           = 2:1e8,        ;;; +prmfix
            /*  If set, a mishap results if a redeclaration of a permanent
                identifier does not agree with its current declaration (in
                respect of identprops and constant/var).
                    In addition, redeclarations do not reset already-assigned
                constants to 'unassigned' status (so that they cannot be
                assigned again and their present value continues to be used,
                rather than their being treated as variables until reassigned).
                See * ident_declare for more details.
                    N.B. This flag is effective only when pop_debugging
                is false.
            */

    VM_NO_FAST                      = 2:1e9,        ;;; +nofast
            /*  If set, sys_use_current_ident and sys_current_ident with 2nd
                argument true will call pop_nofast_trans to translate
                'fast' permanent identifier names to their non-fast versions.
                    sys_use_current_ident is called with 2nd arg true by all
                VM instructions except sysPOP, sysLOCAL and sysPASSIGN (hence
                fast to non-fast translation will be performed for these).
                    sys_current_ident is called with 2nd arg true by itemread
                (hence fast to non-fast translation will be performed for
                Pop-11 macros and syntax words). This flag also disables
                "fast" variants of Pop-11 syntax constructs such as exacc and
                XptVal.
                    Also if set, uses_lib_idents (called by the Pop-11 uses
                construct), will call pop_nofast_trans to load any non-fast
                version of a library in addition to the fast one.
            */


    ;;; Flags that the Pop-11 compiler should always have clear (i.e.
    ;;; that should be locally cleared by a call of pop11_comp_stream).
    VM_POP11_ALWAYS_CLEAR = VM_MIX_NONLOCAL_AND_LOCAL_LEX ||
                            VM_DISCOUNT_LEX_PROC_PUSHES ||
                            VM_NOPROT_LVARS ||
                            VM_NOPROT_PVARS,
    );

iconstant VM_FLAGS_INCLUDED = true;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 16 1996
        Added VM_NO_FAST.
--- John Gibson, Aug 19 1993
        Added VM_POP11_ALWAYS_CLEAR for pop11_comp_stream to use.
--- John Gibson, Oct 26 1992
        Added VM_FLAGS_INCLUDED etc
--- John Gibson, Jun  3 1989
        Added more flags, made definitions iconstants.
--- John Gibson, May  2 1989
        Added 3 new flags (VM_NO_PDR_ENTRY_CHECKS, VM_NO_TYPED_VAR_CHECKS,
        VM_NOPROT_PVARS), and references to new syntax construct
        -compile_mode-.
--- John Gibson, Jan 29 1989
        Put inside normal_compile ... end_normal_compile so useable
        with popc
--- John Williams, Sep 29 1987
        added VM_NO_CHECK_GO_ON_INT
--- John Gibson, Mar 12 1987
        added VM_NO_BACK_JUMP_CHECKS
--- John Williams, Oct 20 1986
        removed VM_ALLOW_LISP_BOOLS (no longer possible to achieve)
--- John Williams, Feb 14 1986
        added VM_ALLOW_LISP_BOOLS and VM_NOPROT_LVARS flags
*/
