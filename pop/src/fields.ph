/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/src/fields.ph
 > Purpose:
 > Author:          John Gibson, May 29 1990 (see revisions)
 */

;;; -------- DEFINITIONS FOR STRUCTURE FIELD DESCRIPTORS, ETC -------------

lconstant macro (

    VM          = [$-Sys$-Vm$-],
    FLD         = [$-Sys$-Fld$-],
    fsv         = "fast_subscrv",

    FULL        = ["full"],
    EXPTR       = ["exptr"],
    EXVAL       = 16:FFFF,      ;;; pretend it's an integer spec

    ;;; field descriptor subscripts
    FD_VAL_SPEC = 1,
    FD_TYPE     = 2,
    FD_BIT_SIZE = 3,
    FD_BIT_OFFS = 4, FD_DEREF_COUNT = 4,
    FD_ACC_P    = 5,
    FD_CONV_P   = 6,
    FD_FLAGS    = 7,
    FD_EXTERN   = 8,
    FD_VEC_LEN  = 8,

    ;;; mask bits in FD_FLAGS -- public ones are MODE_USER_FLAGS >> 8
    M_FD_FIXED_EXPTR        = 2:1e0,    ;;; don't cons new external pointer
    M_FD_ADDR_MODE          = 2:1e1,    ;;; access address of field
    M_FD_STRUCT_FIXED_EXPTR = 2:1e2,    ;;; don't cons new external pointer for struct
    ;;; internal
    M_FD_CAN_UPDATE         = 2:1e8,    ;;; field can be updated

    ;;; bits in MODE argument
    MODE_LEVEL              = 16:00FF,  ;;; mask for access level
    MODE_USER_FLAGS         = 16:FF00,  ;;; mask for user flags
    ;;; internal
    MODE_FORCE_EXACC        = 2:1e16,   ;;; force extern acc if level 0

    );

define :inline lconstant INST0(instp=item);
    VM Cons_inst(VM instp, 1)
enddefine;

define :inline lconstant INST1(instp=item, arg1);
    VM Cons_inst(VM instp, arg1, 2)
enddefine;

define :inline lconstant INST2(instp=item, arg1, arg2);
    VM Cons_inst(VM instp, arg1, arg2, 3)
enddefine;

constant procedure $-Sys$-Vm$-Cons_inst;

section $-Sys$-Fld;

constant
        procedure (Convert_simp_spec, Convert_vec_spec, Convert_rec_speclist,
        Convert_func_spec, Field_instrs, Field_update_convert,
        Arg_check_instrs)
    ;

endsection;



/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct 13 1994
        Added missing INST2
--- John Gibson, Jun 11 1993
        Added MODE_STRUCT_FIXED_EXPTR
--- John Gibson, Sep  4 1992
        Added EXVAL
--- John Gibson, Nov 28 1990
        Added MODE flags
--- John Gibson, Sep 14 1990
        Added FD_EXTERN
--- John Gibson, Jul 27 1990
        EXPTR = "exptr"
 */
