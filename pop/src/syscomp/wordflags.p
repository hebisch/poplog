/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/syscomp/wordflags.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

        FLAGS ASSOCIATED WITH IDENTIFIERS, WORDS, PROCEDURES, KEYS

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section;


    /*  Key flags and types
    */
constant macro (

    ;;; key flags mask values
    M_K_COPY            = 2:1e0,    ;;; class can be copied
    M_K_VECTOR          = 2:1e1,    ;;; is a vectorclass
    M_K_RECORD          = 2:1e2,    ;;; is a recordclass
    M_K_SPECIAL         = 2:1e3,
    M_K_STRING          = 2:1e4,    ;;; is a string ("string", "dstring", "string16" or "dstring16")
    M_K_BYTE_ACCESS     = 2:1e5,    ;;; is 'byte-accessible'
    M_K_FULL_VECTOR     = 2:1e6,
    M_K_SPECIAL_RECORD  = 2:1e7,
    M_K_PROP_ENTRY      = 2:1e8,    ;;; is a property entry
    M_K_ID_TOKEN        = 2:1e9,    ;;; is a token carrying an identifier
    M_K_PLOG_VARNUM     = 2:1e10,   ;;; prolog varnum entry key
    M_K_MATCH_VAR       = 2:1e11,   ;;; is a kind of match var
    M_K_DOUBLE_ALIGN    = #_IF DOUBLE_ALIGN_BITS > WORD_ALIGN_BITS 2:1e12
                          #_ELSE 0
                          #_ENDIF,  ;;; struct must be doubleword aligned
    M_K_WRITEABLE       = 2:1e13,   ;;; class should always be kept writeable
    M_K_NONWRITEABLE    = 2:1e14,   ;;; class can always be made nonwriteable
    M_K_EXTERN_PTR      = 2:1e15,   ;;; is external ptr-class
    M_K_EXTERN_PTR_PROPS= 2:1e16,   ;;; is external ptr-class with props field
    M_K_NO_FULL_FROM_PTR= 2:1e17,   ;;; has no full fields from ptr onwards
    M_K_ALWAYS_FIXED    = 2:1e18,   ;;; struct is always fixed-address
    M_K_STRING16        = 2:1e19,   ;;; is a string16 ("string16" or "dstring16")
    M_K_DSTRING         = 2:1e20,   ;;; is a dstring ("dstring" or "dstring16")

    ;;; non-number type (see src/numbers.ph for number types)
    NUMTYPE_NON_NUMBER   = 7,

    ;;; Prolog gross type codes, used for unification and compilation
    PROLOG_TYPE_VAR     = 1,    ;;; prologvar
    PROLOG_TYPE_PAIR    = 2,    ;;; conspair
    PROLOG_TYPE_TERM    = 3,    ;;; prologterm
    PROLOG_TYPE_NIL     = 4,    ;;; nil
    PROLOG_TYPE_CONST   = 5,    ;;; integers/words (compared with ==)
    PROLOG_TYPE_OTHER   = 6,    ;;; all other data types

    ;;; External coercion type used by _call_external
    EXTERN_TYPE_NORMAL  = 0,    ;;; normal (pass struct pointer)
    EXTERN_TYPE_DEREF   = 1,    ;;; pass SECOND field (e.g. XP_PTR)
    EXTERN_TYPE_DDEC    = 2,    ;;; ddecimal
    EXTERN_TYPE_BIGINT  = 3,    ;;; biginteger
    EXTERN_TYPE_SIMPLE  = 4,    ;;; integer or decimal
    );


    /*  Flags for identifier identprops
    */
constant macro (
    M_ID_PROTECT        = 2:1e0,    ;;; protect from user assignment or declaration
    M_ID_ASSIGNED_CONST = 2:1e1,    ;;; has constant value
    M_ID_CONSTANT       = 2:1e2,    ;;; user declared constant not yet assigned to
    M_ID_SYNTAX         = 2:1e3,    ;;; syntax word
    M_ID_OPERATOR       = 2:1e4,    ;;; operator
    M_ID_MACRO          = 2:1e5,    ;;; macro
    M_ID_ACTIVE         = 2:1e6,    ;;; active
    M_ID_PROCEDURE      = 2:1e7,    ;;; procedure
    M_ID_ACTIVATED      = 2:1e8,    ;;; activated
    M_ID_LEX            = 2:1e9,    ;;; lexically declared identifier
    M_ID_LEX_TOKEN      = 2:1e10,   ;;; lexical identifier token

    M_ID_CONST_OR_PROT  = M_ID_PROTECT || M_ID_ASSIGNED_CONST || M_ID_CONSTANT,
    M_ID_PROCEDURE_VAL  = M_ID_PROCEDURE || M_ID_ACTIVE,
    M_ID_ASSIGN_CHECK   = M_ID_CONST_OR_PROT || M_ID_PROCEDURE_VAL,

    ;;; additional ones used by the pop-11 compiler in "_nextitem_code"
    ;;; M_ID_IS_WORD must be a bit distinct from M_ID_SYNTAX and M_ID_OPERATOR
    M_ID_IS_WORD        = 2:1e0,    ;;; nextitem is a word
    M_ID_TERMIN         = 2:1e1,    ;;; proglist is null
    );

    ;;; permanent identifier flags
constant macro (
    M_PERM_GLOBAL       = 2:1e0,    ;;; import into every section
    M_PERM_REGISTER     = 2:1e1,    ;;; register identifier
    M_PERM_FLAG_MASK    = 2:1e2 -1, ;;; masks PERM flags in WRI_IDTYPE

    ;;; other PERM flags not extracted from WRI_IDTYPE (not used
    ;;; in wlib symbol index)
    M_PERM_SYS_REFER    = 2:1e2,    ;;; identifier referenced in system
    M_PERM_IMPORT       = 2:1e3,    ;;; import into current section
    M_PERM_NOT_DEF      = 2:1e4,    ;;; weak declaration only

    ;;; Other flags for perm identifiers in the WRI_IDTYPE field on the .w file
    ;;; (3 bytes). Bottom byte used as perm flags field in wlib symbol index.
    IDT_INCREMENTAL     = 2:1e2,    ;;; incrementally-linked identifier
    IDT_HAS_UPDATER     = 2:1e3,    ;;; perm const procedure has updater
    IDT_NON_POP         = 2:1e4,    ;;; non-pop identifier
    IDT_PROTECT         = 2:1e5,    ;;; protected
    IDT_VALUE_USED      = 2:1e6,    ;;; value used (only of interest for constants)
    IDT_STRONG_REF      = 2:1e7,    ;;; referenced other than with weakref
    IDT_DECLARED        = 2:1e8,    ;;; declared as an identifier
    IDT_EXPLICIT        = 2:1e9,    ;;; explicitly declared (i.e. decl not autoloaded)
    IDT_GEN_IDENT       = 2:1e10,   ;;; identifier record used
    IDT_GEN_WORDID      = 2:1e11,   ;;; word identifier record used
    IDT_GEN_PDPROPS     = 2:1e12,   ;;; need value for pdprops label
    IDT_GEN_TESTLAB     = 2:1e13,   ;;; need value for testdef label
    IDT_GEN_IN_DICT     = 2:1e14,   ;;; generate identifier in dictionary/sect
    IDT_GEN_FULL_ID     = 2:1e15,   ;;; gen full identifier record
    IDT_OBJMOD_INIT     = 2:1e16,   ;;; init value defines a non-poplink external symbol
    IDT_PROLOG_PRED     = 2:1e17,   ;;; is a prolog predicate
    IDT_PROLOG_DYNAMIC  = 2:1e18,   ;;; is a dynamic predicate
    IDT_USED            = 2:1e19,   ;;; identifier was specified in uses
    WFILE_IDT_MASK      = 2:1e20 - 1,

    ;;; poplink internal use only
    IDT_GEN_WORD        = 2:1e20,   ;;; word record needed
    IDT_GLOB_WORD       = 2:1e21,   ;;; use global label for word record
    IDT_WEAK_DEPENDS    = 2:1e22,   ;;; weakness is dependent on other identifiers
    IDT_GROUP_MARK      = 2:1e23,   ;;; mark used when searching library group
    IDT_OPTIONAL_INIT   = 2:1e24,   ;;; variable initialisation is 'optional'

    ;;; popc internal use only
    IDT_OUTPUT          = 2:1e20,   ;;; recorded for output to .w file
    IDT_WEAK_REF        = 2:1e21,   ;;; weak references only
    IDT_INCR_VAL_USED   = 2:1e22,   ;;; incremental ident value used
    POPC_IDT_MASK       = 2:1e23 - 1,
    ;;; these are arg flags to note_perm_ident
    NPA_WEAK_OPTION     = 2:1e23,   ;;; don't set WEAK or STRONG
    );


    ;;; internal representation of operator precedence
define macro INTERNAL_OP_PREC prec;
    lvars prec;
    intof(prec*10) fi_<< 1 -> prec;
    if prec fi_> 0 then prec fi_+ 1 else -prec endif
enddefine;

    ;;; external representation from internal
define lconstant macro EXTERNAL_OP_PREC prec;
    lvars prec;
    prec fi_>> 1, if prec &&=_0 1 then negate() endif / 10
enddefine;


constant macro (

    ;;; procedure flag bits in PD_FLAGS
    M_PD_CLOSURE            = 2:1e0,    ;;; if this is a closure

    ;;; not a closure
    M_PD_ARRAY              = 2:1e1,    ;;; array procedure
    M_PD_COMPOSITE          = 2:1e2,    ;;; composition of 2 procedures
    M_PD_PROC_DLEXPR_CODE   = 2:1e3,    ;;; run dlexpr code in process suspend/resume
    M_PD_SAVES_NEXTFREE     = 2:1e4,    ;;; has _nextfree_save as local
    M_PD_PLOG_CHOICE        = 2:1e5,    ;;; maintains prolog choice point
    M_PD_DOES_PLOG_PUSH     = 2:1e6,    ;;; pushes on prolog trail

    ;;; closures only
    M_PD_CLOS_PROTECT       = 2:1e1,    ;;; frozvals, etc protected
    M_PD_CLOS_UNDEF         = 2:1e2,    ;;; is an 'undef' closure
    M_PD_CLOS_PROPERTY      = 2:1e3,    ;;; is a property
    M_PD_CLOS_INTEGER_FIELD = 2:1e4,    ;;; is an integer field closure
    M_PD_CLOS_WRITEABLE     = 2:1e5,    ;;; keep writeable after sys_lock_system


    ;;; proper procedure flag bits in PD_FLAGS2
    M_PD2_HAS_DLOCAL_ACTIVE = 2:1e0,    ;;; has dlocal active variable(s)
    );



endsection;     /* $- */


section $-Popas;

    ;;; see also Idprops_flags in ident.p
define idprops_flags(id, separate);
    lvars idprops, num, id, actm, separate;
    if (identprops(id) ->> idprops) == 0 then
        if isident(identtype(id) ->> idprops) then
            identtype(idprops) -> idprops
        endif
    endif;

    if isactive(id) ->> actm then actm else 0 endif -> num;

    if idprops == 0 then
        0
    elseif idprops == "procedure" then
        M_ID_PROCEDURE
    elseif idprops == "syntax" then
        M_ID_SYNTAX
    elseif idprops == "macro" then
        M_ID_MACRO
    elseif isnumber(idprops) then
        ;;; operator
        nonmac INTERNAL_OP_PREC(idprops) -> num;
        #_< M_ID_OPERATOR || M_ID_PROCEDURE >_#
    elseif isstartstring('syntax\s', idprops) then
        ;;; syntax operator
        nonmac INTERNAL_OP_PREC(strnumber(allbutfirst(7, idprops))) -> num;
        #_< M_ID_SYNTAX || M_ID_OPERATOR || M_ID_PROCEDURE >_#
    else
        mishap(idprops, 1, 'UNKNOWN IDENTPROPS')
    endif;

    if actm then () fi_|| M_ID_ACTIVE endif;
    if nonactive_isconstant(id) then () fi_|| M_ID_CONSTANT endif;

    if separate then
        (), num
    else
        () fi_|| (num fi_<< SHORT_BITS)
    endif
enddefine;

define flags_idprops(id_flags);
    lvars id_flags, num = id_flags fi_>> SHORT_BITS, actm = 1;
    if id_flags &&/=_0 M_ID_OPERATOR then
        nonmac EXTERNAL_OP_PREC(num) -> num;
        if id_flags &&/=_0 M_ID_SYNTAX then
            ;;; syntax operator
            consword('syntax\s' sys_>< num)
        else
            ;;; normal operator
            num
        endif
    elseif id_flags &&/=_0 M_ID_SYNTAX then
        ;;; syntax word
        "syntax"
    elseif id_flags &&/=_0 M_ID_MACRO then
        "macro"
    elseif id_flags &&/=_0 M_ID_PROCEDURE then
        "procedure"
    else
        num -> actm;
        0
    endif;

    if id_flags &&/=_0 M_ID_ACTIVE then conspair((), actm) endif,
    if id_flags &&/=_0 M_ID_CONSTANT then 2:01 else 2:00 endif
enddefine;


;;; --- SPECIAL VAR BLOCK ----------------------------------------------

    /*  The vectors special_var_block and (optionally) special_var_block_neg
        define memory words at offsets from _special_var_block. Poplink
        generates special_var_block values at increasing positive offsets
        in the order given (starting at 0), and special_var_block_neg values
        at increasing negative offsets in the order given (starting at
        -WORD_OFFS).

        Both may contain either nonpop variables or pop/nonpop constants
        (but not pop variables). The definitions below put all the nonpop
        variables in special_var_block and all the constants in the optional
        special_var_block_neg (enabled with GEN_SVB_CONSTANTS).

        The _special_var_block slot for a nonpop variable is the actual
        variable, while constant slots just contain a copy of the constant
        value. (Note that inclusion in _special_var_block does not constitute
        a strong reference to an identifier; hence either variables or
        constants can be weakly-defined, or not even declared.)

        Popc uses these vectors to generate offsets in the
        _SVB_OFFS(identifier-name) macro. If genproc.p defines
        SPECIAL_VAR_BLOCK_REG, then Popc generates reg-offset M-operands for
        the variables (and for the constants if GEN_SVB_CONSTANTS is true).

        If the order of things in these vectors is changed, all .p and .s
        files depending on them must be recompiled. So add new things at the
        end of each vector.
    */

constant special_var_block =
        {%  '$-\^_call_stack_lim',
            '$-\^_userlim',
            '$-\^_userhi',
            '$-\^_trap',

            '$-\^_plog_trail_lim',
            '$-\^_plog_trail_sp',
            '$-\^_plog_trail_barrier',
            '$-\^_plog_contn_sp',
            '$-\^_plog_save_contn_sp',
            '$-\^_plog_contn_top',
            '$-\^_plog_next_var',
            '$-\^_plog_contn_barrier',

            '$-Sys$-\^_curr_heap_seg',
            '$-Sys$-\^_open_seg_free_ptr',
            '$-Sys$-\^_curr_seg_free_ptr',
            '$-Sys$-\^_curr_seg_free_lim',

            '$-Sys$-Gc$-\^_lowest_garbageable',
            '$-Sys$-Gc$-\^_deferred',
            '$-Sys$-Gc$-\^_callstack_lim',
            '$-Sys$-Gc$-\^_copy_gc',
            '$-Sys$-Gc$-\^_curr_gcseg_base',
            '$-Sys$-Gc$-\^_curr_gcseg_lim',
            '$-Sys$-Gc$-\^_curr_gcseg_reloc',
            '$-Sys$-Gc$-\^_curr_gcseg_copy',

            '$-Sys$-Extern$-\^_saved_sp',
            '$-Sys$-Extern$-\^_saved_usp',

            '$-Sys$-\^_free_pairs',
            '$-Sys$-\^_in_user_extern',
            '$-Sys$-\^_fpe_handler',
            '$-\^_disable',
#_IF DEF VMS
            '$-Sys$-\^_syserror',
            '$-Sys$-\^_rmserror',
#_ENDIF
            '$-Sys$-Extern$-\^_invocation_fp',
        %};


#_IF DEF GEN_SVB_CONSTANTS

constant special_var_block_neg =
        {%  ;;; nonpop things
            '$-\^_checkall',
            '$-\^_checkplogall',
            '$-\^_checkinterrupt',
            '$-\^_popenter',
            '$-\^_popuenter',
            '$-\^_popuncenter',
            '$-\^_conspair',
            '$-\^_call_sys',

            '$-\^_swap_out_continue',
            '$-\^_swap_in_continue',

            ;;; pop things
            '$-false',
            '$-true',
            '$-nil',

            '$-integer_key',
            '$-decimal_key',
            '$-pair_key',
            '$-ref_key',
            '$-procedure_key',
            '$-prologterm_key',
            '$-prologvar_key',

            '$-mishap',
            '$-Sys$-Array$-Sub_error',
            '$-Sys$-Async_raise_signal',
            '$-Sys$-Call_overflow',
            '$-Sys$-Callstack_reset',
            '$-Sys$-Conspair',
            '$-Sys$-Dlexpr_stackerr',
            '$-Sys$-Exec_nonpd',
            '$-Sys$-Float_qrem',
            '$-Sys$-Plog$-Area_overflow',
            '$-Sys$-Plog$-Assign_pair',
            '$-Sys$-Plog$-New_var',
            '$-Sys$-Syserr_mishap',
            '$-Sys$-User_overflow',

            '$-Sys$-Extern$-result_struct',
        %};

#_ENDIF

endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jan  8 1997
        Added M_K_STRING16.
--- John Gibson, May 25 1995
        Added M_PD2_HAS_DLOCAL_ACTIVE
--- John Gibson, Apr 27 1995
        Made flag M_K_DOUBLE_ALIGN be 0 if DOUBLE_ALIGN_BITS <= WORD_ALIGN_BITS
--- John Gibson, Nov  9 1994
        More additions to special_var_block, and added special_var_block_neg
        conditional on GEN_SVB_CONSTANTS.
--- John Gibson, Oct 18 1994
        Added _free_pairs and _in_user_extern to special_var_block
--- John Gibson, Oct 13 1992
        Rearranged IDT flags
--- John Gibson, Sep 26 1992
        Added IDT_INCREMENTAL
--- John Gibson, Sep  1 1992
        Added M_K_NO_FULL_FROM_PTR
--- John Gibson, Jan  2 1992
        Replaced (unused) M_K_SIMPLE flag with M_K_STRING
--- John Gibson, Mar 14 1990
        Changes to key flags, etc
--- John Gibson, Jul 17 1989
        Version 13.66
--- John Gibson, Jun 13 1989
        Added M_K_WRITEABLE/NONWRITEABLE
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, Feb  3 1989
        Added PD_CLOS_UNDEF
--- John Gibson, Jan 29 1989
        New version of popc
--- John Gibson, Dec 13 1988
        Added M_PERM_NOT_DEF
--- John Gibson, Dec  5 1988
        Added procedure -flags_idprops-
--- John Gibson, Sep  2 1988
        Added -special_var_block- vector
--- John Gibson, Feb  9 1988
        Various revisions
--- John Gibson, Jan 17 1988
        Changes for coping with sections, weakrefs, new format for assembler
        files, etc, etc.
 */
