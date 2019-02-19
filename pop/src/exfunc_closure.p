/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/src/exfunc_closure.p
 > Purpose:
 > Author:          John Gibson, Sep  5 1990 (see revisions)
 > Documentation:   REF *EXTERNAL_DATA
 */

;;; -------------- EXTERNAL FUNCTION 'CLOSURES' --------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'external.ph'
#_INCLUDE 'gctypes.ph'

global constant
        procedure (copy_fixed, Sys$-Checkr_exptrclass, consintvec),
        null_external_ptr
    ;


;;; ----------------------------------------------------------------------

section $-Sys => make_exfunc_closure, isexfunc_closure, exfunc_export;

constant exfunc_closure_key;

    ;;; Popc generates the code in this
lconstant efc_template = make_exfunc_closure(null_external_ptr, false, false);

    /*  Internal version that allows different destinations
        for the arg to be assigned to.
    */
define Cons_exfunc_closure(func_exptr, arg, _hold, _dest_ptr) -> _efc;
    lvars func_exptr, arg, _efc, _hold, _dest_ptr;
    Checkr_exptrclass(func_exptr) -> ;
    copy_fixed(efc_template, _hold) -> _efc;
#_IF DEF AIX
    ;;; make the 2nd word of the descriptor (the Table Of Contents pointer)
    ;;; point to the record itself
    _efc -> _efc!(w)[_1];
#_ELSEIF DEF CACHEFLUSH and not(DEF ALPHA_VMS)  ;;; contains no code in Alpha VMS
    CACHEFLUSH(_efc@EFC_CODE, _:EFC_CODE_SIZE);
#_ENDIF
    func_exptr  -> _efc!EFC_FUNC;
    arg         -> _efc!EFC_ARG;
    _dest_ptr   -> _efc!EFC_ARG_DEST
enddefine;

    /*  Public version assigns its arg to _extern pop_exfunc_arg
    */
define make_exfunc_closure() with_nargs 3;
    Cons_exfunc_closure((), _extern pop_exfunc_arg:data)
enddefine;

    /* turn a pop procedure into an exfunc (ie C-callable)
     */
define exfunc_export(proc,flags,hold);
    lvars proc, flags, hold, data;
    lconstant macro XFLAG = PEF_RETURN_ABEXIT_NEXT;

    ;;; saves creating a new intvec every time for X callbacks
    lconstant Xvec = consintvec(XFLAG,1);

    /* exptr representation for C wrapper procedure
     */
    lconstant exptr = struct EXTERNAL_PTR
                    =>> {%  ;;; necessary to copy this string because it's
                            ;;; marked as a label (it is in fact the same
                            ;;; as _extern _pop_exfunc_callback)
                            copy(EXTERN_NAME(_pop_exfunc_callback)),

                            external_ptr_key,
                            _extern _pop_exfunc_callback
                        %};


    /*  build pop_exfunc_arg structure (accessed by _pop_exfunc_callback)
     *  could flatten it with a new key but is it worth it?
     */
    consvector(if flags == XFLAG then Xvec else consintvec(flags,1) endif,
                        proc, 2) -> data;

    Cons_exfunc_closure(exptr, data, hold, _extern pop_exfunc_arg:data);
enddefine;

;;; --- EXFUNC CLOSURE KEY -----------------------------------------------

define isexfunc_closure(_item) with_nargs 1;
    lvars _item;
    iscompound(_item) and _item!KEY == exfunc_closure_key
enddefine;

define lconstant Eq__Exfunc_clos(_item, _efc);
    lvars _item, _efc;
    iscompound(_item) and _item!KEY == exfunc_closure_key
    and _item!EFC_ARG_DEST == _efc!EFC_ARG_DEST
    and _item!EFC_FUNC!XP_PTR == _efc!EFC_FUNC!XP_PTR
    and _item!EFC_ARG = _efc!EFC_ARG
enddefine;

define lconstant Exfunc_clos_print(efc);
    lvars efc;
    Print_str('<exfunc_closure');
    if pop_pr_level /== 0 then
        cucharout(`\s`), spr(efc!EFC_FUNC), pr(efc!EFC_ARG)
    endif;
    cucharout(`>`)
enddefine;

define lconstant Exfunc_clos_hash() with_nargs 1;
    _pint(()!EFC_FUNC!XP_PTR)
enddefine;

constant
    exfunc_closure_key = struct KEY_R_NAFULL =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD    ;;; K_FLAGS
#_IF DEF ALPHA_VMS
            _biset _:M_K_DOUBLE_ALIGN
#_ENDIF
            _biset _:M_K_ALWAYS_FIXED
            _biset _:M_K_COPY,
        _:GCTYPE_NFULLREC,      ;;; K_GC_TYPE
        Record_getsize,         ;;; K_GET_SIZE

        "exfunc_closure",       ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isexfunc_closure,       ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Exfunc_clos,        ;;; K_SYS_EQUALS
        WREF Eq__Exfunc_clos,   ;;; K_EQUALS
        Exfunc_clos_print,      ;;; K_SYS_PRINT
        WREF Exfunc_clos_print, ;;; K_PRINT
        WREF Exfunc_clos_hash,  ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct EXFUNC_CLOS)++, ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R

        @@(int)[_2],            ;;; K_FULL_OFFS_SIZE
        =>> {%                  ;;; K_FULL_OFFS_TAB[_2]
                @@EFC_FUNC,
                @@EFC_ARG
            %}
        %};


endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar 19 1998
        Added AIX case in Cons_exfunc_closure
--- John Gibson, Jul 28 1995
        Fixed routine name string in exptr in exfunc_export
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 16 1995
        EFC_CODE now a byte field of size EFC_CODE_SIZE
--- John Gibson, Oct 25 1994
        Added M_K_DOUBLE_ALIGN to key flags for Alpha
--- Robert John Duncan, Mar 15 1994
        Number of bytes of code in an exfunc closure record now defined as
        EFC_CODE_SIZE.
--- John Gibson, May 19 1993
        efc_template now created with make_exfunc_closure, relying on
        POPC to generate the code in it.
--- John Gibson, Sep  3 1992
        Added M_K_ALWAYS_FIXED to key flags
--- Roger Evans, Jun 27 1991
        removed numargs arg from exfunc_export
--- Roger Evans, Jun 24 1991
        exfunc_coerce -> exfunc_export
        exported isexfunc_closure
--- Roger Evans, Jun 23 1991
        Added exfunc_coerce
--- Robert John Duncan, Feb 11 1991
        Added cache flush
 */
