/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/boolean.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *RECORDS
 */

;;; --------------------- BOOLEANS -------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

global constant
        procedure Sys$-Unique_hash
    ;


;;; --------------------------------------------------------------------

section $-Sys => not, false, true, isboolean, boolean_key;

define not() with_nargs 1;
    _not();
enddefine;

define isboolean(item);
    lvars item;
    iscompound(item) and item!KEY == boolean_key
enddefine;

define lconstant Bool_print() with_nargs 1;
    Print_str(if () then '<true>' else '<false>' endif)
enddefine;


constant

    false       = false,        ;;; defined by poplink
    true        = true,         ;;;     ditto

    ;;; This key has EXTERN_TYPE_DEREF so that passing <false> or <true>
    ;;; externally will pass the BOOL_VAL field at the pointer,
    ;;; which is (machine) 0 for <false> and 1 for <true>

    boolean_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD,   ;;; K_FLAGS
        _:GCTYPE_NONE,          ;;; K_GC_TYPE
        Rec2_getsize,           ;;; K_GET_SIZE

        "boolean",              ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isboolean,              ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Bool_print,             ;;; K_SYS_PRINT
        WREF Bool_print,        ;;; K_PRINT
        WREF Unique_hash,       ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_DEREF,    ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct BOOLEAN)++,   ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R
        %},
    ;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Aug 26 1992
        Made the key EXTERN_TYPE_DEREF (see comment)
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Apr 14 1988
        Moved out of old primkeys.p
 */
