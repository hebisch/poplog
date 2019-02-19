/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/ref.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;;----------------------- REFERENCES --------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

constant
        procedure (Sys$- Eq__Fullrec1, Sys$-Data_print)
    ;

;;; -----------------------------------------------------------------------

section $-Sys => isref, consref, cont, fast_cont, ref_key;

define lconstant Ref_needed = mishap(% 1, 'REFERENCE NEEDED' %) enddefine;

define isref(_item);
    lvars _item;
    iscompound(_item) and _item!KEY == ref_key
enddefine;

define cont(/*ref*/) with_nargs 1;
    if isref(dup()) then fast_cont() else Ref_needed() endif
enddefine;
;;;
define updaterof cont(/*newcont,ref*/) with_nargs 2;
    if isref(dup()) then () -> fast_cont() else Ref_needed() endif
enddefine;

define fast_cont() with_nargs 1;
    ()!RF_CONT
enddefine;
;;;
define updaterof fast_cont() with_nargs 2;
    () -> ()!RF_CONT
enddefine;


;;; --- REF KEY -----------------------------------------------------------

constant
    ref_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_RECORD _biset _:M_K_COPY _biset _:M_K_WRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_FULLREC1,      ;;; K_GC_TYPE
        Rec1_getsize,           ;;; K_GET_SIZE

        "ref",                  ;;; K_DATAWORD
        [full],                 ;;; K_SPEC
        isref,                  ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Fullrec1,           ;;; K_SYS_EQUALS
        WREF Eq__Fullrec1,      ;;; K_EQUALS
        Data_print,             ;;; K_SYS_PRINT
        WREF Data_print,        ;;; K_PRINT
        WREF Fullrec1_hash,     ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct REF)++,       ;;; K_RECSIZE_R
        consref,                ;;; K_CONS_R
        cont,                   ;;; K_DEST_R
        {^cont},                ;;; K_ACCESS_R
        %};


;;; --- SPECIAL THINGS AT THE END --------------------------------------------
;;; N.B. Execute-level code that references this afterwards
;;; will not get the normal value (i.e. so leave it as the last thing
;;; in the file).

define consref() -> _ref with_nargs 1;
    lvars _ref = Get_store(@@(struct REF)++);
    ref_key -> _ref!KEY;
    () -> _ref!RF_CONT
enddefine;


endsection;     /* $-Sys */


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Sep 12 1990
        Tidied up etc
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Jan 29 1989
        Moved -consref- to end of file so it compiles with new version
        of popc (see comment above).
--- John Gibson, Sep  5 1987
        Changed ref_key to new format
 */
