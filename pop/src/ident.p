/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/ident.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *IDENT
 */

;;; ---------------------- IDENTIFIER KEYS -----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'ident.ph'
#_INCLUDE 'gctypes.ph'


;;; -----------------------------------------------------------------------

section $-Sys => isident fast_idval ident_key;

define isident(item);
    lvars item, _idprops;
    if iscompound(item) and item!KEY == ident_key then
        unless (item!ID_IDENTPROPS ->> _idprops) _bitst _:M_ID_LEX then
            "perm"
        elseif _idprops _bitst _:M_ID_LEX_TOKEN then
            "lextoken"
        else
            "lex"
        endunless
    else
        false
    endif
enddefine;

define fast_idval(/* id */) with_nargs 1;
    ()!ID_VALOF
enddefine;
;;;
define updaterof fast_idval(/* newval, id */) with_nargs 2;
    () -> ()!ID_VALOF
enddefine;

define lconstant Ident_print(id);
    lvars id;
    if pop_pr_level == 0 then return(Minimal_print(id)) endif;
    cucharout(`<`), sys_syspr(dataword(id)),
    cucharout(`\s`), pr(fast_idval(id)), cucharout(`>`)
enddefine;

constant
    ident_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD _biset _:M_K_WRITEABLE _biset _:M_K_COPY,
                                ;;; K_FLAGS
        _:GCTYPE_FULL2ND_REC2,  ;;; K_GC_TYPE
        Rec2_getsize,           ;;; K_GET_SIZE

        "ident",                ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isident,                ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Ident_print,            ;;; K_SYS_PRINT
        WREF Ident_print,       ;;; K_PRINT
        WREF Fullrec1_hash,     ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct IDENT)++,     ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R
        %},

    ;;; short name for ident_key used by poplink
    $-K$-i  = ident_key,

    ;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 25 1995
        _:GCTYPE_ID*ENTIFIER -> _:GCTYPE_FULL2ND_REC2
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Mar  5 1990
        Removed short_i*dent_key (no longer needed)
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, May 15 1989
        Included ident.ph
--- John Gibson, Feb  4 1989
        Moved some things out to other files
--- John Gibson, Apr  5 1988
        Moved -idval- etc to idval.p
--- John Gibson, Mar 14 1988
        Various things moved out to separate files
--- John Gibson, Feb 28 1988
        -identprops-, -identtype- and 'is...' procedures moved to
        identprops.p
--- John Gibson, Dec 18 1987
        Added some missing declarations at top of file
--- John Gibson, Dec 16 1987
        Added declarations at top of file for -identof-, -nonactive_idval-
--- John Gibson, Nov 24 1987
        Added updater of -identof-. Moved -syssynonym- into $popautolib
        defined as identof(word2) -> identof(word1)
 */
