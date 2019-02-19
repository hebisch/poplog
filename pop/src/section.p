/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/section.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

;;; ---------------------- SECTION KEY --------------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'sections.ph'
#_INCLUDE 'gctypes.ph'

;;; -------------------------------------------------------------------------

section $-Sys => pop_section section_key;

constant
    pop_section = pop_section;  ;;; ! (set up by poplink)


define lconstant Sect_print(sect);
    lvars sect;
    dlocal pop_pr_quotes = false;
    Default_print(sect, if sect!SEC_PATH_NAME = '' then
                            false
                        else
                            sect!SEC_PATH_NAME
                        endif)
enddefine;

define lconstant Sect_hash() with_nargs 1;
    syshash(()!SEC_PATH_NAME)
enddefine;

constant
    section_key = struct KEY_R =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD _biset _:M_K_WRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_FULLREC,       ;;; K_GC_TYPE
        Record_getsize,         ;;; K_GET_SIZE

        "section",              ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        procedure() with_nargs 1;
            datakey() == section_key
        endprocedure,           ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Sect_print,             ;;; K_SYS_PRINT
        WREF Sect_print,        ;;; K_PRINT
        WREF Sect_hash,         ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct SECTION)++,   ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R
        %};


define Sect$-Checkr(item) -> item;
    lvars item;
    unless iscompound(item) and item!KEY == section_key then
        mishap(item, 1, 'SECTION NEEDED')
    endunless
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Mar 27 1988
        Split from sections.p
 */
