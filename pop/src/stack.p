/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/stack.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *STACK
 */

;;; ------------- PROCEDURES FOR MANIPULATING THE STACK -----------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'gctypes.ph'

global constant
        procedure (Sys$-Key_hash, Sys$-User_underflow),
        _useras
    ;

;;; ---------------------------------------------------------------------

section $-Sys => identfn, erase, dup, stacklength, clearstack, erasenum,
                    stackmark_key;

define identfn(); enddefine;

define erase() with_nargs 1; -> enddefine;

define dup() with_nargs 1; _dupstk() enddefine;

define clearstack();
    _useras(_stklength())
enddefine;

define erasenum(_num);
    lvars _num, _offs;
    if isinteger(_num)
    and (@@(w)[_int(_num)] -> _offs; _stklength() _greq _offs) then
        _useras(_offs)
    else
        _pint(##(w){_stklength()}) -> _offs;
        Check_integer_range(_num, 0, _offs)
    endif
enddefine;


;;; --- STACKMARK KEY ---------------------------------------------------

define lconstant Stackmark_print(smark);
    lvars smark;
    Default_print(  if smark == popstackmark then
                        "popstackmark"
                    else
                        smark
                    endif, false)
enddefine;

constant
    stackmark_key = struct KEY_R_NAFULL =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD _biset _:M_K_COPY _biset _:M_K_NONWRITEABLE,
                                ;;; K_FLAGS
        _:GCTYPE_NFULLREC,      ;;; K_GC_TYPE
        Record_getsize,         ;;; K_GET_SIZE

        "stackmark",            ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        procedure() with_nargs 1;
            datakey() == stackmark_key
        endprocedure,           ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        nonop ==,               ;;; K_SYS_EQUALS
        WREF nonop ==,          ;;; K_EQUALS
        Stackmark_print,        ;;; K_SYS_PRINT
        WREF Stackmark_print,   ;;; K_PRINT
        WREF Key_hash,          ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_NORMAL,   ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct STACKMARK)++, ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R

        @@(int)[_0],            ;;; K_FULL_OFFS_SIZE
        =>> {}                  ;;; K_FULL_OFFS_TAB[_0]
        %};


;;; --- SPECIAL THINGS AT THE END --------------------------------------------
;;; N.B. Execute-level code that references this afterwards
;;; will not get the normal value (i.e. so leave it as the last thing
;;; in the file).

define stacklength();
    _pint(##(w){_stklength()})
enddefine;


endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 22 1996
        Improved erasenum
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 14 1990
        Change to key layout.
--- John Gibson, Dec  4 1989
        Changes for new pop pointers
--- John Gibson, Jan 29 1989
        Moved -stacklength- to end of file so it compiles with new version
        of popc (see comment above).
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
 */
