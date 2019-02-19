/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/syspop.ph
 > Purpose:
 > Author:          John Gibson, Dec 11 1992 (see revisions)
 */

#_TERMIN_IF DEF SYSPOP_INCLUDED

;;; ------ COMPILE MODES/MACROS FOR CORE-SYSTEM SOURCE FILES --------------
;;;
;;; N.B. Don't put permanent identifier declarations in this file -- these
;;;      should go in declare.ph.


    /*  Default POP11 compile modes:

        strict (see lib compile_mode)
        using sysPOP11
    */
compile_mode :pop11 +strict :popc +syspop;

    /*  Default VM compile modes:

        no checks for interrupts/userstack overflow on backward jumps
        no checks for integer argument to -go_on-
        no checks for run-time assignments to procedure variables
        no interrupt/userstack/callstack overflow checks on procedure entry
        no checks on redeclaration of protected identifiers
    */
compile_mode :vm -bjmpch -goonch -typech -pentch -prmprt;

    /*  (Note that the preceding only affect popc compile mode -- they do
        not affect compilation inside -normal_compile-.)
    */


    /*  Make \ an alphabeticiser
    */
12 -> item_chartype(`\\`, itemread);


;;; --- MACRO DEFINITIONS ------------------------------------------------

#_IF DEF LOWEST_ADDRESS
lconstant macro _LOWEST_POP_ADDRESS = [(_:LOWEST_ADDRESS@~POPBASE)];
#_ELSE
;;; false is the lowest address of any pop structure
lconstant macro _LOWEST_POP_ADDRESS = [false];
#_ENDIF


lconstant macro (
    _NULL               = _0,
    _DISABLE_INTERRUPTS = _2:01,    ;;; disable interrupts
    _DISABLE_STKCHECKS  = _2:10,    ;;; disable user/call stack checks
    _DISABLE_ALL        = _2:11,    ;;; disable interrupts and stack checks

    ;;; Check for user stack overflow
    _CHECKUSER      = [if _user_sp() <@(w) _userlim then _checkall() endif;],

    ;;; Check for interrupt
    _CHECKINTERRUPT = [if _nonzero(_trap) then _checkall() endif;],

    ;;; Save _open_seg_free_ptr or _curr_seg_free_ptr in local _nextfree_save
    ;;; before calling Clawback
    _CLAWBACK_SAVE  = [ dlocal $-Sys$- _nextfree_save =
                                if $-Sys$- _curr_heap_seg == _NULL then
                                    $-Sys$- _open_seg_free_ptr
                                else
                                    $-Sys$- _curr_seg_free_ptr
                                endif;],

    ;;; gap size (as word offset) between heap and userstack
    ;;; -- greater of 1024 words or 2 virtual pages
    _USER_SAFEGAP   = if _pint(##(w)[_2|vpage]) < 1024 then @@(w)[_1024]
                      else @@(vpage)[_2]
                      endif,

    ;;; safe gap (as word offset) between prolog trail and continuation stack
    _PLOG_SAFEGAP   = @@(w)[_512],

    ;;; Flags for coding functions
    _INPUT_INCOMPLETE   = _2:1e0,   ;;; decode
    _BYTE_INPUT         = _2:1e0,   ;;; encode
    _STOP_AT_NEWLINE    = _2:1e1,

    ;;; Pointer mode for Consstring_bptr
    CSB_FIXED       = 0,    ;;; pointer is fixed-address
    CSB_LSTACKMEM   = 1,    ;;; pointer is lstackmem
    CSB_POP_STRING  = 2,    ;;; pointer is pop string

    VM_LOADED       = [testdef sysCOMPILE],
    VM_WEAK         = [weakref[sysCOMPILE]],

    VED_LOADED      = [testdef vedprocess],
    VED_WEAK        = [weakref[vedprocess]],

    );

#_IF DEF VMS
    ;;; length of sysstring
lconstant macro SYSSTRING_LENGTH = 256;
#_ENDIF

deftype
    ;;; lstackmem buffer for encoding filenames, etc
    stackbuff   = byte[760],
    ;;; lstackmem buffer for decoding filenames, etc
    s_stackbuff = short[460];

struct TIMEVAL
  { -long TIM_SEC, TIM_USEC; };

struct CODING_FUNCS
  { (code)  CDFN_ENCODE,    ;;; UNICODE -> something
            CDFN_DECODE;    ;;; something -> UNICODE
  };


define :inline lconstant SAVE_ERRNO(_s1, _s2);
#_IF DEF UNIX \n
    (_ERRNO -> _s1)
#_ELSEIF DEF VMS \n
    (_syserror -> _s1, _rmserror -> _s2)
#_ELSEIF DEF WINDOWS \n
    (_syserror -> _s1)
#_ENDIF
enddefine;

define :inline lconstant RESTORE_ERRNO(_s1, _s2);
#_IF DEF UNIX \n
    (_s1 -> _ERRNO)
#_ELSEIF DEF VMS \n
    (_s1 -> _syserror, _s2 -> _rmserror)
#_ELSEIF DEF WINDOWS \n
    (_s1 -> _syserror)
#_ENDIF
enddefine;


define :inline lconstant EQ(x, y);
    fast_apply(x, y, fast_cont(datakey(dup())!K_EQUALS))
enddefine;

define :inline lconstant CHAIN_EQ(x, y);
    fast_chain(x, y, fast_cont(datakey(dup())!K_EQUALS))
enddefine;


    ;;; syntax word to produce a writeable reference -- used in key definitions
define lconstant syntax WREF;
    lvars item = itemread();
    if item == "nonop" then readitem() -> item endif;   ;;; don't need nonop
    sysPUSH(sys_read_path(item, false, false));
    sysCALL("consref");
    sysCALL("writeable")
enddefine;


define lconstant read_type_();
    lvars item, n = 0;
    pop11_need_nextreaditem("(") -> ;
    until (readitem() ->> item) == ")" and n == 0 do
        if item == "(" then n+1 -> n endif;
        item
    enduntil
enddefine;

define lconstant macro _MOST_POSITIVE_UNSIGNED;
    [ _shift(_1, ##(1)[_1 | %read_type_()% ]) _sub _1 ].dl
enddefine;

define lconstant macro _MOST_POSITIVE_SIGNED;
    [ _shift(_1, ##(1)[_1 | %read_type_()% ] _sub _1) _sub _1 ].dl
enddefine;

define lconstant macro _MOST_NEGATIVE_SIGNED;
    [ _shift(_-1, ##(1)[_1 | %read_type_()% ] _sub _1) ].dl
enddefine;

define lconstant macro EXCEEDS_POPINT_UNSIGNED;
    [ (_pint(##(1)[_1 | %read_type_()% ]) > POPINT_BITS) ].dl
enddefine;

define lconstant macro EXCEEDS_POPINT_SIGNED;
    [ (_pint(##(1)[_1 | %read_type_()% ]) > POPINT_BITS+1) ].dl
enddefine;

define lconstant macro SIZEOF;
    [ #_< _pint( ##(b) [_1 | %read_type_()% ] ) >_# ].dl
enddefine;

define lconstant macro _INIT_NONPOP_STRUCT;
    [ (writeable inits(SIZEOF( %read_type_()% )))@V_WORDS].dl
enddefine;

define :inline lconstant _BYTEVEC_DATA_SIZE(_len);
    @@(w)[(_len) _add ##(b)[_1|w] | b.t]
enddefine;

define :inline lconstant _BYTEVEC_LIM_OFFS(_len);
    @@V_WORDS{_BYTEVEC_DATA_SIZE(_len)}
enddefine;

define :inline lconstant _BYTEVEC_SIZE(_len);
    ( _BYTEVEC_LIM_OFFS(_len) _sub @@POPBASE )
enddefine;

define :inline lconstant _DSTRING_ATTR_CHAR(dstring, _bindex);
    dstring@(w){_BYTEVEC_LIM_OFFS(dstring!V_LENGTH)}!(w->b)[_bindex]
enddefine;

define :inline lconstant _DSTRING_ATTR_PTR(dstring, _bindex);
    dstring@(w){_BYTEVEC_LIM_OFFS(dstring!V_LENGTH)}@(w->b)[_bindex]
enddefine;

define :inline lconstant _STRING16_DATA_SIZE(_len);
    @@(w)[(_len) _add ##(s)[_1|w] | s.t]
enddefine;

define :inline lconstant _STRING16_LIM_OFFS(_len);
    @@V_WORDS{_STRING16_DATA_SIZE(_len)}
enddefine;

define :inline lconstant _STRING16_SIZE(_len);
    ( _STRING16_LIM_OFFS(_len) _sub @@POPBASE )
enddefine;

define :inline lconstant _DSTRING16_ATTR_CHAR(dstring, _bindex);
    dstring@(w){_STRING16_LIM_OFFS(dstring!V_LENGTH)}!(w->b)[_bindex]
enddefine;

define :inline lconstant _DSTRING16_ATTR_PTR(dstring, _bindex);
    dstring@(w){_STRING16_LIM_OFFS(dstring!V_LENGTH)}@(w->b)[_bindex]
enddefine;

lconstant SYSPOP_INCLUDED = true;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  4 1997
        Added deftype stackbuff
--- John Gibson, Jan 17 1997
        Added _STRING16_ things.
--- John Gibson, Jun 11 1996
        Added SAVE_ERRNO/RESTORE_ERRNO
--- John Gibson, Jan  3 1996
        Added EQ and CHAIN_EQ
--- John Gibson, Aug 23 1995
        Removed EXPTR*_NEEDS_DALIGN stuff
--- John Gibson, Jun 30 1995
        Added various macros
--- John Gibson, Mar 18 1995
        Added EXPTR*_NEEDS_DALIGN macro
--- John Gibson, Jun  1 1993
        Added SYSPOP_INCLUDED
 */
