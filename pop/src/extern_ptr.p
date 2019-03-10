/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/extern_ptr.p
 > Purpose:
 > Author:          John Gibson, Mar 19 1990 (see revisions)
 > Documentation:   REF *EXTERNAL
 */

;;; --------------------- EXTERNAL POINTERS ------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'external.ph'
#_INCLUDE 'signals.ph'
#_INCLUDE 'gctypes.ph'

constant
        procedure (is_poplog_item, is_fixed),
        _call_external
    ;

section $-Sys;

vars
        _in_X_call
    ;

weak constant
        procedure (Try_encode_string,
        Fld$-Float_val_s, Extern$-Get_encoding_funcs)
    ;

endsection;


;;; --------------------------------------------------------------------

section $-Sys => consexternal_ptr, external_ptr_props, fill_external_ptr,
                 isexternal_ptr, isexternal_ptr_class, is_valid_external_ptr,
                 is_null_external_ptr, external_NULL, exacc_ntstring,
                 null_external_ptr, external_ptr_key,
                 C_pop_uc_to_mb;

lconstant
    exptrclass_needed   = 'EXTERNAL POINTER-CLASS STRUCTURE NEEDED',
    bad_subscript       = 'BAD SUBSCRIPT FOR INDEXED EXTERNAL POINTER ACCESS',
    ;

;;; a constant writeable external pointer for general system use
constant Sys_external_ptr = writeable struct EXTERNAL_PTR
                                =>> {% false, external_ptr_key, _NULL %};

;;; a constant null external pointer for general use
constant null_external_ptr = struct EXTERNAL_PTR
                                    =>> {% false, external_ptr_key, _NULL %};


;;; --- CHECK PROCEDURES FOR EXTERN FIELDS ETC ----------------------------

define Checkr_exptrclass(_item);
    lvars _item;
    if iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR then
        _item
    else
        mishap(_item, 1, exptrclass_needed)
    endif
enddefine;

define Checkr_exptrclass_ptr(_item);
    lvars _item;
    if iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR then
        _item!XP_PTR
    else
        mishap(_item, 1, exptrclass_needed)
    endif
enddefine;

define Checkr_exptrclass_subscr0(_subs, _item) -> _item -> _subs;
    lvars _item, _subs;
    unless iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR then
        mishap(_item, 1, exptrclass_needed)
    elseunless isinteger(_subs) and 0 fi_< _subs then
        mishap(_subs, _item, 2, bad_subscript)
    endunless
enddefine;

define Checkr_exptrclass_subscr(_subs, _item, _len) -> _item -> _subs;
    lvars _item, _subs, _len;
    unless iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR then
        mishap(_item, 1, exptrclass_needed)
    elseunless isinteger(_subs) and 0 fi_< _subs and _subs fi_<= _len then
        mishap(_subs, _item, 2, bad_subscript)
    endunless
enddefine;

define Checkr_exptrclass_nargs(_nargs, _item) -> _item -> _nargs;
    lvars _item, _nargs;
    unless iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR then
        mishap(_item, 1, exptrclass_needed)
    elseunless isinteger(_nargs) and _nargs fi_>= 0 then
        mishap(_nargs, _item, 2, 'INVALID NUMBER OF ARGS FOR EXTERNAL FUNCTION CALL')
    endunless
enddefine;

define lconstant Check_external_ptr(_exptr);
    lvars _exptr;
    unless iscompound(_exptr) and _exptr!KEY == external_ptr_key then
        mishap(_exptr, 1, 'EXTERNAL POINTER NEEDED')
    endunless
enddefine;

define Cons_extern_ptr(_ptr) -> _exptr;
    lvars _exptr, _ptr;
    Get_store(@@(struct EXTERNAL_PTR)++) -> _exptr;
    external_ptr_key -> _exptr!KEY;
    false -> _exptr!XP_PROPS;
    _ptr -> _exptr!XP_PTR
enddefine;


;;; --- BUILT-IN EXTERNAL POINTER ACCESS PROCEDURES -----------------------

    /*  Null-terminated string
    */
define exacc_ntstring(_exptr);
    lvars _exptr, _decode;
    if iscompound(_exptr) and _exptr!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR then
        (_exptr!XP_PTR, _-1, CSB_FIXED)
    else
        if isword(_exptr) then
            ;;; optional encoding name
            Extern$-Get_encoding_funcs(_exptr) -> (_exptr, _decode)
        elseunless _exptr then
            ;;; false for encoding
            (), false -> (_exptr, _decode)
        endif;
        (Checkr_exptrclass_ptr(_exptr), _-1, CSB_FIXED, _decode)
    endif;
    Consstring_bptr()
enddefine;
;;;
define updaterof exacc_ntstring(string, _exptr);
    lvars string, _exptr, _encode = sys_encoding, _nbytes;
    if iscompound(_exptr) and _exptr!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR then
        _exptr!XP_PTR -> _exptr
    else
        if isword(_exptr) then
            ;;; optional encoding name
            string, Extern$-Get_encoding_funcs(_exptr)
                                -> (string, _exptr, _encode)
        elseunless _exptr then
            ;;; false for encoding
            (), string, false -> (string, _exptr, _encode)
        endif;
        Checkr_exptrclass_ptr(_exptr) -> _exptr
    endif;
    Check_string(string);
    Try_encode_string(string, _exptr, _MOST_POSITIVE_UNSIGNED(i), _encode)
                                                -> (, _nbytes);
    _0 -> _exptr!(b)[_nbytes]
enddefine;


;;; --- EXTERNAL POINTERS -----------------------------------------------

define consexternal_ptr() -> _exptr;
    lvars _exptr;
    Get_store(@@(struct EXTERNAL_PTR)++) -> _exptr;
    external_ptr_key -> _exptr!KEY;
    false -> _exptr!XP_PROPS;
    _NULL -> _exptr!XP_PTR
enddefine;

define external_ptr_props(_exptr);
    lvars _exptr;
    if iscompound(_exptr) and _exptr!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR_PROPS
    then
        _exptr!XP_PROPS
    else
        mishap(_exptr, 1, 'EXTERNAL POINTER-CLASS WITH PROPS NEEDED')
    endif
enddefine;
;;;
define updaterof external_ptr_props(/*newval*/ _exptr) with_nargs 2;
    lvars _exptr;
    external_ptr_props(_exptr) -> ;     ;;; check it
    () -> _exptr!XP_PROPS
enddefine;

    /*  Fill a fixed-address pop structure into an (ordinary) external
        pointer. Make the props field hold the structure too, which
        ensures that the structure is kept alive if the external pointer
        is.
    */
define fill_external_ptr(_popstruct, _exptr) -> _exptr;
    lvars _exptr, _popstruct;
    Check_external_ptr(_exptr);
    if is_fixed(_popstruct) then
        _popstruct ->> _exptr!XP_PTR -> _exptr!XP_PROPS
    else
        mishap(_popstruct, 1, 'FIXED-ADDRESS STRUCTURE NEEDED')
    endif
enddefine;

define isexternal_ptr(_item);
    lvars _item;
    iscompound(_item) and _item!KEY == external_ptr_key
enddefine;

define isexternal_ptr_class(_item);
    lvars _item;
    if iscompound(_item)
    and (_item!KEY ->> _item)!K_FLAGS _bitst _:M_K_EXTERN_PTR then
        _item
    else
        false
    endif
enddefine;

    /*  Return true if an external pointer class has a valid pointer
        (i.e. excluding at the very least 0 and -1 etc, which
        are possible error returns from Unix routines)
    */
define is_valid_external_ptr(_item);
    lvars _item;
    unless iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR then
        mishap(_item, 1, exptrclass_needed)
    endunless;

    _item!XP_PTR -> _item;
#_IF DEF UNIX
  #_IF DEF SHARED_LIBRARIES
    lconstant _HILIM = _MOST_POSITIVE_UNSIGNED(word);
  #_ELSEIF DEF STACK_GROWS_UP
    lvars _HILIM = _sp();
  #_ELSE
    lconstant _HILIM = _:UNIX_USRSTACK;
  #_ENDIF
    _LOWEST_POP_ADDRESS <=@(b) _item and _item <@(b) _HILIM;
#_ELSEIF DEF VMS
    _LOWEST_POP_ADDRESS <=@(b) _item and _item <@(b) _16:80000000;
#_ELSEIF DEF WIN32
    ;;; does the process have read access at the address?
    _nonzero(_extern pop_has_read_access(_item));
#_ELSE_ERROR
#_ENDIF
enddefine;

define is_null_external_ptr(_item);
    lvars _item;
    unless iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR then
        mishap(_item, 1, exptrclass_needed)
    endunless;
    _zero(_item!XP_PTR)
enddefine;

    /*  This returns a NULL value so it can be passed in a full field to
        an external procedure etc; -sys_syspr- prints it as <NULL>.
        It is NOT recognised by -is_poplog_item-, and attempting to access
        anything in it, even its key, will give an access violation.
    */
define external_NULL();
    _NULL
enddefine;


;;; --- CALLING EXTERNAL FUNCTIONS -----------------------------------------

/*
                            NOTE

    In non-SPARC systems, every procedure that calls _call_external must have
    all pop registers localised (by having the dummy variable
    \<ALL_POP_REGISTERS\> dlocal). This means that (a) _call_external does
    not have to localise them itself, and (b) their values are saved in a
    proper stack frame where they can be processed by a GC during callback
    (hence callback need only localise and set them to pop 0/false, etc).

    Because of this (and the fact that _call_external may use the pop
    registers nonlocally), procedures calling _call_external should not
    actually use any pop lvars (or at least, not assume their values will
    survive the _call_external).

*/

lconstant
    null_call = 'CALLING NULL EXTERNAL FUNCTION POINTER (e.g. unloaded function)';

lconstant macro (
    CALL_DECLARE = [
        lvars _save_inX;
        dlocal
            ;;; Popc dummy -- makes all pop regs local (ignored in SPARC)
            \<ALL_POP_REGISTERS\>,

            _external_flags = _0,

            0 %"%"%
                _in_X_call -> _save_inX,
                if _nonzero(_in_X_call) and _zero(_save_inX) then
                    _0 -> _in_X_call;
                    if _nonzero(_extern _pop_Xt_poll_deferred:data!((b))) then
                        _extern _pop_retry_Xt_poll() ->
                    endif
                endif
              %"%"%,
        ;
    ],

    CALL_AFTER = [
        if _ast_queue_flags _bitst _:QC_CALLBACK
        and _call_stack_seg_hi == _call_stack_hi then
            ;;; wiil check AST queue at next interrupt checkpoint
            _extern _pop_add_ast(_:AST_QUEUE_CHECK, _0) ->
        endif;
    ]
);


constant Extern$-result_struct = writeable struct EXTERN_RESULT =>>
        {% @@(struct EXTERN_RESULT)++, rawstruct_key, _0.0, _0 %};


    /*  Invoke X procedure, disabling potentially recursive async X handling.
        NOTE that this passes (d)decimals as single floats, since none of
        the standard Toolkit routines it's used for take explicit float or
        double args( i.e. any actual float args must be XtPointers or in
        a Va arglist).
     */
define Xt$-X_apply(/* _nargs, _routine */);
    CALL_DECLARE;

    _1 -> _in_X_call;       ;;; nonzero = true

    _call_external(_-1);    ;;; -1 for fltsingle arg = pass singles
    Extern$-result_struct!EXRES_WORD;

    CALL_AFTER
enddefine;

    /*  Invoke X procedure, allowing async callback with abnormal exits
        returned.
    */
define Xt$-X_cb_apply(/* _nargs, _routine */);
    CALL_DECLARE;

    _:PEF_ASYNC_CALLBACK _biset _:PEF_ASYNC_RETURN_ABEXIT -> _external_flags;
    _1 -> _in_X_call;           ;;; nonzero = true

    _call_external(_0);         ;;; 0 for fltsingle arg
    Extern$-result_struct!EXRES_WORD;

    CALL_AFTER
enddefine;


    /*  Procedures for inline calls returning raw pointer from
        _call_external (these check for NULL external pointers).
    */

    ;;; Fixed _nargs
define Call_extern(_exptr, _nargs, _fltsingle);
    lvars _exptr, _nargs, _fltsingle;       ;;; no pop lvars -- see above
    CALL_DECLARE;
    if _exptr!XP_PTR == _NULL then
        ;;; chain to ensure pop regs are restored
        chain(_exptr, 1, null_call, mishap)
    endif;
    _call_external(_int(_nargs), _exptr!XP_PTR, _fltsingle);
    ;;; 3 word result structure
    Extern$-result_struct;
    CALL_AFTER
enddefine;

    ;;; Variadic
define Call_extern_nargs(/*_nargs,*/ _exptr, _fltsingle) with_nargs 3;
    lvars _exptr, _fltsingle;               ;;; no pop lvars -- see above
    CALL_DECLARE;
    if _exptr!XP_PTR == _NULL then
        ;;; chain to ensure pop regs are restored
        chain(_exptr, 1, null_call, mishap)
    endif;
    _call_external(_int(), _exptr!XP_PTR, _fltsingle);
    ;;; 3 word result structure
    Extern$-result_struct;
    CALL_AFTER
enddefine;


;;; --- EXTERNAL POINTER KEY --------------------------------------------

define Eq__Extern_ptr(_item, _exptr);
    lvars _item, _exptr;
    iscompound(_item) and _item!KEY!K_FLAGS _bitst _:M_K_EXTERN_PTR
    and _item!XP_PTR == _exptr!XP_PTR
enddefine;

define Extern_ptr_print() with_nargs 1;
    chain(dup()!XP_PROPS, Default_print)
enddefine;

define Extern_ptr_hash() with_nargs 1;
    _pint(()!XP_PTR)
enddefine;

constant
    external_ptr_key = struct KEY_R_NAFULL =>> {%
        _NULL,                  ;;; K_GC_RELOC
        key_key,                ;;; KEY
        _:M_K_SPECIAL_RECORD    ;;; K_FLAGS
            _biset _:M_K_NONWRITEABLE
            _biset _:M_K_NO_FULL_FROM_PTR
            _biset _:M_K_COPY
            _biset _:M_K_EXTERN_PTR
            _biset _:M_K_EXTERN_PTR_PROPS,
        _:GCTYPE_NFULLREC,      ;;; K_GC_TYPE
        Rec2_getsize,           ;;; K_GET_SIZE

        "external_ptr",         ;;; K_DATAWORD
        false,                  ;;; K_SPEC
        isexternal_ptr,         ;;; K_RECOGNISER
        WREF Exec_nonpd,        ;;; K_APPLY
        Eq__Extern_ptr,         ;;; K_SYS_EQUALS
        WREF Eq__Extern_ptr,    ;;; K_EQUALS
        Extern_ptr_print,       ;;; K_SYS_PRINT
        WREF Extern_ptr_print,  ;;; K_PRINT
        WREF Extern_ptr_hash,   ;;; K_HASH

        _:NUMTYPE_NON_NUMBER,   ;;; K_NUMBER_TYPE
        _:PROLOG_TYPE_OTHER,    ;;; K_PLOG_TYPE
        _:EXTERN_TYPE_DEREF,    ;;; K_EXTERN_TYPE
        _0,                     ;;; K_SPARE_BYTE

        @@(struct EXTERNAL_PTR)++,
                                ;;; K_RECSIZE_R
        false,                  ;;; K_CONS_R
        false,                  ;;; K_DEST_R
        false,                  ;;; K_ACCESS_R

        @@(int)[_1],            ;;; K_FULL_OFFS_SIZE
        =>> {% @@XP_PROPS %},   ;;; K_FULL_OFFS_TAB[_1]
        %};

endsection;     /* $-Sys */



;;; --- PRE-LOADED C FUNCTIONS --------------------------------------------
;;; (Needed because Alpha VMS can't external load things from basepop11)

exload extern_ptr
constant
    C_pop_uc_to_mb(ucptr,inlen,bptr,outlen) :int <- pop_uc_to_mb
endexload;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 28 1997
        Added C_pop_uc_to_mb
--- John Gibson, Mar 12 1997
        In is_valid_external_ptr, changed _HILIM for SHARED_LIBRARIES case
        to be _MOST_POSITIVE_UNSIGNED(word) instead of 16:FFFFFFFF (which
        was wrong for 64-bit).
--- John Gibson, Mar 11 1997
        Modified exacc_ntstring to allow decoding/encoding.
--- John Gibson, Sep 10 1996
        Changed CALL_DECLARE macro to include dlocal exit action
        for case of X async polling firing when _in_X_call is true.
--- John Gibson, Aug 23 1995
        Removed EXPTR*_NEEDS_DALIGN stuff
--- John Gibson, Apr  7 1995
        Revised key layout
--- John Gibson, Mar 22 1995
        Added EXPTR*_NEEDS_DALIGN #_IFs
--- John Gibson, Feb 17 1995
        Added Extern$-result_struct
--- John Gibson, Oct 20 1994
        Made all pop registers be local to all procedures that call
        _call_external -- see NOTE.
--- Robert John Duncan, Sep  5 1994
        Reorganised definition of is_valid_external_ptr to include case for
        Win32 which tests for read access at the address
--- John Gibson, May  6 1994
        Changed exacc_ntstring to use Get*_nt_string
--- John Gibson, Apr 13 1994
        Got rid of deferred actions mechanism inside external calls
        -- replaced with AST procedures blocked inside callback.
--- Simon Nichols, Jun 10 1993
        Changed is_valid_external_ptr to cope with UNIX systems with
        shared libraries (which on both SVR4 and HP-UX are located above
        the process address space) and/or a call stack which grows upwards
        (e.g. HP9000/700).
--- John Gibson, Dec 18 1992
        Moved initialisations of _external_flags and _in_X_call to initial.p
        (since they're referred to in asignals.s)
--- John Gibson, Sep 15 1992
        CHanged Xt$-X_apply to pass (d)decimals as singles
--- John Gibson, Sep  2 1992
        Added M_K_NO_FULL_FROM_PTR to key flags
--- John Gibson, Aug 19 1992
        Changed the names of the Xpt- things
--- John Gibson, Aug  6 1992
        Changed external call procedure to take fltsingle arg
--- John Gibson, May 24 1992
        Exported external_ptr_key
--- John Gibson, Jul 13 1991
        Added setting of _:PEF_ASYNC_RETURN_ABEXIT in X_cb_apply
--- John Gibson, Jul  9 1991
        PEF flags now pop not nonpop
--- John Gibson, Jul  6 1991
        Added is_null_external_ptr and null_external_ptr
--- Roger Evans, Jul  2 1991 changed CALL_DLOCALS to CALL_DECLARE
--- Roger Evans, Jul  1 1991 now initialises _in_X_Call (no-one else does?)
--- Roger Evans, Jul  1 1991 added X deferred actions code
--- John Gibson, Mar 14 1991
        Sys$-Xt$-DisableXAsync nnow Sys$- _in_X_call
--- John Gibson, Jan 19 1991
        Added Xt$-Xt_cb_apply
--- John Gibson, Jan 16 1991
        Changed E*xternal_apply to Xt$-X_apply
--- John Gibson, Nov 13 1990
        _call_external now doesn't return result structure, so procedures
        calling it have to return it explicitly; also _m*alloc_use_external
        replaced by _external_flags
--- John Gibson, Sep  2 1990
        Changed -exacc_ntstring- so that it returns <termin> when applied
        to a NULL pointer.
--- Roger Evans, Jul  5 1990
        Added Sys_external_ptr
--- John Gibson, Jun  3 1990
        Added Call_extern procedures
--- John Gibson, May 11 1990
        Made external_NULL be a procedure returning NULL (screws up
        too many things otherwise).
--- John Gibson, May  3 1990
        Added -fill_external_ptr- and -external_NULL-
--- John Gibson, Apr 27 1990
        Added -is_valid_external_ptr-
 */
