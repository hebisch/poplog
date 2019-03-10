/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/conskey.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *KEYS
 */

;;;------------------- CONSTRUCTING NEW KEYS ---------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'fields.ph'
#_INCLUDE 'gctypes.ph'

constant
        procedure Sys$-New_lextoken
    ;

section $-Sys$-Vm;

constant
        procedure (Cons_procedure, Newlab, I_UNWIND_SF, I_RETURN,
        I_PUSH_UINT, I_CALLSUB, I_CALLABS, I_POP,
        I_MOVE, I_MOVEQ
        )
    ;

endsection;


;;; -----------------------------------------------------------------------

section $-Sys$-Fld => conskey, cons_access, field_spec_info;


    ;;; assoc list of procedures for getting the size of vectors
lconstant
    vector_getsize_pdr = list_assoc_val(%[
        ^t_BIT      ^Bitvec_getsize
        ^t_BYTE     ^Bytevec_getsize
        ^t_SHORT    ^Shortvec_getsize
        ^t_INT      ^Intvec_getsize
        ^t_DOUBLE   ^Doublevec_getsize
        ]%);


;;; --- GENERAL ---------------------------------------------------------

    /*  Interface to Cons_procedure
    */
define lconstant Cons_p(_nargs, locals, nonpop_locals, codelist);
    lvars codelist, locals, nonpop_locals, exitlab, _nargs;

    unless locals then
        [] -> locals
    elseunless islist(locals) then
        locals :: [] -> locals
    endunless;
    unless nonpop_locals then
        [] -> nonpop_locals
    elseunless islist(nonpop_locals) then
        nonpop_locals :: [] -> nonpop_locals
    endunless;

    VM Newlab([% INST0(I_UNWIND_SF), INST0(I_RETURN) %]) -> exitlab;
    VM Cons_procedure(codelist nc_<> exitlab,
                    [], _nargs, false, locals, nonpop_locals,
                    [], exitlab, New_lextoken(), [], [], _0)
enddefine;

    /*  Make a new key structure
    */
define lconstant Make_key(datawd, spec, eqpdr, _size, _flags, _gctype) -> key;
    lvars key, datawd, spec, eqpdr, _size, _flags, _gctype, _fo_size;

    ;;; construct the recogniser procedure for a structure
    define lconstant Cons_krecogniser(key);
        lvars key;
        Cons_p(1, false, false,
                [% INST1(I_MOVEQ, key), INST1(I_CALLSUB, _haskey) %])
    enddefine;

    if _gctype == _:GCTYPE_USERNFREC then
        ;;; K_FULL_OFFS_SIZE is passed as extra arg
        () -> _fo_size
    endif;

    Get_store(_size) -> key;            ;;; get space for key
    ;;; make the key safe for garbage collection by zeroing everything first
    _fill(_0, _size, key@POPBASE);
    key_key -> key!KEY;
    _flags _biset _:M_K_COPY -> key!K_FLAGS;    ;;; set flags
    _gctype -> key!K_GC_TYPE;           ;;; garbage collector type
    if _gctype == _:GCTYPE_USERNFREC then
        ;;; key is not safe for GC until this has been set (because it's used
        ;;; to compute the size of the key by Key_getsize)
        _fo_size -> key!K_FULL_OFFS_SIZE
    endif;
    datawd  -> key!K_DATAWORD;          ;;; insert dataword
    spec    -> key!K_SPEC;              ;;; insert spec
    _:NUMTYPE_NON_NUMBER    -> key!K_NUMBER_TYPE;   ;;; not a number key
    _:PROLOG_TYPE_OTHER     -> key!K_PLOG_TYPE;     ;;; not a standard prolog type
    _:EXTERN_TYPE_NORMAL    -> key!K_EXTERN_TYPE;   ;;; normal external treatment
    Cons_krecogniser(key)   -> key!K_RECOGNISER;
    eqpdr                   -> key!K_SYS_EQUALS;
    Data_print              -> key!K_SYS_PRINT;
    ;;; the APPLY, PRINT, EQUALS and HASH fields contain references which
    ;;; then contain the procedure - this is so the built-in keys can be
    ;;; non-writeable and shareable, by making only the refs writeable
    ;;; these fields can be updated in all keys
    consref(eqpdr)      -> key!K_EQUALS;
    consref(Exec_nonpd) -> key!K_APPLY;
    consref(Data_print) -> key!K_PRINT;
    consref(if _flags _bitst _:M_K_RECORD then
                Record_hash
            else
                Vector_hash
            endif) -> key!K_HASH
enddefine;

define lconstant Want_p(want_p, fdesc) -> (want_p, no_upd);
    lvars want_p, fdesc, no_upd;
    if isref(want_p) ->> no_upd then fast_cont(want_p) -> want_p endif;
    unless no_upd then
        fsv(FD_FLAGS,fdesc) &&=_0 M_FD_CAN_UPDATE -> no_upd
    endunless
enddefine;


;;; --- RECORDS ------------------------------------------------------------

    /*  Construct a single access procedure
    */
define lconstant Cons_acc(fdesc, want_p, key) -> accp;
    lvars   accp, fdesc, key, rec_id, fdesc, want_p, no_upd;

    returnunless(Want_p(want_p, fdesc) -> no_upd) (false -> accp);

    Cons_p(1, false, false, [%
            Arg_check_instrs(key, fdesc, false),
            Field_instrs(fdesc, false, false, false)
        %]) -> accp;

    returnif(no_upd);

    ;;; want updater procedure
    if Field_update_convert(fdesc) then
        ;;; avoids Field_instrs having to use I_SWAP
        New_lextoken()
    else
        false
    endif -> rec_id;
    Cons_p(2, rec_id, false, [%
                Arg_check_instrs(key, fdesc, false),
                if rec_id then INST1(I_POP, rec_id) endif,
                Field_instrs(fdesc, true, rec_id, false)
         %]) -> accp!PD_UPDATER
enddefine;      /* Cons_acc */

    /*  Construct a vector of record access procedures
    */
define lconstant Cons_rec_access_vec(fdesclist, want_p_spec, key);
    lvars   fdesc, key, rec_id, fdesclist, want_p_spec, want_p;

    {%  fast_for fdesc in fdesclist do
            if want_p_spec == [] then
                true
            else
                dest(want_p_spec) -> want_p_spec
            endif -> want_p;
            Cons_acc(fdesc, want_p, key)
        endfor
    %}
enddefine;      /* Cons_rec_access_vec */


    /*  Construct a record-type key
    */
define lconstant Cons_rkey(datawd, speclist, _flags) -> key;
    lvars   key, fdesc, fdesclist, speclist, datawd,
            _table, _flags, _num_full, _rec_size, _exptr_at_ptr;

    lconstant key_fdesc =
                consvector(FULL, t_WORD, WORD_BITS, _pint(@@(1){@@KEY|w}),
                                [], [], M_FD_CAN_UPDATE, false, FD_VEC_LEN);

    ;;; construct a record constructor procedure
    define lconstant Cons_krcons(fdesclist, key);
        lvars   fdesc, fdesclist, key, np_rec_id, rec_id, result_id, fd,
                list, convp;

        New_lextoken() ->> np_rec_id -> result_id;
        false -> rec_id;

        [%  INST1(I_PUSH_UINT, _pint(key!K_RECSIZE_R)),
            INST1(I_CALLABS, if key!K_FLAGS _bitst _:M_K_DOUBLE_ALIGN then
                                 Get_store_dalign
                             else
                                Get_store
                             endif),
            INST1(I_POP, result_id),
            INST1(I_MOVEQ, key);
            Field_instrs(key_fdesc, true, result_id, false);
            rev(fdesclist) -> fdesclist;
            fast_for list on fdesclist do
                hd(list) -> fdesc;
                if fsv(FD_CONV_P,fdesc) /== [] and not(rec_id) then
                    ;;; uses conversion procedure(s) -- must make safe any full
                    ;;; fields not yet initialised and move rec to pop id
                    New_lextoken() -> rec_id;
                    fast_for fd in list do
                        nextif(fsv(FD_VAL_SPEC,fd) /== FULL);
                        ;;; temporarily remove any conversion procedures
                        ;;; to initialise the field
                        fsv(FD_CONV_P,fd) -> convp;
                        [] -> fsv(FD_CONV_P,fd);
                        ;;; use the pop id itself to init the field
                        INST1(I_MOVE, rec_id);
                        Field_instrs(fd, true, result_id, false);
                        ;;; replace conversion procedures
                        convp -> fsv(FD_CONV_P,fd)
                    endfor;
                    INST2(I_MOVE, result_id, rec_id ->> result_id);
                endif;
                Field_instrs(fdesc, true, result_id, false)
            endfor,
            INST1(I_MOVE, result_id)
        %] -> list;
        Cons_p(listlength(fdesclist), rec_id, np_rec_id, list)
    enddefine;

    ;;; construct a record destructor procedure
    define lconstant Cons_krdest(fdesclist, key);
        lvars fdesc, fdesclist, key, rec_id = New_lextoken();
        Cons_p(1, rec_id, false, [%
                    Arg_check_instrs(key, key_fdesc, false),
                    INST1(I_POP, rec_id),
                    fast_for fdesc in fdesclist do
                        Field_instrs(fdesc, false, rec_id, false)
                    endfor
                 %])
    enddefine;


    ;;; Convert speclist to include type, size and offset for each field,
    ;;; count full fields, and get word offset size of record.
    ;;; Also say whether to allow byte access to record.
    [% Convert_rec_speclist(speclist, false, false) _biset _flags
        -> _flags -> _exptr_at_ptr -> _num_full -> _rec_size %] -> fdesclist;

    _flags _biset _:M_K_RECORD -> _flags;           ;;; flags for key

    if _flags _bitst _:M_K_EXTERN_PTR then
        unless _exptr_at_ptr then
            mishap(speclist, 1, 'conskey: INVALID RECORD SPEC FOR EXTERNAL POINTER-CLASS')
        elseif hd(speclist) == FULL then
            _flags _biset _:M_K_EXTERN_PTR_PROPS -> _flags
        endunless
    elseif _flags _bitst _:M_K_NO_FULL_FROM_PTR then
        ;;; allow byte access except for external ptrs
        _flags _biset _:M_K_BYTE_ACCESS -> _flags
    endif;

    if @@(w)[_num_full _add _1] == _rec_size then   ;;; +1 for key
        ;;; all contiguous full fields
        _flags _biclear (_:M_K_NO_FULL_FROM_PTR _biset _:M_K_BYTE_ACCESS)
                                    -> _flags;
        Make_key(datawd, speclist, Eq__Fullrec, @@(struct KEY_R)++,
                    _flags, _:GCTYPE_USERREC) -> key;
    else
        ;;; not all full - need table of full field offsets
        ;;; Make_key takes the K_FULL_OFFS_SIZE value as an extra 1st arg
        ;;; for _:GCTYPE_USERNFREC
        Make_key(@@(int)[_num_full],
                    datawd, speclist, Eq__Record,
                    @@(w){@@K_FULL_OFFS_TAB[_num_full] | int.r} _sub @@POPBASE,
                            _flags, _:GCTYPE_USERNFREC) -> key;
        ;;; construct full field offset table (doesn't include the key field)
        key@K_FULL_OFFS_TAB[_0] -> _table;
        fast_for fdesc in fdesclist do
            if fsv(FD_VAL_SPEC,fdesc) == FULL then
                ;;; insert (signed) offset
                @@(w){_int(fsv(FD_BIT_OFFS,fdesc)) | 1.r}
                                                -> _table!(-int)++ -> _table
            endif
        endfor
    endif;

    _rec_size -> key!K_RECSIZE_R;           ;;; record size as word offset
    Record_getsize -> key!K_GET_SIZE;
    Cons_krcons(fdesclist, key) -> key!K_CONS_R;
    Cons_krdest(fdesclist, key) -> key!K_DEST_R;
    Cons_rec_access_vec(fdesclist, [], key) -> key!K_ACCESS_R
enddefine;


;;; --- VECTORS ------------------------------------------------------------

    /*  Construct a vector subscr procedure
    */
define lconstant Cons_subscr(fdesc, vsubs, want_p, key) -> subpdr;
    lvars   subpdr, spec, key, vec_id, subs_id, fdesc, want_p,
            vsubs, no_upd;

    ;;; generate subscripting code for a subscr procedure
    define lconstant Get_subacc(upd);
        lvars upd;
        [%  Arg_check_instrs(key, fdesc, vsubs);
            if vec_id then
                INST1(I_POP, vec_id),   ;;; pop args
                INST1(I_POP, subs_id)
            endif;
            Field_instrs(fdesc, upd, vec_id, subs_id)
        %]
    enddefine;

    returnunless(Want_p(want_p, fdesc) -> no_upd) (false -> subpdr);

    false ->> vec_id -> subs_id;
    Cons_p(2, false, false, Get_subacc(false)) -> subpdr;

    returnif(no_upd);

    ;;; updater wanted
    if Field_update_convert(fdesc) then
        ;;; avoids Field_instrs having to use I_SWAP
        New_lextoken() -> vec_id, New_lextoken() -> subs_id
    endif;
    Cons_p(3, vec_id, subs_id, Get_subacc(true)) -> subpdr!PD_UPDATER
enddefine;      /* Cons_subscr */

    /*  Construct a vector-type key
    */
define lconstant Cons_vkey(datawd, spec, _flags) -> key;
    lvars key, spec, datawd, basetype, org_spec = spec, fdesc, _flags;

    define lconstant Cons_icd(full_p, fullconv_p, nfull_p);
        lvars full_p, fullconv_p, nfull_p;
        Cons_p(1, false, false, [%
                    INST1(I_MOVEQ, key),
                    INST1(I_CALLABS, if spec /== FULL then
                                        nfull_p
                                     elseif fsv(FD_CONV_P,spec) /== [] then
                                        fullconv_p
                                     else
                                        full_p
                                     endif)
                 %])
    enddefine;

    ;;; get descriptor for field
    Convert_vec_spec(conspair(spec,false), false, false) -> -> fdesc;
    fsv(FD_VAL_SPEC,fdesc) -> spec;
    fsv(FD_TYPE,fdesc) fi_&& t_BASE_TYPE -> basetype;

    ;;; produce key structure
    _flags _biset _:M_K_VECTOR -> _flags;
    if spec == FULL then
        _flags _biset _:M_K_FULL_VECTOR -> _flags;
        Make_key(datawd, org_spec, Eq__Fullvec, @@(struct KEY_V)++,
                                                _flags, _:GCTYPE_USERVEC)
    else
        _flags _biset (_:M_K_NO_FULL_FROM_PTR _biset _:M_K_BYTE_ACCESS)
                                            -> _flags;
        if basetype == t_DOUBLE then
            ;;; whole record must be doubleword aligned (if flag /= 0)
            _flags _biset _:M_K_DOUBLE_ALIGN -> _flags
        endif;
        Make_key(datawd, org_spec, Eq__Nfullvec, @@(struct KEY_V)++,
                                                _flags, _:GCTYPE_USERNFVEC)
    endif -> key;

    ;;; insert key field size value
    _int(if basetype == t_BIT then
            -fsv(FD_BIT_SIZE,fdesc)
         else
            basetype
         endif) -> key!K_FIELD_CODE_V;

    ;;; construct procedures
    vector_getsize_pdr(basetype)                -> key!K_GET_SIZE;
    Vector_apply                                -> fast_cont(key!K_APPLY);
    Cons_icd(dup(Init_uservec), Init_usernfvec) -> key!K_INIT_V;
    Cons_icd(Cons_uservec, dup(Cons_usernfvec)) -> key!K_CONS_V;
    Cons_icd(Dest_uservec, dup(Dest_usernfvec)) -> key!K_DEST_V;
    Cons_subscr(fdesc, true, true, key)         -> key!K_SUBSCR_V;
    Cons_subscr(fdesc, true, true, false)       -> key!K_FAST_SUBSCR_V
enddefine;


;;; --- EXTERNAL CALL -----------------------------------------------------

define lconstant Cons_extern_call(fdesc, want_p_spec, checking);
    lvars n, fdesc, want_p_spec, checking;
    returnunless(want_p_spec) (false);
    if fast_front(fsv(FD_EXTERN,fdesc)) ->> n then
        n fi_+ 1    ;;; +1 for the external ptr
    else
        2           ;;; nargs and external ptr
    endif -> n;
    Cons_p(n, false, false,
            [%  Arg_check_instrs(checking, fdesc, false),
                Field_instrs(fdesc, false, false, false)
            %])
enddefine;


;;; --- INTERFACE PROCEDURES -----------------------------------------------

define conskey(datawd, spec) -> key;
    lvars   spec, datawd, flagvec, key, n, attr, _isrec, _flags = _0,
            _extn_deref = false;

    if isvector(spec) then
        ;;; optional flags vector
        spec -> flagvec;
        datawd -> spec;
        -> datawd
    else
        {} -> flagvec
    endif;
    Check_word(datawd);
    islist(spec) -> _isrec;

    fast_for n to datalength(flagvec) do
        fsv(n, flagvec) -> attr;
        if attr == "writeable" then
            _flags _biset _:M_K_WRITEABLE -> _flags
        elseif attr == "nonwriteable" then
            _flags _biset _:M_K_NONWRITEABLE -> _flags
        elseif attr == "external_ptr" or attr == "external_deref" then
            unless _isrec then
                mishap(attr, 1, 'conskey: INVALID KEY ATTRIBUTE FOR VECTORCLASS')
            endunless;
            true -> _extn_deref;
            if attr == "external_ptr" then
                _flags _biset _:M_K_EXTERN_PTR -> _flags
            endif;
        else
            mishap(attr, 1, 'conskey: INVALID KEY ATTRIBUTE')
        endif
    endfor;

    if _isrec then
        ;;; record type
        Cons_rkey(datawd, spec, _flags)
    else
        ;;; vector type
        Cons_vkey(datawd, spec, _flags)
    endif -> key;

    if _extn_deref then _:EXTERN_TYPE_DEREF -> key!K_EXTERN_TYPE endif;

    if vm_pas_mode == "popc" then valof("popc_conskey")(key) endif
enddefine;

define cons_access(want_p_spec, spec, checking, exptr);
    lvars spec, want_p_spec, checking, exptr;
    if isvector(spec) then
        ;;; external function call (exptr must be 1)
        Cons_extern_call(Convert_func_spec(spec,exptr), want_p_spec, checking)

    elseif islist(spec) then
        ;;; spec is a list of structure fields -- generate access procedure
        ;;; for each field
        Cons_rec_access_vec(
                [% Convert_rec_speclist(spec,exptr,false) -> (,,,) %],
                                                want_p_spec, checking)

    elseif ispair(spec) then
        ;;; spec is an array spec (i.e. a pair with false or fixed length
        ;;; in the back) -- generate subscriptor into array
        Cons_subscr(Convert_vec_spec(spec,exptr,false), want_p_spec, checking)

    else
        ;;; simple type access
        Cons_acc(Convert_simp_spec(spec, exptr), want_p_spec, checking)

    endif;

    if vm_pas_mode == "popc" then
        valof("popc_cons_access")(dup(), spec, checking, exptr)
    endif
enddefine;

    /*  Return the base type bitsize of spec and its value spec (or
        false if there are access/conversion procedures)
    */
define field_spec_info(spec) -> (spec, n);
    lvars fdesc, n, spec;

    _CLAWBACK_SAVE;

    Convert_simp_spec(spec, 1) -> fdesc;

    if vm_pas_mode == "popc" then
        ;;; struct layouts might be different on the target m/c ....
        valof("popc_field_spec_info")(spec) -> (spec, n)

    else
        if fsv(FD_CONV_P,fdesc) == [] and fsv(FD_ACC_P,fdesc) == [] then
            fsv(FD_VAL_SPEC,fdesc) -> spec;
            if isvector(spec) then fsv(FD_VAL_SPEC,spec) -> spec endif;
            if isprocedure(spec) then spec!PD_PROPS -> spec endif;
            if spec == EXVAL then "exval" -> spec endif
        else
            false -> spec
        endif;
        fsv(FD_BIT_SIZE,fdesc) -> n
    endif;

    Clawback(0) ->
enddefine;


endsection;     /* $-Sys$-Fld */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  4 1996
        Field_instrs now takes additional argument for the vector subscript
        operand.
--- John Gibson, Oct 16 1996
        For keys of GC type _:GCTYPE_USERNFREC, the K_FULL_OFFS_SIZE value
        was not being set before a GC could happen (which caused crashes
        inside GC). Changed Make_key and Cons_rkey to fix this.
--- John Gibson, May 27 1995
        Call to Cons_procedure now requires an additional [] arg
--- John Gibson, Apr  8 1995
        Change to layout of full-field offset table in not-all-full records
--- Robert John Duncan, Oct 13 1994
        Fixed explicit use of VM Cons_inst; replaced it with new INST2
--- John Gibson, Sep 19 1994
        Uses Cons_inst calls for VM instructions instead of vector expressions
--- John Gibson, Aug 16 1993
        Test for Popc now vm_pas_mode == "popc"
--- John Gibson, Jun 14 1993
        Fixed field_spec_info to return "exval" for such a field, and added
        POPC code
--- John Gibson, Sep  2 1992
        Set M_K_NO_FULL_FROM_PTR in key flags when appropriate
--- John Gibson, Aug 21 1992
        Changed Cons_extern_call to allow for FD_EXTERN field being a
        pair rather than a vec for an external func
--- John Gibson, Jul 16 1992
        Added POPC interface to -cons_access-
--- John Gibson, Jan 15 1991
        Added _CLAWBACK_SAVE etc to -field_spec_info-.
--- John Gibson, Oct 15 1990
        Reorganised handling of simple external specs
--- John Gibson, Sep 25 1990
        Check on -exptr- to -cons_access- removed (now done by fields.p
        procedures)
--- John Gibson, Sep 18 1990
        Fixed problem with conversion procedures in -Cons_krcons-.
--- John Gibson, Sep 14 1990
        Field descriptors now contain exptr arg
--- John Gibson, Jul 28 1990
        Added -field_spec_info-
--- John Gibson, Jun  4 1990
        Added -Cons_extern_call-
--- John Gibson, May 29 1990
        Included fields.ph
--- John Gibson, Mar 20 1990
        Overhauled for external pointer support, added -cons_access-.
--- John Gibson, Dec  8 1989
        Changes for new pop pointers.
--- John Gibson, Jun 13 1989
        Allowed -conskey- to take optional vector arg to specify flags;
        now allows "writeable" and "nonwriteable" as well as "external".
--- John Gibson, May 15 1989
        Removed vmdefs.ph
--- John Gibson, Apr 30 1989
        VM procedures now in $-Sys$-Vm.
--- John Gibson, Mar 15 1989
        Made all run-time procedures perm constants for POPC, and
        moved to other files.
--- John Gibson, Mar 12 1989
        Added call to -popc_conskey- in -conskey-.
--- Roger Evans, Oct 10 1988
        Added -ext- arg (false) to Field_instrs calls
--- John Gibson, Aug  6 1988
        Added support for doubleword alignment of records/vectors
        containing ddecimal fields.
--- John Gibson, Jul 26 1988
        Added [] for new arg to -Cons_procedure-
--- Roger Evans, Jun 28 1988
        Changed optional conskey flag to be a list of keywords
--- Roger Evans, Jun  8 1988
        Added optional flag to conskey for making external classes
--- John Gibson, Mar 27 1988
        -list_assoc_val- into section Sys
--- John Gibson, Feb 29 1988
        Sectionised.
--- John Gibson, Feb 22 1988
        Replaced -Check_subscr- with private procedure
        Moved -Bytevec_getsize- to string.p
 */
