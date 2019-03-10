/* --- Copyright University of Sussex 2005. All rights reserved. ----------
 > File:            C.all/src/fields.p
 > Purpose:
 > Author:          John Gibson & John Williams (see revisions)
 */

;;;------------------- STRUCTURE FIELD MANIPULATION --------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'numbers.ph'
#_INCLUDE 'fields.ph'

constant
        procedure (consexternal_ptr),
    ;

section $-Sys$-Vm;

constant
        procedure (I_PUSH_FIELD, I_POP_FIELD, I_PUSH_FIELD_ADDR,
        I_PUSH_UINT, I_SWAP, I_CALLABS, I_CALLPQ, I_MOVE, I_MOVEQ, I_MOVENUM,
        I_ERASE, I_MAKE_EXPTR)
    ;

endsection;


;;; ----------------------------------------------------------------------

section $-Sys$-Fld;


#_IF WORD_BITS /== DOUBLE_BITS

lconstant macro (
    longlong_SPEC   = [BGWEAK Double_val_s],
    ulonglong_SPEC  = [BGWEAK Double_val_u],
    );

#_ELSE

lconstant macro (
    longlong_SPEC   = -DOUBLE_BITS,
    ulonglong_SPEC  = DOUBLE_BITS,
    );

#_ENDIF


lconstant
    named_field_specs = [
    ;;;  name           internal spec         type

        dfloat       {% FLWEAK Float_val_d, t_DOUBLE %}
        sfloat       {% FLWEAK Float_val_s, t_INT    %}
        ;;; this one is the same as "sfloat" except that as a function result
        ;;; it gives a C-style "float" result
        float        {% FLWEAK Float_val_s, t_INT    %}

        exptr        {% EXPTR,              t_WORD   %}
        exval        {% EXVAL,              t_WORD   %}
        full         {% FULL,               t_WORD   %}
        word         {% -WORD_BITS,         t_WORD   %}
        uword        {% WORD_BITS,          t_WORD   %}
        pint         {% -(POPINT_BITS+1),   t_WORD   %}
        longlong     {% longlong_SPEC,      t_DOUBLE %}
        ulonglong    {% ulonglong_SPEC,     t_DOUBLE %}
        long         {% -LONG_BITS,         t_LONG   %}
        ulong        {% LONG_BITS,          t_LONG   %}
        int          {% -INT_BITS,          t_INT    %}
        uint         {% INT_BITS,           t_INT    %}
        short        {% -SHORT_BITS,        t_SHORT  %}
        ushort       {% SHORT_BITS,         t_SHORT  %}
        sbyte        {% -BYTE_BITS,         t_BYTE   %}
        byte         {% BYTE_BITS,          t_BYTE   %}

        ;;; these two remain for upward-compatibility and are the same as
        ;;; "dfloat" and "float" respectively
        ddecimal     {% FLWEAK Float_val_d, t_DOUBLE %}
        decimal      {% FLWEAK Float_val_s, t_INT    %}
        ],

    type_sizes = [
        ^t_DOUBLE   {% DOUBLE_BITS, STRUCT_DOUBLE_ALIGN_BITS %}
        ^t_INT      {% INT_BITS,    STRUCT_INT_ALIGN_BITS %}
        ^t_SHORT    {% SHORT_BITS,  STRUCT_SHORT_ALIGN_BITS %}
        ^t_BYTE     {% BYTE_BITS,   BYTE_BITS %}
        ^t_BIT      {% 1,           1 %}
        ];

lconstant pop_invalid = 'INVALID FIELD SPEC FOR RECORDCLASS/VECTORCLASS';

    /*  Round offset to a multiple of mult bits
    */
define lconstant round_mult(offset, mult);
    lvars offset, mult;
    if ((offset fi_+ mult fi_- 1) fi_// mult -> offset) fi_< 0 then
        offset fi_- 1 -> offset
    endif;
    offset fi_* mult
enddefine;

define :inline lconstant ADJUST_EXVAL_SPEC(fdesc);
#_IF DEF BIG_ENDIAN \n
    if fsv(FD_TYPE,fdesc) fi_&& t_BASE_TYPE fi_< t_WORD then
        fsv(FD_BIT_OFFS,fdesc) fi_+ WORD_BITS fi_- fsv(FD_BIT_SIZE,fdesc)
                    -> fsv(FD_BIT_OFFS,fdesc)
    endif
#_ENDIF
enddefine;


    /*  Given a field spec (e.g. for conskey), return a descriptor
        vec containing its size in bits, its offset in bits (given the
        current offset in the record), and an integer access type.
    */
lvars nested = false;

define lconstant Spec_fdesc(spec, exptr, bitoffs, on_ptr);
    lvars   spec, bitoffs, bitsize, bitalign, type, vec, exptr,
            repcount, conv_p, flags, n, fdesc, tfdesc, valspec, on_ptr,
            tspec;

    define lconstant check_ptr_val(spec, valspec, fdesc, acc_p, isptrval);
        lvars spec, fdesc, acc_p, valspec, isptrval;
        unless exptr then mishap(spec, 1, pop_invalid) endunless;
        if isprocedure(valspec) then valspec!PD_PROPS -> valspec endif;
        unless (valspec == EXPTR
                or (valspec == EXVAL and isptrval)
                or (ispair(valspec) and acc_p))
        and fsv(FD_CONV_P,fdesc) == []
        and (acc_p or fsv(FD_ACC_P,fdesc) == []) then
            mishap(spec, 1, 'EXTERNAL FIELD ACCESS SPEC INVALID FOR FIELD TYPE')
        endunless
    enddefine;

    if isclosure(spec) and datalength(spec) == 2 then
        ;;; pdpart is conversion procedure, frozval(1) is spec,
        ;;; frozval(2) is true if conversion procedure or false if
        ;;; access procedure
        M_FD_CAN_UPDATE -> flags;       ;;; means can have an updater
        pdpart(spec) -> conv_p;
        Spec_fdesc(fast_frozval(1, spec), exptr, bitoffs, on_ptr) -> fdesc;
        if isvector(fsv(FD_VAL_SPEC,fdesc) ->> valspec) then
            fsv(FD_VAL_SPEC,valspec) -> valspec
        endif;
        if isclosure(conv_p) and not(isundef(conv_p))
        and datalength(conv_p) == 2 and pdpart(conv_p) == identfn
        and fast_frozval(1,conv_p) == ":" then
            ;;; implicit access -- frozval(2) has spec
            fast_frozval(2,conv_p) -> tspec;
            check_ptr_val(spec, valspec, fdesc, false, isref(tspec));
            if isvector(fsv(FD_VAL_SPEC,fdesc) ->> valspec) then
                fsv(FD_DEREF_COUNT,valspec) fi_+ 1
            else
                1
            endif -> n;     ;;; derefs from subspec
            Spec_fdesc(tspec, 1, 0, true) -> tfdesc;
            fsv(FD_CONV_P,tfdesc) -> fsv(FD_CONV_P,fdesc);
            fsv(FD_ACC_P,tfdesc) -> fsv(FD_ACC_P,fdesc);
            fsv(FD_VAL_SPEC,tfdesc) -> valspec;
            if fsv(FD_EXTERN,tfdesc) == 0 then
                n fi_- 1 -> n   ;;; getting pointer value
            endif;
            if n == 0 then
                ;;; resultant not indirect at all
                fsv(FD_TYPE,tfdesc) -> fsv(FD_TYPE,fdesc);
                fsv(FD_BIT_SIZE,tfdesc) -> fsv(FD_BIT_SIZE,fdesc);
                fsv(FD_BIT_OFFS,fdesc) fi_+ fsv(FD_BIT_OFFS,tfdesc)
                                -> fsv(FD_BIT_OFFS,fdesc);
                valspec -> tfdesc
            else
                if isvector(valspec) then
                    valspec -> tfdesc;
                    fsv(FD_VAL_SPEC,tfdesc) -> valspec;
                    fsv(FD_DEREF_COUNT,tfdesc) fi_+ n -> n
                endif;
                n -> fsv(FD_DEREF_COUNT,tfdesc);
                if ispair(valspec) then
                    ;;; treat compound valspec as external pointer
                    EXPTR -> valspec;
                    if n == 1 then
                        valspec -> tfdesc
                    else
                        valspec   -> fsv(FD_VAL_SPEC,tfdesc);
                        t_WORD    -> fsv(FD_TYPE,tfdesc);
                        WORD_BITS -> fsv(FD_BIT_SIZE,tfdesc);
                        n fi_- 1 -> fsv(FD_DEREF_COUNT,tfdesc)
                    endif
                endif
            endif;
            tfdesc -> fsv(FD_VAL_SPEC,fdesc)

        elseif conv_p == identfn then
            ;;; just means don't cons new external ptr -- otherwise ignored
            flags fi_|| M_FD_FIXED_EXPTR -> flags
        else
            if frozval(2, spec) then
                FD_CONV_P
            else
                check_ptr_val(spec, valspec, fdesc, true, false);
                FD_ACC_P
            endif -> n;
            fsv(n,fdesc) nc_<> [^conv_p] -> fsv(n,fdesc)
        endif;
        if ispair(valspec) and fsv(FD_ACC_P,fdesc) == [] then
            ;;; can't have updater
            flags fi_&&~~ M_FD_CAN_UPDATE -> flags
        endif;
        flags -> fsv(FD_FLAGS,fdesc);
        return(fdesc)

    elseif isvector(spec) or ispair(spec) then
        ;;; 'compound' field (field value is address)
        unless exptr then mishap(spec, 1, pop_invalid) endunless;
        dlocal nested = true;
        if isvector(spec) then
            ;;; external function -- only allowed through indirection
            if on_ptr then
                ;;; just treat as unsized byte array
                conspair("byte", false) -> spec
            else
                mishap(spec, 1, 'INVALID USE OF EXTERNAL FUNCTION SPEC')
            endif
        endif;
        if islist(spec) then
            ;;; sub struct
            Convert_rec_speclist(spec, exptr fi_|| MODE_FORCE_EXACC, true)
                                        -> (bitalign, bitsize, type);
            if type /== -1 then
                round_mult(bitoffs, bitalign) -> bitoffs
            elseunless on_ptr then
                mishap(spec, 1, 'INVALID EMPTY STRUCTURE SPEC')
            else
                t_WORD -> type
            endif
        else
            ;;; repeated field (i.e. array)
            Spec_fdesc(fast_front(spec), exptr, bitoffs, false) -> fdesc;
            fsv(FD_TYPE,fdesc)      -> type;
            fsv(FD_BIT_SIZE,fdesc)  -> bitsize;
            fsv(FD_BIT_OFFS,fdesc)  -> bitoffs;
            fast_back(spec) -> repcount;
            unless bitsize
            and (not(repcount) or isinteger(repcount) and repcount fi_>= 0)
            then
                mishap(spec, 1, 'INVALID EXTERNAL ARRAY FIELD SPEC')
            endunless;
            if repcount then bitsize fi_* repcount else false endif -> bitsize
        endif;
        ;;; can't have updater (except with access procedures on top)
        return(consvector(spec, type, bitsize, bitoffs, [], [], 0, exptr,
                                                                FD_VEC_LEN))

    elseif isref(spec) then
        ;;; access of external pointer value
        unless exptr and exptr /== 0 then
            mishap(spec, 1, pop_invalid)
        endunless;
        Spec_fdesc(fast_cont(spec), exptr, 0, false) -> fdesc;
        fsv(FD_VAL_SPEC,fdesc) -> valspec;
        if isprocedure(valspec) then valspec!PD_PROPS -> valspec endif;
        unless isinteger(valspec) or valspec == "decimal"
        or valspec == FULL then
            mishap(spec, 1, 'INVALID POINTER-VALUE SPEC')
        elseunless on_ptr then
            mishap(spec, 1, 'INVALID USE OF POINTER-VALUE SPEC')
        endunless;
        ADJUST_EXVAL_SPEC(fdesc);
        exptr fi_- 1 -> fsv(FD_EXTERN,fdesc);
        return(fdesc)

    elseif list_assoc_val(spec, named_field_specs) ->> vec then
        if isundef(fsv(1,vec)) then
            mishap(spec, 1, 'INVALID FIELD SPECIFIER (floating-point not loaded)')
        endif;

        explode(vec) -> (spec, type);
        explode(list_assoc_val(type, type_sizes)) -> (bitsize, bitalign);
        if spec == FULL then WORD_BITS -> bitalign endif;
        ;;; align offset for field
        round_mult(bitoffs, bitalign) -> bitoffs

    elseif isinteger(spec) and spec /== 0
    and #_< -WORD_BITS >_# fi_<= spec and spec fi_<= WORD_BITS then
        if spec fi_< 0 then 0 fi_- spec else spec endif -> bitsize;
        t_BIT -> type

    else
        mishap(spec, 1, 'INVALID FIELD SPECIFIER')
    endif;

    ;;; skip key field if necessary
    lconstant KEY_BITOFFS = _pint(@@(1){@@KEY|w}),
              AFTER_KEY = KEY_BITOFFS+WORD_BITS;
    unless bitoffs fi_+ bitsize fi_<= KEY_BITOFFS
    or AFTER_KEY fi_<= bitoffs then
        AFTER_KEY -> bitoffs            ;;; skip key
    endunless;

    if type == t_BIT and not(nested) then
        ;;; improve field access type if can
        if bitsize == BYTE_BITS and bitoffs fi_rem BYTE_BITS == 0 then
            t_BYTE
        elseif bitsize == SHORT_BITS and bitoffs fi_rem SHORT_ALIGN_BITS == 0 then
            t_SHORT
        elseif bitsize == INT_BITS and bitoffs fi_rem INT_ALIGN_BITS == 0 then
            t_INT
        else
            t_BIT
        endif -> type
    endif;

    if isinteger(spec) and spec fi_< 0 then
        ;;; signed version
        type fi_|| tv_SIGNED -> type
    endif;

    ;;; return descriptor
    consvector(spec, type, bitsize, bitoffs, [], [], M_FD_CAN_UPDATE, exptr,
                                                                FD_VEC_LEN)
enddefine;      /* Spec_fdesc */

define lconstant Decode_exptr(exptr) -> (exptr, descflags);
    lvars exptr, descflags = 0, flags;
    returnunless(exptr);
    Check_integer(exptr, 0);
    (exptr fi_&& MODE_USER_FLAGS) fi_>> 8 -> descflags;
    exptr -> flags;
    exptr fi_&& MODE_LEVEL -> exptr;
    if flags &&=_0 MODE_FORCE_EXACC and exptr == 0 then
        false -> exptr
    ;;; else force external even if 0 level
    endif
enddefine;

define lconstant Set_flags(fdesc, descflags);
    lvars fdesc, descflags, flags = fsv(FD_FLAGS,fdesc);
    if descflags &&/=_0 M_FD_STRUCT_FIXED_EXPTR
    and flags &&=_0 M_FD_CAN_UPDATE then
        flags fi_|| M_FD_FIXED_EXPTR -> flags
    endif;
    if descflags &&/=_0 M_FD_ADDR_MODE then
        flags fi_&&~~ M_FD_CAN_UPDATE -> flags
    endif;
    flags fi_|| descflags -> fsv(FD_FLAGS,fdesc)
enddefine;

define Convert_simp_spec(spec, exptr) -> fdesc;
    lvars spec, exptr, fdesc, descflags;
    Decode_exptr(exptr) -> (exptr, descflags);
    unless exptr then mishap(spec, 1, pop_invalid) endunless;
    Spec_fdesc(spec, exptr, 0, true) -> fdesc;
    Set_flags(fdesc, descflags)
enddefine;

    /*  Spec is an array spec (i.e. a pair with false or fixed length
        in the back)
    */
define Convert_vec_spec(spec, exptr, n) -> vsubs -> fdesc;
    lvars spec, exptr, n, fdesc, vsubs, descflags;
    Decode_exptr(exptr) -> (exptr, descflags);
    fast_destpair(spec) -> vsubs -> spec;
    if isinteger(vsubs) then
        ;;; (ignored for pop case)
        Check_integer(vsubs, 0)
    else
        true -> vsubs
    endif;
    Spec_fdesc(spec, exptr, 0, false) -> fdesc;
    Set_flags(fdesc, descflags);

    unless fsv(FD_BIT_SIZE,fdesc) then
        mishap(spec, 1, 'UNSIZED EXTERNAL FIELD SPEC INVALID FOR SUBSCRIPTOR')
    endunless;
    if n then
        ;;; want n-th element, so not a subscripted access
        ;;; (should really check n <= vsubs if vsubs is an integer)
        Check_integer(n, 1);
        false -> vsubs;     ;;; not a subscripted access
        ;;; compute bit offset to n-th element of vector
        _pint(@@V_BITS) fi_+ (n fi_- 1) fi_* fsv(FD_BIT_SIZE,fdesc)
    else
        ;;; false offset means vector/array spec
        false
    endif -> fsv(FD_BIT_OFFS,fdesc)
enddefine;      /* Convert_vec_spec */


    /*  Work out layout of a record from its speclist, computing
        the bit offset and bitsize of each field.
            If -mode- is an integer then only the details for the mode-th field
        are required; if mode is true, return overall type of struct and
        bitsize; otherwise, return descriptors for each field and
        the other details for the record as a whole.
    */
define Convert_rec_speclist(speclist, exptr, mode);
    lvars   spec, speclist, org_speclist = speclist, type, fdesc, offs,
            n = mode,
            bitoffs,                        ;;; the current offset in bits
            exptr,                          ;;; if external record
            mode,                           ;;; if want mode-th field only
            bitalign,
            descflags,
            struct_type = -1,
            tbitoffs,
            _flags    = _:M_K_NO_FULL_FROM_PTR, ;;; no full initially
            _num_full = _0,                 ;;; the number of full fields
            _exptr_at_ptr = false,
            _offset,
        ;
    lconstant startoffs = _pint(@@(1){@@POPBASE|w});

    Decode_exptr(exptr) -> (exptr, descflags);
    startoffs -> tbitoffs;

    repeat
        if exptr and (null(speclist) or hd(speclist) /== ">->") then
            mishap(speclist, 1, 'EXTERNAL STRUCTURE SPEC MUST START AT POINTER')
        endif;
        startoffs -> bitoffs;

        until null(speclist) or hd(speclist) == "|" do
            dest(speclist) -> (spec, speclist);

            unless bitoffs then
                mishap(org_speclist, 1, 'UNSIZED EXTERNAL ARRAY FIELD NOT LAST IN STRUCT')
            endunless;

            if spec == ">->" then
                ;;; align next field at pointer
                if bitoffs fi_> 0 then
                    mishap(org_speclist, 1, 'INVALID POSITION FOR >-> IN RECORD SPEC')
                else
                    0 -> bitoffs
                endif;
                nextloop
            endif;

            Spec_fdesc(spec, exptr, bitoffs, false) -> fdesc;
            Set_flags(fdesc, descflags);
            fsv(FD_VAL_SPEC,fdesc) -> spec;
            fsv(FD_TYPE,fdesc) fi_&& t_BASE_TYPE -> type;
            fsv(FD_BIT_OFFS,fdesc) -> bitoffs;

            if isinteger(mode) then
                ;;; want details of mode-th field only
                n fi_- 1 -> n;
                returnif(n == 0) (fdesc)
            elseif mode then
                fi_max(type, struct_type) -> struct_type
            else
                if spec == FULL then
                    _num_full _add _1 -> _num_full;         ;;; count it
                    ;;; there are full fields at or after pointer
                    if bitoffs fi_>= 0 then
                        _flags _biclear _:M_K_NO_FULL_FROM_PTR -> _flags
                    endif
                elseif spec == EXPTR and bitoffs == 0 then
                    true -> _exptr_at_ptr
                endif;
                if type == t_DOUBLE then
                    ;;; whole record must be doubleword aligned (if flag /= 0)
                    _flags _biset _:M_K_DOUBLE_ALIGN -> _flags
                endif;

                ;;; return descriptor(s) on stack
                fdesc
            endif;

            ;;; add field size to offset
            if (fsv(FD_BIT_SIZE,fdesc) ->> offs) then
                offs fi_+ bitoffs
            else
                ;;; variable external array
                false
            endif -> bitoffs
        enduntil;

        if tbitoffs and bitoffs then
            fi_max(bitoffs, tbitoffs)
        else
            false
        endif -> tbitoffs;

        quitif(null(speclist));

        ;;; `union'
        unless exptr then
            mishap(0, 'UNIONS NOT ALLOWED IN A RECORDCLASS')
        endunless;
        tl(speclist) -> speclist    ;;; remove |
    endrepeat;

    if isinteger(mode) then
        mishap(mode, org_speclist, 2, 'FIELD NUMBER TOO LARGE FOR RECORD SPEC')
    elseif mode then
        if struct_type /== -1 then
            fsv(2, list_assoc_val(struct_type, type_sizes)) -> bitalign;
            if tbitoffs then round_mult(tbitoffs, bitalign) -> tbitoffs endif
        endif;
        (bitalign, tbitoffs, struct_type)
    else
        ;;; return whole record info, including word offset size of record
        @@(w){_int(tbitoffs) | 1.r} -> _offset; ;;; round to word offset
        if _neg(_offset) then _0 -> _offset endif;
        (_offset _sub @@POPBASE, _num_full, _exptr_at_ptr, _flags)
    endif
enddefine;      /* Convert_rec_speclist */


    /*  Function spec is a 2-element vector.
            spec(1) is the number of args or false, or a pair containing
            that in the front and the fltsingle arg in the back.
            The fltsingle arg is (big)integer in which bit N set
            specifies that a (d)decimal passed for the (N+1)th arg
            should be passed as sfloat rather than (the default) dfloat.

            spec(2) is the result spec, or "void"/false if none.
    */
define Convert_func_spec(spec, exptr) -> fdesc;
    lvars   fdesc, spec, exptr, nargs, descflags, valspec, spec_ok = false,
            fltsingle;

    Decode_exptr(exptr) -> (exptr, descflags);

    unless exptr then
        mishap(spec, 1, pop_invalid)
    elseunless exptr == 1 then
        mishap(exptr, 1, 'NUMBER OF LEVELS MUST BE 1 FOR EXTERNAL FUNCTION CALL')
    endunless;

    if datalength(spec) == 2 then
        fsv(1,spec) -> nargs;
        if ispair(nargs) then
            destpair(nargs)
        else
            nargs, 0    ;;; default fltsingle to 0
        endif -> (nargs, fltsingle);
        if not(nargs) or (isinteger(nargs) and nargs fi_>= 0) then
            true -> spec_ok;
            ;;; check fltsingle within range of signed machine word and then
            ;;; convert back to unsigned popint
            Uint_->_pint(Pint_->_sint(fltsingle, _MOST_POSITIVE_SIGNED(w)))
                                        -> fltsingle
        endif;
    endif;
    unless spec_ok then
        mishap(spec, 1, 'INVALID EXTERNAL FUNCTION SPECIFIER')
    endunless;

    conspair(nargs, fltsingle) -> exptr;

    ;;; result spec
    fsv(2,spec) -> spec;
    if spec and spec /== "void" then
        Spec_fdesc(spec, 1, 0, false) -> fdesc;

        ;;; modify descriptor for access thru pointer returned by
        ;;; _call_external to result struct (possible float result in
        ;;; first 2 words, possible word result in 3rd).

        if isprocedure(fsv(FD_VAL_SPEC,fdesc) ->> valspec) then
            valspec!PD_PROPS -> valspec
        endif;

        if valspec == "decimal" then
            ;;; single floating-point result
   #_IF DEF ALPHA
               ;;; all Alpha float results are in double form
               FLWEAK Float_val_s_C -> fsv(FD_VAL_SPEC,fdesc)
   #_ELSEIF DEF LINUX and DEF PC
               ;;; Inserted by A.S. 6 Jan 2005
               ;;; all PC linux float results are in double form
               FLWEAK Float_val_s_C -> fsv(FD_VAL_SPEC,fdesc)
   #_ELSE
               unless spec == "sfloat" then
                   ;;; Single result returned as double on some systems
                   ;;; (e.g. C "float" result)
                   FLWEAK Float_val_s_C -> fsv(FD_VAL_SPEC,fdesc)
               endunless;
   #_ENDIF
#_IF DEF MIPS and DEF BIG_ENDIAN
            ;;; MIPS architecture puts the single float in the
            ;;; least-significant word of the double result (even
            ;;; on big-endian machines) so adjust the offset appropriately
            _pint(@@(1)[_1|sfloat]) -> fsv(FD_BIT_OFFS, fdesc);
#_ENDIF
        elseif ispair(valspec) then
            mishap(spec, 1, 'EXTERNAL FUNCTION RESULT SPEC CANNOT BE COMPOUND TYPE')
        elseunless valspec == "ddecimal" then
            ;;; integer/pointer result
            _pint(@@(1)[_1|dfloat]) -> fsv(FD_BIT_OFFS,fdesc);
            ADJUST_EXVAL_SPEC(fdesc)
        endif
    else
        ;;; no result -- fdesc just carries FD_EXTERN and false FD_VAL_SPEC
        initv(FD_VEC_LEN) -> fdesc;
        false -> fsv(FD_VAL_SPEC,fdesc);
        0 -> fsv(FD_FLAGS,fdesc)
    endif;

    Set_flags(fdesc, descflags);
    exptr -> fsv(FD_EXTERN,fdesc)   ;;; pair flags as extern function call
enddefine;

    /*  Return val spec if Field_instrs would have to use I_SWAP to
        update a field
    */
define Field_update_convert(fdesc);
    lvars spec = fsv(FD_VAL_SPEC,fdesc), fdesc;
    if isvector(spec) then fsv(FD_VAL_SPEC,spec) -> spec endif;
    if fsv(FD_CONV_P,fdesc) /== []
    or isinteger(spec)
    or spec == EXPTR and fsv(FD_ACC_P,fdesc) == [] then
        spec
    else
        false
    endif
enddefine;


    /*  Generating instructions to access/update a field
    */
define Field_instrs(fdesc, upd, opnd, subs_opnd);
    lvars   size, spec, type, offset, opnd, instr, exptr, upd,
            p, fdesc, conv_p, acc_p, valspec, n, flags, isextn,
            addr_mode, org_ptr, subs_opnd;

    define lconstant call_p_instr(p, upd);
        lvars p, upd;
        if upd then
            if updater(p) ->> upd then
                upd -> p
            else
                mishap(p, 1, 'FIELD ACCESS/CONVERSION PROCEDURE HAS NO UPDATER')
            endif
        endif;
        returnif(p == identfn or p = #_< identfn(%%) >_#);
        VM Cons_inst(if p >=@(w) _system_end then
                        VM I_CALLPQ
                     else
                        VM I_CALLABS
                     endif, p, 2)
    enddefine;

    ;;; call conversion and/or check-and-return procedure(s)
    define lconstant upd_convert(spec);
        lvars spec, p, do_swap = false;
        unless opnd and (offset or subs_opnd) then
            ;;; struct and/or subscript on top of stack
            ;;; must swap it with field-value for conversion/checking
            if opnd or (offset or subs_opnd) then 1 else 2 endif -> do_swap;
            VM Cons_inst(VM I_SWAP, 0, do_swap, 3)
        endunless;

        ;;; run updaters of conversion procedures backwards
        fast_for p in rev(conv_p) do call_p_instr(p, true) endfor;

        if spec == EXVAL then
            ;;; returns value to assign in
            INST1(I_CALLABS, Exval_val)
        elseif isinteger(spec) then
            ;;; generate type to check integer field ranges and return value
            if spec fi_> 0 then
                if spec fi_> POPINT_BITS then
                    ;;; need to accept +ve bigints
                    Pint_->_uint
                else
                    Simpint_->_uint
                endif
            else
                abs(spec) fi_- 1 -> spec;
                if spec fi_> POPINT_BITS then
                    ;;; need to accept +ve or -ve bigints
                    Pint_->_sint
                else
                    Simpint_->_sint
                endif;
            endif -> p;
            ;;; _rangemask arg to procedure
            INST1(I_PUSH_UINT, 1<<spec-1),
            INST1(I_CALLABS, p)
        elseif spec == EXPTR and acc_p == [] then
            ;;; returns actual pointer
            INST1(I_CALLABS, Checkr_exptrclass_ptr)
        endif;
        if do_swap then
            ;;; swap converted/checked field-value back
            VM Cons_inst(VM I_SWAP, 0, do_swap, 3)
        endif
    enddefine;

        ;;; run access procedures on result, updater of last if update mode
    define lconstant run_access_p();
        lvars l;
        fast_for l on acc_p do
            call_p_instr(fast_front(l), upd and fast_back(l) == [])
        endfor
    enddefine;

    define lconstant fixed_exptr();
        acc_p /== [] or conv_p /== [] or flags &&/=_0 M_FD_FIXED_EXPTR
    enddefine;

    ;;; external ptr result -- construct external_ptr rec unless conversion
    ;;; procedure specified, in which case return a fixed one to save
    ;;; garbage.
    define lconstant exptr_result(run_acc_p);
        lvars run_acc_p;
        ;;; arg is true to cons a new pointer, false to use fixed one
        INST1(I_MAKE_EXPTR, not(fixed_exptr()));
        if run_acc_p then chain(run_access_p) endif
    enddefine;

    define :inline lconstant PUSH_FIELD(type, size, isint);
        VM Cons_inst(VM I_PUSH_FIELD, type, size, opnd,
#_IF DEF OLD_FIELD_INSTRUCTIONS offset or subs_opnd, isint, exptr, 7)
#_ELSE                          subs_opnd, offset, exptr, isint, 8)
#_ENDIF
    enddefine;

    define :inline lconstant PUSH_FIELD_ADDR();
        VM Cons_inst(VM I_PUSH_FIELD_ADDR, type, size, opnd,
#_IF DEF OLD_FIELD_INSTRUCTIONS offset or subs_opnd, exptr, 6)
#_ELSE                          subs_opnd, offset, exptr, 7)
#_ENDIF
    enddefine;

    define :inline lconstant POP_FIELD();
        VM Cons_inst(VM I_POP_FIELD, type, size, opnd,
#_IF DEF OLD_FIELD_INSTRUCTIONS offset or subs_opnd, exptr, 6)
#_ELSE                          subs_opnd, offset, exptr, 7)
#_ENDIF
    enddefine;


#_IF not(DEF OLD_FIELD_INSTRUCTIONS)
    ;;; convert to instructions if idents supplied
    if opnd and not(isvector(opnd)) then INST1(I_MOVE, opnd) -> opnd endif;
    if subs_opnd and not(isvector(subs_opnd)) then
        INST1(I_MOVE, subs_opnd) -> subs_opnd
    endif;
#_ENDIF

    explode(fdesc) -> (spec, type, size, offset, acc_p, conv_p, flags, exptr);

    flags &&/=_0 M_FD_ADDR_MODE -> addr_mode;
    true -> org_ptr;

    if upd then
        if flags &&=_0 M_FD_CAN_UPDATE or addr_mode or ispair(exptr) then
            mishap(0, 'INVALID EXTERNAL UPDATE OPERATION')
        elseif Field_update_convert(fdesc) ->> valspec then
            ;;; generate conversion/check on value going into field
            upd_convert(valspec)
        endif
    elseif ispair(exptr) then
        ;;; external function call -- fdesc refers to result (spec false if
        ;;; none)
        if addr_mode then
            mishap(0, 'ADDRESS MODE INVALID WITH EXTERNAL FUNCTION CALL')
        endif;
        if opnd then
            if isvector(opnd) then opnd else INST1(I_MOVE, opnd) endif
        endif;
        if fast_front(exptr) ->> n then
            ;;; fixed nargs
            INST1(I_MOVENUM, n),
            Call_extern
        else
            ;;; variadic with nargs as last arg
            Call_extern_nargs
        endif -> p;
        INST1(I_PUSH_UINT, fast_back(exptr));       ;;; fltsingle arg
        INST1(I_CALLABS, p);
        if spec then
            ;;; result is 0-level external access
            0 -> exptr;
            false ->> opnd -> org_ptr
        else
            INST1(I_ERASE, true);       ;;; erase result from _call_external
            return
        endif
    endif;

    exptr -> isextn;
    if exptr == 0 then false -> exptr endif;

    if isvector(spec) and not(addr_mode) then
        ;;; access thru pointer a number of times -- push it
        fsv(FD_TYPE,spec)       -> type;
        fsv(FD_BIT_SIZE,spec)   -> size;
        fsv(FD_DEREF_COUNT,spec)-> n;
        fsv(FD_VAL_SPEC,spec)   -> spec;
        unless offset == 0 then
            PUSH_FIELD(t_WORD, WORD_BITS, false);
            false ->> opnd ->> subs_opnd -> exptr;
            0 -> offset;
            n fi_- 1 -> n
        endunless;
        unless n == 0 then
            unless exptr then 0 -> exptr endunless;
            exptr fi_+ n -> exptr
        endunless;
        false -> org_ptr
    endif;

    ;;; size as multiple of type -- should be 1 for everything other
    ;;; than bitfields and external compound fields
    size fi_div fsv(1, list_assoc_val(type fi_&& t_BASE_TYPE, type_sizes))
                                                    -> size;

    if ispair(spec) or addr_mode then
        ;;; external 'compound' field -- get its address
        if offset == 0 and org_ptr and exptr == 1 and type == t_WORD
        and fixed_exptr() and not(addr_mode) then
            ;;; just return the original exptr
            if opnd then
                if isvector(opnd) then opnd else INST1(I_MOVE, opnd) endif
            endif;
            run_access_p()
        else
            PUSH_FIELD_ADDR();
            exptr_result(not(addr_mode))
        endif;

    elseif isprocedure(spec) then
        ;;; access/update procedure taking address of field
        PUSH_FIELD_ADDR();
        if not(upd) and spec!PD_PROPS == EXVAL then
            ;;; pass false or an exptr as 2nd arg
            INST1(I_MOVEQ, fixed_exptr() and writeable consexternal_ptr())
        endif;
        ;;; call system access/update procedure
        INST1(I_CALLABS, if upd then spec!PD_UPDATER else spec endif)

    elseif upd and acc_p == [] then
        ;;; update field
        POP_FIELD()

    else
        lvars isint = isinteger(spec) and spec /== EXVAL;
        ;;; access
        ;;; last element true if want conversion to popint
        PUSH_FIELD(type, size, isint) ->> instr;
        if isint and (spec fi_> POPINT_BITS or spec fi_< #_< -POPINT_BITS-1 >_#)
        then
            ;;; may need conversion to bigint
            false -> fsv(#_IF DEF OLD_FIELD_INSTRUCTIONS 6 #_ELSE 8 #_ENDIF,
                                        instr);     ;;; no conversion to popint
            INST1(I_CALLABS, if spec == WORD_BITS then
                                Uint_->_pint
                             else
                                Sint_->_pint
                             endif)
        elseif spec == EXPTR or spec == EXVAL then
            exptr_result(spec==EXPTR)
        elseif isextn and spec == FULL then
            ;;; check values coming out of external full fields to
            ;;; be pucker pop objects
            INST1(I_CALLABS, Full_val_extern)
        endif

    endif;

    unless upd or addr_mode then
        ;;; run conversion procedures
        fast_for p in conv_p do call_p_instr(p, false) endfor
    endunless
enddefine;

    /*  Code to check the args to a field access on the stack, leaving
        them unchanged.
    */
define Arg_check_instrs(checking, fdesc, vsubs);
    lvars checking, exptr, vsubs, p, fdesc;
    returnunless(checking);     ;;; fast, no checks
    if fsv(FD_EXTERN,fdesc) ->> exptr then
        ;;; external
        if isinteger(vsubs) then
            ;;; check subscript against max length
            INST1(I_MOVENUM, vsubs);
            Checkr_exptrclass_subscr
        elseif vsubs then
            ;;; just check subscript > 0
            Checkr_exptrclass_subscr0
        elseif ispair(exptr) and not(fast_front(exptr)) then
            ;;; check nargs to variadic external call
            Checkr_exptrclass_nargs
        else
            Checkr_exptrclass
        endif
    else
        returnunless(iskey(checking));
        INST1(I_MOVEQ, checking);
        if vsubs then Checkr_vec_subscr else Checkr_record endif
    endif -> p;
    INST1(I_CALLABS, p)
enddefine;

endsection;     /* $-Sys$-Fld */



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan  6 2005
        inserted for linux on PC
            ;;; all PC linux float results are in double form
            FLWEAK Float_val_s_C -> fsv(FD_VAL_SPEC,fdesc)

--- John Gibson, Nov  4 1996
        Changed Field_instrs to take extra subs_opnd argument, and generate
        new-style I_PUSH/POP_FIELD(_ADDR) instructions when
        DEF OLD_FIELD_INSTRUCTIONS is false.
--- John Gibson, Jun  3 1996
        Added (u)longlong fields specs (not yet coped with as external
        func arg/result, however).
--- John Gibson, Aug 25 1995
        Fixed Exval_val for 64-bit case. Commented out code for doubleword
        integer fields (potentially allows types (u)longlong if required)
--- John Gibson, Mar 30 1995
        Changes to allow long/ulong and exval fields to be double word
--- John Gibson, Mar 21 1995
        Revised types in named_field_specs etc
--- John Gibson, Oct 21 1994
        Fix for Alpha in Convert_func_spec
--- John Gibson, Sep 19 1994
        Uses Cons_inst calls for VM instructions instead of vector expressions
--- John Gibson, Jun 11 1993
        Added support for M_FD_STRUCT_FIXED_EXPTR, i.e. fixed exptrs for
        structure fields only
--- John Gibson, Sep 24 1992
        Changed Field_instrs to return the original external ptr arg under
        certain conditions
--- John Gibson, Sep  4 1992
        Added "exval" field type
--- John Gibson, Sep  2 1992
        Substituted M_K_NO_FULL_FROM_PTR for byte access flag
--- John Gibson, Aug 22 1992
        Fixed bug in Field_instrs -- was mistakenly calling conversion
        procedures in address mode
--- John Gibson, Aug 21 1992
        Changed Convert_func_spec to allow for fltsingle arg. FD_EXTERN field
        for external func is now a pair [<nargs>|<fltsingle>] rather than a
        vector.
--- John Gibson, Aug  6 1992
        Added 0 fltsingle arg to external calls
--- Robert John Duncan, Jun  9 1992
        Fix for MIPS external calls returning single float results: on
        big-endian machines (e.g. SGI) the single float is in the *second*
        word of the result structure.
--- John Gibson, Apr  2 1991
        Added "float" field type -- which behaves as "sfloat" except as an
        external function result, when it gives a C "float" result. "sfloat"
        as a function result now always assumes a single.
--- John Gibson, Nov 26 1990
        Added unions to -Cons_rec_speclist-
--- John Gibson, Oct 15 1990
        Various changes
--- John Gibson, Sep 25 1990
        Added `address-mode' accessing. Revised interpretation of pointer-
        value specs when size less than a word.
--- John Gibson, Sep 14 1990
        Field descriptors now contain exptr arg.
        Added support for `pointer-value' specs (refs)
--- John Gibson, Aug 18 1990
        Changed -Field_instrs- to make -size- argument passed to
        I_PUSH_FIELD etc be in units of type size.
--- John Gibson, Jul  2 1990
        Bug fixes/mods to -Spec_fdesc-
--- John Gibson, Jun  4 1990
        Added external function call support
--- John Gibson, Jun  1 1990
        Added "pint" field type
--- John Gibson, May 29 1990
        Moved macros to fields.ph, changed representation of float
        fields in descriptors.
--- John Gibson, Mar 26 1990
        Added new named field specs for integer types, and closures
        for field spec + comversion procedure.
            Changed -Convert_rec_spec- to recognise ">->" as in spec list
        to mean align next field at pointer.
            Combined updater and base procedures of -Field_instrs- into one,
        and added support for external pointer fields.
--- John Gibson, Dec  7 1989
        Combined record layout code from conskey.p and vm_fields.p and
        moved here.
--- John Gibson, Apr 30 1989
        VM procedures now in $-Sys$-Vm.
--- John Gibson, Mar 14 1989
        Moved simple integer checking procedures to int_convert.p
        (and renamed them). Float procedures now named instead of being
        in a vector.
--- Roger Evans, Oct 10 1988
        Added -extn- arg to Field_instrs, and ability to handle -offset-
        <false> to mean offset on stack
--- John Gibson, Aug  5 1988
        Added use of DOUBLE_ALIGN_BITS
--- John Gibson, Feb 29 1988
        Sectionised.
--- John Gibson, Feb 28 1988
        Moved float field access procedures to float_fields.p
--- John Gibson, Feb 10 1988
        Changes for sectioned identifiers, etc.
--- John Gibson, Jan 28 1988
        Altered mishap message on negative return from -Dfloat- to be
        'REAL NUMBER NEEDED'.
 */
