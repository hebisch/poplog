/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/syscomp/syspop.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 > Documentation:   REF *SYSPOP11
 */

/* -------------------------------------------------------------------------

                    SYSPOP11 SYNTAX & DECLARATIONS

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas;

constant
    procedure (get_sfloat_int, pointers_into, read_input_svb_offset)
    ;

#_IF DEF VMS
vars $-popc_vms_extern_weak;
#_ENDIF

define External_dummy_id = newproperty([], 4, false, "tmparg") enddefine;


;;; --- SYSINT ARITHMETIC -----------------------------------------------

section Syspop;

define 0 _int(i) -> _i;
    lvars i, _i;
    unless isintegral(i) then
        mishap(i, 1, 'INTEGER NEEDED FOR _int')
    else
        if i == 0 then '0' else i sys_>< '' endif -> _i;
        i -> _intval(_i);
        #_< LAB_GLOBAL || LAB_LITERAL >_# -> islabel(_i)
    endunless
enddefine;

define 0 _pint(_i) -> i;
    lvars _i, i;
    unless _intval(_i) ->> i then
        mishap(_i, 1, 'SYSTEM INTEGER NEEDED')
    endunless;
enddefine;

constant procedure (
    $-Popas$-syspop\:_int   = _int,
    $-Popas$-syspop\:_pint  = _pint,
    );

define lconstant _arith2(_x, _y, op);
    lvars _x, _y, procedure op;
    _int(op(_pint(_x),  _pint(_y)))
enddefine;

define 2 _divq() -> _i with_nargs 2;
    lvars _i;
    _arith2(nonop div) -> _i ->
enddefine;

constant
    5 ( _add        = _arith2(%nonop +%),
        _sub        = _arith2(%nonop -%),
      ),

    4 ( _mult       = _arith2(%nonop *%),
        _biset      = _arith2(%nonop ||%),
        _biclear    = _arith2(%nonop &&~~%),
        _bimask     = _arith2(%nonop &&%),
        _bixor      = _arith2(%nonop ||/&%),
      ),

        _shift      = _arith2(%nonop <<%),
    ;

    ;;; Other subroutines that need declaring for in-line compilation
    ;;; (some of these don't actually exist in .s files, so can't be
    ;;; used except for in-line code).

constant
    7 ( _eq,            ;;; ==
        _neq,           ;;; /==
      ),

    6 ( _gr,            ;;; >   (unsigned machine integers)
        _greq,          ;;; >=
        _lt,            ;;; <
        _lteq,          ;;; <=
        _sgr,           ;;; >   (signed machine integers)
        _sgreq,         ;;; >=
        _slt,           ;;; <
        _slteq,         ;;; <=
        _psgr,          ;;; fi_>
        _psgreq,        ;;; fi_>=
        _pslt,          ;;; fi_<
        _pslteq,        ;;; fi_<=
      ),

    5 ( _padd,          ;;; fi_+
        _psub,          ;;; fi_-
      ),

    4 ( _pmult,         ;;; fi_*
        _div,           ;;; //  (machine integers)
        _pdiv,          ;;; fi_//
        _por,           ;;; fi_||
        _pand,          ;;; fi_&&
        _bitst,         ;;; bit test (machine integers)
      ),

        _negate,        ;;; negate
        _shift,         ;;; shift integer
        _logcom,        ;;; logical complement i.e. 1's complement
        _int,           ;;; pop integer to integer
        _pint,          ;;; integer to pop integer
        _pint_testovf,  ;;; integer to pop integer with overflow test
        _stklength,     ;;; stacklength in bytes (integer)
        _sp,            ;;; pointer to current call stack frame
        _sp_flush,      ;;; same with callstack flush
        _caller_sp,     ;;; point to caller's stack frame
        _caller_sp_flush, ;;; same with callstack flush
        _user_sp,       ;;; pointer to top of user stack
        _subsv,         ;;; fast_subscrv
        _subsv0,        ;;; fast_subscrv with first element at subscript 0
        _subss,         ;;; fast_subscrs
        _subsfroz,      ;;; fast_frozval
        _mksimple,      ;;; disguise pointer as simple
        _mkcompound,    ;;; recover pointer from simple
        _mksimple2,     ;;; disguise pointer as simple2
        _mkcompound2,   ;;; recover pointer from simple2
        _issimple,      ;;; true if simple or simple2
        _issimple2,     ;;; true if simple2 (when known to be simple)
        _iscompound,    ;;; true if pointer
        _isinteger,     ;;; true if pop integer
        _isaddress,     ;;; true if a valid address of a pop object
        _zero,          ;;; true if integer 0
        _nonzero,       ;;; true if not integer 0
        _neg,           ;;; true if negative
        _nonneg,        ;;; true if not negative
        _not,           ;;; not
        _dupstk,        ;;; same as dup
        _setstklen,
        _has_structkey,
        _caller_return,
        _chainfrom_caller, _fast_chainfrom_caller,
        _srchain,       ;;; chain subroutine
        _bitfield,      ;;; access bitfield
        _plog_trail_push,   ;;; push prologvar onto trail
        _dlocal_abexit_return,
        $-Sys$- _cache_flush,
    ;


define macro _: item -> item;
    lvars item, labinfo;
    if isintegral(item) then
        _int(item) -> item
    elseif isddecimal(item) then
        dfloat_key -> data_key(item)
    elseif isdecimal(item) then
        get_sfloat_int(item, false) -> item;
        islabel(item) fi_|| LAB_SFLOAT -> labinfo;
        if item = '0' then 0 -> _intval(copy(item) ->> item) endif;
        labinfo -> islabel(item)
    elseif isstring(item) then
        LAB_LITERAL -> islabel(item)
    else
        mishap(item, 1, 'ILLEGAL ITEM FOLLOWING _:')
    endif
enddefine;

define macro _:- item;
    lvars item;
    dlocal popdprecision = true;
    if isintegral(item) or isdecimal(item) then
        nonmac _:(-item)
    else
        mishap(item, 1, 'ILLEGAL ITEM FOLLOWING _- OR _:-')
    endif
enddefine;

constant procedure $-Popas$-syspop\:_: = nonmac _: ;

constant macro (
    _-  = nonmac _:-,
    );

    ;;; Dummy identifiers that can be dlocal'ed in a procedure to
    ;;; force all the pop or nonpop registers to be local (N/A to SPARC)
vars \<ALL_POP_REGISTERS\>, _\<ALL_NONPOP_REGISTERS\>;

constant
    $-Popas$-localise_pop_reg_id    = ident \<ALL_POP_REGISTERS\>,
    $-Popas$-localise_nonpop_reg_id = ident _\<ALL_NONPOP_REGISTERS\>,
    ;

endsection;     /* Syspop */


;;; --- STRUCTURES AND FIELD ACCESSING -------------------------------------

section;

constant macro (

    ;;; Codes for 3-bit base data types (bits 0 - 2)
    ;;; (NOTE: These together with tv_SIGNED are also used in the system.)
    t_BASE_TYPE     = 2:111,        ;;; mask for base type
    t_BIT           = 0,
    t_BYTE          = 1,
    t_SHORT         = 2,
    t_INT           = 3,
    t_DOUBLE        = 4,

    ;;; Codes for 3-bit value type (bits 3 - 5)
    tv_VAL_TYPE     = 2:111<<3,     ;;; mask for value type
    tv_UNSIGNED     = 0<<3,         ;;; field is unsigned integer
    tv_SIGNED       = 1<<3,         ;;; field is signed integer
    tv_FULL         = 2<<3,         ;;; full field
    tv_EXPTR        = 3<<3,         ;;; exptr field
    tv_EXVAL        = 4<<3,         ;;; exval field
    tv_FLOAT        = 5<<3,         ;;; floating-point field (t_INT, t_DOUBLE)
t_SIGNED        = tv_SIGNED,

    ;;; Other flags (bits 6 - 10)
    t_OTHER_FLAGS   = 2:1111<<6,
    ;;; next two are mutually exclusive
    t_PINT          = 1<<6,         ;;; t_(UN)SIGNED is within range of popint
    t_FLOAT_C_SINGLE= 1<<6,         ;;; tv_FLOAT is C "float" result

    t_WORDRANGE_DOUBLE= 1<<7,       ;;; t_DOUBLE containing word-range value

    t_ADDR_MODE     = 1<<8,         ;;; return addr when accessed
    t_IS_STRUCT     = 1<<9,         ;;; is a structure
    t_INVERTED      = 1<<10,        ;;; bit saying field is inverted

    ;;; Offset size of field starts at bit 11
    t_OFFS_SHIFT    = 11,
    );

constant macro (
    t_WORD          = #_IF WORD_BITS==INT_BITS t_INT #_ELSE t_DOUBLE #_ENDIF,
    t_LONG          = #_IF LONG_BITS==INT_BITS t_INT #_ELSE t_DOUBLE #_ENDIF,
    t_EXPTR         = #_IF EXPTR_BITS==INT_BITS t_INT #_ELSE t_DOUBLE #_ENDIF,
    );

#_IF not(DEF SEGMENT_OFFS)
constant macro SEGMENT_OFFS = VPAGE_OFFS;
#_ENDIF

endsection;     /* $- */


constant macro HUGE = 2:1e50;

define t_cons(offs, type);
    lvars offs, type;
    type &&~~ #_< -1 << t_OFFS_SHIFT >_# -> type;
    (offs << t_OFFS_SHIFT) || type
enddefine;

define t_offset(type, err) -> offs;
    lvars type, err, offs;
    type >> t_OFFS_SHIFT -> offs;
    if err and offs >= HUGE then
        err(0, 'invalid use of array data type')
    endif
enddefine;

    ;;; type codes for standard types
constant macro (
    T_BYTE      = t_cons(BYTE_OFFS,   t_BYTE),
    T_SHORT     = t_cons(SHORT_OFFS,  t_SHORT),
    T_INT       = t_cons(INT_OFFS,    t_INT),
    T_DOUBLE    = t_cons(DOUBLE_OFFS, t_DOUBLE),
    T_WORD      = t_cons(WORD_OFFS,   t_WORD),

    T_LONG      = t_cons(LONG_OFFS,   t_LONG)
                    || if t_LONG>t_WORD then t_WORDRANGE_DOUBLE else 0 endif,
    T_EXPTR     = t_cons(EXPTR_OFFS,  t_EXPTR)||tv_EXPTR
                    || if t_EXPTR>t_WORD then t_WORDRANGE_DOUBLE else 0 endif,
    T_EXVAL     = (T_EXPTR&&~~tv_EXPTR)||tv_EXVAL,

    T_INT_DOUBLE = T_DOUBLE||t_WORDRANGE_DOUBLE,

    T_LONGLONG  = if t_DOUBLE>t_WORD then T_INT_DOUBLE else T_DOUBLE endif,

    T_SGN_BYTE  = T_BYTE||tv_SIGNED,
    T_SGN_SHORT = T_SHORT||tv_SIGNED,
    T_SGN_INT   = T_INT||tv_SIGNED,
    T_SGN_WORD  = T_WORD||tv_SIGNED,
    T_SGN_LONG  = T_LONG||tv_SIGNED,
    T_SGN_LONGLONG = T_LONGLONG||tv_SIGNED,

    T_FULL      = T_WORD||tv_FULL,
    T_SFLOAT    = T_INT||tv_FLOAT,
    T_DFLOAT    = T_DOUBLE||tv_FLOAT,
    );

define pointer_equiv_type = newproperty([], 8, T_WORD, "tmparg") enddefine;

lconstant procedure (
    field_name_spec     = newassoc([]),

    base_type_bitsize = newassoc([
            [^t_BIT     1]
            [^t_BYTE    ^BYTE_BITS]
            [^t_SHORT   ^SHORT_BITS]
            [^t_INT     ^INT_BITS]
            [^t_DOUBLE  ^DOUBLE_BITS]   ]),

    base_type_unit_bitsize = newassoc([
            [^t_BIT     1]
            [^t_BYTE    ^(BYTE_BITS/BYTE_OFFS)]
            [^t_SHORT   ^(SHORT_BITS/SHORT_OFFS)]
            [^t_INT     ^(INT_BITS/INT_OFFS)]
            [^t_DOUBLE  ^(DOUBLE_BITS/DOUBLE_OFFS)]   ]),

    base_type_bitalign = newassoc([
            [^t_BIT     1]
            [^t_BYTE    ^BYTE_BITS]
            [^t_SHORT   ^SHORT_ALIGN_BITS]
            [^t_INT     ^INT_ALIGN_BITS]
            [^t_DOUBLE  ^DOUBLE_ALIGN_BITS]   ]),

    base_type_struct_bitalign = newassoc([
            [^t_BIT     1]
            [^t_BYTE    ^BYTE_BITS]
            [^t_SHORT   ^STRUCT_SHORT_ALIGN_BITS]
            [^t_INT     ^STRUCT_INT_ALIGN_BITS]
            [^t_DOUBLE  ^STRUCT_DOUBLE_ALIGN_BITS]   ]),

    named_type_spec = newassoc([%
            [byte   ^T_BYTE],
            [b      ^T_BYTE],
            [sb     ^T_SGN_BYTE],
            [short  ^T_SHORT],
            [s      ^T_SHORT],
            [ss     ^T_SGN_SHORT],
            [int    ^T_INT],
            [i      ^T_INT],
            [si     ^T_SGN_INT],
            [long   ^T_LONG],
            [l      ^T_LONG],
            [sl     ^T_SGN_LONG],
            [word   ^T_WORD],
            [w      ^T_WORD],
            [sw     ^T_SGN_WORD],
            [double ^T_DOUBLE],
            [d      ^T_DOUBLE],
            [sfloat ^T_SFLOAT],
            [dfloat ^T_DFLOAT],
            [full   ^T_FULL],
            [vpage  ^(t_cons(VPAGE_OFFS, t_WORD))],
            [segment ^(t_cons(SEGMENT_OFFS, t_WORD))],
#_IF DEF VMS
            [vpagelet ^(t_cons(512, t_WORD))],
#_ENDIF
        %]),
    );

named_type_spec("CODE_POINTER_TYPE".valof) -> named_type_spec("code");
constant macro T_BIT_POINTER = named_type_spec("BIT_POINTER_TYPE".valof);

define t_bitoffset(type_spec, err);
    lvars type_spec, err;
    t_offset(type_spec, err)
        * base_type_unit_bitsize(type_spec && t_BASE_TYPE)
enddefine;

define lconstant set_addr_mode(type) -> type;
    lvars type;
    if type &&/=_0 t_WORDRANGE_DOUBLE then
        type || t_ADDR_MODE -> type
    endif
enddefine;

define lconstant pop_named_type_spec =
        newassoc([
            [dfloat         ^(T_DFLOAT||t_ADDR_MODE)]
            [sfloat         ^(T_SFLOAT||t_ADDR_MODE)]
            [float          ^(T_SFLOAT||t_ADDR_MODE)]
            [exptr          ^T_EXPTR]
            [exval          ^(T_EXVAL.set_addr_mode)]
            [full           ^T_FULL]
            [word           ^T_SGN_WORD]
            [uword          ^T_WORD]
            [pint           ^(T_SGN_WORD||t_PINT)]
            [longlong       ^(T_SGN_LONGLONG.set_addr_mode)]
            [ulonglong      ^(T_LONGLONG.set_addr_mode)]
            [long           ^(T_SGN_LONG.set_addr_mode)]
            [ulong          ^(T_LONG.set_addr_mode)]
            [int            ^T_SGN_INT]
            [uint           ^T_INT]
            [short          ^T_SGN_SHORT]
            [ushort         ^T_SHORT]
            [sbyte          ^T_SGN_BYTE]
            [byte           ^T_BYTE]

            [ddecimal       ^(T_DFLOAT||t_ADDR_MODE)]
            [decimal        ^(T_SFLOAT||t_ADDR_MODE)]
        ])
enddefine;




constant macro (
    VECDESC_TYPE        = 1,    ;;; "field", "struct" or "function"

    ;;; Field descriptor (a vector)
    FIELD_IDENT         = 2,    ;;; false for padding field
    FIELD_TYPE_SPEC     = 3,
    FIELD_OFFSET        = 4,
    FIELD_STRUCT_TYPE   = 5,
        FIELD_VEC_LEN       = 5,
    ;;; additional (optional) fields for pop types
    FIELD_ACC_P         = 6,
    FIELD_CONV_P        = 7,
        FIELD_POP_VEC_LEN   = 7,

    ;;; Struct spec structure (a vector)
    STRUC_TYPE_SPEC     = 2,
    STRUC_FIELD_LIST    = 3,
    STRUC_FLAGS         = 4,
    STRUC_PTR_OFFSET    = 5,
        STRUC_VEC_LEN       = 5,

    ;;; Flags in STRUC_FLAGS
    M_STRUC_VARIFORM    = 2:1e0,
    M_STRUC_DALIGN      = 2:1e1,
    M_STRUC_POP         = 2:1e2,

    ;;; Function descriptor
    FUNC_RESULT_SPEC    = 2,
    FUNC_NARGS          = 3,
    FUNC_FLTSINGLE      = 4,
        FUNC_VEC_LEN        = 4,

    POP_VECTOR_FIELD    = 4,    ;;; field num of vec field in field list
    POP_EXPTR_FIELD     = 4,    ;;; field num of exptr field in field list
    );


lconstant procedure get_struct_spec;

define lconstant cons_field_spec(fid, type_spec, offs, struct_type) -> spec;
    lvars spec, fid, type_spec, offs, struct_type;
    initv(FIELD_VEC_LEN) -> spec;
    "field"     -> spec(VECDESC_TYPE);
    fid         -> spec(FIELD_IDENT);
    type_spec   -> spec(FIELD_TYPE_SPEC);
    offs        -> spec(FIELD_OFFSET);
    struct_type -> spec(FIELD_STRUCT_TYPE)
enddefine;


define lconstant listitem_check();
    if islist(nextitem()) then
        [% "[", dl(itemread()), "]" %] nc_<> proglist -> proglist
    endif
enddefine;

define lconstant vecitem_check();
    if isvector(nextitem()) then
        [% "{", explode(itemread()), "}" %] nc_<> proglist -> proglist
    endif
enddefine;

define lconstant worditem_check(word);
    lvars word, item = nextitem();
    if isword(item) and isstartstring(word, item) and item /= word then
        itemread() -> ;
        [% word, allbutfirst(datalength(word), item) %]
                nc_<> proglist -> proglist
    endif
enddefine;

define lconstant cons_array(type_spec, size, isfld);
    lvars type_spec, size, isfld;
    if isfld then consref(size) -> size endif;
    conspair(type_spec, size)
enddefine;

define lconstant read_array_size(type_spec, isfld, err) -> type_spec;
    lvars arraysize, type_spec, isfld, procedure err;
    ;;; read array spec if any
    if (listitem_check(), pop11_try_nextitem("[")) then
        itemread() -> arraysize;
        if arraysize == "]" then
            HUGE -> arraysize           ;;; means variable length
        else
            unless isinteger(arraysize) and arraysize fi_>= 0 then
                err(arraysize, 1, 'invalid array size')
            endunless;
            pop11_need_nextitem("]") ->
        endif;
        cons_array(type_spec, arraysize, isfld) -> type_spec
    endif
enddefine;

define lconstant read_type_spec(terminators, pop11_try_need, err)
                                            -> (terminator, type_spec);
    lvars   item, type_spec, signed = 0, arraysize, terminator,
            terminators, invert = false, procedure (pop11_try_need, err);
    dlocal  pop_autoload = false;   ;;; want macros, but no autoloading

    if nextitem() == "inv" then
        itemread() -> ;
        true -> invert
    endif;
    worditem_check("<");
    itemread() -> item;
    if isinteger(item) then
        if item < 0 then abs(item) -> item, tv_SIGNED -> signed endif;
        if item > INT_BITS  then
            err(item, 1, 'type bitsize too big')
        endif;
        t_cons(item, t_BIT) || signed -> type_spec
    elseif item == "(" or item == "<" then
        ;;; pointer to something
        ;;; (...) means a pop word field; <...> means an external (ie C) field.
        ;;; The two are the same except on Alpha OSF where the latter is
        ;;; 8 bytes (same as long).

        lconstant need_nextitem = worditem_check(%">"%) <> pop11_need_nextitem;

        read_type_spec(if item == "(" then [)] else [>] endif, need_nextitem,
                                    err) -> (, type_spec);
        consref(type_spec) -> type_spec;
        if item == "<" then
            T_EXPTR &&~~ tv_EXPTR -> pointer_equiv_type(type_spec)
        endif
    elseif item == "struct" then
        ;;; struct
        get_struct_spec() -> type_spec
    else
        if item == "-" then
            tv_SIGNED -> signed, itemread() -> item
        endif;
        unless named_type_spec(item) ->> type_spec then
            err(item, 1, 'unknown type specifier')
        elseif signed /== 0 then
            unless isintegral(type_spec)
            and type_spec && tv_VAL_TYPE == tv_UNSIGNED then
                err(item, 1, 'invalid signed type')
            endunless;
            type_spec || signed -> type_spec
        endunless
    endif;

    if invert then
        if ispair(type_spec) or isvector(type_spec) then
            err(0, 'invalid inverted type')
        else
            identfn(%type_spec%) -> type_spec
        endif
    endif;

    ;;; read array spec if any
    read_array_size(type_spec, false, err) -> type_spec;

    pop11_try_need(terminators) -> terminator
enddefine;

define eval_type_spec(type_spec);
    lvars type_spec, size, dtype;
    if isintegral(type_spec) then
        type_spec
    elseif isref(type_spec) then
        ;;; pointer
        pointer_equiv_type(type_spec)
    elseif ispair(type_spec) then
        ;;; array type/array field
        back(type_spec) -> size;
        if isref(size) then fast_cont(size) -> size endif;
        eval_type_spec(front(type_spec)) -> type_spec;
        t_cons(size * t_offset(type_spec,false), type_spec)
    elseif isvector(type_spec) then
        ;;; field or struct vector
        type_spec(VECDESC_TYPE) -> dtype;
        if dtype == "struct" then
            type_spec(STRUC_TYPE_SPEC)
        elseif dtype = "function" then
            ;;; external function -- just treat as unsized byte array
            t_cons(HUGE, t_BYTE)
        else
            ;;; field
            eval_type_spec(type_spec(FIELD_TYPE_SPEC))
        endif
    else
        ;;; inverted type (1 element closure)
        eval_type_spec(frozval(1,type_spec)) ||/& t_INVERTED
    endif
enddefine;

define lconstant non_array_spec(type_spec);
    lvars type_spec;
    if ispair(type_spec) and isref(back(type_spec)) then
        ;;; array field -- use the underlying spec
        front(type_spec)
    else
        type_spec
    endif
enddefine;

define lconstant gen_pad_field(nbits, offset, max_type_code, field_list);
    lvars   max_type = max_type_code && t_BASE_TYPE, max_type_code, nbits,
            bitsize, offset, pad_spec, field_list, repcount;
    until max_type == t_BIT
    or (nbits rem base_type_bitsize(max_type) == 0
        and offset rem base_type_bitalign(max_type) == 0)
    do
        max_type-1 -> max_type
    enduntil;
    base_type_bitsize(max_type) -> bitsize;
    t_cons(bitsize/base_type_unit_bitsize(max_type), max_type) -> pad_spec;
    pad_spec || (max_type_code && t_INVERTED) -> pad_spec;
    if (nbits/bitsize ->> repcount) /== 1 then
        cons_array(pad_spec, repcount, false) -> pad_spec
    endif;
    offset+nbits,
    if field_list then
        cons_field_spec(false, pad_spec, offset, false) :: field_list
    else
        pad_spec
    endif
enddefine;

define lconstant align_field(offset, type_code, type_bitalign, field_list);
    lvars   type, type_code, offset, field_list, n, padbits, bitalign,
            procedure type_bitalign;
    type_code && t_BASE_TYPE -> type;
    if type_code && tv_VAL_TYPE == tv_FULL then
        WORD_BITS
    else
        type_bitalign(type)
    endif -> bitalign;

    if ((offset+bitalign-1) // bitalign -> n) < 0 then n-1 -> n endif;
    n*bitalign - offset -> padbits;
    if padbits == 0 then
        offset, field_list
    else
        gen_pad_field(padbits, offset, type_code, field_list)
    endif
enddefine;

define lconstant cons_struct(ptr_offset, offset, field_list, struct_type,
                             struct_flags, has_full, invbit, err)
                                    -> struct_spec;
    lvars type, type_spec, spec, ptr_offset, offset, field_list,
        struct_type, struct_spec, struct_flags, has_full, invbit,
        name, old, pointer_spec, org_struct_type = struct_type, procedure err
        ;

    define lconstant check_align(type, offset);
        lvars type, offset;
        unless offset rem base_type_bitalign(type && t_BASE_TYPE) == 0 then
            err(0, 'struct pointer misaligned')
        endunless
    enddefine;

    define lconstant scale_offset(type, offset);
        lvars type, offset;
        offset / base_type_unit_bitsize(type && t_BASE_TYPE)
    enddefine;

    initv(STRUC_VEC_LEN) -> struct_spec;
    "struct" -> struct_spec(VECDESC_TYPE);

#_IF DOUBLE_ALIGN_BITS > WORD_ALIGN_BITS
    if struct_type == t_DOUBLE then
        struct_flags || M_STRUC_DALIGN -> struct_flags
    endif;
#_ENDIF
    if has_full then
        if has_full == "pop" then
            struct_flags || M_STRUC_POP -> struct_flags;
            t_WORD -> struct_type
        endif;
        struct_type || tv_FULL -> struct_type
    endif;
    struct_type fi_|| invbit -> struct_type;

    if offset < HUGE then
        align_field(offset, struct_type, base_type_struct_bitalign,
                                    field_list) -> field_list -> offset;
        if ptr_offset == "offset" then offset -> ptr_offset endif;
        scale_offset(struct_type, offset) -> offset
    elseif ptr_offset == "offset" then
        err(0, 'invalid position for pointer')
    endif;
    t_cons(offset, struct_type) -> struct_spec(STRUC_TYPE_SPEC);

    check_align(org_struct_type, ptr_offset);

    check_align(t_BYTE, ptr_offset);
    scale_offset(struct_type, ptr_offset) -> struct_spec(STRUC_PTR_OFFSET);

    cons_field_spec(false, "pointer", 0, struct_type) -> pointer_spec;
    [%  fast_for spec in rev(field_list) do
            spec(FIELD_TYPE_SPEC) -> type_spec;
            eval_type_spec(type_spec) -> type;
            if type && t_INVERTED /== invbit then
                err(0, 'mixing inverted and non-inverted fields')
            endif;

            spec(FIELD_OFFSET) - ptr_offset -> offset;
            if offset == 0 and pointer_spec then
                pointer_spec, false -> pointer_spec
            endif;

            check_align(type, offset);
            scale_offset(type, offset) -> offset;
            if isvector(type_spec) and type_spec(VECDESC_TYPE) == "struct" then
                ;;; add offset from start to pointer
                offset + type_spec(STRUC_PTR_OFFSET) -> offset
            endif;
            offset -> spec(FIELD_OFFSET);
            struct_type -> spec(FIELD_STRUCT_TYPE);

            if isword(spec(FIELD_IDENT) ->> name) then
                unless field_name_spec(name) ->> old then
                    spec -> field_name_spec(name)
                elseif spec = old then
                    old -> spec
                else
                    err(name, 1, 'ambiguous redefinition of field name')
                endunless
            endif;
            spec
        endfor;
        if pointer_spec then pointer_spec endif
    %] -> struct_spec(STRUC_FIELD_LIST);
    struct_flags -> struct_spec(STRUC_FLAGS)
enddefine;

define get_named_field_spec(name) -> spec;
    lvars name, spec;
    unless field_name_spec(name) ->> spec then
        mishap(name, 1, 'NO DEFINITION FOR STRUCT FIELD NAME')
    endunless
enddefine;


;;; --- POP STRUCTURES ----------------------------------------------------

lblock;

lvars bitoffs, field_list, field_num, struct_type, k_start;

lconstant procedure spec_fdesc;

define lconstant insert_key();
    if bitoffs == 0 then
        gen_pad_field(k_start, 0, t_WORD, field_list)
    else
        align_field(bitoffs, T_FULL, base_type_bitsize, field_list)
    endif -> (bitoffs, field_list);
    cons_field_spec("KEY", T_FULL, bitoffs, false) :: field_list -> field_list;
    bitoffs+WORD_BITS -> bitoffs
enddefine;

define lconstant make_record_struct(speclist, create_substr, pop);
    lvars speclist, create_substr, pop, base, isfield, totbitoffs = 0, spec;
    dlocal bitoffs = 0, field_list = [], field_num = 0, struct_type, k_start;

    if pop then

        define lconstant name_fld_bitoffs(name);
            lvars name;
            get_named_field_spec(name)(FIELD_OFFSET)
                            * base_type_unit_bitsize(t_WORD)
        enddefine;

        -name_fld_bitoffs("POPBASE") -> base;
        t_WORD, name_fld_bitoffs("KEY")+base, true

    else
        0 -> base;
        t_BIT, false, create_substr or "nested";
        false -> create_substr

    endif -> (struct_type, k_start, isfield);

    for spec in speclist do
        if spec == ">->" then
            ;;; align next field at key for pop, pointer for extern
            if pop then k_start else base endif -> bitoffs
        elseif spec == "|" then
            ;;; end of record in 'union' (only when pop false)
            max(bitoffs, totbitoffs) -> totbitoffs;
            0 -> bitoffs
        else
            ;;; adds fields to field_list
            spec_fdesc(spec, false, [], [], create_substr, isfield)
        endif
    endfor;

    if pop and bitoffs <= k_start then insert_key() endif;
    max(bitoffs, totbitoffs) -> totbitoffs;
    if totbitoffs == 0 then t_WORD -> struct_type endif;

    cons_struct(base, totbitoffs, field_list, struct_type, 0,
                    if pop then "pop" else false endif, 0, mishap)
enddefine;

define lconstant spec_fdesc(spec, impacc, acc_p, conv_p, create_substr,
                                                                isfield);
    lvars   spec, impacc, acc_p, conv_p, create_substr, isfield,
            p, type, type_code, type_spec, fdesc, result_spec, nargs,
            extern_acc = false, offs = 0, exval_pad = false;
    lconstant isantiref = newproperty([], 4, false, "tmparg");

    define lconstant adjust_exval_spec(spec);
        lvars spec;
#_IF DEF BIG_ENDIAN
        lvars   type_spec = spec(FIELD_TYPE_SPEC),
                type = type_spec && t_BASE_TYPE;
        if type < t_EXPTR then
            spec(FIELD_OFFSET)
                    + t_bitoffset(T_EXPTR,false)/base_type_unit_bitsize(type)
                    - t_offset(type_spec,false)
                -> spec(FIELD_OFFSET)
        endif;
#_ENDIF
    enddefine;

    while isclosure(spec) do
        ;;; pdpart is conversion procedure, frozval(1) is spec,
        ;;; frozval(2) is true if conversion procedure or false if
        ;;; access procedure
        pdpart(spec) -> p;

        if isclosure(p) and not(isundef(p)) and datalength(p) == 2
        and pdpart(p) == identfn
        and ((frozval(1,p) ->> type) == ":" or type == "|") then
            ;;; implicit access -- frozval(2) has spec
            type == "|" -> extern_acc;
            spec_fdesc(frozval(2,p), impacc, acc_p, conv_p,
                                            extern_acc, false) -> impacc;
            [] ->> acc_p -> conv_p
        elseif frozval(2, spec) then
            p :: conv_p -> conv_p
        else
            p :: acc_p -> acc_p
        endif;
        frozval(1,spec) -> spec;
        false -> create_substr
    endwhile;

    if isref(spec) then
        ;;; access of external pointer value (isfield and impacc must be false)
        spec_fdesc(fast_cont(spec), false, acc_p, conv_p, false, false)
                        -> spec;
        adjust_exval_spec(spec);
        consref(spec) -> spec;
        true -> isantiref(spec);
        return(spec)
    endif;

    ;;; rest are 'proper' fields

    if isvector(spec) or ispair(spec) then
        ;;; 'compound' field (field value is address)

        if isvector(spec) then
            ;;; external function -- only allowed through indirection
            initv(FUNC_VEC_LEN) -> fdesc;
            "function" -> fdesc(VECDESC_TYPE);
            if ispair(spec(1) ->> nargs) then
                destpair(nargs)
            else
                nargs, 0
            endif -> (fdesc(FUNC_NARGS), fdesc(FUNC_FLTSINGLE));
            if (spec(2) ->> result_spec) and result_spec /== "void" then
                spec_fdesc(result_spec, false, [], [], false, false) -> spec;
                ;;; modify descriptor for access thru pointer returned by
                ;;; _call_external to 3-word result array
                ;;; (possible float result in first 2 words, possible word
                ;;; result in 3rd).
                DOUBLE_OFFS -> spec(FIELD_OFFSET);
                if isintegral(spec(FIELD_TYPE_SPEC) ->> type_code) then
                    if type_code && tv_VAL_TYPE == tv_FLOAT then
                        0 -> spec(FIELD_OFFSET);
#_IF DEF MIPS and DEF BIG_ENDIAN
                        if type_code && t_BASE_TYPE == t_INT then
                            INT_OFFS -> spec(FIELD_OFFSET)
                        endif;
#_ENDIF
#_IF DEF ALPHA
                        if type_code && t_BASE_TYPE == t_INT
#_ELSE
                        if result_spec == "float" or result_spec == "decimal"
#_ENDIF
                        then
                            type_code || t_FLOAT_C_SINGLE
                                        -> spec(FIELD_TYPE_SPEC)
                        endif
                    else
                        adjust_exval_spec(spec)
                    endif
                endif;
                spec
            else
                false
            endif -> fdesc(FUNC_RESULT_SPEC);
            fdesc

        elseif islist(spec) then
            ;;; record
            make_record_struct(spec, create_substr, false)
        else
            ;;; repeated field (i.e. array)
            spec_fdesc(fast_front(spec), false, [], [], false, false)
                                                        -> fdesc;
            cons_array(fdesc, fast_back(spec) or HUGE, true)
        endif;

        unless create_substr then
            eval_type_spec(()) || #_< t_ADDR_MODE || t_IS_STRUCT >_#
        endunless;

    elseif isinteger(spec) then
        lvars signed = 0;
        if spec < 0 then abs(spec) -> spec, tv_SIGNED -> signed endif;
        if isfield == true then
            t_INT -> type;
            until spec == base_type_bitsize(type)
            and bitoffs rem base_type_bitalign(type) == 0 do
                quitif((type-1 ->> type) == t_BIT)
            enduntil
        else
            t_BIT -> type
        endif;
        t_cons(spec/base_type_unit_bitsize(type), type) || signed

    elseif pop_named_type_spec(spec) ->> type_spec then
        if impacc then
            ;;; spec = exptr/exval
            if isantiref(impacc) then
                ;;; access of pointer value -- remove indirection
                cont(impacc) -> impacc;
                if datalength(impacc) == FIELD_POP_VEC_LEN then
                    impacc(FIELD_ACC_P) -> acc_p;
                    impacc(FIELD_CONV_P) -> conv_p
                endif;
                impacc(FIELD_OFFSET) -> offs;
                impacc(FIELD_TYPE_SPEC) -> type_spec;
                type_spec && t_BASE_TYPE < t_EXPTR -> exval_pad
            elseunless isintegral(impacc(FIELD_TYPE_SPEC) ->> type)
            and type &&/=_0 t_IS_STRUCT and not(extern_acc) then
                consref(impacc) -> type_spec
            else
                ;;; else treat compound value as its external pointer (i.e.
                ;;; ignore impacc)
                if datalength(impacc) == FIELD_POP_VEC_LEN then
                    impacc(FIELD_ACC_P) -> acc_p;
                    impacc(FIELD_CONV_P) -> conv_p
                endif
            endif
        endif;
        type_spec

    else
        mishap(spec, 1, 'UNKNOWN TYPE SPEC')

    endif -> type_spec;

    cons_field_spec(false, type_spec, offs, false) -> fdesc;
    unless acc_p == [] and conv_p == [] then
        lvars tmp = fdesc;
        initv(FIELD_POP_VEC_LEN) -> fdesc;
        move_subvector(1, tmp, 1, fdesc, FIELD_VEC_LEN);
        acc_p -> fdesc(FIELD_ACC_P);
        conv_p -> fdesc(FIELD_CONV_P)
    endunless;

    ;;; return field descriptor if not allocating field in a structure
    returnunless(isfield) (fdesc);

    ;;; align bitoffs for field type
    eval_type_spec(type_spec) -> type_spec;
    lvars   ftype = if exval_pad then T_EXPTR else type_spec endif,
            fsize = t_bitoffset(ftype,false),
            size = t_bitoffset(type_spec,false);

    lvars (n_offset, n_list) =
        align_field(bitoffs, ftype, base_type_struct_bitalign, field_list);

    ;;; check where to insert key field
    if k_start and bitoffs <= k_start and k_start < n_offset+fsize then
        ;;; insert key
        insert_key();
        align_field(bitoffs, ftype, base_type_struct_bitalign, field_list)
    else
        n_offset, n_list
    endif -> (bitoffs, field_list);

#_IF DEF BIG_ENDIAN     ;;; pad before
    if exval_pad then
        gen_pad_field(fsize-size, bitoffs, type_spec, field_list)
                                    -> (bitoffs, field_list)
    endif;
#_ENDIF

    ;;; add field to struct list
    bitoffs -> fdesc(FIELD_OFFSET);
    field_num+1 ->> field_num -> fdesc(FIELD_IDENT);
    fdesc :: field_list -> field_list;
    ;;; step bitoffs
    bitoffs + size -> bitoffs;

#_IF not(DEF BIG_ENDIAN)    ;;; pad after
    if exval_pad then
        gen_pad_field(fsize-size, bitoffs, type_spec, field_list)
                                    -> (bitoffs, field_list)
    endif;
#_ENDIF

    max(ftype && t_BASE_TYPE, struct_type) -> struct_type
enddefine;

define pop_struct_spec(spec, exptr);
    lvars spec, exptr, recstruct, speclist, create_substr, procedure prop;

    lconstant procedure (
        struct_spec     = newmapping([], 16, false, false),
        ext_struct_spec = newmapping([], 16, false, false)
        );

    exptr && 16:FF -> exptr;
    if exptr /== 0 then ext_struct_spec else struct_spec endif -> prop;
    returnif(prop(spec) ->> recstruct) (recstruct);

    spec -> speclist;
    false -> create_substr;
    if exptr /== 0 then
        ;;; external access -- outer struct is external pointer
        [full %identfn(%"|", spec%)(% "exptr", false %)%] -> speclist
    elseunless islist(spec) then
        ;;; vectorclass
        true -> create_substr;
        ;;; "uword" for V_LENGTH
        [uword %if ispair(spec) then spec else conspair(spec, false) endif%]
                                                    -> speclist
    endif;

    make_record_struct(speclist, create_substr, true) ->> prop(spec)
enddefine;

define get_ident_field_spec(fid, spec_list) -> spec;
    lvars spec, fid, spec_list;
    fast_for spec in spec_list do
        quitif(spec(FIELD_IDENT) == fid)
    endfor
enddefine;

define $- popc_field_spec_info(spec) -> (valspec, bitsize);
    lvars   spec, valspec, bitsize, type_code, vtype,
            fdesc = spec_fdesc(spec, false, [], [], false, false);
    eval_type_spec(fdesc) -> type_code;
    t_bitoffset(type_code, false) -> bitsize;
    if bitsize >= HUGE then false -> bitsize endif;
    if datalength(fdesc) == FIELD_VEC_LEN then
        if type_code &&/=_0 t_IS_STRUCT then
            spec
        else
            type_code && tv_VAL_TYPE -> vtype;
            if vtype == tv_FULL then
                "full"
            elseif vtype == tv_EXPTR then
                "exptr"
            elseif vtype == tv_EXVAL then
                "exval"
            elseif vtype == tv_FLOAT then
                if type_code && t_BASE_TYPE == t_DOUBLE then
                    "ddecimal"
                else
                    "decimal"
                endif
            else
                if type_code &&/=_0 t_PINT then
                    -(POPINT_BITS+1)
                else
                    bitsize, if vtype == tv_SIGNED then negate() endif
                endif
            endif
        endif
    else
        ;;; contains conv/acc procedures
        false
    endif -> valspec
enddefine;

endlblock;


;;; --- STRUCTURE SYNTAX -------------------------------------------------

define structs_prop = newassoc([]) enddefine;

define lconstant is_non_syntax_word(item);
    lvars item, idprops;
    if isword(item) and isdeclared(item) then
        identprops(item) -> idprops;
        not( isword(idprops) and isstartstring('syntax', idprops) )
    else
        true
    endif
enddefine;

define lconstant get_struct_spec() -> struct_spec;
    lvars   struct_name = false, field_list, ptr_offset, struct_type,
            offset, struct_flags, type, type_spec, type_code,
            struct_spec, repcount, invbit = 0, has_full;
    dlocal  pop_autoload = false;   ;;; want macros, but no autoloading

    define lconstant struct_error(n, mess);
        lvars n, mess;
        mishap(consword('(IN struct '><struct_name<>')'), n+1,
                        'ERROR IN struct (' <> mess <> ')');
    enddefine;

    lconstant ambig_ptr = 'ambiguous struct pointers specified';

    lconstant procedure read_struct;

    define lconstant read_fields(ptr_offset, offset)
                            -> item -> ptr_offset -> offset -> field_list;
        lvars   offset, item, type, type_spec, type_code, field_list = [],
                repcount, this_spec, ptr_offset;

        define lconstant do_ptr();
            if ptr_offset then
                struct_error(0, ambig_ptr)
            else
                "offset" -> ptr_offset
            endif
        enddefine;

        until (vecitem_check(), itemread() ->> item) == "}" or item == "|" do
            if offset >= HUGE then
                struct_error(0, 'variable size item not last in struct')
            elseif item == ">->" then
                ;;; where the pointer will point to
                do_ptr(), nextloop
            elseif item == "{" then
                (read_struct(ptr_offset, offset) -> ptr_offset -> offset)
                                        nc_<> field_list -> field_list;
                nextloop
            endif;

            ;;; read item type
            item :: proglist -> proglist;
            read_type_spec([], identfn, struct_error) -> type_spec -> ;
            eval_type_spec(type_spec) -> type_code;
            if type_code && tv_VAL_TYPE == tv_FULL and not(has_full) then
                true -> has_full
            endif;

            ;;; align offset for field type
            align_field(offset, type_code, base_type_struct_bitalign,
                                        field_list) -> field_list -> offset;

            ;;; overall struct type
            type_code && t_BASE_TYPE -> type;
            max(type, struct_type) -> struct_type;
            (type_code && t_INVERTED) fi_|| invbit -> invbit;

            ;;; read field names and repeat counts
            repeat
                itemread() -> item;
                if offset >= HUGE then
                    struct_error(0, 'variable size item not last in struct')
                elseif item == ">->" then
                    ;;; where the pointer will point to
                    do_ptr(), nextloop
                elseunless is_non_syntax_word(item)
                and not(named_type_spec(item)) then
                    struct_error(item, 1, 'invalid item for field name')
                endif;

                if ptr_offset == "offset" then offset -> ptr_offset endif;

                ;;; hack to distinguish pop structures ...
                if item == "KEY" then "pop" -> has_full endif;
                1 -> repcount;
                read_array_size(type_spec, true, struct_error) -> this_spec;
                if this_spec /== type_spec then
                    ;;; array field
                    cont(back(this_spec)) -> repcount
                endif;

                ;;; add field to struct list
                cons_field_spec(item, this_spec, offset, false)
                                                :: field_list -> field_list;

                ;;; compute field size and step offset
                t_bitoffset(type_code,false) * repcount + offset -> offset;

                quitif((itemread() ->> item) == ";");
                unless item == "," then
                    struct_error(item, 1, 'field separator expected')
                endunless
            endrepeat
        enduntil
    enddefine;  /* read_fields */

    define lconstant read_struct(ptr_offset, startoffset)
                                -> new_ptr_offset -> offset -> field_list;
        lvars startoffset, offset = startoffset, item, field_list = [],
            ptr_offset, new_ptr_offset = termin, nptr;
        repeat
            (max(read_fields(ptr_offset, startoffset) -> item -> nptr, offset)
                            -> offset) nc_<> field_list -> field_list;
            if new_ptr_offset == termin then
                nptr -> new_ptr_offset
            elseif nptr /== new_ptr_offset then
                struct_error(0, ambig_ptr)
            endif;
            quitif(item == "}");
            struct_flags || M_STRUC_VARIFORM -> struct_flags
        endrepeat
    enddefine;

    vecitem_check();
    unless nextitem() == "{" then
        itemread() -> struct_name;
        vecitem_check()
    endunless;
    if nextitem() == "{" then
        ;;; read new struct
        itemread() -> ;
        false -> has_full;
        0 -> struct_flags;
        t_BIT -> struct_type;
        read_struct(false, 0) -> ptr_offset -> offset -> field_list;
        unless ptr_offset then 0 -> ptr_offset endunless;
        cons_struct(ptr_offset, offset, field_list, struct_type,
                     struct_flags, has_full, invbit, struct_error)
                                -> struct_spec;
        if struct_name then struct_spec -> structs_prop(struct_name) endif
    elseunless structs_prop(struct_name) ->> struct_spec then
        struct_error(0, 'unknown struct')
    endif
enddefine;  /* get_struct_spec */


define syntax Syspop$- deftype;
    lvars typename, item, type_spec, spec;
    dlocal pop_autoload = false;

    define lconstant deftype_err(n, mess);
        lvars n, mess;
        mishap(n, 'ERROR IN deftype (' <> mess <> ')')
    enddefine;

    repeat
        itemread() -> typename;
        unless is_non_syntax_word(typename) then
            deftype_err(typename, 1, 'invalid item for type name')
        elseif (itemread() ->> item) /== "=" then
            deftype_err(item, 1, '= expected after type name')
        else
            read_type_spec([, ;], pop11_need_nextitem, deftype_err)
                                                    -> (item, type_spec);
            if named_type_spec(typename) ->> spec then
                if spec /= type_spec then
                    deftype_err(typename, 1, 'redefining type name')
                endif
            else
                type_spec -> named_type_spec(typename)
            endif;
            quitif(item == ";")
        endunless
    endrepeat;
    ";" :: proglist -> proglist
enddefine;

define syntax Syspop$- =>>;
    vecitem_check();
    pop11_need_nextitem("{") -> ;
    nonsyntax {();
    sysCALLQ(procedure(v) -> v;
                lvars v;
                struct_init_key -> data_key(v)
             endprocedure);
enddefine;

define lconstant struct_err(mess);
    lvars mess;
    mishap((), 'struct INITIALISATION ERROR (' <> mess <> ')')
enddefine;

define match_struct_init(init, struct_spec, base_p, pop, popacc, top)
                                                                -> offset;
    lvars   n, init, size, spec_list, struct_spec, field_spec, pop, popacc,
            fid, type_spec, offset, top, procedure base_p;
    dlvars  _0 = Syspop$- _int(0);      ;;; guaranteed to be same every time
    dlocal  popdprecision = true;

    define lconstant match_spec_init(init, type_spec);
        lvars n, type_spec, type, init, size, procedure sub_p;

        if isref(type_spec) then
            ;;; pointer
            pointer_equiv_type(type_spec) -> type_spec
        endif;
        if isintegral(type_spec) then
            ;;; call procedure arg on field
            base_p(type_spec, init);
            ;;; update offset
            t_bitoffset(type_spec, struct_err) + offset -> offset
        elseif ispair(type_spec) then
            ;;; array type/array field
            if isref(back(type_spec) ->> size) then
                fast_cont(size) -> size
            endif;
            eval_type_spec(front(type_spec)) -> type_spec;
            if init == _0 then
                if size >= HUGE then
                    struct_err(0, '=>> {...} needed for array field')
                endif;
                fast_repeat size times
                    match_spec_init(_0, type_spec)
                endfast_repeat
            else
                unless pop or data_key(init) == struct_init_key then
                    struct_err(0, '=>> {...} needed for array field')
                endunless;
                if size < HUGE and datalength(init) /== size then
                    struct_err(0, '=>> {...} wrong length for array field')
                endif;
                datalength(init) -> size;
                if pop then
                    popacc      ;;; the fast subscriptor
                else
                    fast_subscrv
                endif -> sub_p;
                fast_for n to size do
                    match_spec_init(sub_p(n, init), type_spec)
                endfor
            endif
        elseif isvector(type_spec) and type_spec(VECDESC_TYPE) == "struct" then
            ;;; struct vector
            match_struct_init(init, type_spec, base_p, false, false, false)
                                            + offset -> offset
        else
            struct_err(type_spec, init, 2, 'illegal initialisation for field')
        endif
    enddefine;  /* match_spec_init */

    ;;; match struct vector
    if struct_spec(STRUC_FLAGS) &&/=_0 M_STRUC_VARIFORM then
        struct_err(0, 'can\'t init variform struct')
    endif;
    struct_spec(STRUC_FIELD_LIST) -> spec_list;
    struct_spec(STRUC_TYPE_SPEC) -> struct_spec;

    unless init == _0 then
        unless pop or data_key(init) == struct_init_key then
            struct_err(0, '=>> {...} needed for struct field')
        endunless;
        datalength(init) -> size;
        1 -> n
    endunless;

    0 -> offset;                ;;; bit offset of fields generated

    fast_for field_spec in spec_list do
        field_spec(FIELD_TYPE_SPEC) -> type_spec;
        if type_spec == "pointer" then
            if top then base_p("pointer", 0) endif;
            nextloop
        endif;
        nextif(t_offset(eval_type_spec(type_spec), false) == 0);
        if field_spec(FIELD_IDENT) ->> fid then
            ;;; explicit field
            if iskey(pop) then
                ;;; doing pop recordclass -- popacc is the vector of access
                ;;; procedures
                if fid == "KEY" then
                    pop
                else
                    popacc(n)(init), n+1 -> n
                endif
            elseif init == _0 then
                _0
            else
                if n > size then
                    struct_err(0, '=>> {...} wrong length for struct field')
                endif;
                f_subv(n, init), n+1 -> n
            endif
        else
            ;;; padding field
            _0
        endif;
        match_spec_init((), type_spec)
    endfor;
    if init /== _0 and n <= size then
        struct_err(0, '=>> {...} wrong length for struct field')
    endif;

    ;;; ensure aligned
    align_field(offset, struct_spec, base_type_struct_bitalign, false)
                                                -> type_spec -> offset;
    if type_spec then
        match_spec_init(_0, type_spec)
    endif
enddefine;  /* match_struct_init */


define lconstant free_struct_cons(init, struct_spec) -> v;
    lvars init, struct_spec, v;

    define lconstant do_field(type_code, init);
        lvars type, vtype, init, type_code, dk, labinfo;

        returnif(type_code == "pointer") (#_< conspair("pointer", 0) >_#);

        type_code && t_BASE_TYPE -> type;
        islabel(init) -> labinfo;
        data_key(init) -> dk;
        if dk /== struct_init_key
#_IF not(DEF asm_outdouble)
        and (type /== t_DOUBLE or type_code == T_DFLOAT)
#_ENDIF
        then
            type_code && tv_VAL_TYPE -> vtype;
            if vtype == tv_FULL then
                not(fast_lmember(dk, #_< [  ^dfloat_key
                                            ^popc_pointer_key
                                            ^popc_pointer_into_key] >_# ))
            elseif vtype == tv_FLOAT then
                if type == t_DOUBLE then
                    dk == dfloat_key
                else
                    labinfo and labinfo &&/=_0 LAB_SFLOAT
                endif
            elseif type < t_INT then
                _intval(init)
                or (type /== t_BIT and labinfo
                    and labinfo &&/=_0 LAB_LITERAL
                    and labinfo &&=_0 LAB_SFLOAT)
            else
                true
            endif
        else
            false
        endif;
        unless () then
            struct_err(type_code, init, 2, 'illegal initialisation for field')
        endunless;

        ;;; result is a pair -- see :GEN free_struct
        conspair(type_code, init)
    enddefine;

    {% match_struct_init(init, struct_spec, do_field, false, false, true) -> %}
                                                        -> v;

    if struct_spec(STRUC_FLAGS) &&=_0 M_STRUC_POP then
        struct_err(0, 'outer struct must be a pop structure')
    endif;
    if struct_spec(STRUC_FLAGS) &&/=_0 M_STRUC_DALIGN then
        ;;; needs padding to double align (mark with outer ref)
        consref(v) -> v
    endif;
    free_struct_key -> data_key(v)
enddefine;

define syntax Syspop$- struct;
    lvars spec = get_struct_spec();
    if nextitem() == "=>>" then
        ;;; initialising struct
        itemread() -> ;
        nonsyntax Syspop$- =>>();
        sysPUSHQ(spec);
        sysCALLQ(free_struct_cons)
    endif
enddefine;

define cons_free_struct(struct_name, init_vec);
    lvars struct_name, struct_spec, init_vec;
    if structs_prop(struct_name) ->> struct_spec then
        struct_init_key -> data_key(init_vec);
        free_struct_cons(init_vec, struct_spec)
    else
        mishap(struct_name, 1, 'UNKNOWN STRUCT NAME FOR cons_free_struct')
    endif
enddefine;

    /*  Currently only used in vmsdefs.ph to define RMS field names
    */
define Syspop$- popc_def_field_spec(field_name, type_name, offset);
    lvars field_name, type_name, offset, type_spec;
    if named_type_spec(type_name) ->> type_spec then
        cons_field_spec(field_name, type_spec, offset, T_WORD)
                                            -> field_name_spec(field_name)
    else
        mishap(type_name, 1, 'INVALID TYPE FOR popc_def_field_spec')
    endif
enddefine;

define field_## = newactproperty([], 4, false, true,
    procedure(field_name, prop) -> index;
        lvars spec, index, field_name, prop;
        if field_name_spec(field_name) ->> spec then
            spec(FIELD_OFFSET) /
                t_offset(eval_type_spec(non_array_spec(spec(FIELD_TYPE_SPEC))),
                            false) -> index;
            if isinteger(index) then
                index -> prop(field_name);
                return
            endif
        endif;
        mishap(field_name, 1, 'field_##: UNKNOWN/INVALID FIELD NAME')
    endprocedure)
enddefine;

define lstackmem_data = newproperty([], 4, false, "tmparg") enddefine;

define syntax Syspop$- lstackmem;
    lvars name, type_spec, offs_var, type_code;
    dlocal pop_autoload = false;

    define lconstant sf_field =
        newassoc([
            [^t_BIT         SF_LOCALS_BIT]
            [^t_BYTE        SF_LOCALS_B]
            [^t_SHORT       SF_LOCALS_S]
            [^t_INT         SF_LOCALS_I]
            [^t_DOUBLE      SF_LOCALS_D]
        ])
    enddefine;

    define lconstant err(n, mess);
        lvars n, mess;
        mishap(n, 'ERROR IN lstackmem (' <> mess <> ')')
    enddefine;

    repeat
        read_type_spec([], identfn, err) -> (, type_spec);
        readitem() -> name;
        unless is_non_syntax_word(name) and not(named_type_spec(name))
        and datalength(name) fi_>= 2 and name(1) == `_` then
            err(name, 1, 'invalid variable name')
        endunless;
        eval_type_spec(read_array_size(type_spec, false, err)) -> type_code;
        t_offset(type_code, err) -> ;

        "__" <> name -> offs_var;
        sysLVARS(offs_var, 0);
        conspair(type_code, base_type_unit_bitsize(type_code && t_BASE_TYPE))
                            -> lstackmem_data(sys_current_ident(offs_var));

        sysLCONSTANT(name, "macro");
        sysPASSIGN(
            [( _sp()@ % sf_field(type_code && t_BASE_TYPE) % { ^offs_var } )],
            name);

        quitif(pop11_need_nextitem([, ;]) == ";")
    endrepeat;

    ";" :: proglist -> proglist
enddefine;


;;; --- POINTER AND FIELD OPERATIONS ------------------------------------

define type_error(n, ms);
    lvars n, ms;
    mishap(n, 'INVALID FIELD TYPE FOR ! OR @ (' <> ms <> ')')
enddefine;


section Syspop;

lvars procedure (p_PUSHQ, p_CALL);

define lconstant call_ptr_op(type_code, name, flush, err);
    lvars type, type_code, name, flush, procedure err;
    if (type_code && t_BASE_TYPE ->> type) == t_BIT then
        err(0, 'illegal bit pointer operation')
    endif;
    p_PUSHQ(_int(type));
    if flush then
        p_CALL(name)
    else
        sysCALL -> pop_expr_inst;
        name -> pop_expr_item
    endif
enddefine;

define lconstant mult_by(n);
    lvars pow, n;
    returnif(n == 1);
    if is_power2(n) ->> pow then
        p_PUSHQ(_int(pow));
        p_CALL("_shift")
    else
        p_PUSHQ(_int(n));
        p_CALL("_mult")
    endif
enddefine;

define lconstant div_by(n);
    lvars pow, n;
    returnif(n == 1);
    if is_power2(n) ->> pow then
        p_PUSHQ(_int(-pow));
        p_CALL("_shift")
    else
        p_PUSHQ(_int(n));
        p_CALL("_divq")
    endif
enddefine;

define lconstant convert_offset(i_type_code, o_type_code, isindex,
                                                        r_or_t, err);
    lvars   i_mult, o_div, o_offs, i_type_code, o_type_code, isindex, h,
            r_or_t, err;

    base_type_unit_bitsize(i_type_code && t_BASE_TYPE) -> i_mult;
    base_type_unit_bitsize(o_type_code && t_BASE_TYPE) -> o_div;

    if isindex then
        i_mult * t_offset(i_type_code, err) -> i_mult   ;;; index -> offset
    endif;
    if r_or_t then
        ;;; round or truncate
        t_offset(o_type_code, err) -> o_offs;
        o_div * o_offs -> o_div             ;;; = o_bitsize
    endif;
    ;;; reduce i_mult and o_div to lowest common terms
    destratio(i_mult / o_div) -> (i_mult, o_div);

    mult_by(i_mult);        ;;; i_type offset -> bits
    if r_or_t then
        ;;; round or truncate
        unless o_div == 1 or r_or_t == "t" then
            p_PUSHQ(_int(o_div-1));
            p_CALL("_add")
        endunless;
        div_by(o_div);      ;;; bits truncated -> o_type index
        mult_by(o_offs)     ;;; o_type index -> o_type offset
    else
        ;;; straight offset conversion
        div_by(o_div)       ;;; bits -> o_type offset
    endif
enddefine;

define $-Popas$-convert_pointer_code(r_or_t, i_type_code, o_type_code,
                                                        err, p_PUSHQ, p_CALL);
    lvars r_or_t, i_type_code, o_type_code, t, err;
    dlocal p_PUSHQ, p_CALL;

    call_ptr_op(i_type_code, "_ptr_to_offs", true, err);
    if r_or_t and i_type_code &&/=_0 t_INVERTED then
        if r_or_t == "t" then "r" else "t" endif -> r_or_t
    endif;
    if o_type_code&&t_BASE_TYPE == t_BIT then
        T_BIT_POINTER
    else
        o_type_code
    endif -> t;
    convert_offset(i_type_code, t, false, r_or_t, err);
    if i_type_code&&t_INVERTED /== o_type_code&&t_INVERTED then
        p_PUSHQ(_int(t_offset(o_type_code, err)));
        p_CALL(if o_type_code &&/=_0 t_INVERTED then "_add" else "_sub" endif)
    endif;
    call_ptr_op(t, "_offs_to_ptr", true, err)
enddefine;

define lconstant flush_instr();
    pop_expr_inst(pop_expr_item);
    pop11_FLUSHED -> pop_expr_inst
enddefine;


define lconstant compile_field_access(!_or_@, inc_dec, doing_@~) -> type_code;
    lvars   !_or_@, inc_dec, type_code, ind_or_off, field_spec,
            field_name, _field_offset, i_type_code, r_or_t, post_op,
            _type_code, w, +_or_-, ptr_diff, pointer, bitfield,
            doing_@~, cvtptr, save_pop_autoload = pop_autoload;
    dlvars  _0 = _int(0);
    dlocal  pop_autoload, p_PUSHQ = sysPUSHQ, p_CALL = sysCALL;

    define lconstant err(n, ms);
        lvars n, ms;
        mishap(n, 'ERROR IN ! OR @ (' <> ms <> ')')
    enddefine;

    define lconstant read_ext_type_spec(terminators) -> type_code -> r_or_t
                                                        -> terminator;
        lvars terminator, r_or_t = false, terminators, type_code;
        dlocal pop_autoload = false;    ;;; want macros, but no autoloading
        read_type_spec("." :: terminators, pop11_need_nextitem, err)
                                        -> type_code -> terminator;
        if terminator == "." then
            itemread() -> r_or_t;
            unless r_or_t == "r" or r_or_t == "t" then
                err(r_or_t, 1, '.r or .t expected after type spec')
            endunless;
            pop11_need_nextitem(terminators) -> terminator
        endif;
    enddefine;

    pop_expr_inst(pop_expr_item);
    !_or_@ /== "__@@" -> pointer;

    false -> pop_autoload;      ;;; want macros, but no autoloading
    itemread() -> field_name;

    if field_name == "(" then
        ;;; pointer field_spec
        if (read_ext_type_spec(#_< [%")"% ->] >_#) ->> i_type_code
                                                    -> type_code -> r_or_t)
            == "->" then
            ;;; alternate pointer spec
            unless pointer then
                err(0, 'unwanted pointer type specifier')
            endunless;
            read_type_spec(#_< [%")"%] >_#, pop11_need_nextitem, err)
                                                    -> type_code ->
        endif;
        _0 -> _field_offset
    else
        ;;; must be structure field
        unless field_name_spec(field_name) ->> field_spec then
            err(field_name, 1, 'unknown structure field')
        endunless;
        non_array_spec(field_spec(FIELD_TYPE_SPEC)) -> type_code;
        field_spec(FIELD_STRUCT_TYPE) -> i_type_code;
        if isintegral(field_spec(FIELD_OFFSET) ->> _field_offset) then
            _int(_field_offset) -> _field_offset
        endif;
        false -> r_or_t
    endif;

    eval_type_spec(type_code) -> type_code;
    eval_type_spec(i_type_code) -> i_type_code;
    type_code&&t_BASE_TYPE == t_BIT -> bitfield;

    lconstant ITYPE = t_BASE_TYPE||t_INVERTED;
    pointer and (r_or_t or i_type_code&&ITYPE /== type_code&&ITYPE) -> cvtptr;
    if cvtptr and not(doing_@~) then
        ;;; input pointer conversion
        convert_pointer_code(r_or_t, i_type_code, type_code, err,
                                                        p_PUSHQ, p_CALL)
    endif;

    save_pop_autoload -> pop_autoload;  ;;; can autoload now

    ;;; see if index, offset or pointer difference specified
    false -> ind_or_off;
    if pop11_try_nextitem("-") then "_-" else "_+" endif -> +_or_-;
    if (listitem_check(), pop11_try_nextitem("[")) then
        ;;; index specified
        "_ind" -> ind_or_off;
        if pop11_comp_expr_to( #_< [%"]"% |] >_# ) == "|" then
            read_ext_type_spec(#_< [%"]"%] >_#) -> i_type_code -> r_or_t ->;
            eval_type_spec(i_type_code) -> i_type_code;
            convert_offset(i_type_code, type_code, true, r_or_t, err);
            "_off" -> ind_or_off
        endif;
    elseif (vecitem_check(), pop11_try_nextitem("{")) then
        ;;; offset or pointer difference specified
        "_off" -> ind_or_off;
        pop11_comp_expr_to( #_< [%"}"% , |] >_# ) -> w;
        false -> ptr_diff;
        if w == "," then
            ;;; pointer difference
            pop11_comp_expr_to( #_< [%"}"% |] >_# ) -> w;
            true -> ptr_diff
        endif;
        type_code -> i_type_code;
        if w == "|" then
            read_ext_type_spec(#_< [%"}"%] >_#) -> i_type_code -> r_or_t ->;
            eval_type_spec(i_type_code) -> i_type_code
        endif;
        if ptr_diff then
            call_ptr_op(i_type_code, "_ptr_sub", true, err);
            if i_type_code &&/=_0 t_INVERTED then sysCALL("_negate") endif
        endif;
        if w == "|" then
            convert_offset(i_type_code, type_code, false, r_or_t, err)
        endif;
    elseif +_or_- == "_-" then
        "-" :: proglist -> proglist;        ;;; no index/offset, put back "-"
        "_+" -> +_or_-
    endif;

    ;;; see if autoincrement
    if pop11_try_nextitem("++") then
        if inc_dec then
            err(0, '-- and ++ specified together')
        endif;
        "++" -> inc_dec
    endif;

    define lconstant invert();
        ;;; negate field offset and index or offset
        _0 _sub _field_offset -> _field_offset;
        if +_or_- == "_-" then "_+" else "_-" endif -> +_or_- ;
        if inc_dec then
            if inc_dec == "++" then "--" else "++" endif -> inc_dec
        endif
    enddefine;

    if type_code &&/=_0 t_INVERTED then
        type_code &&~~ t_INVERTED -> type_code;
        if pointer then
            invert();
            if not(inc_dec) and !_or_@ == "__!" then
                _field_offset _sub _int(t_offset(type_code, err))
                                                    -> _field_offset
            endif
        endif
    endif;

    if doing_@~ then invert() endif;

    _int(type_code) -> _type_code;
    if inc_dec then
        if bitfield then
            err(0, '-- or ++ specified with bit field/pointer')
        endif;
        if ind_or_off or _field_offset /= _0 then
            if pointer then
                !_or_@, "__@",
            else
                "__@", "__@@"
            endif -> !_or_@ -> post_op
        else
            sysPUSHQ(_type_code);
            !_or_@ <> inc_dec -> pop_expr_item;
            sysCALL -> pop_expr_inst;
            return
        endif
    endif;

    ;;; plant access subroutine call
    if bitfield and !_or_@ == "__@"
    and (ind_or_off or _field_offset /= _0) then
        err(0, 'illegal use of @ with bit field/pointer')
    endif;
    unless ind_or_off then
        sysPUSHQ(_0);           ;;; default 0 offset
        "_off" -> ind_or_off
    endunless;
    sysPUSHQ(_field_offset);
    sysPUSHQ(_type_code);
    !_or_@ <> +_or_- <> ind_or_off -> pop_expr_item;

    if inc_dec then
        sysCALL(pop_expr_item);
        sysPUSHQ(_type_code);
        post_op <> inc_dec -> pop_expr_item
    endif;

    sysCALL -> pop_expr_inst;

    if cvtptr and doing_@~ then
        ;;; output pointer conversion
        flush_instr();
        convert_pointer_code(r_or_t, type_code, i_type_code, err,
                                                        p_PUSHQ, p_CALL)
    endif
enddefine;      /* compile_field_access */


define syntax 2 !;
    compile_field_access("__!", false, false) -> ;
enddefine;

define syntax 2 --!;
    compile_field_access("__!", "--", false) -> ;
enddefine;

define syntax 2 @;
    compile_field_access("__@", false, false) -> ;
    flush_instr()
enddefine;

define syntax 2 --@;
    compile_field_access("__@", "--", false) -> ;
    flush_instr()
enddefine;

define syntax 2 @~;
    compile_field_access("__@", false, true) -> ;
    flush_instr()
enddefine;

define syntax @@;
    compile_field_access("__@@", false, false) -> ;
    flush_instr()
enddefine;

define syntax --@@;
    compile_field_access("__@@", "--", false) -> ;
    flush_instr()
enddefine;

    /* Non-opener syntax words */
constant syntax (
    |   = pop_undef,
    ++  = pop_undef,
    );


define lconstant do_##(inc_dec);
    lvars type_code, inc_dec;
    dlocal p_PUSHQ = sysPUSHQ, p_CALL = sysCALL;

    define lconstant err(ms);
        lvars ms;
        mishap('ERROR IN ## (' <> ms <> ')')
    enddefine;

    compile_field_access("__@@", inc_dec, false) -> type_code;
    flush_instr();
    div_by(t_offset(type_code, err))
enddefine;

define syntax ##;
    do_##(false)
enddefine;

define syntax --##;
    do_##("--")
enddefine;

define lconstant ptr_compare(sr_name, inv_sr_name);
    lvars type_code, sr_name, inv_sr_name;
    dlocal p_PUSHQ = sysPUSHQ, p_CALL = sysCALL;

    define lconstant err(ms);
        lvars ms;
        mishap('ERROR IN <@, ETC (' <> ms <> ')')
    enddefine;

    pop_expr_inst(pop_expr_item);
    pop11_need_nextitem("(") -> ;
    read_type_spec(#_< [%")"%] >_#, pop11_need_nextitem, err) -> type_code ->;
    eval_type_spec(type_code) -> type_code;
    pop11_comp_prec_expr(INTERNAL_OP_PREC 6, false) -> ;
    call_ptr_op(type_code,  if type_code &&/=_0 t_INVERTED then
                                inv_sr_name
                            else
                                sr_name
                            endif, false, err)
enddefine;

constant syntax 6 (
    <@  = ptr_compare(% "_ptr_lt",   "_ptr_gr" %),
    <=@ = ptr_compare(% "_ptr_lteq", "_ptr_greq" %),
    >@  = ptr_compare(% "_ptr_gr",   "_ptr_lt" %),
    >=@ = ptr_compare(% "_ptr_greq", "_ptr_lteq" %),
    );


;;; --- EXECUTE-LEVEL POINTER OPS ------------------------------------------

define lconstant _@_op(ptr, offs_index, offs, type_code, isindex, +_or_-);
    lvars   ptr, offs_index, offs, type_code, isindex, scale,
            item, lab, pointers, procedure (+_or_-, _addp);
    if isindex then
        _int(t_offset(_pint(type_code),type_error)) _mult offs_index
                            -> offs_index
    endif;
    if +_or_- == nonop + then nonop _add else nonop _sub endif -> _addp;
    _pint(_addp(offs_index, offs)) -> offs;
    if islabel(ptr) ->> lab then
        if _intval(ptr) then
            _addp(ptr, _int(offs))
        elseif +_or_- == nonop + then
            ptr label_+ offs
        else
            ptr label_- offs
        endif;
        return
    elseif issimple(ptr) then
        mishap(ptr, 1, 'POINTER NEEDED FOR @')
    endif;
    +_or_-( if (data_key(ptr) ->> lab) == popc_pointer_key
            or lab == popc_pointer_into_key then
                destpair(ptr)
            else
                ptr, 0
            endif, offs) -> offs -> item;
    if isstring(item) then
        ;;; allow labels to be generated in the middle of strings
        unless pointers_into(item) ->> pointers then
            newproperty([], 4, false, false) ->> pointers
                                                    -> pointers_into(item)
        endunless;
        lvars sub = offs - field_##("V_BYTES") + 1;
        unless 1 <= sub and sub <= datalength(item)+1 then
            mishap(item, offs, 2, 'INVALID POINTER INTO STRING')
        endunless;
        unless pointers(sub) ->> ptr then
            conspair(item, offs) ->> pointers(sub) -> ptr;
            popc_pointer_into_key -> data_key(ptr)
        endunless
    else
        conspair(item, offs) -> ptr;
        popc_pointer_key -> data_key(ptr)
    endif;
    ptr
enddefine;

define lconstant _@@_op(offs_index, offs, type_code, isindex, +_or_-);
    lvars offs_index, offs, type_code, isindex, +_or_-;
    _@_op(_int(0), offs_index, offs, type_code, isindex, +_or_-)
enddefine;

define lconstant _--@++_op(ptr, type_code, +_or_-);
    lvars ptr, type_code, +_or_-;
    _@_op(ptr, _int(1), _int(0), type_code, true, +_or_-)
enddefine;

define lconstant _--@@++_op(type_code, +_or_-);
    lvars type_code, +_or_-;
    _--@++_op(_int(0), type_code, +_or_-)
enddefine;

constant procedure (
    __@_+_ind   = _@_op(% true,  nonop + %),
    __@_-_ind   = _@_op(% true,  nonop - %),
    __@_+_off   = _@_op(% false, nonop + %),
    __@_-_off   = _@_op(% false, nonop - %),
    __@@_+_ind  = _@@_op(% true,  nonop + %),
    __@@_-_ind  = _@@_op(% true,  nonop - %),
    __@@_+_off  = _@@_op(% false, nonop + %),
    __@@_-_off  = _@@_op(% false, nonop - %),

    __@++       = _--@++_op(% nonop + %),
    __@--       = _--@++_op(% nonop - %),
    __@@++      = _--@@++_op(% nonop + %),
    __@@--      = _--@@++_op(% nonop - %),
    );

define _ptr_to_offs(ptr, type) -> offs;
    lvars ptr, offs, type;
    ptr -> offs
enddefine;

define _offs_to_ptr(offs, type) -> ptr;
    lvars ptr, offs, type;
    offs -> ptr
enddefine;

define lconstant get_!_index(offs_index, offs, type_code, isindex, +_or_-);
    lvars offs_index, offs, type_code, isindex, procedure +_or_-;
    unless (_pint(type_code) ->> type_code) == T_FULL then
        mishap(0, 'ILLEGAL USE OF !')
    endunless;
    if isindex then
        _int(t_offset(type_code, type_error)) _mult offs_index -> offs_index
    endif;
    +_or_-(0, _pint(_add(offs_index, offs))) / t_offset(type_code, false) - 1
enddefine;

define lconstant _!_op();
    lvars index, item;
    get_!_index() -> index -> item;
    fast_subscrv(index, item)
enddefine;
;;;
define updaterof _!_op();
    lvars index, item;
    get_!_index() -> index -> item;
    -> fast_subscrv(index, item)
enddefine;

constant procedure (
    __!_+_ind   = _!_op(% true,  nonop + %),
    __!_-_ind   = _!_op(% true,  nonop - %),
    __!_+_off   = _!_op(% false, nonop + %),
    __!_-_off   = _!_op(% false, nonop - %),

    __!++,
    __!--,

    _ptr_gr,
    _ptr_greq,
    _ptr_lt,
    _ptr_lteq,
    _ptr_sub,
    );


;;; --- OTHER SYSPOP SYNTAX/MACROS ------------------------------------------

define lconstant read_extern_name() -> name;
    lvars name = itemread();
    dlocal pop_autoload = false;
    if nextreaditem() == "/" then
        ;;; name is language name prefix
        readitem() -> ;
        consstring(#| explode(name), `/`, explode(readitem()) |#) -> name
    endif;
    if nextreaditem() == ":" then
        ;;; following item is type ('data' etc)
        readitem() -> ;
        consstring(#| explode(name), `:`, explode(readitem()) |#) -> name
    endif
enddefine;

define syntax _extern;
    lvars   xpdr, count, item, routine, alist, indir = false, nointr = false,
            isweak = false, old_autoload = pop_autoload,
                        sign_extend = false;
    dlocal  pop_autoload = false;

    if islist(nextitem()) then
        [% "[", dl(itemread()), "]" %] nc_<> proglist -> proglist
    endif;
    if nextitem() == "[" then
        listread() -> alist;
        if fast_lmember("INDIR", alist) then
            ;;; indirect thru identifier
            true -> indir
        endif;
        if fast_lmember("NI", alist) then
            ;;; ignore EINTR in Unix
            true -> nointr
        endif;
        if fast_lmember("WEAK", alist) then
            ;;; weak symbol in VMS
            true -> isweak
        endif;
                if fast_lmember("SE", alist) then
                        true -> sign_extend
                endif
    endif;

    read_extern_name() -> xpdr;

    if indir then
        unless isword(xpdr) then
            mishap(xpdr, 1, 'IDENTIFIER NAME NEEDED AFTER _extern[INDIR]')
        endunless
    else
        get_extern_label(xpdr) -> xpdr;
#_IF DEF VMS
        if isweak and not(member(xpdr, popc_vms_extern_weak)) then
            xpdr :: popc_vms_extern_weak -> popc_vms_extern_weak
        endif
#_ENDIF
    endif;
    old_autoload -> pop_autoload;
    unless pop11_try_nextitem("(") then
        ;;; not calling -- error if indirect
        if indir then
            mishap(itemread(), 1, '( EXPECTED FOR _extern[INDIR]');
        else
            ;;; pushing xpdr's address
            sysPUSHQ(xpdr);
            return
        endif
    endunless;

    ;;; calling -- read and count arguments
    0 -> count;
    unless pop11_try_nextitem(")") then
        repeat
            if pop11_try_nextitem([, )]) ->> item then
                sysPUSHQ(_int(0))
            else
                pop11_comp_expr_to([, )]) -> item
            endif;
            count+1 -> count;
            quitif(item == ")");
        endrepeat;
    endunless;
    if pop11_try_nextitem("with_nargs") then
        ;;; ignore the count - use next expression instead
        pop11_comp_prec_expr(INTERNAL_OP_PREC 11, false);   ;;; prec of ->
        sysCALL("_int")
    else
        sysPUSHQ(_int(count));
    endif;

    if indir then sysPUSH(xpdr) else sysPUSHQ(xpdr) endif;

#_IF DEF SIGN_EXTEND_EXTERN

    if sign_extend then
            sysCALL(if nointr then "Call_sys_nointr_se"
                              else "_call_sys_se" endif)
    else
#_ENDIF

#_IF DEF UNIX
        sysCALL(if nointr then "Call_sys_nointr" else "_call_sys" endif)
#_ELSE
        sysCALL("_call_sys")
#_ENDIF

#_IF DEF SIGN_EXTEND_EXTERN
    endif
#_ENDIF

enddefine;

define macro _SVB_OFFS;
    read_input_svb_offset()
enddefine;

define macro EXTERN_NAME -> symbol;
    lvars item, symbol;
    dlocal pop_autoload = false;
    if (itemread() ->> item) /== "(" then
        mishap(item, 1, '( EXPECTED BEFORE EXTERNAL SYMBOL')
    endif;
    read_extern_name() -> symbol;
    if (itemread() ->> item) /== ")" then
        mishap(item, 1, ') EXPECTED AFTER EXTERNAL SYMBOL')
    endif;
    get_extern_label(symbol) -> symbol; ;;; a string
enddefine;

define syntax define_extern_name;
    lvars name, popname, closer = false;
    until closer == ";" or (readitem() ->> name) == ";" or name == termin do
        pop11_need_nextreaditem("=") -> ;
        pop11_comp_expr_to([, ;]) -> closer;

        word_identifier("'_<EXTERN>\s'" <> name, pop_section, "undef")
                                    -> popname;
        sysCONSTANT(popname, 0);
        get_extern_label(name) -> External_dummy_id(isdeclared(popname));
        sysPOP(popname)
    enduntil;
    ";" :: proglist -> proglist
enddefine;


;;; ----------------------------------------------------------------------

#_INCLUDE 'symdefs.p'

    /*  Collect up all Syspop identifiers before cancelling so we can
        resurrect them at top-level when running in syspop mode
    */
constant $-Popas$-syspop_ident_vec =
    {% fast_app_sect_idents(current_section, identfn) %};

    ;;; "undef" here means leave undef records in identifiers alone
section_cancel(current_section, "undef");

endsection;     /* Syspop */


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 15 1996
        Changes to _extern to allow language name and type specs on external
        names.
--- John Gibson, Jan 13 1996
        Excluded "_" as a nonpop ident
--- John Gibson, Aug  2 1995
        Fixed nasty bug in spec_fdesc -- missing parens around an or
        expression inside an and expression.
--- John Gibson, Mar 21 1995
        More changes to type representations
--- John Gibson, Mar  3 1995
        Added T_INT_DOUBLE type etc
--- John Gibson, Oct 22 1994
        Added define_extern_name
--- John Gibson, Oct 20 1994
        Added dummy variables \<ALL_POP_REGISTERS\> etc
--- John Gibson, Jun  1 1994
        Added lstackmem syntax
--- John Gibson, May 10 1994
        Added NI and INDIR flags to _extern and got rid of _e*xtern_indir
        as a separate construct.
--- John Gibson, Jul  1 1993
        Removed macro _ (all uses now replaced with _:)
--- John Gibson, May 19 1993
        Moved in EXTERN_NAME from do_asm.p as a real macro
--- John Gibson, Apr 28 1993
        Fixed bug in make_record_struct (wasn't setting total rec size
        correctly for pop struct with only 1 field)
--- John Gibson, Feb 11 1993
        Made extern_proc set pop_autoload false while reading external
        name.
--- John Gibson, Dec 18 1992
        Added missing declarations for __!_-_ind and __!_-_off
--- John Gibson, Sep  1 1992
        Allowed for fltsingle arg in external func spec
--- John Gibson, Jul 31 1992
        Corrected bug in spec_fdesc
--- Robert John Duncan, Jul 27 1992
        External symbols now processed with -extern_name_translate- defined
        in "asmout.p"
--- John Gibson, Jul 21 1992
        Version 14.21 changes
--- John Gibson, May 12 1990
        Added support for new -class_spec- base types.
--- Rob Duncan, Feb 21 1990
        Changed -extern_proc- not to prefix external names with '_'
        for COFF format object files.
--- John Gibson, Jan  7 1990
        Version 13.7 for new pointers.
--- John Gibson, Dec  4 1989
        Added @~
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, Feb 16 1989
        Made -popliblist- locally [] in -read_struct_spec- so that reading
        field names, etc doesn't go grindingly slow because of trying to
        autoload the names.
--- Rob Duncan, Feb 16 1989
        Added #_IF DEF for SUN386 in -extern_proc-
--- John Gibson, Jan 29 1989
        New version of popc
--- John Gibson, Aug  5 1988
        Added DOUBLE as a proper field type
--- John Gibson, Jun 26 1988
        Made correction to -is_power2-.
--- Roger Evans, May 20 1988
        Added 'with_nargs' option to _extern
--- John Gibson, Feb 26 1988
        Added syntax -declare_strong-
--- John Gibson, Jan 17 1988
        Changes for coping with sections, weakrefs, new format for assembler
        files, etc, etc.
 */
