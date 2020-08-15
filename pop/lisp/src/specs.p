/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/specs.p
 > Purpose:         Lisp type-specifier <-> Pop-11 key & field spec conversion
 > Author:          John Williams, Feb  9 1987 (see revisions)
 > Documentation:
 > Related Files:
 */

lisp_compile_mode;

include sysdefs.ph

section $-lisp;

/* Field spec <-> element type conversions */

lconstant Word_spec_to_int
    = newproperty([ %
#_IF sys_word_bits = 64
                   [ulong    64],
                   [long    -64]
#_ELSE
                   [ulong    32],
                   [long    -32]
#_ENDIF
                   %
                   [uint     32]
                   [int     -32]
                   [uint    32]
                   [pint    -30]
                   [ushort   16]
                   [short   -16]
                   [byte      8]
                   [sbyte    -8]], 8, false, "perm");


lconstant Int_spec_to_word
    = newproperty([ %
#_IF sys_word_bits = 64
                   [64    ulong],
                   [-64    long]
#_ENDIF
                   %
                   [-32     int]
                   [32      uint]
                   [-30     pint]
                   [-16     short]
                   [16      ushort]
                   [-8      sbyte]
                   [8       byte]], 8, false, "perm");


lconstant Spec_to_type
    = newproperty([[full        ^@T]
                   [character   ^@CHARACTER]
                   [1           ^@BIT]
                   [pint        ^@FIXNUM]
                   [decimal     ^@SINGLE-FLOAT]
                   [ddecimal    ^@DOUBLE-FLOAT]
                   [sfloat      ^@SINGLE-FLOAT]
                   [dfloat      ^@DOUBLE-FLOAT]], 8, false, "perm");


lconstant Type_to_spec
    = newproperty([[^@T                 full]
                   [^@STANDARD-CHAR     character]
                   [^@BASE-CHAR         character]
                   [^@EXTENDED-CHAR     character]
                   [^@STRING-CHAR       character]  /* Not strictly needed */
                   [^@CHARACTER         character]
                   [^@BIT               1]
                   [^@FIXNUM            pint]
                   [^@SINGLE-FLOAT      sfloat]
                   [^@DOUBLE-FLOAT      dfloat]], 8, false, "perm");


define spec_->_etype(spec);
    lvars type, int;
    if (Spec_to_type(spec) ->> type) then
         type
    elseif (Word_spec_to_int(spec) ->> int) then
        if int fi_> 0 then
            [^@UNSIGNED-BYTE ^int]
        else
            [^@SIGNED-BYTE ^(negate(int))]
        endif
    elseif isinteger(spec) then
        if spec fi_> 0 then
            [^@INTEGER 0 ^(1 << spec - 1)]
        else
            negate(spec) - 1 -> spec;
            [^@INTEGER ^(negate(1 << spec)) ^(1 << spec - 1)]
        endif
    else
        false
    endif
enddefine;


define etype_->_spec(type);
    lvars sym, spec, lo, hi, xtype;
    item_or_front(type) -> sym;
    if (Type_to_spec(type) ->> spec) then
        spec
    elseif sym == @SINGLE-FLOAT then
        dest_num_type(type, issdecimal) -> (, );
        Type_to_spec(sym)
    elseif sym == @DOUBLE-FLOAT then
        dest_num_type(type, isddecimal) -> (, );
        Type_to_spec(sym)
    elseif sym == @SIGNED-BYTE then
        dest_num_type(type, isinteger) -> (lo, );
        negate(lo or 8) -> lo;
        Int_spec_to_word(lo) or lo
    elseif sym == @UNSIGNED-BYTE then
        dest_num_type(type, isinteger) -> (lo, );
        lo or 8 -> lo;
        Int_spec_to_word(lo) or lo
    elseif sym == @INTEGER then
        dest_num_type(type, isintegral) -> (lo, hi);
        if lo and hi then
            if lo >= 0 then
                integer_length(hi)
            else
                negate(max(integer_length(lo), integer_length(hi)) + 1)
            endif
        else
            "full"
        endif
    elseif (type_expand1(type) -> xtype) then
        fast_chain(xtype, etype_->_spec)
    else
        "full"
    endif
enddefine;


define upgraded_array_element_type(type);
    spec_->_etype(etype_->_spec(type))
enddefine;


define upgraded_complex_part_type(type);
    @REAL
enddefine;



/* Key <-> spec/element-type conversions */

lconstant Spec_to_key
    = newproperty([[1           ^bitvector_key]
                   [character   ^string_key]
                   [byte        ^bytevec_key]
                   [sbyte       ^sbytevec_key]
                   [ushort      ^ushortvec_key]
                   [short       ^shortvec_key]
                   [uint        ^untvec_key]
                   [int         ^intvec_key]
                   [ulong       ^ulongvec_key]
                   [long        ^longvec_key]
                   [full        ^vector_key]], 31, false, true);


define spec_->_key(spec) -> key;
    if isintegral(spec) then
            if abs(spec) > sys_word_bits then
                "full"
#_IF sys_word_bits = 64
            elseif abs(spec) > 32 then
               if spec > 0 then "ulong" else "long" endif;
#_ENDIF
            elseif abs(spec) > 16 then
               if spec > 0 then "uint" else "int" endif;
            elseif abs(spec) > 8 then
               if spec > 0 then "ushort" else "short" endif;
            elseif abs(spec) > 1 then
               if spec > 0 then "byte" else "sbyte" endif;
            else spec endif -> spec;
    elseif spec == "decimal" then
        "sfloat" -> spec
    elseif spec == "ddecimal" then
        "dfloat" -> spec
    endif;
    unless (Spec_to_key(spec) ->> key) do
        conskey(consword("lisp_vector_" sys_>< spec), spec)
            ->> key -> Spec_to_key(spec);
        CLASS @VECTOR -> get_class_by_key(key)
    endunless
enddefine;


define key_->_spec(key);
    if key == pair_key then
        "full"
    elseif key == string_key then
        "character"
    else
        class_field_spec(key)
    endif
enddefine;


define key_->_etype() with_nargs 1;
    spec_->_etype(key_->_spec())
enddefine;


define etype_->_key() with_nargs 1;
    spec_->_key(etype_->_spec())
enddefine;


/* Spec & key -> default value conversions */

define spec_->_init(spec);
    if spec == "full" then
        nil
    elseif spec == "character" then
        #_< conscharacter(`\s`) >_#
    else
        0
    endif
enddefine;


define key_->_init() with_nargs 1;
    spec_->_init(key_->_spec())
enddefine;


/* Sequence type -> key conversion */

lconstant procedure Stype_to_key
    = newproperty([[^@LIST              ^pair_key]
                   [^@ARRAY             ^vector_key]
                   [^@SIMPLE-ARRAY      ^vector_key]
                   [^@VECTOR            ^vector_key]
                   [^@SIMPLE-VECTOR     ^vector_key]
                   [^@STRING            ^string_key]
                   [^@SIMPLE-STRING     ^string_key]
                   [^@BASE-STRING       ^string_key]
                   [^@SIMPLE-BASE-STRING ^string_key]
                   [^@BIT-VECTOR        ^bitvector_key]
                   [^@SIMPLE-BIT-VECTOR ^bitvector_key]], 8, false, false);


define stype_->_key(type) -> (key, type);
    lvars xtype;
    false -> key;
    unless (Stype_to_key(type) ->> key) do
        if type starts_with @ARRAY
        or type starts_with @SIMPLE-ARRAY then
            etype_->_key(cadr(type)) -> key
        elseif (type_expand1(type) -> xtype) then
            fast_chain(xtype, stype_->_key)
        elseif (is_pop11_type(type) ->> key) then
            unless is_vector_key(key) do
                false -> key
            endunless
        endif
    endunless
enddefine;


/* Field spec comparisons */

define is_float_spec(spec);
    spec == "sfloat" or spec == "dfloat"
        or spec == "decimal" or spec == "ddecimal"
enddefine;


define 6 x spec_<= y;
    if x == y then
        true
    elseif y == "full" then
        true
    elseif x == "full" then
        false
    else
        Word_spec_to_int(x) or x -> x;
        Word_spec_to_int(y) or y -> y;
        if isinteger(x) and isinteger(y) then
            if x fi_< 0 then            ;;; signed byte (n + 1)
                abs(x) fi_- 1 -> x      ;;; includes unsigned byte (n)
            endif;
            if y fi_< 0 then
                abs(y) fi_- 1 -> y
            endif;
            x fi_<= y
        else
            false
        endif
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Apr 27 1994
        etype_->_spec no longer uses subtypep to check type specifier syntax.
--- John Williams, Apr 26 1994
        Now recognises new ANSI character and string types.
        Bug fix: etype_->_spec("float") is now "full" because array must be
        able to contain all types of float.
        Added upgraded_array_element_type and upgraded_complex_part_type.
___ John Williams, Dec 21 1993
        Removed is_vector_spec and is_record_spec (superceded by
        is_vector_key and is_record_key in SRC * LISPCORE.P).
--- John Williams, Aug 27 1993
        stype_->_key now returns expanded type specifier.
--- John Williams, Jan 16 1991
        Changed -class_spec- to -class_field_spec-
 */
