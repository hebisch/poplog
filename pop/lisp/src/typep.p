/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/typep.p
 > Purpose:         TYPEP and COERCE
 > Author:          John Williams, Jul 22 1987 (see revisions)
 > Documentation:   CLtL, p72 (TYPEP), p51-2 (COERCE)
 > Related Files:   C.all/lisp/src/types.p, C.all/lisp/src/clos-types.p
 */

lisp_compile_mode;

section $-lisp;

define typep(item, type);
    lvars pdr, xtype;
    if atom(type)
    and (type_predicate(type) ->> pdr) then
        pdr(item)
    elseif ispair(type)
    and (list_type_predicate(fast_front(type)) ->> pdr) then
        pdr(item, type)
    elseif (type_expand1(type) -> xtype) then
        fast_chain(item, xtype, typep)
    elseif (is_pop11_type(type) ->> pdr) then
        datakey(item) == pdr
    else
        lisp_error('Unrecognised type specifier: ~S', [^type])
    endif
enddefine;


/* List type predicates */

define lconstant Apply_satisfies_type(item, type);
    checkr_function(checkr_type_arg(type)) -> type;
    if pop_true(lisp_apply(item, type, 1, 1)) then
        true
    else
        false
    endif
enddefine;


define lconstant Apply_eql_type(item, type);
    item ==# checkr_type_arg(type)
enddefine;


define lconstant Apply_not_type(item, type);
    not(typep(item, checkr_type_arg(type)))
enddefine;


define lconstant Apply_and_type(item, type);
    lvars t;
    for t in_cl_list fast_back(type) do
        returnunless(typep(item, t)) (false)
    endfor;
    true
enddefine;


define lconstant Apply_or_type(item, type);
    lvars t;
    for t in_cl_list fast_back(type) do
        returnif(typep(item, t)) (true)
    endfor;
    false
enddefine;


define lconstant Apply_member_type(item, type);
    lvars t;
    for t in_cl_list fast_back(type) do
        returnif(item ==# t) (true)
    endfor;
    false
enddefine;


define lconstant Apply_cons_type(item, type);
    lvars cartype, cdrtype;
    dest_cons_type(type) -> (cartype, cdrtype);
    ispair(item)
        and
    (not(cartype) or typep(fast_front(item), cartype))
        and
    (not(cdrtype) or typep(fast_back(item), cdrtype))
enddefine;


define lconstant Apply_fv_type(item, type);
    lisp_error('TYPEP cannot handle type-specifier', [^type])
enddefine;


define lconstant Apply_real_type(item, type, procedure numtest);
    lvars lo, hi;
    dest_num_type(type, numtest) -> (lo, hi);
    numtest(item) and (not(lo) or item >= lo) and (not(hi) or item <= hi)
enddefine;


define lconstant Apply_complex_type(item, type);
    checkr_type_arg(type) -> type;
    iscomplex(item)
        and typep(realpart(item), type)
        and typep(imagpart(item), type)
enddefine;


define lconstant Apply_sv_type(item, type);
    checkr_type_arg(type) -> type;
    isvector(item) and fast_vector_length(item) == type
enddefine;


define lconstant Apply_array_type(item, type, procedure arraytest);
    lvars spec, dims, rank;
    dest_array_type(type) -> (spec, rank, dims);
    unless arraytest(item) do
        return(false)
    endunless;
    if spec and spec /== key_->_spec(array_key(item)) then
        return(false)
    endif;
    if rank and rank /== array_rank(item) then
        return(false)
    endif;
    if dims and not(compare_dims(array_dimensions(item), dims, nonop ==)) then
        return(false)
    endif;
    true
enddefine;


/* Set up type predicates for special cases - the rest were done in
    clos-types.p which is loaded before this file.
*/

lblock;

lvars item;

for item in
    [[^@ATOM            ^atom]
     [^@BIT             ^(procedure(x); x == 1 or x == 0 endprocedure)]
     [^@SINGLE-FLOAT    ^issdecimal]
     [^@NIL             ^erase1_false]
     [^@PACKAGE         ^ispackage]
     [^@STRING-CHAR     ^string_char_p]
     [^@SYMBOL          ^issymbol]
     [^@T               ^erase1_true]
    ]
    do
        item(2) -> type_predicate(item(1))
endfor;


for item in
    [[^@AND             ^Apply_and_type]
     [^@EQL             ^Apply_eql_type]
     [^@MEMBER          ^Apply_member_type]
     [^@NOT             ^Apply_not_type]
     [^@OR              ^Apply_or_type]
     [^@SATISFIES       ^Apply_satisfies_type]
     [^@FUNCTION        ^Apply_fv_type]
     [^@VALUES          ^Apply_fv_type]
     [^@CONS            ^Apply_cons_type]
     [^@INTEGER         ^(Apply_real_type(% isintegral %))]
     [^@RATIO           ^(Apply_real_type(% isratio %))]
     [^@RATIONAL        ^(Apply_real_type(% isrational %))]
     [^@SINGLE-FLOAT    ^(Apply_real_type(% issdecimal %))]
     [^@DOUBLE-FLOAT    ^(Apply_real_type(% isddecimal %))]
     [^@FLOAT           ^(Apply_real_type(% isdecimal  %))]
     [^@REAL            ^(Apply_real_type(% isreal     %))]
     [^@COMPLEX         ^Apply_complex_type]
     [^@SIMPLE-VECTOR   ^Apply_sv_type]
     [^@ARRAY           ^(Apply_array_type(% arrayp %))]
     [^@SIMPLE-ARRAY    ^(Apply_array_type(% simple_array_p %))]]
    do
        item(2) -> list_type_predicate(item(1))
endfor;

endlblock;


/* Coerce */

constant procedure (isboundtoken);


define coerce(item, type);
    lvars key, xtype;
    if typep(item, type) then
        item
    else
        stype_->_key(type) -> (key,);
        if key then
            dest_seq(item, key == string_key);
            if key == pair_key then
                conslist()
            else
                fast_apply(class_cons(key))
            endif
        elseif type == @FUNCTION then
            lblock;
                lvars sym, ftok;
                if item starts_with @LAMBDA then
                    compile_lambda(item)
                elseif (fname_sym(item, false) ->> sym)
                and isboundtoken(sf_token(sym) ->> ftok)
                and ft_flags(ftok) _bitst _FT_FUNCTION then
                    ft_valof(ftok)
                else
                    goto ERR
                endif
            endlblock
        elseif type == @CHARACTER then
            character(item)
        elseif type == @FLOAT or type == @SINGLE-FLOAT then
            float(item, decimal_0)
        elseif type == @DOUBLE-FLOAT then
            float(item, ddecimal_0)
        elseif type == @COMPLEX then
            complex(item, false)
        elseif type starts_with @COMPLEX then
            checkr_type_arg(type) -> type;
            coerce(realpart(item), type) +: coerce(imagpart(item), type)
        elseif type_expand1(type) -> xtype then
            chain(item, xtype, coerce)
        else
            ERR:
            lisp_error('Cannot coerce to type ~S', [^type ^item])
        endif
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Mar 23 1995
        Predicates for NIL and T now use erase1_false and erase1_true.
--- John Williams, Apr 26 1994
        Fixed serious bug in Apply_or_type, which meant it was ignoring
        the first type specifier after the OR.
        Type predicates for built-in classes now set up in clos-types.p.
        Added support for compound form of CONS type specifier.
        coerce can now coerce function names and lambda expressions to
        functions (as per Steele 1990 p65).
--- John Williams, Feb 14 1994
        Type COMPILED-FUNCTION now uses predicate compiled_function_p.
--- John Williams, Dec 21 1993
        Added REAL and EQL (Steele 1990 p50-1).
--- John Williams, Aug 27 1993
        Tidied up.
 */
