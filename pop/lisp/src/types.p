/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/types.p
 > Purpose:         Some basic type specifier handling utilities
 > Author:          John Williams, Feb  9 1987 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/{specs, clos-types, typep, subtypep}.p
 */

lisp_compile_mode;

section $-lisp;

defprop
    type_info,
    type_predicate,
    list_type_predicate,
    ;


/* Creating new type specs */

defclass type_info
    {   tpi_xpdr,
        tpi_protected,
        tpi_file,
    };


define declare_new_type(sym);
    lvars tpi;
    if (type_info(sym) ->> tpi) then
        if tpi_protected(tpi) and not(lisp_system_building) then
            redefine_cerror(sym, @TYPE)
        endif;
        lisp_system_building -> tpi_protected(tpi);
        popfilename -> tpi_file(tpi)
    else
        constype_info(false, lisp_system_building, popfilename)
            -> type_info(sym)
    endif;
    if lisp_system_building then
        lblock;
            lvars pkg = symbol_package(sym);
            if pkg == lisp_package or pkg == poplog_package then
                export(sym, pkg)
            endif
        endlblock
    endif
enddefine;


define type_expander(sym);
    lvars tpi;
    check_name(sym, @TYPE);
    (type_info(sym) ->> tpi) and tpi_xpdr(tpi)
enddefine;


define updaterof type_expander(item, sym);
    declare_new_type(sym);
    item -> tpi_xpdr(type_info(sym))
enddefine;


/* Recognising some classes of type spec */

define is_basic_system_type(type);
    lvars tpi;
    issymbol(type)
        and (type_info(type) ->> tpi)
        and tpi_protected(tpi)
        and type_predicate(type)
enddefine;


define is_pop11_type(type);
    issymbol(type)
        and (symbol_package(type) == pop11_package)
        and key_of_dataword(sym_to_word(type))
enddefine;


/* Utilities for parsing type specs */

define lconstant Compound_type_error(type, kind);
    defaults kind 'compound';
    lisp_error('Malformed ~A type specifier', [^kind ^type])
enddefine;


define checkr_type_arg(type);
    unless islistlength(type, 2) do
        Compound_type_error(type, false)
    endunless;
    fast_front(fast_back(type))
enddefine;


define dest_cons_type(type) -> (cartype, cdrtype);
    false ->> cartype -> cdrtype;
    go_on destlist(type) to ONE TWO THREE else ERR;
THREE:
    -> cdrtype;
    if cdrtype == @* then
        false -> cdrtype
    endif;
TWO:
    -> cartype;
    if cartype == @* then
        false -> cartype
    endif;
ONE:
    ->;
    return;
ERR:
    Compound_type_error(type, false)
enddefine;


define dest_num_type(type, pred) -> (lo, hi);
    false ->> lo -> hi;
    if atom(type) then
        return
    elseunless (pred or type_predicate(fast_front(type)) ->> pred) do
        goto ERR
    endif;
    go_on destlist(type) to ONE TWO THREE else ERR;
THREE:
    -> hi;
    if islistlength(hi, 1) and pred(fast_front(hi) ->> hi) then
        hi - 1 -> hi
    elseif hi == @* then
        false -> hi
    elseunless pred(hi) do
        goto ERR
    endif;
TWO:
    -> lo;
    if islistlength(lo, 1) and pred(fast_front(lo) ->> lo) then
        lo + 1 -> lo
    elseif lo == @* then
        false -> lo
    elseunless pred(lo) do
        goto ERR
    endif;
    if lo and hi and lo > hi then
        goto ERR
    endif;
ONE:
    ->;
    return;
ERR:
    Compound_type_error(type, 'numeric')
enddefine;


constant procedure etype_->_spec;

define dest_array_type(type) -> (spec, rank, dims);
    false ->> spec ->> rank -> dims;
    if atom(type) then
        return
    endif;
    go_on destlist(type) to ONE TWO THREE else ERR;
THREE:
    -> dims;
    if isinteger(dims) then
        dims -> rank;
        false -> dims
    elseif dims == @* then
        false -> dims
    endif;
TWO:
    -> spec;
    if spec == @* then
        false -> spec
    elseunless (etype_->_spec(spec) ->> spec) do
        goto ERR
    endif;
ONE:
    ->;
    return;
ERR:
    Compound_type_error(type, 'array')
enddefine;


define compare_dims(d1, d2, procedure test);
    lvars i1, i2;
    until atom(d1) or atom(d2) do
        fast_destpair(d1) -> (i1, d1);
        fast_destpair(d2) -> (i2, d2);
        unless i2 == @* or (i1 /== @* and test(i1, i2)) do
            return(false)
        endunless;
    enduntil;
    d1 == d2
enddefine;


/* Basic user type expansion mechanism */

lconstant Workpair = writeable [0];


define type_expand1(type) -> type -> flag;
    lvars tpi;
    dlvars sym, xpdr;
    item_or_front(type) -> sym;

    if (type_info(sym) ->> tpi)
    and (tpi_xpdr(tpi) ->> xpdr) then
        true -> flag;
        if isprocedure(xpdr) then
            if ispair(type) then
                lisp_apply(type, xpdr, 1, 1)
            else
                procedure();
                    dlocal % fast_front(Workpair) % = sym;
                    lisp_apply(Workpair, xpdr, 1, 1)
                endprocedure()
            endif
        else
            xpdr
        endif -> type
    else
        check_name(sym, @TYPE);
        false -> flag;
        if sym /== type and fast_back(type) == nil then
            sym -> type;
            true -> flag
        endif
    endif
enddefine;


define type_expand(type) -> type;
    lvars flag;
    repeat
        type_expand1(type) -> type -> flag;
        quitunless(flag)
    endrepeat
enddefine;


/* Create keys for some built-in types -- see Steele p43-5 */

defclass pathname
    {   pt_host,
        pt_device,
        pt_dir,
        pt_name,
        pt_type,
        pt_version,
    };


defclass readtable
    {   readtable_vector,
        readtable_id,
        readtable_case_sym,
    };


defclass stream
    {   stream_read_p,
        stream_write_p,
        stream_source,
        stream_dest,
    };


/* Declare names of logical type specifiers - the rest are defined in
    clos-types.p and types.lsp
*/


applist([% @AND, @EQL, @MEMBER, @NIL, @NOT, @OR, @SATISFIES, @VALUES %],
        declare_new_type);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Apr  3 1995
        New fields for readtables.
--- John Williams, Mar 30 1995
        New names for fields of streams.
--- John Williams, Mar 20 1995
        Fixed bug in dest_num_type (in exclusive lower bounds of numeric
        type specifiers).
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Feb 27 1995
        declare_new_types now exports type name if lisp_system_building
        is true and the symbol's home package is "COMMON-LISP".
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        Added tpi_file field to type_info records.
        Moved definitions of type specifiers for built-in classes to
        clos-types.p TYPE-OF now defined in clos.lsp.
--- John Williams, Dec 21 1993
        Added EQL and REAL (Steele 1990 p50-1), removed COMMON.
--- John Williams, Aug 27 1993
        Type info records are now proper defclass structures. Tidied up.
--- John Williams, Jul  9 1993
        Uses defclass instead of recordclass
--- John Williams, Sep 10 1990
        Uses -word_to_sym- now.
 */
