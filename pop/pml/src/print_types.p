/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/pml/src/print_types.p
 > Purpose:         PML: Printing types
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

constant procedure (fullname);

lconstant procedure (   ;;; forward
    pr_type,
);

lvars
    bound_var_num,
    free_var_num,
    varnames,
    usednames,
;

;;; tyvar_chars:
;;;     bound variables print as: 'a, 'b, 'c, ..., 'a1, 'b1, etc.
;;;     free variables print as: 'A, 'B, 'C etc.

define lconstant tyvar_chars(free);
    lvars n, c, free;
    if free then
        (`A`, free_var_num) -> (c, n);
        free_var_num fi_+ 1 -> free_var_num;
    else
        (`a`, bound_var_num) -> (c, n);
        bound_var_num fi_+ 1 -> bound_var_num;
    endif;
    if n < 26 then
        c fi_+ n;
    else
        c fi_+ (n // 26 -> n);
        dest_characters(n);
    endif;
enddefine;

;;; varname:
;;;     obtains a name for a type variable

define lconstant varname(tv) -> name;
    lvars tv, name;
    returnif(alookup(tv, varnames) ->> name);
    if type_contents(tv) ->> name then
        ;;; constraint variable: -name- is a tyvar id
        word_string(tvid_name(name)) -> name;
    endif;
    until name and not(member(name, usednames)) do
        cons_with consstring {%
            `'`;
            if type_eqtyvar(tv) then `'` endif;
            if type_imptyvar(tv) then `_` endif;
            tyvar_chars(type_decnum(tv));
        %} -> name;
    enduntil;
    conspair(name, usednames) -> usednames;
    acons(tv, name, varnames) -> varnames;
enddefine;

;;; pr_vartype, pr_recordtype, ...
;;;     printers for different type structures

define lconstant pr_vartype(ty);
    lvars ty;
    pr(varname(ty));
enddefine;

define lconstant pr_recordtype(ty);
    lvars labels, fields, iswild, ty;
    type_labels(ty) -> labels;
    type_fields(ty) -> fields;
    Destpair(labels) -> labels -> iswild;
    if labels == [] then
        pr(iswild and '{...}' or "unit");
    else
        pr("{");
        pr(Destpair(labels) -> labels);
        pr(' : ');
        pr_type(Destpair(fields) -> fields, 0);
        until labels == [] do
            pr(', ');
            pr(Destpair(labels) -> labels);
            pr(' : ');
            pr_type(Destpair(fields) -> fields, 0);
        enduntil;
        if iswild then pr(', ...') endif;
        pr("}");
    endif;
enddefine;

define lconstant pr_constype(ty);
    lvars ty, tycon, tys;
    type_constructor(ty) -> tycon;
    type_arguments(ty) -> tys;
    if tys == [] then
        /* nothing */
    elseif Back(tys) == [] then
        pr_type(Front(tys), 2);
        pr(space);
    else
        pr("(");
        pr_type(Front(tys), 0);
        For ty in Back(tys) do
            pr(', ');
            pr_type(ty, 0);
        endfor;
        pr(') ');
    endif;
    pr(fullname(tycon_parent(tycon), tycon_name(tycon)));
enddefine;

define lconstant pr_tupletype(ty);
    lvars ty, tys;
    type_fields(ty) -> tys;
    pr_type(Front(tys), 2);
    For ty in Back(tys) do
        sys_syspr(' * ');
        pr_type(ty, 2);
    endfor;
enddefine;

define lconstant pr_funtype(ty);
    lvars ty;
    pr_type(type_domain(ty), 1);
    pr(' -> ');
    pr_type(type_range(ty), 0);
enddefine;

;;; pr_type:
;;;     local type printer. -level- controls the bracketing:
;;;     level = 0 => don't bracket;
;;;     level = 1 => bracket function types;
;;;     level = 2 => bracket tuple and function types

define lconstant pr_type(/* ty, */ level) with_nargs 2;
    lvars ty = type_deref(/* ty */), level;
    if isvartype(ty) then
        pr_vartype(ty);
    elseif isrecordtype(ty) then
        recordtype_deref(ty) -> ty;
        if isinteger(type_labels(ty)) then
            ;;; tuple type
            if level > 1 then pr("(") endif;
            pr_tupletype(ty);
            if level > 1 then pr(")") endif;
        else
            pr_recordtype(ty);
        endif;
    elseif isfuntype(ty) then
        if level > 0 then pr("(") endif;
        pr_funtype(ty);
        if level > 0 then pr(")") endif;
    elseif isconstype(ty) then
        pr_constype(ty);
    else
        bad_type(ty);
    endif;
enddefine;

;;; print_type:
;;;     global type printer. Optional boolean argument -init- (default
;;;     <true>) indicates whether variable names should be reinitialised.
;;;     This allows different calls to share variable names.

define print_type(ty /* ?init */);
    lvars   ty, init = true;
    dlocal  pop_pr_quotes = false, pr = ml_pr;
    if isboolean(ty) then ty -> init -> ty endif;
    if init then
        ;;; re-initialise
        0 -> bound_var_num;
        0 -> free_var_num;
        [] -> varnames;
        [] -> usednames;
    endif;
    pr_type(ty, 0);
enddefine;

;;; make -print_type- the default printer for all type structures

procedure(ty);
    lvars ty;
    unless pr == ml_pr then printf('<type ') endunless;
    print_type(ty);
    unless pr == ml_pr then printf('>') endunless;
endprocedure
    ->> class_print(vartype_key)
    ->> class_print(recordtype_key)
    ->> class_print(funtype_key)
    ->  class_print(constype_key)
;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Nov  4 1994
        Changed empty record type to print as "unit"
--- Robert John Duncan, Mar  1 1991
        Free type variables now print as 'A, 'B, 'C, ...
--- Robert John Duncan, Feb  4 1991
        Type variable properties (-type_eqtyvar/type_imptyvar-) now boolean
 */
