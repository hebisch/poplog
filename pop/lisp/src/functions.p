/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/functions.p
 > Purpose:         "function_token" and "function_info" records
 > Author:          John Williams, May 29 1987 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/apply.p, C.all/lisp/src/plant.p
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back;

/* Function tokens */

defprop
    f_inline,
    ft_compiler_macro,
    ft_function_info,
    ft_setf_method,
    ;


define copy_function_token(ftok) -> new;
    copy(ftok) -> new;
    ft_setf_method(ftok) -> ft_setf_method(new);
    ft_function_info(ftok) -> ft_function_info(new)
enddefine;


constant macro (
    _FT_SPECIAL     =   2:0000,
    _FT_FUNCTION    =   2:0001,
    _FT_MACRO       =   2:0010,
    _FT_INLINE      =   2:0100,
    _FT_LOCAL       =   2:1000,
    _FT_PERM_FLAGS  =   (_FT_INLINE || _FT_LOCAL),
    );


define ft_perm_flags() with_nargs 1;
    ft_flags() && _FT_PERM_FLAGS
enddefine;


vars Local_function_tokens = [];


define current_sf_token(sym);
    list_assoc(sym, Local_function_tokens) or sf_token(sym)
enddefine;


define updaterof current_sf_token(ftok, sym);
    acons(sym, ftok, Local_function_tokens) -> Local_function_tokens
enddefine;


define checkr_lexical_env(env) -> env;
    defaults env [];
    returnif (env == Local_function_tokens or env == []);
    lvars l = env;
    until endp(l) do
        unless issymbol(front(l))
        and ispair(back(l) ->> l)
        and isfunction_token(front(l)) do
            lisp_error('Malformed lexical environment', [^env])
        endunless;
        back(l) -> l
    enduntil
enddefine;


/* Function infos */

defclass function_info
    {   fni_min     : 8,
        fni_max     : 8,
        fni_results : 8,
        fni_flags   : 8,
        fni_name,
        fni_file,
    };


constant Undef_function_info
            = consfunction_info(255, 255, 255, 0, false, false);


procedure(fni);
    lvars name;
    if (fni_name(fni) ->> name) then
        pr(name)
    else
        sys_syspr(fni)
    endif
endprocedure -> class_print(function_info_key);


/* Mapping function_info records to procedures */

defprop sys_pdr_fn_info;

constant procedure (always, always_function_info,
                    complement_apply, complement_function_info);


define function_info(item);
    lvars temp;
    if isfunction_info(item) then
        item
    elseif isfunction_token(item) then
        ft_function_info(item) or Undef_function_info
    elseif isprocedure(item) then
        if isfunction_info(pdprops(item) ->> temp) then
            temp
        elseif (istraced(item) ->> temp) then
            function_info(temp)
        elseif pdpart(item) == complement_apply then
            complement_function_info(fast_frozval(1, item))
                ->> function_info(item)
        elseif pdpart(item) == always then
            always_function_info(fast_frozval(1, item))
                ->> function_info(item)
        else
            sys_pdr_fn_info(item) or Undef_function_info
        endif
    else
        Undef_function_info
    endif
enddefine;


define updaterof function_info(fni, item);
    lvars temp;
    if isfunction_token(item) then
        fni -> ft_function_info(item)
    else
        if (istraced(item) ->> temp) then
            temp -> item
        endif;
        pdprops(item) -> temp;
        if isinheap(item)
        and (lisp_system_building or isfunction_info(temp) or not(temp)) then
            fni -> pdprops(item)
        elseunless sys_pdr_fn_info(item) do
            fni -> sys_pdr_fn_info(item)
        endif
    endif
enddefine;


/* Accessing function infos */

define lconstant Fni_field(item, field);
    fast_apply(function_info(item), field)
enddefine;


define updaterof lconstant Fni_field(val, item, field);
    lvars fni;
    if (function_info(item) ->> fni) == Undef_function_info then
        copy(fni) ->> fni -> function_info(item)
    endif;
    val -> fast_apply(fni, field)
enddefine;


define lconstant Fni_byte_field() -> val with_nargs 2;
    if (Fni_field() ->> val) == 255 then false -> val endif
enddefine;


define updaterof Fni_byte_field(val, item, field);
    val or 255 -> Fni_field(item, field)
enddefine;


constant procedure (
    f_min       =   Fni_byte_field(% fni_min %),
    f_max       =   Fni_byte_field(% fni_max %),
    f_results   =   Fni_byte_field(% fni_results %),
    f_file      =   Fni_field(% fni_file %),
    );


define f_name(item);
    Fni_field(item, fni_name) or recursive_front(pdprops(item))
enddefine;


define updaterof f_name(name, item);
    name -> Fni_field(item, fni_name)
enddefine;


/* Making function infos */

define new_function_info(name);
    consfunction_info(255, 255, 255, 0, name, false)
enddefine;


define fill_function_info(fni, fmin, fmax, fresults);
    fmin or 255 -> fni_min(fni);
    fmax or 255 -> fni_max(fni);
    if fresults then
        fresults -> fni_results(fni)
    endif
enddefine;


define merge_function_info(declared, defined);

    ;;; called when either
    ;;;     (1) defining a token that has a prior declaration
    ;;;     (2) declaring a token that has a prior definition

    returnif(defined == declared);

    if fni_min(defined) == fni_min(declared)
    and fni_max(defined) == fni_max(declared) then
        if fni_results(defined) == 255 then
            fni_results(declared) -> fni_results(defined)
        elseunless fni_results(defined) == fni_results(declared) do
            warn('Declared nresults (~D) inconsistent with defined (~D)',
                  [% fni_results(declared),
                     fni_results(defined),
                     fni_name(defined) %]) ->
        endif
    else
        warn('Declared min/max nargs (~D/~D) inconsistent with defined (~D/~D)',
             [% fni_min(declared), fni_max(declared),
                fni_min(defined), fni_max(defined),
                fni_name(defined) %]) ->
    endif
enddefine;


/* Assigning to function tokens */

constant procedure (exec_special_form, generic_function_p);

vars constant_functions;


define ft_valof() with_nargs 1;
    valof()
enddefine;


define updaterof ft_valof(pdr, ftok);
    lvars fni;
    if lisp_system_building then
        f_inline(ft_valof(ftok)) -> f_inline(pdr)
    endif;
    if (ft_function_info(ftok) ->> fni) then
        merge_function_info(fni, function_info(pdr))
    elseif pop_true(constant_functions) then
        unless f_results(pdr)
        or pdpart(pdr) == exec_special_form
        or generic_function_p(pdr) do
            advise('Note: ~S has undefined nresults', [% ft_name(ftok) %])
        endunless
    endif;
    sysPASSIGN(pdr, ftok);
    if lisp_system_building then
        sysprotect(ftok);
        false -> ft_function_info(ftok)
    else
        false -> ft_compiler_macro(ftok)
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 27 1994
        Undefined nresults warning not issued for generic functions.
--- John Williams, Aug 27 1993
        function_info now creates fni records for closures of
        complement_apply.
--- John Williams, Jul 12 1993
        Uses pop_true(constant_functions) instead of popdefineconstant
--- John Williams, Jul  9 1993
        Uses defclass instead of recordclass
 */
