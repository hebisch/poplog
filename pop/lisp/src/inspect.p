/* --- Copyright University of Sussex 1999.  All rights reserved. ---------
 > File:           C.all/lisp/src/inspect.p
 > Purpose:        Lisp interface to LIB * INSPECT
 > Author:         John Williams, Apr 29 1987 (see revisions)
 */

lisp_compile_mode;

section $-lisp;

uses inspect;

vars
    inspect_objects = true,         /* Not accessible to Lisp user yet */
    inspect_print_length = 5,
    inspect_print_level  = 3,
    ;

propsheet_idents
    inspect_print_length, inspect_print_level;


define lconstant Lget_char_accessor(char, n);
    if n == 0 then
        char, fast_char_code, 'Code'
    else
        false
    endif
enddefine;


define Lget_instance_accessor(Instance, n);
    lvars istr, slots, l;
    if n == 0 then
        i_class_instance(Instance), identfn, 'Class'
    else
        current_i_structure(Instance) -> istr;
        i_slots(Instance) -> slots;
        fast_vector_length(slots) -> l;
        if n fi_<= l then
            n, slots, Access_slot,
            symbol_string(fast_subscrv(n, istr_local_slot_names(istr)))
        else
            n fi_- l -> n;
            istr_shared_slot_cells(istr) -> slots;
            fast_vector_length(slots) -> l;
            if n fi_<= l then
                1, Access_slot(n, slots), Access_slot,
                symbol_string(fast_subscrv(n, istr_shared_slot_names(istr)))
            else
                false
            endif
        endif
    endif
enddefine;


define lconstant Lget_package_accessor(pkg, n);
    dlvars n, i;
    if n fi_< 5 then
        pkg;
        explode(fast_subscrv(n fi_+ 1,
            #_<{{% package_name,                '     Name' %}
                {% package_nicknames,           'Nicknames' %}
                {% package_used_by_list,        '  Used by' %}
                {% package_use_list,            '     Uses' %}
                {% package_shadowing_symbols,   '  Shadows' %}}>_#))
    else
        n fi_- 5 -> n;
        0 -> i;
        apppackage(pkg,
                   procedure(sym);
                       if n == i then
                          exitfrom(sym, identfn, true, Lget_package_accessor)
                       endif;
                       i fi_+ 1 -> i
                   endprocedure,
                   true, false);
        false
    endif
enddefine;


define lconstant Lget_pathname_accessor(path, n);
    n fi_+ 1 -> n;
    if n fi_< 7 then
        path;
        explode(fast_subscrv(n,
            #_<{{% pt_host,     '     Host' %}
                {% pt_device,   '   Device' %}
                {% pt_dir,      'Directory' %}
                {% pt_name,     '     Name' %}
                {% pt_type,     '     Type' %}
                {% pt_version,  '  Version' %}}>_#))
    else
        false
    endif
enddefine;


define lconstant Lget_pdr_accessor(pdr, n);
    lvars hi, lo, fp;
    if isarray(pdr) then
        arrayseq_bounds(pdr) -> hi -> lo;
        hi fi_- lo fi_+ 1 -> hi;
        if n fi_< hi then
            $-inspect$-Iget_access_pdr(arrayvector(pdr), lo fi_+ n fi_- 1)
        elseif n == hi and (fast_fill_pointer(pdr) ->> fp) then
            pdr, fill_pointer, 'Fill pointer'
        else
            false
        endif
    elseif isbytespecpdr(pdr) then
        if n == 0 then
            pdr, byte_size,     '    Byte size'
        elseif n == 1 then
            pdr, byte_position, 'Byte position'
        else
            false
        endif
    elseif n fi_< 5 then
        pdr;
        explode(fast_subscrv(n fi_+ 1,
                #_< {{% f_name,      lisp_true_apply,  '       Name' %}
                     {% f_min,       lisp_true_apply,  '  Min nargs' %}
                     {% f_max,       lisp_true_apply,  '  Max nargs' %}
                     {% f_results,   lisp_true_apply,  '   Nresults' %}
                     {% f_file,      lisp_true_apply,  'Source file' %}} >_#
                ))
    else
        false
    endif
enddefine;


define lconstant Lget_readtable_accessor(r, n);
    if n == 0 then
        r, readtable_id, '  Id'
    elseif n == 1 then
        r, readtable_case_sym, 'Case'
    else
        false
    endif
enddefine;


define Lget_stackframe_accessor(sf, n);
    lvars vec, num;
    if n == 0 then
        if iscaller(inspect_display) then
            nprintf(' Owner:')
        endif;
        return(sf_owner(sf), identfn, true)
    endif;
    sf_lvars(sf) -> vec;
    fast_vector_length(vec) fi_>> 1 -> num;
    if n fi_<= num then
        if n == 1 and iscaller(inspect_display) then
            nprintf(' Lexical bindings:')
        endif;
        n fi_<< 1 -> n;
        return(fast_subscrv(n, vec),
                idval,
                symbol_string(fast_subscrv(n fi_- 1, vec)))
    endif;
    n fi_- num -> n;
    sf_pvars(sf) -> vec;
    fast_vector_length(vec) -> num;
    if n fi_<= num then
        if n == 1 and iscaller(inspect_display) then
            nprintf(' Special bindings:')
        endif;
        return(fast_subscrv(n, vec),
                db_caller_valof,
                symbol_string(fast_subscrv(n, vec)))
    endif;
    false
enddefine;


define lconstant Lget_string_accessor(string, n);
    if n fi_< fast_vector_length(string) then
        n fi_+ 1, string, lisp_subscrs, true
    else
        false
    endif
enddefine;


define lconstant Lget_struct_accessor(struct, n);
    lvars sti, i, slot;

    define lconstant Access_struct(struct, slot);
        lisp_apply(struct, ssi_accessor(slot), 1, 1)
    enddefine;

    define updaterof lconstant Access_struct(val, struct, slot);
        lisp_apply(val, struct, ssi_updater(slot), 2, 0)
    enddefine;

    structure_info(sys_type_of(struct)) -> sti;
    0 -> i;
    fast_for slot in sti_slots(sti) do
        quitif(i fi_> n);
        if n == i then
            return(struct,
                   if ssi_updater(slot) then
                        slot, Access_struct
                   else
                        ssi_accessor(slot), 1, 1, lisp_apply
                   endif,
                   symbol_string(ssi_name(slot)))
        endif;
        i fi_+ 1 -> i
    endfast_for;
    false
enddefine;


define lconstant Lget_symbol_accessor(sym, n);
    lconstant procedure (
        Symval = sv_token <> $-inspect$-Ivalof,
        Symfn  = sf_token <> $-inspect$-Ivalof,
        )
    ;
    n fi_+ 1 -> n;
    if n fi_< 6 then
        sym;
        explode(fast_subscrv(n,
                #_< {{% symbol_name,    '    Name' %}
                     {% Symval,         '   Value' %}
                     {% Symfn,          'Function' %}
                     {% symbol_package, ' Package' %}
                     {% symbol_plist,   '   Plist' %}} >_# ))
    else
        false
    endif
enddefine;


define lconstant Lread_var() -> var;
    dlocal popprompt = 'Variable name? ';
    unless issymbol(lispreadform() ->> var) do
        false -> var
    endunless
enddefine;


define lconstant Lread_slotval();
    dlocal popprompt = 'New slot value? ';
    eval(acons(@VALUES, lispreadform(), []))
enddefine;


define lconstant Laccess(item);
    lvars key;
    if issymbol(item) then
        Lget_symbol_accessor
    elseif isproperty(item) then
        false
    else
        datakey(item) -> key;
        if key == string_key then
            Lget_string_accessor
        elseif key == character_key then
            Lget_char_accessor
        elseif key == instance_key and inspect_objects then
            Lget_instance_accessor
        elseif key == package_key then
            Lget_package_accessor
        elseif key == pathname_key then
            Lget_pathname_accessor
        elseif key == procedure_key then
            Lget_pdr_accessor
        elseif key == readtable_key then
            Lget_readtable_accessor
        elseif key == stream_key then
            procedure(); ->; ->; false endprocedure
        elseif key == stackframe_key then
            Lget_stackframe_accessor
        elseif structure_info(sys_type_of(item)) then
            Lget_struct_accessor
        else
            false
        endif
    endif
enddefine;


define lconstant Ldescribe(item);
    lvars type, a;
    if isstackframe(item) then
        sf_callnum(item), 'Stackframe %i'
    elseif arrayp(item) then
        array_element_type(item);
        array_dimensions(item);
        if simple_array_p(item) then
            'A simple array, dimensions %p, element type %p'
        else
            'An array, dimensions %p, element type %p'
        endif
    else
        uppertolower(symbol_string(sys_type_of(item))) -> type;
        if strmember(fast_subscrs(1, type), 'aeiou') then
            'An'
        else
            'A'
        endif -> a;
        if isword(item) or isintegral(item) or isdecimal(item)
        or ischaracter(item) or isstream(item) or isdevice(item)
        or not(item) or item == termin then
            '%S %S, %p', [% a, type, item %]
        else
            '%S %S', [% a, type %]
        endif
    endif;
    true
enddefine;


define lispinspect() with_nargs 1;
    dlocal
        inspect_read_var        =   Lread_var,
        inspect_read_slotval    =   Lread_slotval,
        inspect_special_access  =   Laccess,
        inspect_special_describe =  Ldescribe,
        standard_input          =   Term_io,
        standard_output         =   Term_io,
        print_escape            =   true,
        print_length,
        print_level,
        ;
    RESET_PRINT_VARS;
    unless inspect_print_length == @:IGNORE do
        inspect_print_length -> print_length
    endunless;
    unless inspect_print_level == @:IGNORE do
        inspect_print_level -> print_level
    endunless;
    clear_input(nil) ->;
    fresh_line(nil) ->;
    inspect()
enddefine;


define describe() with_nargs 1;
    dlocal
        inspect_special_access      =   Laccess,
        inspect_special_describe    =   Ldescribe,
        print_escape                =   true,
        print_length,
        print_level,
        ;
    RESET_PRINT_VARS;
    unless inspect_print_length == @:IGNORE do
        inspect_print_length -> print_length
    endunless;
    unless inspect_print_level == @:IGNORE do
        inspect_print_level -> print_level
    endunless;
    clear_input(nil) ->;
    fresh_line(nil) ->;
    inspect1()
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 27 1999
        replaced inst*ance with Instance
--- John Williams, Aug 11 1995
        Removed redundant lvar declarations.
--- John Williams, Apr 12 1995
        Modified Lget_pathname_accessor.
--- John Williams, Apr  3 1995
        Lget_readtable_accessor updated for readtable case.
--- John Williams, Jul 15 1994
        Uses binding instead of variable when describing stackframes.
--- John Williams, Apr 26 1994
        Changes for CLOS. Ldescribe now prints 'An' instead of 'A' when
        appropriate!
--- John Williams, Jun 17 1993
        Implemented @:IGNORE option to inspect_print_length and
        inspect_print_level.
--- John Williams, Jun 22 1989
        Changed stackframe handling
 */
