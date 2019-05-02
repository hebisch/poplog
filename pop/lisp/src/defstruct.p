/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/defstruct.p
 > Purpose:         Common Lisp DEFSTRUCT macro
 > Author:          John Williams, May 29 1987 (see revisions)
 > Documentation:   CLtL, p305-321
 > Related Files:   C.all/lisp/src/defstruct.lsp
 */

lisp_compile_mode;

section $-lisp;

defprop structure_info;

defclass structure_info
    {   sti_name,
            ;;; name of structure type
        sti_key,
            ;;; key of structure
        sti_type,
            ;;; lisp type spec of structure (or false)
        sti_issubtypekey,
            ;;; property of keys of included types (or procedure not)
        sti_include,
            ;;; name of included structure (or false)
        sti_slots,
            ;;; list of ssi records
        sti_maker,
            ;;; #S constructor function (or false)
        sti_printer,
            ;;; user supplied print function (or false)
    };


defclass structure_slot_info
    {   ssi_name,
            ;;; name (or false for offset or name slot)
        ssi_spec,
            ;;; field spec (conskey format)
        ssi_init,
            ;;; initform for slot, either (quote item) or (funcall pdr)
        ssi_accessor,
            ;;; access function (true if undefined)
        ssi_updater,
            ;;; update function (true if undefined, false if :READ-ONLY)
    };


/* Structure recogniser, accessors, updaters, copier, and equality p */

constant procedure (inline_nth);


define lconstant Cons_recognise(named, offset, structlen, key, issubtypekey);
    lvars structkey, struct, endlab;
    dlocal pop_debugging = false;

    /* code generated is
        -> struct;
        datakey(struct) -> structkey;
        structkey == key
        IF UNTYPED
            or issubtypekey(structkey)
        ELSE
            IF LIST
                and listlength_>=(struct, structlen)
            ELSE
                and datalength(struct) >= structlen
            ENDIF
            IF NAMED
                and NTH(struct, offset + 1) == named
            ENDIF
        ENDIF
    */

    sysPROCEDURE(false, 1);
    sysNEW_LVAR() -> structkey;
    sysNEW_LVAR() -> struct;
    sysNEW_LABEL() -> endlab;
    sysPOP(struct);
    sysPUSH(struct);
    sysCALLQ(datakey);
    sysPOP(structkey);
    sysPUSH(structkey);
    sysPUSHQ(key);
    sysCALLQ(nonop ==);
    if isproperty(issubtypekey) then
        sysOR(endlab);
        sysPUSH(structkey);
        sysCALLQ(issubtypekey)
    else
        sysAND(endlab);
        sysPUSH(struct);
        if key == pair_key then
            sysPUSHQ(structlen);
            sysCALLQ(listlength_>=)
        else
            sysCALLQ(datalength);
            sysPUSHQ(structlen);
            sysCALLQ(nonop fi_>=)
        endif;
        if named then
            sysAND(endlab);
            sysPUSH(struct);
            lispNTH(offset + 1, key);
            sysPUSHQ(named);
            sysCALLQ(nonop ==)
        endif
    endif;
    sysLABEL(endlab);
    sysENDPROCEDURE()
enddefine;


define lconstant Compile_type_check(recogniser, err_p);
    lvars lab;
    sysNEW_LABEL() -> lab;
    sysPUSHS(0);
    sysCALLQ(recogniser);
    sysIFSO(lab);
    sysCALLQ(err_p);
    sysLABEL(lab);
enddefine;


define lconstant Cons_predicate(name, recogniser) -> p;
    lvars fni;
    new_function_info(name) ->> fni -> function_info(recogniser);
    fill_function_info(fni, 1, 1, 1);
    lisp_true_apply(% recogniser %) -> p;
    fni -> function_info(p);
    check_fmin_apply(% p, 1 %) -> p;
    fni -> function_info(p)
enddefine;


define lconstant Cons_access(name, n, key, recogniser, err_p) -> p;
    dlvars name, n, key, recogniser, err_p, p;
    lispFCOMPILE(
        name,
        @FUNCTION,
        procedure();
            fill_function_info(Current_function_info, 1, 1, 1);
            lispCHECK_NARGS(1, false);
            Compile_type_check(recogniser, err_p);
            lispNTH(n, key);
        endprocedure) -> p;
    inline_nth(% n, key %) -> f_inline(p)
enddefine;


define lconstant Cons_update(name, n, key, recogniser, err_p) -> p;
    dlvars name, n, key, recogniser, err_p, p;
    lispFCOMPILE(
        name,
        @FUNCTION,
        procedure();
            fill_function_info(Current_function_info, 2, 2, 0);
            lispCHECK_NARGS(2, false);
            Compile_type_check(recogniser, err_p);
            lispU_NTH(n, key);
        endprocedure) -> p;
    updater(inline_nth)(% n, key %) -> f_inline(p)
enddefine;


define lconstant Cons_copy(name, key, recogniser, err_p);
    dlvars name, key, recogniser, err_p;
    lispFCOMPILE(
        name,
        @FUNCTION,
        procedure();
            fill_function_info(Current_function_info, 1, 1, 1);
            lispCHECK_NARGS(1, false);
            Compile_type_check(recogniser, err_p);
            if key == pair_key then
                sysCALLQ(copy_list)
            else
                sysCALLQ(copy)
            endif
        endprocedure)
enddefine;


define lconstant Cons_maker(lamlist, body) -> p;
    compile_lambda([^@LAMBDA ^lamlist ^body]) -> p;
    1 -> f_results(p)
enddefine;


define lconstant Cons_equal(key);
    lvars spec, lab, x, y, fspec, n;

    /* Code generated is:
        x == y
        or (datakey(x) == key
                and
            <apply value of "struct_field_equal" to each pair of elements>)
       This assumes that y is of the specified type.
    */

    class_field_spec(key) -> spec;
    sysPROCEDURE(false, 2);
    sysNEW_LABEL() -> lab;
    sysNEW_LVAR() -> x;
    sysNEW_LVAR() -> y;
    sysPOP(y);
    sysPOP(x);
    sysPUSH(x);
    sysPUSH(y);
    sysCALLQ(nonop ==);
    sysOR(lab);
    sysPUSH(x);
    sysCALLQ(datakey);
    sysPUSHQ(key);
    sysCALLQ(nonop ==);
    sysAND(lab);
    1 -> n;
    for fspec in spec do
        sysPUSH(x);
        sysFIELD_VAL(n, spec);
        sysPUSH(y);
        sysFIELD_VAL(n, spec);
        sysCALL("ident struct_field_equal");
        sysAND(lab);
        n + 1 -> n
    endfor;
    sysPUSHQ(true);
    sysLABEL(lab);
    sysENDPROCEDURE()
enddefine;


/* Default printer for structures */

define pr_structure(struct, sti);
    dlvars struct, sti, p;
    dlocal depth_printed, length_printed = 0;
    if (sti_printer(sti) ->> p) then
        lisp_apply(struct, standard_output, depth_printed,
                    checkr_function(p), 3, 0)
    else
        CHECK_PR_LEVEL;
        print_logical_block(
            '#S(', ')',
            procedure();
                lvars slot, name;
                print_indent(@:CURRENT, 0);
                _lisppr(sti_name(sti));
                1 -> length_printed;
                for slot in sti_slots(sti) do
                    if (ssi_name(slot) ->> name) then
                        cucharout(`\s`);
                        CHECK_PR_LEN;
                        print_newline(@:FILL);
                        _lisppr(conskeyword(name));
                        cucharout(`\s`);
                        CHECK_PR_LEN;
                        _lisppr(lisp_apply(struct, ssi_accessor(slot), 1, 1))
                    endif
                endfor
            endprocedure)
    endif
enddefine;


/* Parsing DEFSTRUCT options and slots */

define lconstant Parse_defstruct_options(options) -> (conc_name,
                                                      copier, pred,
                                                      includes, printer,
                                                      type, named, offset);
    lvars done;
    dlvars Item, Option;

    false ->> conc_name ->> copier ->> pred ->> includes ->> printer
          ->> type -> named;
    0 -> offset;
    [] -> done;

    define lconstant No_arg_err();
        program_error('No argument to ~S option supplied', [^Option])
    enddefine;

    define lconstant Malformed_err();
        program_error('Malformed DEFSTRUCT option', [^Item])
    enddefine;

    define lconstant Get_val(need_arg);
        lvars len;
        if ispair(Item) then
            if need_arg == cdr then
                cdr(Item)
            else
                listlength(Item) -> len;
                if len == 2 then
                    cadr(Item)
                elseif len == 1 then
                    if need_arg then
                        No_arg_err()
                    else
                        false
                    endif
                else
                    Malformed_err()
                endif
            endif
        elseif need_arg then
            No_arg_err()
        else
            false
        endif
    enddefine;

    for Item in_cl_list options do
        item_or_front(Item) -> Option;
        nextif(Option == @:CONSTRUCTOR);    /* Dealt with elsewhere */
        if lmember(Option, done) then
            program_error('Duplicate ~S Option', [^Item])
        endif;
        conspair(Option, done) -> done;
        if Option == @:CONC-NAME then
            Get_val(true) -> conc_name
        elseif Option == @:COPIER then
            Get_val(false) -> copier
        elseif Option == @:PREDICATE then
            Get_val(false) -> pred
        elseif Option == @:INCLUDE then
            Get_val(cdr) -> includes
        elseif Option == @:PRINT-FUNCTION then
            Get_val(false) or @:DEFAULT -> printer
        elseif Option == @:TYPE then
            Get_val(true) -> type
        elseif Option == @:NAMED then
            pop_true(Get_val(false) or true) -> named
        elseif Option == @:INITIAL-OFFSET then
            Get_val(true) -> offset
        else
            program_error('Unrecognised DEFSTRUCT Option', [^Item])
        endif
    endfor
enddefine;


define lconstant Parse_defstruct_slot(slot, spec);
    lvars name, type, read_only, init, s, option;

    false ->> type -> read_only;

    if atom(slot) then
        slot -> name;
        spec_->_init(spec) -> init
    else
        destpair(slot) -> (name, s);
        destpair(s) -> (init, s);

        define lconstant Get_val();
            if ispair(s) then
                destpair(s) -> s
            else
                true    /* Correct for both :TYPE and :READ-ONLY options */
            endif
        enddefine;

        until endp(s) do
            destpair(s) -> (option, s);
            if option == @:TYPE then
                if type then
                    program_error('Duplicate :TYPE slot option', [^slot])
                endif;
                Get_val() -> type
            elseif option == @:READ-ONLY then
                if read_only then
                    program_error('Duplicate :READ-ONLY slot option', [^slot])
                endif;
                Get_val() -> read_only
            else
                program_error('Unrecognised DEFSTRUCT slot option ~S',
                            [^option ^slot])
            endif
        enduntil
    endif;

    check_name(name, 'SLOT');
    if type then
        unless (etype_->_spec(type) ->> spec) do
            lisp_error('Invalid structure slot type specifier', [^type])
        endunless;
        if spec == "character" then
            /* Can't handle character conversions from record slots */
            "full" -> spec
        endif
    endif;

    consstructure_slot_info(name, spec, init, true, not(pop_true(read_only)))
enddefine;


define lconstant Parse_boa_lamlist(lamlist, slots);
    lvars lamvars, lamkeysym, item, slot, var;

    define lconstant Is_slot(name);
        lvars slot;
        for slot in slots do
            returnif(ssi_name(slot) == name) (slot)
        endfor;
        false
    enddefine;

    false -> lamkeysym;
    [] -> lamvars;
    [% while ispair(lamlist) do
        destpair(lamlist) -> (item, lamlist);
        if islamkeysym(item) then
            item -> lamkeysym
        elseif atom(item) then
            conspair(item, lamvars) -> lamvars;
            if lamkeysym == @&OPTIONAL
            or lamkeysym == @&KEY then
                if (Is_slot(item) ->> slot) then
                    [^item ^(ssi_init(slot))] -> item
                endif
            elseif lamkeysym == @&AUX then
                if (Is_slot(item) ->> slot) then
                    [^item ^(spec_->_init(ssi_spec(slot)))] -> item
                endif
            endif
        elseif lamkeysym == @&KEY
        and item matches [[= =]] then               /* ((key var)) */
            car(item) -> item;                      /*  (key var) */
            cadr(item) -> var;
            if (Is_slot(var) ->> slot) then
                [^item ^(ssi_init(slot))] -> item
            endif;
            conspair(var, lamvars) -> lamvars
        else
            conspair(front(item), lamvars) -> lamvars
        endif;
        item
    endwhile;
    unless lamkeysym == @&AUX do
        @&AUX
    endunless;
    for slot in slots do
        if (ssi_name(slot) ->> item)
        and not(lmember(item, lamvars)) then
            [^item ^(ssi_init(slot))]
        endif
    endfor %]
enddefine;


/* DEFSTRUCT itself */

define define_struct(name, options, makers, doc, slots) -> name;
    lvars conc_name, copier, pred, includes, printer, type, named, offset,
            key, spec, init, incsti, incslots, inctype,
            slotnames, x, slot, nslots,
            sname, issubtypekey, sti,
            err_p, recogniser, maker;


    check_name(name, @STRUCTURE);

    Parse_defstruct_options(options)
        -> (conc_name, copier, pred, includes, printer, type, named, offset);

    /* Determine structure type, whether named, etc. */

    if type then
        if type == @LIST then
            pair_key -> key
        elseif type == @VECTOR then
            vector_key -> key
        else
            unless type starts_with @VECTOR
            and islistlength(type, 2)
            and (etype_->_key(cadr(type)) ->> key) do
                lisp_error('Invalid structure :TYPE option', [^type])
            endunless
        endif;
        check_positive(offset) -> offset;
        if named then
            unless key_->_spec(key) == "full" do
                lisp_error('Structure of type ~S cannot be named', [^type])
            endunless;
            [^@QUOTE ^name] -> named
        endif
    else
        false -> key;
        declare_new_type(name)
    endif;

    if key then
        key_->_spec(key), key_->_init(key)
    else
        "full", nil
    endif -> (spec, init);

    /* Check included structure name */

    false ->> incsti -> inctype;
    [] -> incslots;
    if includes then
        if ispair(includes) then
            destpair(includes) -> (includes, incslots)
        endif;
        unless (structure_info(includes) ->> incsti) do
            check_name(includes, @STRUCTURE);
            lisp_error('Cannot include undefined structure type', [^includes])
        endunless;
        sti_type(incsti) -> inctype;
        unless sys_=(inctype, type) do
            lisp_error('~:[Untyped structure~;Structure of type ~:*~S~] cannot include ~:[untyped structure~;structure of type ~:*~S~]',
                       [% lisp_true(type), lisp_true(inctype) %])
        endunless
    endif;

    /* Create ssi records for slots */

    [] -> slotnames;

    define lconstant Check_slot_name(ssi);
        lvars name;
        if (ssi_name(ssi) ->> name) then
            symbol_string(name) -> name;
            if fast_member(name, slotnames) then
                program_error('Duplicate slot name: ~S', [^name])
            else
                conspair(name, slotnames) -> slotnames
            endif
        endif
    enddefine;

    [%  if includes then
            [% for x in_cl_list incslots do
                Parse_defstruct_slot(x, spec)
            endfor %] -> incslots;
            for slot in sti_slots(incsti) do
                Check_slot_name(slot);
                copy(slot) ->> slot;
                for x in incslots do
                    if ssi_name(x) == ssi_name(slot) then
                        unless ssi_spec(x) spec_<= ssi_spec(slot) do
                            lisp_error(
                                'Slot type given in :INCLUDE option not a subtype',
                                [% ssi_name(x) %])
                        endunless;
                        ssi_init(x) -> ssi_init(slot);
                        ssi_updater(x) -> ssi_updater(slot);
                        quitloop
                    endif
                endfor
            endfor
        endif;

        repeat offset times
            consstructure_slot_info(false, spec, init, false, false)
        endrepeat;

        if named then
            consstructure_slot_info(false, "full", named, false, false)
        endif;

        for slot in slots do
            Parse_defstruct_slot(slot, spec);
            Check_slot_name(dup())
        endfor

    %] -> slots;

    if includes then
        listlength(sti_slots(incsti)) + offset -> offset;
    endif;
    listlength(slots) -> nslots;

    /* Construct key and class_= */

    symbol_string(name) -> sname;
    not -> issubtypekey;
    unless type do
        maplist(slots, ssi_spec) -> spec;
        if (structure_info(name) ->> sti) then
            if sys_=(class_field_spec(sti_key(sti) ->> key), spec) then
                sti_issubtypekey(sti) -> issubtypekey
            else
                advise('Changing shape of structure type ~S', [^name]);
                false -> key
            endif
        endif;
        unless isproperty(issubtypekey) do
            newanyproperty([], 1, 1, 1, false, false, "perm", false, false)
                -> issubtypekey
        endunless;
        unless key do
            conskey(consword(sname), spec) -> key;
            Cons_equal(key) -> class_=(key)
        endunless
    endunless;

    /* Create constructor function(s) */
    lblock;
        lvars slotvars, item;

        [% for slot in slots do
            if (ssi_name(slot) ->> x) then
                x
            endif
        endfor %] -> slotnames;

        maplist(slotnames, make_symbol) -> slotvars;

        define lconstant Make_default_lamlist(slotvars);
            [%  @&KEY;
                for slot in slots do
                    if ssi_name(slot) then
                        destpair(slotvars) -> (x, slotvars);
                        [[^(conskeyword(x)) ^x] ^(ssi_init(slot))]
                    endif
                endfor
            %]
        enddefine;

        define lconstant Make_body(slotvars);
            [%  if key == pair_key then
                    @LIST
                else
                    class_cons(key)
                endif;
                for slot in slots do
                    if ssi_name(slot) then
                        destpair(slotvars) -> slotvars
                    else
                        ssi_init(slot)
                    endif;
                    if key == string_key then
                        -> x;
                        [^@CHAR-CODE ^x]
                    endif
                endfor;
                unless is_record_key(key) do
                    nslots
                endunless
            %]
        enddefine;

        Cons_maker(Make_default_lamlist(slotvars), Make_body(slotvars))
            -> maker;
        if makers == [] then
            sysintern('MAKE-' <> sname, package) -> x;
            maker -> symbol_function(x)
        else
            for item in makers do
                nextif(item == []);
                destpair(item) -> (x, item);
                if item == [] then
                    maker
                elseif back(item) == [] then
                    Cons_maker(
                        Parse_boa_lamlist(front(item), slots),
                        Make_body(slotnames))
                else
                    program_error('Malformed DEFSTRUCT option',
                                [[^@:CONSTRUCTOR ^x ^^item]])
                endif -> symbol_function(x)
            endfor
        endif
    endlblock;

    /* Create recogniser */

    Cons_recognise(named and name, offset, nslots, key, issubtypekey)
        -> recogniser;

    unless pred == nil do
        if type == false or named then
            pred or sysintern(sname <> '-P', package) -> pred;
            Cons_predicate(pred, recogniser) -> symbol_function(pred)
        endif
    endunless;

    /* Type error procedure */

    type_error(% name %) -> err_p;

    /* Create copier */

    unless copier == nil do
        copier or sysintern('COPY-' <> sname, package) -> copier;
        Cons_copy(copier, key, recogniser, err_p) -> symbol_function(copier)
    endunless;

    /* Create slot access/update functions */

    lblock;
        lvars n = 0, sym, p, usym;
        if pop_true(conc_name) then
            symbol_string(conc_name) -> conc_name
        endif;
        for slot in slots do
            n + 1 -> n;
            nextunless(ssi_name(slot) ->> sym);
            unless conc_name == nil do
                sysintern(if conc_name then
                              conc_name <> symbol_string(sym)
                          else
                              sname <> '-' <> symbol_string(sym)
                          endif, package) -> sym
            endunless;
            if (ssi_accessor(slot) ->> p) then
                if isprocedure(p)
                and fboundp(sym)
                and p == fdefinition(sym) then
                    ;;; [Using ^p as accessor for ^sym slot] =>
                else
                    Cons_access(sym, n, key, recogniser, err_p)
                        ->> ssi_accessor(slot) -> symbol_function(sym)
                endif
            endif;
            if (ssi_updater(slot) ->> p) then
                fname_sym([^@SETF ^sym]) -> usym;
                if isprocedure(p)
                and fboundp(sym)
                and p == fdefinition(usym) then
                    ;;; [Using ^p as updater for ^sym slot] =>
                else
                    Cons_update(usym, n, key, recogniser, err_p)
                        ->> ssi_updater(slot) -> symbol_function(usym)
                endif
            else
                false -> setf_method(sym)
            endif
        endfor
    endlblock;

    /* Check print function */

    if printer then
        if type then
            warn(':PRINT-FUNCTION not supported for structures of type ~S',
                 [^type ^printer]) ->;
            false -> printer
        elseif printer starts_with @LAMBDA then
            compile_lambda(printer) -> printer
        elseif printer == @:DEFAULT then
            false -> printer    /* i.e. use default */
        endif
    elseif includes then
        sti_printer(incsti) -> printer
    endif;

    /* Lastly create structure_info record */

    consstructure_info(
        name,
        key,
        type,
        issubtypekey,
        includes,
        slots,
        maker,
        printer) ->> sti -> structure_info(name);

    if type then
        false -> type_predicate(name)
    else
        recogniser -> type_predicate(name);
        structure_class(name, includes, key);
        pr_structure(% sti %) -> lisp_class_print(key);
        if includes then
            while incsti do
                true -> sti_issubtypekey(incsti)(key);
                structure_info(sti_include(incsti)) -> incsti
            endwhile
        endif
    endif;

    if doc then
        doc -> documentation(name, @STRUCTURE)
    endif
enddefine;


/* Various */

define allocate_structure(name, args);
    lvars sti, n;
    if (structure_info(name) ->> sti) then
        lisp_apply(destlist(args) -> n, sti_maker(sti), n, false)
    else
        lisp_error('No defstruct form for class ~S seen yet', [^name])
    endif
enddefine;


define make_load_form_saving_slots(object, slots);
    lvars type, slot, sti, a;
    sys_type_of(object) -> type;
    if isinstance(object) then
        defaults slots i_local_slot_names(object);
        [^@ALLOCATE-INSTANCE [^@FIND-CLASS [^@QUOTE ^type]]];
        [%  @PROGN,
            for slot in slots do
                if slot_boundp(object, slot) then
                    [^@SETF [^@SLOT-VALUE ^object [^@QUOTE ^slot]]
                            [^@QUOTE ^(slot_value(object, slot))]]
                endif
            endfor
        %]
    elseif (structure_info(type) ->> sti) then
        defaults slots [];
        [^@SYS:ALLOCATE-STRUCTURE [^@QUOTE ^type] []];
        [%  @PROGN,
            for slot in sti_slots(sti) do
                nextif (eql_member(ssi_name(slot), slots));
                ssi_accessor(slot) -> a;
                if isprocedure(ssi_updater(slot)) then
                    [^@SETF [^(f_name(a)) ^object]
                            [^@QUOTE ^(lisp_apply(object, a, 1, 1))]]

                endif
            endfor
        %]
    else
        type_error(object, [^@OR ^@STANDARD-OBJECT ^@STRUCTURE-OBJECT])
    endif
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 19 1995
        Removed copy_structure (not part of Steele 1990 and not used).
--- John Williams, Jun  5 1995
        Added make_load_form_saving_slots.
--- John Williams, Mar 20 1995
        pr_structure now uses keywords when printing slot names.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Oct 13 1994
        Fixed bug in parsing of :INCLUDE option.
--- John Williams, Jun 13 1994
        Default (&KEY) constructor function lambda-list no longer uses
        slot names as variables (uses unique uninterned symbols instead).
--- John Williams, Jun 10 1994
        Fixed for Steele 1990.
--- John Williams, Apr 26 1994
        Serious bug fix - no longer overwrites accessor/updater function names
        if these are the same (via :conc-name) as those of an included
        structure.
        Added copy_structure and allocate_structure (for CLOS).
--- John Williams, Dec 21 1993
        Now uses is_record_key.
--- John Williams, Dec 21 1993
        No longer assigns to SYS:COMMON-SUBTYPES.
--- John Williams, Aug 27 1993
        Handles :TYPE option without using stype_->_key.
--- John Williams, Jul  9 1993
        Uses defclass instead of recordclass
--- John Williams, Jan 16 1991
        Changed -class_spec- to -class_field_spec-
 */
