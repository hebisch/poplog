/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lisp/src/methods.p
 > Purpose:         CLOS generic functions and methods.
 > Author:          John Williams, Apr 26 1994 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/clos.p, C.all/lisp/src/clos-utils.p
 */

lisp_compile_mode;

section $-lisp;

fastprocs front, back, destpair, subscrv, vmember, for;


defclass generic_function
    {   gfn_name,
        gfn_methods,
        gfn_lamlist,
        gfn_lamspec,
        gfn_instance,
    };


defclass method
    {   method_name,
        method_source,
        method_qcode,
        method_specs,
        method_tests,
        method_body,
        method_instance,
    };


define_fast_accessors(generic_function_key);
define_fast_accessors(method_key);


constant Qcodes
    = {^@:AROUND ^@:BEFORE ^@NIL ^@:AFTER};

constant
    Qcode_NIL       =   vmember(@NIL, Qcodes),
    Qcode_AROUND    =   vmember(@:AROUND, Qcodes),
    ;


procedure(m);
    lvars q;
    appdata('<method ', cucharout);
    pr(method_name(m));
    unless (method_qcode(m) ->> q) == Qcode_NIL then
        if q <= fast_vector_length(Qcodes) then
            cucharout(`\s`);
            pr(Qcodes(q))
        else
            printf(' <Illegal qcode %P> ', [^q])
        endif
    endunless;
    appdata(method_specs(m),
            procedure(c);
                cucharout(`\s`);
                pr(c)
            endprocedure);
    cucharout(`>`)
endprocedure -> class_print(method_key);


define Return_iom(method) -> iom;           /* iom = instance of method */
    unless (method_instance(method) ->> iom) do
        consinstance(class_istr(CLASS @STANDARD-METHOD), {^method})
            ->> iom -> method_instance(method)
    endunless
enddefine;


define Accept_iom(iom) -> method;
    if isinstance(iom)
    and i_structure(iom) == class_istr(CLASS @STANDARD-METHOD) then
        subscrv(1, i_slots(iom)) -> method
    elseif ismethod(iom) then
        iom -> method
    else
        type_error(iom, @STANDARD-METHOD)
    endif
enddefine;


/* Method selection and application */

constant procedure eql = nonop ==#;

vars current_argv = false;      /* Vector of arguments to generic function */


define method_is_applicable(m, argv);
    lvars arg, p;
    fastprocs method_tests;
    for arg, p in_vector (argv, method_tests(m)) do
        returnunless (fast_apply(arg, p)) (false)
    endfor;
    true
enddefine;


define lconstant Method_before_p(m1, m2);
    lvars c1, c2, i, c;
    fastprocs method_specs;
    for c1, c2 with_index i in_vector (method_specs(m1), method_specs(m2)) do
        unless c1 == c2 do
            return(
                if ispair(c1) then      /* EQL parameter specializer */
                    true
                elseif ispair(c2) then
                    false
                else
                    /* Assumes current_argv is set up */
                    sys_class_of(subscrv(i, current_argv)) -> c;
                    is_subclass_of(c, c1) <= is_subclass_of(c, c2)
                endif)
        endunless
    endfor;
    true
enddefine;


define lconstant Method_after_p(m1, m2);
    Method_before_p(m2, m1)
enddefine;


define compute_applicable_methods(p, args) -> methods;

    /* This function is described in Steele 1990, p822. It only sorts by
        precedence (i.e. ignores qualifiers), and is not used by
        apply_generic_function (see below).
    */

    lvars gfn, m;
    dlocal current_argv;

    if (is_gfn_clos(p) ->> gfn) then
        consvector(destlist(args)) -> current_argv;
        [] -> methods;
        for m in gfn_methods(gfn) do
            if method_is_applicable(m, current_argv) then
                insert(m, methods, Method_before_p) -> methods
            endif
        endfor;
        ncmaplist(methods, Return_iom) -> methods
    else
        type_error(p, @GENERIC-FUNCTION)
    endif
enddefine;


define get_applicable_methods(gfn) -> (around, before, primary, after);

    /* This is the function used by apply_generic_function. It sorts by
        precedence and groups methods by qualifiers.
    */

    lvars m, around = [], before = [], primary = [], after = [];
    fastprocs gfn_methods, method_qcode;

    for m in gfn_methods(gfn) do
        if method_is_applicable(m, current_argv) then
            go_on method_qcode(m) to AROUND BEFORE PRIMARY AFTER else ERR;
            ERR:
                lisp_error('System error: unknown method_qcode ~S',
                            [^(method_qcode(m)) ^m]);
            AROUND:
                insert(m, around, Method_before_p) -> around;
                nextloop;
            BEFORE:
                insert(m, before, Method_before_p) -> before;
                nextloop;
            AFTER:
                insert(m, after, Method_after_p) -> after;
                nextloop;
            PRIMARY:
                insert(m, primary, Method_before_p) -> primary
        endif
    endfor
enddefine;


vars
    error_print_array,
    current_gfn         =   false,
    current_method      =   false,
    next_methods        =   false,
    around_methods      =   false,
    before_methods      =   false,
    primary_methods     =   false,
    after_methods       =   false,
    ;


define next_method_p();
    ispair(next_methods)
        or
    (method_qcode(current_method) == Qcode_AROUND)
enddefine;


define call_method() with_nargs 2;
    fastprocs method_body;
    -> next_methods;
    -> current_method;
    destvector(current_argv) -> LISP_N_ARGS;
    fast_apply(method_body(current_method))
enddefine;


define call_iom(iom, next);
    if iom starts_with @MAKE-METHOD then
        lisp_error('MAKE-METHOD forms not supported yet', [^iom])
    endif;
    call_method(Accept_iom(iom), ncmaplist(next, Accept_iom))
enddefine;


define lconstant Run_before_primary_and_after_methods();
    lvars m;
    for m in before_methods do
        call_method(m, false)
    endfor;
    call_method(destpair(primary_methods));
    for m in after_methods do
        call_method(m, false)
    endfor
enddefine;


define apply_generic_function(current_gfn);
    dlocal current_gfn, current_argv, current_method, next_methods,
            around_methods, before_methods, primary_methods, after_methods,
            error_print_array = true;

    consvector(LISP_N_ARGS) -> current_argv;

    get_applicable_methods(current_gfn)
        -> (around_methods, before_methods, primary_methods, after_methods);

    if around_methods == []
    and before_methods == []
    and primary_methods == []
    and after_methods == [] then
        @NO-APPLICABLE-METHOD(
            gfn_instance(current_gfn),
            destvector(current_argv) + 1);
        return
    endif;

    if primary_methods == [] then
        control_error(
            'No applicable primary methods',
            [% gfn_instance(current_gfn),
               conslist(destvector(current_argv)) %])
    endif;

    if around_methods /== [] then
        call_method(destpair(around_methods))
    else
        Run_before_primary_and_after_methods()
    endif
enddefine;


define call_next_method(N);
    lvars m, q;
    dlocal current_argv, current_method, next_methods;

    unless current_method do
        control_error('CALL-NEXT-METHOD called out of context', [])
    endunless;

    unless N == 0 do
        lblock;
            lvars argv = current_argv, around, before, primary, after;
            consvector(N) -> current_argv;
            get_applicable_methods(current_gfn)
                -> (around, before, primary, after);
            unless equal(around, around_methods)
            and equal(before, before_methods)
            and equal(primary, primary_methods)
            and equal(after, after_methods)
            do
                lisp_error('New set of arguments inconsistent with old',
                            [% conslist(destvector(current_argv)),
                               conslist(destvector(argv)) %])
            endunless
        endlblock
    endunless;

    if ispair(next_methods) then
        call_method(destpair(next_methods))
    else
        current_method -> m;
        if (method_qcode(m) ->> q) == Qcode_AROUND then
            /* This must be the last :AROUND method. Also, we have already
                checked that at least one primary method exists.
            */
            Run_before_primary_and_after_methods()
        elseif next_methods == [] then
            @NO-NEXT-METHOD(gfn_instance(current_gfn), Return_iom(m), 2)
        else
            control_error('Cannot use CALL-NEXT-METHOD in a ~S method',
                          [^(subscrv(q, Qcodes)) ^m])
        endif
    endif
enddefine;


/* Defining generic functions */

define is_gfn_clos(item);
    lvars p;
    if isprocedure(item) then
        if (istraced(item) ->> p) then
            p -> item
        endif;
        pdpart(item) == apply_generic_function and frozval(1, item)
    else
        false
    endif
enddefine;


define gfn_clos_name(p);
    lvars gfn;
    (is_gfn_clos(p) ->> gfn) and gfn_name(gfn)
enddefine;


define new_gfn_clos(sym, lamlist, lamspec) -> p;
    lvars fni;
    apply_generic_function(% 0 %) -> p;
    consgeneric_function(sym, [], lamlist, lamspec, p)
        -> frozval(1, p);
    new_function_info(sym) ->> fni -> function_info(p);
    fill_function_info(fni, lamspec(1), false, false);
    popfilename -> f_file(fni)
enddefine;


define lamlist_info(lamlist, check) -> (rargs, oargs, rest);
    lvars item;

    define lconstant Illegal_parameter(type, item);
        warn('~A parameters not supported in this context', [^type ^item]) ->
    enddefine;

    0 -> rargs;
    0 -> oargs;
    false -> rest;
    until endp(lamlist) or islamkeysym(front(lamlist)) do
        back(lamlist) -> lamlist;
        rargs fi_+ 1 -> rargs
    enduntil;
    if lamlist starts_with @&OPTIONAL then
        back(lamlist) -> lamlist;
        until endp(lamlist) or islamkeysym(front(lamlist) ->> item) do
            if check
            and ispair(item)
            and back(item) /== [] then
                Illegal_parameter('Default values for &OPTIONAL', item)
            endif;
            back(lamlist) -> lamlist;
            oargs fi_+ 1 -> oargs
        enduntil
    endif;
    if lamlist starts_with @&REST
    or lamlist starts_with @&KEY then
        true -> rest;
        destpair(lamlist) -> (item, lamlist)
    endif;
    if item == @&KEY then
        until endp(lamlist) or islamkeysym(front(lamlist) ->> item) do
            if check
            and ispair(item)
            and back(item) /== [] then
                Illegal_parameter('Default values for &KEY', item)
            endif;
            back(lamlist) -> lamlist
        enduntil
    endif;
    if lamlist starts_with @&ALLOW-OTHER-KEYS then
        back(lamlist) -> lamlist
    endif;
    if check
    and lamlist starts_with @&AUX then
        Illegal_parameter(@&AUX, lamlist)
    endif
enddefine;


define lconstant Check_congruent_lamlist(lamlist, gfn, source);
    lvars rargs, oargs, rest, spec;

    /* source can be one of:
        defgeneric  -   if lamlist comes from defgeneric
        defmethod   -   if lamlist comes from defmethod
        nil         -   if lamlist comes from ensure-generic-function
        false       -   if lamlist comes from define_method
    */

    returnif(lamlist == gfn_lamlist(gfn));

    lamlist_info(lamlist, source and source /== @DEFMETHOD)
        -> (rargs, oargs, rest);
    gfn_lamspec(gfn) -> spec;
    unless rargs == spec(1)
    and oargs == spec(2)
    and rest == spec(3) do
        lisp_error('Lambda list ~S not congruent with existing generic function lambda list',
            [^lamlist ^(gfn_lamlist(gfn))])
    endunless;
    if source and source /== @DEFMETHOD then
        lamlist -> gfn_lamlist(gfn)
    endif
enddefine;


define ensure_generic_function(name, source, env, lamlist, doc, options) -> p;
    lvars sym, ftok, rargs, oargs, rest, gfn, spec;
    dlocal Local_function_tokens = env;

    if list_assoc(name, env) then
        lisp_error('Cannot convert local function ~S to a generic function',
                    [^name])
    endif;

    unless options == [] do
        lisp_error('Generic function options not yet supported', [^options])
    endunless;

    fname_sym(name) -> sym;
    current_sf_token(sym) -> ftok;
    false -> p;

    if sym /== @NIL
    and isboundtoken(ftok)
    and (is_gfn_clos(ft_valof(ftok) ->> p) ->> gfn) then
        Check_congruent_lamlist(lamlist, gfn, source);
        if source == @DEFGENERIC then
            ncdelete_if(
                gfn_methods(gfn),
                procedure(m);
                    if method_source(m) == @DEFGENERIC then
                        ;;; [Deleting prior DEFGENERIC method ^(Return_iom(m))] =>
                        true
                    else
                        false
                    endif
                endprocedure)
                -> gfn_methods(gfn)
        endif
    else
        if p then
            type_cerror('Convert ~3*~S to a generic function',
                        p, @GENERIC-FUNCTION)
        endif;
        new_gfn_clos(sym, lamlist,
                        {% lamlist_info(lamlist, source /== @DEFMETHOD) %})
            -> p;
        unless sym == @NIL do
            re_declare_token(ftok, pop_true(constant_functions), false);
            p -> ft_valof(ftok)
        endunless
    endif;
    if pop_true(doc)
    and sym /== nil then
        doc -> documentation(name, @FUNCTION)
    endif
enddefine;


define remove_method(p, iom) -> p;
    lvars gfn;
    if (is_gfn_clos(p) ->> gfn) then
        ncdelete(Accept_iom(iom), gfn_methods(gfn)) -> gfn_methods(gfn)
    endif
enddefine;


define delete_method(m);
    /* Called by clos_define_class to remove obsolete read/write methods */
    lvars name, gfn;
    fname_sym(method_name(m)) -> name;
    if fboundp(name)
    and (is_gfn_clos(symbol_function(name)) ->> gfn) then
        ;;; [Deleting ^m] =>
        ncdelete(m, gfn_methods(gfn)) -> gfn_methods(gfn)
    endif
enddefine;


define method_specs_eql(s1, s2);
    lvars c1, c2;
    for c1, c2 in_vector s1, s2 do
        unless c1 == c2
        or (ispair(c1) and ispair(c2) and eql(cadr(c1), cadr(c2))) do
            return(false)
        endunless
    endfor;
    true
enddefine;


define lconstant Locate_method(gfn, q, specs);
    lvars methods, m;
    gfn_methods(gfn) -> methods;
    until methods == [] do
        front(methods) -> m;
        if method_qcode(m) == q
        and method_specs_eql(method_specs(m), specs) then
            return(methods)
        endif;
        back(methods) -> methods
    enduntil;
    false
enddefine;


define add_method(p, iom) -> p;
    lvars m, gfn, name, pair, old;
    Accept_iom(iom) -> m;
    if (is_gfn_clos(p) ->> gfn) then
        if (method_name(m) ->> name) == nil then
            gfn_name(gfn) ->> method_name(m) -> f_name(method_body(m))
        elseunless name == gfn_name(gfn) do
            lisp_error('Method ~S does not belong to generic function ~S',
                        [^m ^(gfn_name(gfn))])
        endif;
        /* Should check lambda-list congruence here */
        if (Locate_method(gfn, method_qcode(m), method_specs(m)) ->> pair)
        then
            ;;; [Replacing ^(front(pair))] =>
            if istraced(method_body(front(pair)) ->> old) then
                copy(old) -> old;
                method_body(m) -> frozval(1, old);
                old -> method_body(m)
            endif;
            m -> front(pair)
        else
            conspair(m, gfn_methods(gfn)) -> gfn_methods(gfn)
        endif
    endif
enddefine;


/* DEFMETHOD */

define lconstant Create_method_qcode(qualifiers);
    lvars q;
    if qualifiers == [] then
        Qcode_NIL
    elseif ispair(qualifiers)
    and back(qualifiers) == []
    and (vmember(front(qualifiers), Qcodes) ->> q) then
        q
    else
        lisp_error('Only standard method combination supported at present',
                    [^qualifiers])
    endif
enddefine;


define lconstant Create_method_specs(p_specs);
    lvars p, c;
    {% for p in_cl_list p_specs do
        if (get_class_by_name(p) ->> c) then
            c
        elseif isprocedure(p) then
            [% @EQL, lisp_apply(p, 0, false) %]
        else
            /* Specializers from FIND-METHOD */
            if p starts_with @EQL then
                p
            elseif is_instance_of(p, CLASS @CLASS) then
                Accept_ioc(p, false)
            else
                lisp_error('Invalid parameter specializer', [^p])
            endif
        endif
    endfor %}
enddefine;


define lconstant Create_method_tests(specs);
    lvars c, p;
    {% for c in_vector specs do
        if c starts_with @EQL then
            eql(% cadr(c) %)
        elseif (type_predicate(Return_ioc(c) ->> c) ->> p) then
            p
        else
            lisp_error('Invalid parameter specializer', [^c])
        endif
    endfor %}
enddefine;


define define_method(p, source, qualifiers, lamlist, p_specs, body) -> iom;
    lvars gfn, q, specs, tests, m, doc;
    unless (is_gfn_clos(p) ->> gfn) do
        lisp_error('System error: non-generic function supplied to define_method',
                    [^p])
    endunless;
    Check_congruent_lamlist(lamlist, gfn, false);
    Create_method_qcode(qualifiers) -> q;
    Create_method_specs(p_specs) -> specs;
    Create_method_tests(specs) -> tests;
    checkr_function(body) -> body;
    consmethod(nil, source, q, specs, tests, body, false)
        -> m;
    add_method(p, m) ->;
    Return_iom(m) -> iom;
    if (documentation(body, @FUNCTION) ->> doc) then
        doc -> documentation(iom, @METHOD)
    endif
enddefine;


/* FIND-METHOD */

define method_qualifiers(iom) -> q;
    subscrv(method_qcode(Accept_iom(iom)), Qcodes) -> q;
    unless listp(q) do
        conspair(q, []) -> q
    endunless
enddefine;


define find_method(p, qualifiers, p_specs, errp);
    lvars gfn, pair;
    if (is_gfn_clos(p) ->> gfn) then
        if (Locate_method(gfn, Create_method_qcode(qualifiers),
                               Create_method_specs(p_specs)) ->> pair)
        then
            Return_iom(front(pair))
        elseif pop_true(errp) then
            lisp_error('Unable to find method', [^p ^qualifiers ^p_specs])
        else
            nil
        endif
    endif
enddefine;


/* Printing instances */

vars print_instance_slots = true;

defprop instance_id;

vars next_instance_id = 0;

procedure(i, p);
    next_instance_id + 1 ->> next_instance_id ->> p(i)
endprocedure -> property_active(instance_id);


define pr_instance(Instance);
    lvars class, c, m, q;
    dlocal print_array = true;
    dlocal depth_printed, length_printed = 0;
    CHECK_PR_LEVEL

    i_class(Instance) -> class;
    cucharout(`#`);
    cucharout(`<`);
    _lisppr(class_name(class));
    cucharout(`\s`);
    if class == (CLASS @STANDARD-CLASS)
    or class == (CLASS @STRUCTURE-CLASS)
    or class == (CLASS @BUILT-IN-CLASS) then
        _lisppr(class_name(Accept_ioc(Instance, false)))
    elseif class == (CLASS @STANDARD-METHOD)
    and ismethod(Accept_iom(Instance) ->> m) then
        unless (method_qcode(m) ->> q) == Qcode_NIL do
            _lisppr(subscrv(q, Qcodes));
            cucharout(`\s`)
        endunless;
        _lisppr(method_name(m));
        cucharout(`\s`);
        _lisppr([% for c in_vector method_specs(m) do
                    if ispair(c) then c else class_name(c) endif
                endfor %])
    elseif pop_true(print_instance_slots) then
        _lisppr(i_slots(Instance))
    else
        cucharout(`#`);
        _lisppr(instance_id(Instance))
    endif;
    cucharout(`>`)
enddefine;


pr_instance -> lisp_class_print(instance_key);


procedure() with_nargs 1;
    ->;
    appdata('#<UNBOUND>', cucharout)
endprocedure -> lisp_class_print(unbound_slot_value_key);


endsection;


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 27 1999
        Changed comment to refer to clos_define_class and instance to Instance
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, May 19 1995
        Added fastprocs declarations. Slightly improved qcode handling.
        call_next_method mishaps if called out of context.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Feb  9 1995
        ensure_generic_function now signals an error if applied to a local
        function name.
--- John Williams, May 23 1994
        Added variable print_instance_slots (not yet exported to Lisp),
        also fixed bug in tracing of methods.
--- John Williams, Apr 27 1994
        Lexical environments not yet supported warning issued with warn_once.
 */
