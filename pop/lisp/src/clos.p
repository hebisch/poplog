/* --- Copyright University of Sussex 1999. All rights reserved. ----------
 > File:            C.all/lisp/src/clos.p
 > Purpose:         CLOS classes and instances
 > Author:          John Williams, Feb 3 1994 (see revisions)
 > Documentation:
 > Related Files:   C.all/lisp/src/clos-types.p, C.all/lisp/src/methods.p
 */

lisp_compile_mode;

section $-lisp;

constant procedure (eql_member, fdefinition, nconc);

fastprocs front, back, destpair, subscrv, lmember, vmember, for;


defprop
    get_class_by_name,
    get_class_by_key,
    ;


defclass instance
    {   i_structure,
        i_slots
    };


defclass instance_structure
    {   istr_local_slot_names,
        istr_shared_slot_names,
        istr_shared_slot_cells,
        istr_class,
        istr_current
    };


/* Cancel global identifiers which may have been defined by Objectclass */

applist([class_name class_supers define_method isclass], syscancel);


defclass class
    {   class_name,
        class_meta,
        class_supers,
        class_prec_list,
        class_subclasses,
        class_is_subclass,
        class_rebuild_args,
        class_local_slot_info,
        class_shared_slot_info,
        class_default_initargs,
        class_istr,
        class_rw_methods,
        class_instance,
    };


procedure(c);
    printf('<class %S>', [% symbol_name(class_name(c)) %])
endprocedure -> class_print(class_key);


defclass slot_info
    {   sli_name,
        sli_inherited,
        sli_initargs,
        sli_initform,
    };


defclass unbound_slot_value
    {};

constant Unbound = consunbound_slot_value();

define lconstant Return_unbound(); Unbound enddefine;


/* Constructing new instances, instance_structures, and classes */

define newinstance(istr);
    lvars slots;
    initvectorclass(
        fast_vector_length(istr_local_slot_names(istr)),
        Unbound,
        vector_key) -> slots;
    consinstance(istr, slots)
enddefine;


define newinstance_structure(class);
    consinstance_structure(nullvector, nullvector, nullvector, class, true)
enddefine;


define newclass(name, meta);
    lvars is_subclass_prop;
    check_name(name, @CLASS);
    check_name(meta, @:METACLASS);
    newanyproperty([], 8, 1, 8, false, false, "perm", false, false)
        -> is_subclass_prop;
    consclass(name, meta, [], false, [], is_subclass_prop,
                false, {}, {}, [], false, [], false)
enddefine;


define Return_ioc(class) -> ioc;            /* ioc = instance of class */
    lvars meta, istr;
    unless (class_instance(class) ->> ioc) do
        if (get_class_by_name(class_meta(class)) ->> meta)
        and (class_istr(meta) ->> istr) then
            consinstance(istr, {^class}) ->> ioc -> class_instance(class);
            type_predicate(class_name(class)) -> type_predicate(ioc)
        else
            class_name(class) -> ioc;
            advise('Cannot create instance of class ~S', [^ioc])
        endif
    endunless
enddefine;


define i_class() with_nargs 1;
    istr_class(i_structure())
enddefine;


define i_class_instance() with_nargs 1;
    Return_ioc(i_class())
enddefine;


define Accept_ioc(ioc, meta) -> class;
    if isinstance(ioc)
    and not(meta)
    or i_class(ioc) == get_class_by_name(meta) then
        subscrv(1, i_slots(ioc)) -> class
    elseif isclass(ioc) then
        ioc -> class
    else
        type_error(ioc, meta)
    endif
enddefine;


/* Dealing with incomplete or obsolete classes */

constant procedure build_class;


define rebuild_class(class, do_subclasses);
    ;;; [Resuming definition of ^class] =>
    build_class(
        class,
        explode(class_rebuild_args(class)),
        do_subclasses);
    class_istr(class)
enddefine;


define current_class_istr(class) -> istr;
    unless (class_istr(class) ->> istr) do
        if class_rebuild_args(class) then
            unless (rebuild_class(class, false) ->> istr) do
                if class_prec_list(class) then
                    lisp_error('Unable to re-build obsolete class', [^class])
                else
                    lisp_error('Class has undefined superclasses', [^class])
                endif
            endunless
        else
            lisp_error('Cannot create instance of forward referenced superclass',
                        [^class])
        endif
    endunless
enddefine;


define make_instances_obsolete(ioc);

    define lconstant Recover_class_rebuild_args(class, oldistr);
        lvars vec, i, sli;
        {%  oldistr,
            [% class_local_slot_info(class) -> vec;
            for i from fast_vector_length(vec) by -1 to 1 do
                subscrv(i, vec) -> sli;
                unless sli_inherited(sli) do
                    sli
                endunless
            endfor %];
            [% class_shared_slot_info(class) -> vec;
            for i from fast_vector_length(vec) by -1 to 1 do
                subscrv(i, vec) -> sli;
                unless sli_inherited(sli) do
                    sli
                endunless
            endfor %];
            false;
            false;
        %}  -> class_rebuild_args(class)
    enddefine;

    define lconstant Make_class_obsolete(class);
        lvars oldistr;
        ;;; [Making ^class obsolete] =>
        if (class_istr(class) ->> oldistr) then
            false ->> istr_current(oldistr) -> class_istr(class)
        endif;
        unless class_rebuild_args(class) do
            Recover_class_rebuild_args(class, oldistr)
        endunless;
        applist(class_subclasses(class), Make_class_obsolete)
    enddefine;

    Make_class_obsolete(Accept_ioc(ioc, @STANDARD-CLASS))
enddefine;


/* Locating and accessing slots in an instance */

constant macro Access_slot = "fast_subscrv";

constant procedure (class_of, update_instance);

vars instance_being_updated = false;


define current_i_structure(Instance) -> istr;
    i_structure(Instance) -> istr;
    unless istr_current(istr) or Instance == instance_being_updated do
        update_instance(Instance);
        i_structure(Instance) -> istr
    endunless;
enddefine;


define lconstant Locate_slot(name, Instance);
    lvars istr, i;

    current_i_structure(Instance) -> istr;
    if (vmember(name, istr_local_slot_names(istr)) ->> i) then
        i, i_slots(Instance)
    elseif (vmember(name, istr_shared_slot_names(istr)) ->> i) then
        1, Access_slot(i, istr_shared_slot_cells(istr))
    else
        false, false
    endif
enddefine;


define lconstant Slot_exists(name, object);
    if isinstance(object) then
        Locate_slot(name, object)
    else
        false, false
    endif
enddefine;


define slot_boundp(object, name);
    lvars i, vec;
    Slot_exists(name, object) -> (i, vec);
    if i then
        Access_slot(i, vec) /== Unbound
    else
        lisp_apply(
            class_of(object), object, name, @SLOT-BOUNDP,
            fdefinition(@SLOT-MISSING), 4, 1)
    endif
enddefine;


define slot_exists_p(object, name);
    if erase(Slot_exists(name, object)) then
        true
    else
        false
    endif
enddefine;


define slot_makunbound(object, name) -> object;
    lvars i, vec;
    Slot_exists(name, object) -> (i, vec);
    if i then
        Unbound -> Access_slot(i, vec)
    else
        lisp_apply(
            class_of(object), object, name, @SLOT-MAKUNBOUND,
            fdefinition(@SLOT-MISSING), 4, 0)
    endif
enddefine;


define slot_value(object, name);
    lvars i, vec, val;
    Slot_exists(name, object) -> (i, vec);
    if i then
        Access_slot(i, vec) -> val;
        if val == Unbound then
            lisp_apply(
                i_class_instance(object), object, name,
                fdefinition(@SLOT-UNBOUND), 3, 1)
        else
            val
        endif
    else
        lisp_apply(
            class_of(object), object, name, @SLOT-VALUE,
            fdefinition(@SLOT-MISSING), 4, 1)
    endif
enddefine;


define updaterof slot_value(val, object, name);
    lvars i, vec;
    Slot_exists(name, object) -> (i, vec);
    if i then
        val -> Access_slot(i, vec)
    else
        lisp_apply(
            class_of(object), object, name, @SETF, val,
            fdefinition(@SLOT-MISSING), 5, 0)
    endif
enddefine;


define i_local_slot_names() with_nargs 1;
    /* For MAKE-LOAD-FORM-SAVING-SLOTS */
    conslist(destvector(istr_local_slot_names(current_i_structure())))
enddefine;


/* Recognising sub-classes and instances of a given class */

vars procedure sys_class_of;


define is_subclass_of(class, super);
    if class == super then
        0
    else
        class_is_subclass(super)(class)
    endif
enddefine;


define is_instance_of(item, c);
    lvars class;
    sys_class_of(item) -> class;
    class == c
        or
    (class_is_subclass(c)(class) and true)
enddefine;



/* FIND-CLASS */

define ensure_class(name, meta) -> class;
    if (get_class_by_name(name) ->> class) then
        unless name == @T do
            if class_meta(class) /== meta then
                type_error(Return_ioc(class), meta)
            endif
        endunless
    else
        newclass(name, meta) ->> class -> get_class_by_name(name)
    endif
enddefine;


define syntax CLASS;
    lvars item, class;
    itemread() -> item;
    if (get_class_by_name(item) ->> class) then
        sysPUSHQ(class)
    else
        sysPUSHQ(item);
        sysCALL("ident get_class_by_name")
    endif
enddefine;


define find_class(name, errp, env);
    lvars class, sti;
    defaults errp true, env nil;
    if (get_class_by_name(name) ->> class) then
        Return_ioc(class)
    elseif pop_true(errp) then
        lisp_error('No class called ~S', [^name])
    else
        nil
    endif
enddefine;


define updaterof find_class(newioc, name, errp, env);
    lvars class;
    Accept_ioc(newioc, @STANDARD-CLASS) -> class;
    check_name(name, @CLASS);
    declare_new_type(name);
    class -> get_class_by_name(name);
    if lisp_apply(newioc, fdefinition(@CLASS-NAME), 1, 1) == name then
        type_predicate(newioc) or is_instance_of(% class %)
    else
        false
    endif -> type_predicate(name)
enddefine;


/* Computing the Class Precedence List */

define lconstant Tsort(Sc, R);
    lvars C, pair, on_lefts, result = [];

    repeat
        /* "Topological sorting proceeds by finding a class C in Sc
            such that no other class precedes that element according to
            the elements in R".

            I.e, find the classes in Sc that only occur on the lefts of
            pairs in R.
        */

        [% for C in Sc do
            for pair in R do
                if C == pair(2) then
                    nextloop(2)
                endif
            endfor;
            C
        endfor %] -> on_lefts;

        /* "Stop when no element can be found that has no predecessor.
            If Sc is not empty, the set R is inconsistent".
        */

        #_IF DEF DEBUG_TSORT
            [Sc ^Sc] =>
            [R ^R] =>
            [on_lefts ^on_lefts] =>
            [result ^result] =>
            nl(1);
        #_ENDIF

        if on_lefts == [] then
            if Sc == [] then
                return(ncrev(result))
            else
                mishap(Sc, R, 2, 'Set R is inconsistent')
            endif
        endif;

        /* "The class C is placed first in the result.
            Remove C from Sc, and remove all pairs of the form (C,D) from R".
        */

        if back(on_lefts) == [] then
            front(on_lefts) -> C;
           GOT_C:
            C :: result -> result;
            ncdelete(C, Sc) -> Sc;
            ncdelete_if(R, procedure(p); p(1) == C endprocedure) -> R;
        else
            /* "Sometimes there are several classes with no predecessors.
                In this case select the one that has a direct subclass
                rightmost in the class precedence list computed so far".

                I.e. Select the class from on_lefts whose direct subclass
                appears *leftmost* in result (because result is reversed).
            */
            lvars c, s;
            for c in result do
                for s in class_supers(c) do
                    if lmember(s, on_lefts) then
                        s -> C;
                        goto GOT_C
                    endif
                endfor
            endfor;
            mishap(on_lefts, result, 2,
                    'Cannot select next class for precedence list');
        endif
    endrepeat
enddefine;


define class_precedence_list(class);
    lvars c, Sc, R;

    define lconstant Compute_Sc(class);
        unless on_stack(class) do
            class;
            applist(class_supers(class), Compute_Sc)
        endunless
    enddefine;

    define lconstant Compute_Rc(class);
        lvars c, prev;
        class -> prev;
        for c in class_supers(class) do
            [% prev, c %];
            c -> prev
        endfor
    enddefine;

    [% Compute_Sc(class) %] -> Sc;

    [% for c in Sc do
        Compute_Rc(c)
    endfor %] -> R;

    Tsort(Sc, R)
enddefine;


/* DEFCLASS */

constant procedure (define_method, delete_method, ensure_generic_function);


define lconstant Parse_slot(slot) -> (name, allocation, initargs, initform,
                                        readers, writers);
    lvars
        allocation  =   false,
        initargs    =   [],
        initform    =   false,
        readers     =   [],
        writers     =   [],
        s, option, item;

    if atom(slot) then
        slot, []
    else
        destpair(slot)
    endif -> (name, s);

    define lconstant Get_val();
        if ispair(s) then
            destpair(s) -> s
        else
            program_error('No value for slot option ~S', [^option ^slot])
        endif
    enddefine;

    check_name(name, 'slot');
    until endp(s) do
        destpair(s) -> (option, s);
        if option == @:READER then
            pushnew(Get_val(), readers) -> readers
        elseif option == @:WRITER then
            pushnew(Get_val(), writers) -> writers
        elseif option == @:ACCESSOR then
            Get_val() -> item;
            pushnew(item, readers) -> readers;
            pushnew(item, writers) -> writers
        elseif option == @:ALLOCATION then
            if allocation then
                program_error('Duplicate :ALLOCATION slot option', [^slot])
            endif;
            Get_val() -> allocation
        elseif option == @:INITFORM then
            if initform then
                program_error('Duplicate :INITFORM slot option', [^slot])
            endif;
            Get_val() -> initform
        elseif option == @:INITARG then
            pushnew(Get_val(), initargs) -> initargs
        elseif option == @:DOCUMENTATION then
            Get_val() ->;
            warn_once('Sorry, :DOCUMENTATION slot option not implemented yet',
                        [^slot])
        elseif option == @:TYPE then
            Get_val() ->;
            warn_once('Sorry, :TYPE slot option not implemented yet', [^slot])
        else
            program_error('Unrecognised slot option ~S', [^option ^slot])
        endif
    enduntil;
    ncrev(readers) -> readers;
    ncrev(writers) -> writers;
    allocation or @:INSTANCE -> allocation;
    initform or Return_unbound -> initform;
    ncrev(initargs) -> initargs
enddefine;


define lconstant Parse_slots(slots) -> (local_slot_infos, shared_slot_infos,
                                         readers, writers);

    lvars names, slot, name, allocation, initargs, initform, r, w, sli, i;

    [] ->> names ->> local_slot_infos -> shared_slot_infos;
    [] ->> readers -> writers;

    for slot in_cl_list slots do
        Parse_slot(slot) -> (name, allocation, initargs, initform, r, w);
        if lmember(name, names) then
            program_error('Duplicate slot name ~S', [^name ^slots])
        endif;
        name :: names -> names;
        consslot_info(name, false, initargs, initform) -> sli;
        if allocation == @:INSTANCE then
            sli :: local_slot_infos -> local_slot_infos
        elseif allocation == @:CLASS then
            sli :: shared_slot_infos -> shared_slot_infos
        else
            program_error('Unrecognised slot allocation type ~S',
                        [^allocation])
        endif;
        nconc(readers, [% for i in r do conspair(i, name) endfor %])
            -> readers;
        nconc(writers, [% for i in w do conspair(i, name) endfor %])
            -> writers;
    endfor
enddefine;


define build_class(class, oldistr, local_slot_infos, shared_slot_infos,
                    readers, writers, do_subclasses);
    lvars shared_slot_cells, c, sli, i, istr;
    dlvars local_slot_infos, shared_slot_infos, prec_list;

    /* Check all super classes are defined */
    for c in class_supers(class) do
        unless class_istr(c)
        or (class_rebuild_args(c) and rebuild_class(c, false))
        do
            unless class_rebuild_args(class) do
                {% oldistr, local_slot_infos, shared_slot_infos,
                   readers, writers
                %} -> class_rebuild_args(class)
            endunless;
            ;;; [Suspending definition of ^class] =>
            return
        endunless
    endfor;

    false -> class_rebuild_args(class);

    /* Remove class from the class_is_subclass properties of classes
        in its previous class precedence list.
    */
    if class_prec_list(class) then
        for c in class_prec_list(class) do
            false -> class_is_subclass(c)(class)
        endfor
    endif;

    /* Now create new class_prec_list for class */

    back(class_precedence_list(class))
        ->> prec_list -> class_prec_list(class);
    0 -> i;
    for c in prec_list do
        (i + 1 ->> i) -> class_is_subclass(c)(class)
    endfor;

    /* Deal with inherited initargs */

    define lconstant Inherited_initargs(name);
        lvars c, vec, sli, i;
        [%  for c in prec_list do
                for vec in [% class_local_slot_info(c),
                              class_shared_slot_info(c) %] do
                    for sli in_vector vec do
                        if sli_name(sli) == name
                        and not(sli_inherited(sli)) then
                            for i in sli_initargs(sli) do
                                unless on_stack(i) do i endunless
                            endfor
                        endif
                    endfor
                endfor
            endfor
        %]
    enddefine;

    for sli in local_slot_infos do
        nconc(sli_initargs(sli), Inherited_initargs(sli_name(sli)))
            -> sli_initargs(sli)
    endfor;
    for sli in shared_slot_infos do
        nconc(sli_initargs(sli), Inherited_initargs(sli_name(sli)))
            -> sli_initargs(sli)
    endfor;

    /* Create shared slot cells */

    lblock;
        lvars oldnames, oldcells;
        if oldistr then
            istr_shared_slot_names(oldistr) -> oldnames;
            istr_shared_slot_cells(oldistr) -> oldcells;
            [% for sli in shared_slot_infos do
                if (vmember(sli_name(sli), oldnames) ->> i) then
                    Access_slot(i, oldcells)
                else
                    false
                endif
            endfor %]
        else
            [% for sli in shared_slot_infos do false endfor %]
        endif -> shared_slot_cells
    endlblock;

    /* Make sli records for inherited slots */

    define lconstant Got_slot_name(name);
        lvars sli;
        for sli in local_slot_infos do
            returnif (sli_name(sli) == name) (true)
        endfor;
        for sli in shared_slot_infos do
            returnif (sli_name(sli) == name) (true)
        endfor;
        false
    enddefine;

    for c in prec_list do
        for sli in_vector class_local_slot_info(c) do
            unless Got_slot_name(sli_name(sli)) do
                unless sli_inherited(sli) do
                    copy(sli) -> sli;
                    c -> sli_inherited(sli);
                    sli :: local_slot_infos -> local_slot_infos
                endunless
            endunless
        endfor;
        for sli with_index i in_vector class_shared_slot_info(c) do
            unless Got_slot_name(sli_name(sli)) do
                unless sli_inherited(sli) do
                    copy(sli) -> sli;
                    c -> sli_inherited(sli);
                    sli :: shared_slot_infos -> shared_slot_infos;
                    Access_slot(i, istr_shared_slot_cells(class_istr(c)))
                        :: shared_slot_cells -> shared_slot_cells
                endunless
            endunless
        endfor;
    endfor;

    /* Copy sli records from local_slot_infos and shared_slot_infos
        into class_local_slot_info and class_shared_slot_info fields
        of class. Remember the lists are in reverse order.
    */
    ncrev(local_slot_infos) -> local_slot_infos;
    ncrev(shared_slot_infos) -> shared_slot_infos;
    ncrev(shared_slot_cells) -> shared_slot_cells;
    consvector(destlist(local_slot_infos)) -> class_local_slot_info(class);
    consvector(destlist(shared_slot_infos)) -> class_shared_slot_info(class);

    /* Create class_istr field for class */

    newinstance_structure(class) ->> istr -> class_istr(class);
    {% applist(local_slot_infos, sli_name) %}
        -> istr_local_slot_names(istr);
    {% applist(shared_slot_infos, sli_name) %}
        -> istr_shared_slot_names(istr);

    /* Initialise istr_shared_slot_cells for istr */

    {% for c, sli in_list shared_slot_cells, shared_slot_infos do
        c or {% lisp_apply(sli_initform(sli), 0, false) %}
    endfor %} -> istr_shared_slot_cells(istr);

    /* Create read/write methods */

    lblock;
        lvars p, c, lamlist;
        lconstant i = make_symbol('INSTANCE');
        lconstant v = make_symbol('VALUE');
        class_name(class) -> c;
        [%  if readers then
                [^i] -> lamlist;
                for p in readers do
                    define_method(
                        ensure_generic_function(
                            front(p), nil, [], lamlist, [], []),
                        nil, [], lamlist, [^c],
                        slot_value(% back(p) %)) ->;
                endfor
            endif;
            if writers then
                [^v ^i] -> lamlist;
                for p in writers do
                    define_method(
                        ensure_generic_function(
                            [% @SETF, front(p) %], nil, [], lamlist, [], []),
                        nil, [], lamlist, [^@T ^c],
                        updater(slot_value)(% back(p) %)) ->;
                endfor
            endif
        %] -> class_rw_methods(class)
    endlblock;

    /* Try completing any suspended definitions of classes that have
        class as a super class */

    if do_subclasses then
        for c in class_subclasses(class) do
            unless class_istr(c) do
                if class_rebuild_args(c) then
                    rebuild_class(c, true) ->
                endif
            endunless
        endfor
    endif
enddefine;


define clos_define_class(name, meta, supers, slots, default_initargs, doc, res);
    lvars class, oldistr, local_slot_infos, shared_slot_infos,
            readers, writers, s, c;

    declare_new_type(name);
    ensure_class(name, meta) -> class;
    class_istr(class) -> oldistr;

    if meta == @STANDARD-CLASS then
        if supers == [] then
            [^@STANDARD-OBJECT] -> supers
        endif;
    elseif meta == @STRUCTURE-CLASS then
        if supers == [] then
            [^@STRUCTURE-OBJECT] -> supers
        endif
    elseif meta == @BUILT-IN-CLASS then
        unless slots == [] do
            program_error('Cannot specify slots for a built-in class', [^slots])
        endunless;
        if supers == [] then
            unless name == @T do
                [^@T] -> supers
            endunless
        endif
    endif;

    Parse_slots(slots)
        -> (local_slot_infos, shared_slot_infos, readers, writers);

    /* Remove class from class_subclasses lists of previous supers,
        and add class to class_subclasses lists of new supers.
    */

    for c in class_supers(class) do
        ncdelete(class, class_subclasses(c)) -> class_subclasses(c)
    endfor;

    [% for s in_cl_list supers do
        ensure_class(s, meta) ->> c;
        pushnew(class, class_subclasses(c)) -> class_subclasses(c)
    endfor %] -> class_supers(class);

    if meta == @STANDARD-CLASS then
        if oldistr then
            lisp_apply(
                Return_ioc(class),
                fdefinition(@MAKE-INSTANCES-OBSOLETE),
                1, 0)
        endif;
        applist(class_rw_methods(class), delete_method)
    endif;

    unless type_predicate(name) do
        is_instance_of(% class %) -> type_predicate(name)
    endunless;
    default_initargs -> class_default_initargs(class);
    build_class(class, oldistr, local_slot_infos, shared_slot_infos,
                readers, writers, true);
    if pop_true(doc) then
        doc -> documentation(name, @TYPE)
    endif;
    if res then
        Return_ioc(class)
    endif
enddefine;


/* Creating and initializing instances */

define allocate_instance() with_nargs 1;
    newinstance(current_class_istr(Accept_ioc(@STANDARD-CLASS)))
enddefine;


define shared_initialize(Instance, Slot_names, Initargs) -> Instance;
    lvars istr, class, vec, sli, i;
    dlvars Slot_names, Initargs, Preclist, All;

    returnif(Slot_names == [] and Initargs == []);

    current_i_structure(Instance) -> istr;
    istr_class(istr) -> class;
    conspair(class, class_prec_list(class)) -> Preclist;
    Slot_names == @T -> All;

    define lconstant Try_initarg(sli);
        lvars slot_initargs, iarg, c;
        dlocal Initargs;
        sli_initargs(sli) -> slot_initargs;
        until Initargs == [] do
            destpair(Initargs) -> (iarg, Initargs);
            if Initargs == [] then
                lisp_error('No value for initialization argument ~S', [^iarg])
            endif;
            if lmember(iarg, slot_initargs) then
                return(front(Initargs))
            endif;
            back(Initargs) -> Initargs
        enduntil;
        for c in Preclist do
            class_default_initargs(c) -> Initargs;
            until Initargs == [] do
                destpair(Initargs) -> (iarg, Initargs);
                if lmember(iarg, slot_initargs) then
                    return(lisp_apply(front(Initargs), 0, false))
                endif;
                back(Initargs) -> Initargs
            enduntil
        endfor;
        false
    enddefine;

    define lconstant Initialise_slot(i, sli, vec);
        lvars val;
        if (Try_initarg(sli) ->> val)
        or All
        or eql_member(sli_name(sli), Slot_names) then
            if val then
                val -> Access_slot(i, vec)
            elseif Access_slot(i, vec) == Unbound then
                lisp_apply(sli_initform(sli), 0, false)
                    -> Access_slot(i, vec)
            endif
        endif
    enddefine;

    /* Initialise local slots */
    i_slots(Instance) -> vec;
    for sli with_index i in_vector class_local_slot_info(class) do
        Initialise_slot(i, sli, vec)
    endfor;

    /* Initialise shared slots */
    istr_shared_slot_cells(istr) -> vec;
    for sli with_index i in_vector class_shared_slot_info(class) do
        Initialise_slot(1, sli, Access_slot(i, vec))
    endfor
enddefine;


define change_class(Instance, newioc) -> Instance;
    lvars newistr, oldistr, oldslots, name, i, vec;

    current_class_istr(Accept_ioc(newioc, @STANDARD-CLASS))
        -> newistr;
    current_i_structure(Instance) -> oldistr;
    i_slots(Instance) -> oldslots;

    {% for name in_vector istr_local_slot_names(newistr) do
        Locate_slot(name, Instance) -> (i, vec);
        if i then
            Access_slot(i, vec)
        else
            Unbound
        endif
    endfor %} -> i_slots(Instance);
    newistr -> i_structure(Instance);

    lisp_apply(
        consinstance(oldistr, oldslots),
        Instance,
        fdefinition(@UPDATE-INSTANCE-FOR-DIFFERENT-CLASS),
        2, 0)
enddefine;


define newly_added_slots(old, new);
    lvars name;
    [% for name in_vector istr_local_slot_names(i_structure(new)) do
        unless erase(Locate_slot(name, old)) do
            name
        endunless
    endfor %]
enddefine;


define update_instance(instance_being_updated);
    lvars oldistr, class, old_local_slot_names,
            newistr, new_local_slot_names, new_shared_slot_names,
            added_slot_names, new_local_slots, old_local_slots,
            name, i, vec, val, discarded_slot_names, discarded_slot_plist;

    dlocal instance_being_updated;

    i_structure(instance_being_updated) -> oldistr;
    istr_class(oldistr) -> class;
    istr_local_slot_names(oldistr) -> old_local_slot_names;
    current_class_istr(class) -> newistr;
    istr_local_slot_names(newistr) -> new_local_slot_names;
    istr_shared_slot_names(newistr) -> new_shared_slot_names;

    /* Local slots specified by the new class definition that are
        not specified as either local or shared by the old class
        are added. (Steele 1990 p811)
    */
    [] -> added_slot_names;
    {%  for name in_vector new_local_slot_names do
            Locate_slot(name, instance_being_updated) -> (i, vec);
            if i then
                Access_slot(i, vec)
            else
                name :: added_slot_names -> added_slot_names;
                Unbound
            endif
        endfor
    %} -> new_local_slots;

    /* Slots not specified as local or shared by the new class definition
        that are specified as local by the old class are discarded.
        (Steele 1990 p811)
    */
    i_slots(instance_being_updated) -> old_local_slots;
    [] -> discarded_slot_names;
    [%  for name with_index i in_vector old_local_slot_names do
            unless vmember(name, new_local_slot_names)
            or vmember(name, new_shared_slot_names) do
                name :: discarded_slot_names -> discarded_slot_names;
                Access_slot(i, old_local_slots) -> val;
                unless val == Unbound do
                    name, val
                endunless
            endunless
        endfor
    %]  -> discarded_slot_plist;

    newistr -> i_structure(instance_being_updated);
    new_local_slots -> i_slots(instance_being_updated);

    lisp_apply(
        instance_being_updated,
        added_slot_names,
        discarded_slot_names,
        discarded_slot_plist,
        fdefinition(@UPDATE-INSTANCE-FOR-REDEFINED-CLASS),
        4, 0)
enddefine;


endsection


/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug 27 1999
        changes to prevent clashes with objectclass:
            instance -> Instance (local variables)
            define_class -> clos_define_class
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Jun  5 1995
        Added i_local_slot_names (for make_load_form_saving_slots).
--- John Williams, May 19 1995
        Added fastprocs declarations.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Nov 16 1994
        Now cancels identifiers that may have already been defined by
        LIB * OBJECTCLASS.
--- John Williams, Apr 27 1994
        Warnings about unimplemented slot options now issued by warn_once.
 */
