/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/modules.p
 > Purpose:         PML: Static semantics of modules
 > Author:          Rob Duncan & Simon Nichols, Mar  8 1989 (see revisions)
 */


section $-ml;

/*
 *  Signature Names
 */

defclass signame [writeable] {
    signame_name    : full,     ;;; the signature name as a word
};

;;; new_signame:
;;;     constructor for signature names

constant macro new_signame = "conssigname";


/*
 *  Structure Names
 */

defclass strname [writeable] {
    strname_contents    : full,     ;;; for structure sharing
    strname_name        : full,     ;;; the structure name as a word
    strname_parent      : full,     ;;; optional parent structure
    strname_open        : bool,     ;;; whether the structure has been opened
};

;;; new_strname:
;;;     creates a new structure name from name and parent

define new_strname(name, parent);
    lvars name, parent;
    consstrname(stamp(), name, parent, false);
enddefine;

;;; fullname:
;;;     constructs a long identifier from a short identifier and its
;;;     defining structure name. Expansion stops at an open or anonymous
;;;     structure. If no short identifier is given, just the structure name
;;;     is expanded.

define fullname(id) -> id;
    lvars strname, id;
    if isword(id) then
        -> strname;
    else
        ;;; -id- should be a structure name
        strname_parent(id) -> strname;
        strname_name(id) -> id;
    endif;
    while strname and not(strname_open(strname)) and strname_name(strname) do
        conslongid(strname_name(strname), id) -> id;
        strname_parent(strname) -> strname;
    endwhile;
enddefine;

procedure(strname);
    lvars strname;
    unless pr == ml_pr then printf('<') endunless;
    printf('structure '); ml_pr(fullname(strname));
    unless pr == ml_pr then printf('>') endunless;
endprocedure -> class_print(strname_key);

;;; strname_deref:
;;;     follow a chain of sharing structure names to the end

define strname_deref(strname) -> strname;
    lvars strname;
    returnunless(isstrname(strname));
    while isstrname(fast_cont(strname)) do
        fast_cont(strname) -> strname;
    endwhile;
enddefine;

;;; is_substructure:
;;;     <true> if -strname1- is a proper substructure of -strname2-
;;;     (i.e. -strname2- is an ancestor of -strname1-)

define lconstant is_substructure(strname1, strname2);
    lvars strname1, strname2;
    while isstrname(strname1) do
        strname_deref(strname_parent(strname1)) -> strname1;
        returnif(strname2 == strname1)(true);
    endwhile;
    false;
enddefine;


/*
 *  Binding Structure and Type Names
 */

vars
    bound_names = false,
        ;;; indicates whether new type and structure names should be bound
;

;;; make_bound:
;;;     bind a structure or type name by setting its contents to <false>

define make_bound(name);
    lvars name;
    false -> fast_cont(name);
enddefine;

;;; is_bound_strname, is_bound_typename:
;;;     <true> if -strname-/-typename- is bound. Assumes that the name has
;;;     already been dereferenced.

define is_bound_strname(strname);
    lvars strname;
    isstrname(strname) and not(fast_cont(strname));
enddefine;

define is_bound_typename(typename);
    lvars typename;
    istypename(typename) and not(fast_cont(typename));
enddefine;


/*
 *  Type and Structure Sharing
 */

;;; admits_equality:
;;;     <true> if the (dereferenced) type function -tfnc- admits equality

define lconstant admits_equality(tfnc);
    lvars tfnc;
    if istypename(tfnc) then
        typename_equality(tfnc);
    else
        is_equality_type(typealias_type(tfnc));
    endif;
enddefine;

;;; same_constructors:
;;;     <true> if the two type constructors have the same constructors:
;;;     to be the same it's sufficient to agree on number, names, arities
;;;     and representation; checking the types is too hard.

define lconstant same_constructors(tycon1, tycon2);
    lvars con1, con2, cons1, cons2, tycon1, tycon2;
    tycon_cons(tycon1) -> cons1;
    tycon_cons(tycon2) -> cons2;
    until cons1 == [] or cons2 == [] do
        Destpair(cons1) -> cons1 -> con1;
        Destpair(cons2) -> cons2 -> con2;
        unless val_name(con1) == val_name(con2)
        and val_arity(con1) == val_arity(con2)
        then
            return(false);
        endunless;
    enduntil;
    cons1 == cons2 and tycon_datarep(tycon1) == tycon_datarep(tycon2);
enddefine;

;;; same_type_function:
;;;     <true> if -tycon1- and -tycon2- have the same type function.
;;;     Its assumed that their arities are known to be the same.

define lconstant same_type_function(tycon1, tycon2);
    lvars tv, tvs, tycon1, tycon2;
    [%
        Repeat tycon_arity(tycon1) times
            anytype(1) -> tv;
            true -> type_constraint(tv);
            tv;
        endrepeat
    %] -> tvs;
    test_unify(constype(tycon1, tvs), constype(tycon2, tvs));
enddefine;

;;; occurs_in_type:
;;;     <true> if the type function -tfnc- occurs in -ty-

define lconstant occurs_in_type(tfnc, ty);
    lvars   ty;
    dlvars  tfnc;

    define lconstant occurs(ty);
        lvars ty, tfnc1;
        if isvartype(ty) then
            false;
        elseif isrecordtype(ty) then
            any(type_fields(ty), occurs);
        elseif isfuntype(ty) then
            occurs(type_domain(ty)) or occurs(type_range(ty));
        elseif isconstype(ty) then
            typename_deref(type_function(ty)) -> tfnc1;
            tfnc == tfnc1
            or istypealias(tfnc1) and occurs(typealias_type(tfnc1))
            or any(type_arguments(ty), occurs);
        else
            bad_type(ty);
        endif;
    enddefine;

    occurs(ty);
enddefine;

;;; share_types:
;;;     tries sharing -tycon1- with -tycon2-

define lconstant share_types(tycon1, tycon2);
    lvars tycon1, tycon2, tfnc1, tfnc2;
    unless tycon_arity(tycon1) == tycon_arity(tycon2)
    and (tycon_cons(tycon1) == [] or same_constructors(tycon1, tycon2))
    then
        false;
    else
        typename_deref(tycon_function(tycon1)) -> tfnc1;
        typename_deref(tycon_function(tycon2)) -> tfnc2;
        if tfnc1 == tfnc2 then
            true;
        elseif is_bound_typename(tfnc1)
        and (istypename(tfnc2) or not(occurs_in_type(tfnc1, typealias_type(tfnc2))))
        then
            if typename_equality(tfnc1) and not(admits_equality(tfnc2)) then
                false;
            else
                tfnc2 -> fast_cont(tfnc1);
                true;
            endif;
        else
            same_type_function(tycon1, tycon2);
        endif;
    endunless;
enddefine;

;;; share_structures:
;;;     tries sharing -struct1- with -struct2-. The only real constraint
;;;     on structure sharing is cycle-freedom, since in principal two
;;;     structures with completely different signatures could still be
;;;     views of the same underlying structure.

define lconstant share_structures(struct1, struct2);
    lvars struct1, struct2, strname1, strname2;
    if isstr(struct1) then str_env(struct1) -> struct1 endif;
    if isstr(struct2) then str_env(struct2) -> struct2 endif;
    strname_deref(structenv_strname(struct2)) -> strname2;
    strname_deref(structenv_strname(struct1)) -> strname1;
    if strname1 == strname2 then
        true;
    elseif not(is_bound_strname(strname1)) then
        ;;; struct1 already sharing with something concrete:
        ;;; struct2 should be identical
        false;
    elseif is_substructure(strname1, strname2)
    or is_substructure(strname2, strname1)
    then
        ;;; cyclic
        false;
    else
        strname2 -> fast_cont(strname1);
        true;
    endif;
enddefine;


/*
 *  Sharing Constraints
 */

define lconstant find_common_entries(env, entries) -> entries;
    lvars entry, es, entries, env;
    For entry in env do
        For es on entries do
            if entry_name(entry) == entry_name(Front(Front(es))) then
                conspair(entry, Front(es)) -> Front(es);
                nextloop(2);
            endif;
        endfor;
        conspair([^entry], entries) -> entries;
    endfor;
enddefine;

define lconstant expand_structure_sharing(strs) -> structures -> types;
    lvars str, strs, tycons, structures = [], types = [];
    For str in strs do
        find_common_entries(structenv_strs(str_env(str)), structures)
            -> structures;
        find_common_entries(structenv_tycons(str_env(str)), types)
            -> types;
    endfor;
    For tycons in (types, [] -> types) do
        unless Back(tycons) == [] then
            ;;; more than one entry -- include them as sharing
            conspair(tycons, types) -> types;
        endunless;
    endfor;
    For strs in (structures, [] -> structures) do
        unless Back(strs) == [] then
            ;;; more than one entry -- include them as sharing
            conspair(strs, structures) -> structures;
            expand_structure_sharing(strs)  ;;; returns structures and types
                <> structures -> structures,
                <> types -> types;
        endunless;
    endfor;
enddefine;

define lconstant do_sharing(sets, share);
    lvars item, canon, set, sets, procedure share;
    For set in sets do
        ;;; share all items in -set-
        Front(set) -> canon;
        For item in Back(set) do
            if share(item, canon) then
                ;;; OK
            elseif share(canon, item) then
                item -> canon;
            else
                return(item, canon, false);
            endif;
        endfor;
    endfor;
    true;
enddefine;

define make_shared(types, structures, linenum);
    lvars   strs, types, structures, linenum;
    ;;; for each list of sharing structures, add all their substructures
    ;;; and types into the sharing lists
    For strs in structures do
        expand_structure_sharing(strs)
            <> structures -> structures,
            <> types -> types;
    endfor;
    ;;; sort the sharing lists into equivalence sets and do the sharing
    unless do_sharing(make_disjoint(structures), share_structures)
    and do_sharing(make_disjoint(types), share_types)
    then
        ;;; -do_sharing- will have left the unshareable items on the stack
        lvars (item1, item2) = ();
        ml_error('impossible sharing constraint in signature\n%s\n',
            ['\t%p = %p' ^item1 ^item2], popfilename, linenum);
    endunless;
enddefine;


/*
 *  Signature and Functor Instances
 */

lvars
    newstrnames,
        ;;; mapping from bound structure names to new structure names
    newtypenames,
        ;;; mapping from bound type names to new type names
    newtycons,
        ;;; mapping from type constructors to new copies (this is used
        ;;; for saving space rather than for any semantic reason)
;

;;; rename_str, rename_type:
;;;     provide consistent renaming of bound structures and types

define lconstant rename_str(/* n */) -> n with_nargs 1;
    lvars n = strname_deref(/* n */), newn;
    if is_bound_strname(n) then
        if alookup(n, newstrnames) ->> newn then
            newn -> n;
        else
            acons(n, copy(n) ->> n, newstrnames) -> newstrnames;
            unless bound_names then stamp() -> fast_cont(n) endunless;
        endif;
    endif;
enddefine;

define lconstant rename_type(/* n */) -> n with_nargs 1;
    lvars n = typename_deref(/* n */), newn;
    if is_bound_typename(n) then
        if alookup(n, newtypenames) ->> newn then
            newn -> n;
        else
            acons(n, copy(n) ->> n, newtypenames) -> newtypenames;
            unless bound_names then stamp() -> fast_cont(n) endunless;
        endif;
    endif;
enddefine;

;;; structenv_instance:
;;;     creates an instance of a structure environment, i.e. a complete
;;;     copy with all bound type and structure names replaced consistently
;;;     throughout with new ones. The new names may be bound or not
;;;     depending on the value of -bound_names-. Type constructors are
;;;     replaced consistently too.

define lconstant structenv_instance(strenv, bound_names) -> strenv;
    lvars   strenv, strname;
    dlocal  bound_names;

    lconstant procedure copy_tycon;

    define lconstant fix_tycon(tycon) -> tycon;
        lvars tycon, tfnc;
        typename_deref(tycon_function(tycon)) -> tfnc;
        unless istypename(tfnc) and fast_cont(tfnc) then
            ;;; typealias or bound typename
            copy_tycon(tycon) -> tycon;
        endunless;
    enddefine;

    define lconstant copy_val(val) -> val;
        lvars val;
        copy(val) -> val;
        strname -> val_parent(val);
        maptype(val_type(val), fix_tycon) -> val_type(val);
        unless bound_names then
            ;;; generative -- applies to run-time values too
            copy(val_ident(val)) -> val_ident(val);
        endunless;
    enddefine;

    define lconstant copy_tycon(tycon) -> tycon;
        lvars tycon, tfnc, newtycon;
        returnif(alookup(tycon, newtycons) ->> newtycon)(newtycon -> tycon);
        acons(tycon, copy(tycon) ->> tycon, newtycons) -> newtycons;
        rename_str(tycon_parent(tycon)) -> tycon_parent(tycon);
        rename_type(tycon_function(tycon)) -> tfnc;
        if istypealias(tfnc) then
            copy(tfnc) -> tfnc;
            maptype(typealias_type(tfnc), fix_tycon) -> typealias_type(tfnc);
        endif;
        tfnc -> tycon_function(tycon);
        map(tycon_cons(tycon), copy_val) -> tycon_cons(tycon);
    enddefine;

    define lconstant copy_str(str) -> str;
        lvars str;
        copy(str) -> str;
        rename_str(str_strname(str)) -> str_strname(str);
        structenv_instance(str_env(str), bound_names) -> str_env(str);
    enddefine;

    copy(strenv) -> strenv;
    rename_str(structenv_strname(strenv)) -> strname;
    strname -> structenv_strname(strenv);
    map(structenv_strs(strenv), copy_str) -> structenv_strs(strenv);
    map(structenv_tycons(strenv), copy_tycon) -> structenv_tycons(strenv);
    map(structenv_vals(strenv), copy_val) -> structenv_vals(strenv);
enddefine;

;;; signature_instance:
;;;     creates an instance of a signature

define signature_instance(sig) -> sig;
    lvars   sig;
    dlocal  newstrnames = [], newtypenames = [], newtycons = [];
    copy(sig) -> sig;
    structenv_instance(sig_env(sig), true) -> sig_env(sig);
enddefine;

;;; functor_instance:
;;;     creates an instance of a functor: note how names in the argument
;;;     environment are kept bound while those in the result environment
;;;     are not.

define functor_instance(fnc) -> fnc;
    lvars   fnc;
    dlocal  newstrnames = [], newtypenames = [], newtycons = [];
    copy(fnc) -> fnc;
    structenv_instance(fnc_arg(fnc), true) -> fnc_arg(fnc);
    structenv_instance(fnc_env(fnc), false) -> fnc_env(fnc);
enddefine;


/*
 *  Signature Matching
 */

;;; find_entry:
;;;     find an entry in -env- with the same name as -entry-

define lconstant find_entry(entry, env) -> env -> entry;
    lvars name, env, entry;
    entry_name(entry) -> name;
    until env == [] or entry_name(Front(env)) == name do Back(env) -> env enduntil;
    if env == [] then sigmatch_error(entry, false) endif;
    Destpair(env) -> env -> entry;
enddefine;

;;; bind_names:
;;;     binds bound names in -strenv1- to the corresponding names in -strenv2-

define lconstant bind_names(strenv1, strenv2);
    lvars entry1, entry2, env, strenv1, strenv2;
    ;;; check sharing of -strenv1-
    unless share_structures(strenv1, strenv2) then
        sigmatch_error(strenv1, strenv2);
    endunless;
    structenv_strname(strenv2) -> structenv_strname(strenv1);
    ;;; bind sub-structure names of -strenv1-
    structenv_strs(strenv2) -> env;
    For entry1 in structenv_strs(strenv1) do
        find_entry(entry1, env) -> env -> entry2;
        str_strname(entry2) -> str_strname(entry1);
        bind_names(str_env(entry1), str_env(entry2));
    endfor;
    ;;; bind type names of -strenv1-
    structenv_tycons(strenv2) -> env;
    For entry1 in structenv_tycons(strenv1) do
        find_entry(entry1, env) -> env -> entry2;
        ;;; check any sharing constraint on -entry1-
        unless share_types(entry1, entry2) then
            sigmatch_error(entry1, entry2);
        endunless;
        ;;; copy tycon attributes from -entry2- to -entry1-
        tycon_parent(entry2) -> tycon_parent(entry1);
        tycon_function(entry2) -> tycon_function(entry1);
        ;;; map tycon2 to tycon1 to allow for translation of alias types
        acons(entry2, entry1, newtycons) -> newtycons;
    endfor;
enddefine;

;;; match_env:
;;;     matches constructor, exception and variable names in -strenv2-
;;;     against the corresponding names in -strenv-

define lconstant match_env(strenv1, strenv2);
    lvars entry1, entry2, env, strenv1, strenv2;

    define lconstant newtycon(tycon);
        lvars tycon;
        alookup(tycon, newtycons) or tycon;
    enddefine;

    define lconstant match_type(ty1, ty2) -> ty1;
        lvars ty1, ty2;
        ;;; unification works on type instances containing no bound variables
        type_instance(ty1, 0) -> ty1;
        unless test_unify(ty1, type_instance(ty2, 1)) then
            sigmatch_error(entry1, entry2);
        endunless;
        type_generalise(ty1, 1, false);
    enddefine;

    structenv_strs(strenv2) -> env;
    For entry1 in structenv_strs(strenv1) do
        find_entry(entry1, env) -> env -> entry2;
        match_env(str_env(entry1), str_env(entry2));
    endfor;
    For entry1 in structenv_tycons(strenv1) do
        if istypealias(tycon_function(entry1)) then
            maptype(tycon_type(entry1), newtycon) -> tycon_type(entry1);
        endif;
    endfor;
    structenv_vals(strenv2) -> env;
    For entry1 in structenv_vals(strenv1) do
        find_entry(entry1, env) -> env -> entry2;
        if val_iscon(entry1) then
            unless val_iscon(entry2)
            and val_isexn(entry1) == val_isexn(entry2)
            then
                sigmatch_error(entry1, entry2);
            endunless;
        endif;
        match_type(val_type(entry1), val_type(entry2)) -> val_type(entry1);
        val_parent(entry2) -> val_parent(entry1);
        val_ident(entry2) -> val_ident(entry1);
        ;;; copy overloading if entry2 is a var
        unless val_iscon(entry2) then
            val_overloading(entry2) -> val_overloading(entry1);
        endunless;
        1 -> val_usage(entry1);
    endfor;
enddefine;

;;; signature_match:
;;;     matches the structure environment -strenv2- against the signature
;;;     environment -strenv1-

define signature_match(sigmatch_node, strenv1, strenv2);
    lvars   strenv1, strenv2;
    dlocal  sigmatch_node, newtycons = [];
    bind_names(strenv1, strenv2);
    match_env(strenv1, strenv2);
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  4 1995
        Made module classes writeable by default
--- Robert John Duncan, Dec 20 1994
        Concentration of run-time information in the mlident part of a val
        record means less copying when creating a module instance, since
        the identifier can be shared; it also obviates the need for
        indirections (one val pointing to another) and improves the
        propagation of run-time information.
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Oct 24 1994
        Changes to error reporting
--- Robert John Duncan, Oct 10 1991
        Stricter occurs check for type sharing
--- Robert John Duncan, Apr 26 1991
        Fixed stupid bug in -same_type_function-
--- Robert John Duncan, Feb 11 1991
        Exceptions, constructors and variables now represented by a common
        "val" record.
--- Robert John Duncan, Feb  4 1991
        Changes for new environment interface.
--- Simon Nichols, Jun 21 1990
        Changes to support optimisation of tupled functions. -match_env-
        now copies across the -val_tupled- attribute of a var.
 */
