/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/types.p
 > Purpose:         PML: Type structures, type utilities and type unification
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

/*
 *  Type Structures
 */


;;; Type Variables:

defclass vartype [writeable] {
    type_contents   : full,     ;;; variable binding: an optional type
    type_decnum     : full,     ;;; for free ("nongeneric") variables, an
                                ;;; integer indicating where the variable was
                                ;;; created; <false> for "generic" variables
    type_constraint : bool,     ;;; => user variable; can't be instantiated
    type_eqtyvar    : bool,     ;;; => equality type variable
    type_imptyvar   : bool,     ;;; => imperative type variable
};

;;; vartype:
;;;     constructor for type variables

constant macro vartype = "consvartype";

;;; anytype:
;;;     creates an object which can stand for any type: an uninstantiated
;;;     variable

define anytype(decnum);
    lvars decnum;
    vartype(false, decnum, false, false, false);
enddefine;

;;; constraint_type:
;;;     creates a new constraint type variable with attributes as specified
;;;     by the given tyvar id. Since constraint variables can't be
;;;     instantiated, the -type_contents- field is used to store the id
;;;     for printing.

define constraint_type(tvid, decnum) -> ty;
    lvars len, tvid, decnum, ty;
    anytype(decnum) -> ty;
    tvid -> type_contents(ty);
    true -> type_constraint(ty);
    tvid_equality(tvid) -> type_eqtyvar(ty);
    tvid_imperative(tvid) -> type_imptyvar(ty);
enddefine;

;;; type_deref:
;;;     dereferences a chain of type variables, pruning intermediate links

define type_deref(ty);
    lvars ty;
    returnunless(isvartype(ty) and fast_cont(ty) and not(type_constraint(ty)))(ty);
    ;;; instantiated type variable
    type_deref(fast_cont(ty)) ->> fast_cont(ty);
enddefine;


;;; Record Types:

defclass recordtype [writeable] {
    type_labels : full,     ;;; for general records, an ordered list of
                            ;;; labels; for tuples, an integer length
    type_fields : full,     ;;; list of types, ordered to agree with labels
};

;;; recordtype:
;;;     constructor for record types

constant macro recordtype = "consrecordtype";

;;; tupletype:
;;;     constructor for tuple types

define tupletype(tys);
    lvars tys;
    recordtype(llength(tys), tys);
enddefine;

;;; recordtype_deref:
;;;     allows for special treatment of wildcard records. A recordtype
;;;     may be an indirection record, indicated by its label field being
;;;     <false> (!): in this case, the fields field is another, equivalent
;;;     recordtype. -recordtype_deref- follows chains of indirections to
;;;     the end, pruning intermediate links.

define recordtype_deref(ty);
    lvars ty;
    returnif(type_labels(ty))(ty);
    recordtype_deref(type_fields(ty)) ->> type_fields(ty);
enddefine;

;;; is_tuple_recordtype, is_wildcard_recordtype:
;;;     the label field of a recordtype (when not <false> -- see
;;;     -recordtype_deref- above) may be an integer or a pair.
;;;     An integer -n- indicates that it is a tuple record, {#1 ... #n},
;;;     while a pair consists of a flag indicating whether it is a
;;;     wildcard record and an ordered list of the record labels.

define is_tuple_recordtype(/* ty */) with_nargs 1;
    isinteger(type_labels(recordtype_deref(/* ty */)));
enddefine;

define is_wildcard_recordtype(/* ty */) with_nargs 1;
    lvars labels;
    ispair(type_labels(recordtype_deref(/* ty */)) ->> labels)
    and Front(labels);
enddefine;


;;; Function Types:

defclass funtype [writeable] {
    type_domain : full,
    type_range  : full,
};

;;; funtype:
;;;     constructor for function types

constant macro funtype = "consfuntype";


;;; Constructed Types:

defclass constype [writeable] {
    type_constructor    : full,     ;;; a tycon (see "env.p")
    type_arguments      : full,     ;;; list of type arguments
};

;;; type names: uniquely identify new types
defclass typename [writeable] {
    typename_contents   : full,     ;;; for type sharing (see "modules.p")
    typename_arity      : full,
    typename_equality   : full,     ;;; <false>, <true> or "ref"
};

;;; type aliases: for aliased (or "abbreviated") types
defclass typealias [writeable] {
    typealias_boundvars : full,
    typealias_type      : full,
};

;;; constype:
;;;     constructor for constructed types

constant macro constype = "consconstype";

;;; new_type_name:
;;;     constructs a new type name with given arity and equality attribute

define new_type_name(arity, equality);
    lvars arity, equality;
    constypename(stamp(), arity, equality);
enddefine;

;;; new_type_alias:
;;;     constructor for abbreviated types

constant macro new_type_alias = "constypealias";

;;; typename_deref:
;;;     follows chains of sharing type names. The result is either another
;;;     type name or a type alias.

define typename_deref(t);
    lvars t, t1;
    returnunless(istypename(t))(t);
    while istypename(fast_cont(t) ->> t1) do t1 -> t endwhile;
    if istypealias(t1) then
        t1;
    else
        t;
    endif;
enddefine;

;;; tycon_arity:
;;;     gets the arity of a type constructor from its type function

define tycon_arity(/* tycon */) with_nargs 1;
    lvars tfnc = tycon_function(/* tycon */);
    if istypename(tfnc) then
        typename_arity(tfnc);
    else
        llength(typealias_boundvars(tfnc));
    endif;
enddefine;

;;; tycon_equality:
;;;     gets/sets the equality attribute of a type name via its type
;;;     constructor. Dereferencing isn't done: this is only called on
;;;     newly created tycons

constant procedure tycon_equality = tycon_function <> typename_equality;

;;; tycon_type:
;;;     gets the type alias from a type constructor

constant procedure tycon_type = tycon_function <> typealias_type;

;;; type_name, type_function etc.
;;;     extract information from the type constructor of a type

constant procedure (

    type_name       = type_constructor <> tycon_name,
    type_parent     = type_constructor <> tycon_parent,
    type_function   = type_constructor <> tycon_function <> typename_deref,
    type_arity      = type_constructor <> tycon_arity,
    type_cons       = type_constructor <> tycon_cons,
    type_orig_cons  = type_constructor <> tycon_orig_cons,
    type_datarep    = type_constructor <> tycon_datarep,
    type_printer    = type_constructor <> tycon_printer,
    type_pervasive  = type_constructor <> tycon_pervasive,

);

;;; is_alias_type:
;;;     returns <true> if a type is an abbreviation

define is_alias_type(ty);
    lvars ty;
    isconstype(ty) and istypealias(type_function(ty));
enddefine;


/*
 *  Type Utilities
 */

;;; istype:
;;;     recogniser for all type structures

define istype(item);
    lvars item;
    isvartype(item) or isrecordtype(item)
    or isfuntype(item) or isconstype(item);
enddefine;

;;; bad_type:
;;;     complains that an object is not a type structure

constant procedure bad_type = mishap(% 1, 'TYPE NEEDED' %);

;;; maptype:
;;;     copies a type, transforming each type constructor with -p-

define maptype(/* ty, */ p) with_nargs 2;
    dlvars procedure p;

    define lconstant copytype(/* ty */) with_nargs 1;
        lvars ty = type_deref(/* ty */), tycon;
        if isvartype(ty) then
            ty;
        elseif isrecordtype(ty) then
            recordtype_deref(ty) -> ty;
            returnif(type_fields(ty) == [])(ty);
            recordtype(type_labels(ty), map(type_fields(ty), copytype));
        elseif isfuntype(ty) then
            funtype(copytype(type_domain(ty)), copytype(type_range(ty)));
        elseif isconstype(ty) then
            p(type_constructor(ty)) -> tycon;
            if type_arguments(ty) == [] and tycon == type_constructor(ty) then
                ty;
            else
                constype(tycon, map(type_arguments(ty), copytype));
            endif;
        else
            bad_type(ty);
        endif;
    enddefine;

    copytype(/* ty */);
enddefine;

;;; type_full_deref:
;;;     prunes all chains of type variables and indirection records
;;;     from a type

constant procedure type_full_deref = maptype(% identfn %);

;;; type_expand:
;;;     expands a type abbreviation (assuming the type has been deref'ed)

define type_expand(ty) -> ty;
    lvars tv, tys, ty, tfnc;
    if isconstype(ty) then
        type_function(ty) -> tfnc;
        if istypealias(tfnc) then
            if (type_arguments(ty) ->> tys) == [] then
                ;;; no bound variables: return the alias type directly
                typealias_type(tfnc) -> ty;
            else
                ;;; instantiate the bound variables to the actual arguments
                For tv in typealias_boundvars(tfnc) do
                    Destpair(tys) -> tys -> fast_cont(tv);
                endfor;
                ;;; copy the alias type
                type_full_deref(typealias_type(tfnc)) -> ty;
                ;;; unbind the variables again
                For tv in typealias_boundvars(tfnc) do
                    false -> fast_cont(tv);
                endfor;
            endif;
        endif;
    endif;
enddefine;

;;; type_instance:
;;;     builds an instance of a type scheme, i.e. returns a type with the
;;;     same structure as the scheme, but with bound variables replaced
;;;     consistently with new ones.
;;;     Bound variables may sometimes be marked as "constrained", i.e.
;;;     can't be instantiated; this is normally unset here, except in the
;;;     special case where -thisdec- is zero - an "impossible" case used
;;;     to indicate a call inside a signature match (see "modules.p").
;;;     This is written to do the minimum amount of copying: ground types
;;;     are preserved wherever possible. This reduces garbage, but also
;;;     increases the number of times that the unifier can succeed with a
;;;     single identity test.

define type_instance(/* ty, */ thisdec) with_nargs 2;
    lvars thisdec, newvars = [];

    lconstant procedure instance;

    define lconstant instance_args(tys);
        lvars n = 0, tys, copied = false;
        until tys == [] do
            if instance(Destpair(tys) -> tys) then true -> copied endif;
            n fi_+ 1 -> n;
        enduntil;
        if copied then
            [], until n == 0 do conspair(); n fi_- 1 -> n enduntil, true;
        else
            until n == 0 do n fi_- 1 -> n -> enduntil, false;
        endif;
    enddefine;

    define lconstant procedure instance(/* ty */) with_nargs 1;
        lvars newvar, ty = type_deref(/* ty */);
        if isvartype(ty) then
            if type_decnum(ty) then
                ;;; variable not bound in this scheme -- leave it alone
                ty, false;
            elseif alookup(ty, newvars) ->> newvar then
                ;;; bound variable, already copied as -newvar-
                newvar, true;
            else
                ;;; bound variable, first occurrence -- copy it,
                ;;; recording the copy in -newvars-
                copy(ty) -> newvar;
                acons(ty, newvar, newvars) -> newvars;
                if thisdec == 0 then
                    ;;; inside a signature match -- leave flags alone
                    1 -> type_decnum(newvar);
                else
                    thisdec -> type_decnum(newvar);
                    ;;; make the variable assignable
                    false -> type_constraint(newvar);
                    false -> fast_cont(newvar);
                endif;
                newvar, true;
            endif;
        elseif isrecordtype(ty) then
            recordtype_deref(ty) -> ty;
            ;;; a wildcard record must not be copied since it might be
            ;;; updated later, but such a type can't be elaborated
            ;;; anyway
            if is_wildcard_recordtype(ty) then
                ty, false;
            else
                type_labels(ty);
                if instance_args(type_fields(ty)) then
                    recordtype(), true;
                else
                    ->, ty, false;
                endif;
            endif;
        elseif isfuntype(ty) then
            if instance(type_domain(ty)) then
                funtype(instance(type_range(ty)) -> ), true;
            elseif instance(type_range(ty)) then
                funtype(), true;
            else
                -> ->, ty, false;
            endif;
        elseif isconstype(ty) then
            if type_arguments(ty) == [] then
                ty, false;
            else
                type_constructor(ty);
                if instance_args(type_arguments(ty)) then
                    constype(), true;
                else
                    ->, ty, false;
                endif;
            endif;
        else
            bad_type(ty);
        endif;
    enddefine;

    instance(/* ty */) -> ;
enddefine;

;;; type_generalise:
;;;     converts a type to a more general type scheme on exit from a
;;;     declaration by binding those type variables which were introduced
;;;     within the declaration (where "binding" means unsetting the
;;;     -type_decnum- counter in the variable).

define type_generalise(/* ty, */ thisdec, expansive) with_nargs 3;
    lvars thisdec, expansive;

    define lconstant generalise(/* ty */) with_nargs 1;
        lvars ty = type_deref(/* ty */);
        if isvartype(ty) then
            if type_decnum(ty) fi_>= thisdec then
                ;;; type variable introduced in this declaration and not
                ;;; yet bound: bind it, unless its imperative and we're in
                ;;; an expansive context
                unless type_imptyvar(ty) and expansive then
                    false -> type_decnum(ty);
                endunless;
            endif;
        elseif isrecordtype(ty) then
            app(type_fields(recordtype_deref(ty)), generalise);
        elseif isfuntype(ty) then
            generalise(type_domain(ty));
            generalise(type_range(ty));
        elseif isconstype(ty) then
            app(type_arguments(ty), generalise);
        else
            bad_type(ty);
        endif;
    enddefine;

    generalise(/* ty */);
enddefine;

;;; is_ground_type:
;;;     <true> if a type contains no variables

define is_ground_type(/* ty */) with_nargs 1;
    lvars ty = type_deref(/* ty */);
    if isvartype(ty) then
        false;
    elseif isrecordtype(ty) then
        all(type_fields(recordtype_deref(ty)), is_ground_type);
    elseif isfuntype(ty) then
        is_ground_type(type_domain(ty)) and is_ground_type(type_range(ty));
    elseif isconstype(ty) then
        all(type_arguments(ty), is_ground_type);
    else
        bad_type(ty);
    endif;
enddefine;

;;; is_generic_type:
;;;     <true> if a type contains no free variables

define is_generic_type(/* ty */) with_nargs 1;
    lvars ty = type_deref(/* ty */);
    if isvartype(ty) then
        not(type_decnum(ty));
    elseif isrecordtype(ty) then
        all(type_fields(recordtype_deref(ty)), is_generic_type);
    elseif isfuntype(ty) then
        is_generic_type(type_domain(ty)) and is_generic_type(type_range(ty));
    elseif isconstype(ty) then
        all(type_arguments(ty), is_generic_type);
    else
        bad_type(ty);
    endif;
enddefine;

;;; is_equality_type:
;;;     <true> if a type admits equality.
;;;     NB: strictly speaking, this is equality on type *functions* rather
;;;     than types, because it returns <true> for all type variables
;;;     regardless of their equality attribute.

define is_equality_type(/* ty */) with_nargs 1;
    lvars tfnc, ty = type_deref(/* ty */);
    if isvartype(ty) then
        true;
    elseif isrecordtype(ty) then
        all(type_fields(recordtype_deref(ty)), is_equality_type);
    elseif isfuntype(ty) then
        false;
    elseif isconstype(ty) then
        type_function(ty) -> tfnc;
        if istypealias(tfnc) then
            is_equality_type(type_expand(ty));
        elseif not(typename_equality(tfnc)) then
            false;
        elseif typename_equality(tfnc) == "ref" then
            true;
        else
            all(type_arguments(ty), is_equality_type);
        endif;
    else
        bad_type(ty);
    endif;
enddefine;

;;; most_general_type:
;;;     converts a type constructor into a type

define most_general_type(tycon);
    lvars tycon;
    constype(
        tycon,
        [% Repeat tycon_arity(tycon) times anytype(false) endrepeat %]);
enddefine;


/*
 *  Type Unification
 */

;;; Error messages returned by the unifier to indicate why the unification
;;; failed:

constant
    TYPERR_CTS  = 1,    ;;; cyclic type structure
    TYPERR_TCV  = 2,    ;;; type constraint violation
    TYPERR_ETN  = 3,    ;;; equality type needed
    TYPERR_ITN  = 4,    ;;; imperative type needed
    TYPERR_FTN  = 5,    ;;; function type needed
    TYPERR_RTN  = 6,    ;;; record type needed
    TYPERR_CTN  = 7,    ;;; constructed type needed
    TYPERR_RDM  = 8,    ;;; record domain mismatch
;

;;; propagate:
;;;     propagates the attributes of a type variable through a type.
;;;     Returns <true> if the propagation is successful, or <false>
;;;     plus an error message if the attributes can't be propagated.

define lconstant propagate(tv, ty);
    lvars tv, ty, decnum, equality, imperative;

    define lconstant Propagate(/* ty */) with_nargs 1;
        lvars   tfnc, ty = type_deref(/* ty */);
        dlocal  equality;
        if isvartype(ty) then
            ;;; occurs check
            returnif(ty == tv)(TYPERR_CTS, false);
            ;;; propagate equality and imperative attributes
            if type_constraint(ty) then
                ;;; -ty- is a constraint variable, so can't have its equality
                ;;; and imperative attributes changed
                returnif(equality and not(type_eqtyvar(ty)))(TYPERR_ETN, false);
                returnif(imperative and not(type_imptyvar(ty)))(TYPERR_ITN, false);
            else
                if equality then true -> type_eqtyvar(ty) endif;
                if imperative then true -> type_imptyvar(ty) endif;
            endif;
            ;;; propagate nongeneric-ness via the declaration number
            if decnum fi_< type_decnum(ty) then
                decnum -> type_decnum(ty);
            endif;
            true;
        elseif isrecordtype(ty) then
            all(type_fields(recordtype_deref(ty)), Propagate);
        elseif isfuntype(ty) then
            returnif(equality)(TYPERR_ETN, false);
            Propagate(type_domain(ty)) and Propagate(type_range(ty));
        elseif isconstype(ty) then
            type_function(ty) -> tfnc;
            returnif(istypealias(tfnc))(Propagate(type_expand(ty)));
            ;;; check equality attribute matches this type name
            returnif(equality and not(typename_equality(tfnc)))(TYPERR_ETN, false);
            ;;; stop propagation of equality at types with reference equality
            if typename_equality(tfnc) == "ref" then false -> equality endif;
            all(type_arguments(ty), Propagate);
        else
            bad_type(ty);
        endif;
    enddefine;

    type_decnum(tv) -> decnum;
    type_eqtyvar(tv) -> equality;
    type_imptyvar(tv) -> imperative;
    Propagate(ty);
enddefine;


;;; instantiate:
;;;     instantiates a type variable to a type, checking that the
;;;     instantiation is valid and propagating the attributes of the
;;;     variable through the type. It is assumed that the variable and
;;;     the type are distinct and that the type has already been
;;;     dereferenced.

define lconstant instantiate(tv, ty);
    lvars tv, ty;
    if type_constraint(tv) then
        ;;; -tv- is a constraint variable so can't be instantiated.
        if isvartype(ty) and not(type_constraint(ty)) then
            ;;; -ty- is an unconstrained variable, so swap the two
            tv, ty -> tv -> ty;
        else
            ;;; instantiation impossible
            return(TYPERR_TCV, false);
        endif;
    endif;
    returnunless(propagate(tv, ty))(false);
    ty -> fast_cont(tv);
    return(true);
enddefine;


;;; unify_types:
;;;     type unification. Returns <true> for success, <false> plus an error
;;;     message on failure

define unify_types(ty1, ty2);
    lvars ty1, ty2;

    define lconstant unify_args(args1, args2);
        lvars args1, args2;
        until args1 == [] or args2 == [] do
            returnunless(unify_types(Front(args1), Front(args2)))(false);
            Back(args1) -> args1;
            Back(args2) -> args2;
        enduntil;
        returnif(args1 == args2)(true);
        mishap(0, 'UNEQUAL ARGUMENT LISTS');
    enddefine;

    define lconstant unify_records(/* ty1, ty2 */) with_nargs 2;
        lvars   labels, labels1, labels2, fields, fields1, fields2,
                iswild1, iswild2, ty1, ty2, n;
        recordtype_deref(/* ty2 */) -> ty2;
        recordtype_deref(/* ty1 */) -> ty1;
        type_labels(ty1) -> labels1;
        type_labels(ty2) -> labels2;
        type_fields(ty1) -> fields1;
        type_fields(ty2) -> fields2;
        ;;; if the labels match, try straight unification
        returnif(labels1 = labels2)(unify_args(fields1, fields2));
        ;;; determine whether either record has a wildcard component
        ispair(labels1) and (Destpair(labels1) -> labels1) -> iswild1;
        ispair(labels2) and (Destpair(labels2) -> labels2) -> iswild2;
        ;;; if neither record is wild, the unification has failed already
        returnunless(iswild1 or iswild2)(TYPERR_RDM, false);
        ;;; if either type is a tuple, expand its label field to a list
        if isinteger(labels1) then
            fromto(1, labels1) -> labels1;
        elseif isinteger(labels2) then
            fromto(1, labels2) -> labels2;
        endif;
        ;;; merge the labels and fields of the two types, unifying fields
        ;;; whenever the corresponding labels match
        [] -> labels;
        [] -> fields;
        until labels1 == [] or labels2 == [] do
            if Front(labels1) == Front(labels2) then
                returnunless(unify_types(Front(fields1), Front(fields2)))
                    (false);
                conspair(Destpair(labels1) -> labels1, labels) -> labels;
                conspair(Destpair(fields1) -> fields1, fields) -> fields;
                Back(labels2) -> labels2;
                Back(fields2) -> fields2;
            elseif iswild2 and label_<=(Front(labels1), Front(labels2))
            then
                conspair(Destpair(labels1) -> labels1, labels) -> labels;
                conspair(Destpair(fields1) -> fields1, fields) -> fields;
            elseif iswild1 and label_<=(Front(labels2), Front(labels1))
            then
                conspair(Destpair(labels2) -> labels2, labels) -> labels;
                conspair(Destpair(fields2) -> fields2, fields) -> fields;
            else
                return(TYPERR_RDM, false);
            endif;
        enduntil;
        ;;; merge in any remaining labels & fields from one or other type
        if labels1 /== [] then
            returnunless(iswild2)(TYPERR_RDM, false);
            rev(labels1) <> labels -> labels;
            rev(fields1) <> fields -> fields;
        elseif labels2 /== [] then
            returnunless(iswild1)(TYPERR_RDM, false);
            rev(labels2) <> labels -> labels;
            rev(fields2) <> fields -> fields;
        endif;
        if iswild1 and iswild2 then
            conspair(true, fast_ncrev(labels)) -> labels;
        elseif labels /== []
        and isinteger(Front(labels) ->> n) and n == listlength(labels)
        and n /== 1
        then
            ;;; tuple
            n -> labels;
        else
            conspair(false, fast_ncrev(labels)) -> labels;
        endif;
        fast_ncrev(fields) -> fields;
        ;;; restore labels and fields to one of the recordtypes and make
        ;;; the other share with it
        if iswild1 then
            labels -> type_labels(ty2);
            fields -> type_fields(ty2);
            false -> type_labels(ty1);
            ty2 -> type_fields(ty1);
        else
            labels -> type_labels(ty1);
            fields -> type_fields(ty1);
            false -> type_labels(ty2);
            ty1 -> type_fields(ty2);
        endif;
        ;;; return <true> for successful unification
        true;
    enddefine;

    repeat
        type_deref(ty1) -> ty1;
        type_deref(ty2) -> ty2;
        if ty1 == ty2 then
            return(true);
        elseif isvartype(ty1) then
            return(instantiate(ty1, ty2));
        elseif isvartype(ty2) then
            return(instantiate(ty2, ty1));
        elseif isrecordtype(ty1) then
            unless isrecordtype(ty2) then
                while is_alias_type(ty2) do
                    type_expand(ty2) -> ty2;
                endwhile;
                unless isrecordtype(ty2) then
                    return(TYPERR_RTN, false);
                endunless;
            endunless;
            return(unify_records(ty1, ty2));
        elseif isfuntype(ty1) then
            unless isfuntype(ty2) then
                while is_alias_type(ty2) do
                    type_expand(ty2) -> ty2;
                endwhile;
                unless isfuntype(ty2) then
                    return(TYPERR_FTN, false);
                endunless;
            endunless;
            returnunless(unify_types(type_domain(ty1), type_domain(ty2)))
                (false);
            type_range(ty1) -> ty1;
            type_range(ty2) -> ty2;
        elseif isconstype(ty1) then
            if isconstype(ty2) then
                if type_function(ty1) == type_function(ty2) then
                    return(unify_args(type_arguments(ty1), type_arguments(ty2)));
                elseif is_alias_type(ty2) then
                    type_expand(ty2) -> ty2;
                elseif is_alias_type(ty1) then
                    type_expand(ty1) -> ty1;
                else
                    return(TYPERR_CTN, false);
                endif;
            elseif is_alias_type(ty1) then
                type_expand(ty1) -> ty1;
            else
                return(TYPERR_CTN, false);
            endif;
        else
            bad_type(ty1);
        endif;
    endrepeat;
enddefine;


;;; test_unify:
;;;     as -unify_types-, but throws away the error code

define test_unify(/* ty1, ty2 */) with_nargs 2;
    unify_types(/* ty1, ty2 */) or (/* errcode */ ->, false);
enddefine;


/*
 *  Forward declarations of built-in types
 *  (defined later in "StdTypes.ml")
 */

constant

    unittype,
    inttype,
    realtype,
    stringtype,
    exntype,
    booltype,

    procedure (
        reftype,
        listtype,
    ),
;
endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  4 1995
        Made type classes writeable by default
--- Robert John Duncan, Apr 27 1995
        Changed name of unification procedure to unify_types to prevent
        clash with Prolog's unifier
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Oct 24 1994
        Changed unify_types to return an integer code indicating the reason
        for failure rather than a string; codes now exported. Procedure
        check_u*nify replaced by check_type in "typecheck.p".
--- Robert John Duncan, Sep 27 1991
        Added -type_orig_cons-
--- Robert John Duncan, Apr 26 1991
        Fixed a unifier bug.
--- Robert John Duncan, Mar 18 1991
        More descriptive messages from the unifier.
--- Robert John Duncan, Mar  1 1991
        The name in a constrained type variable is now a -tvid-.
--- Robert John Duncan, Feb  4 1991
        Changed to use -defclass-. Flags in records now boolean.
--- Robert John Duncan, Dec 13 1990
        Fixed -unify_records- not to turn the type {1:'ty} into a tuple
--- Rob Duncan, Jan  5 1990
        Changed -type_generalise- to fix a bug in the handling of imperative
        type variables: if not bound, imperative variables should have their
        declaration number left unchanged rather than made zero (which makes
        it impossible for them ever to become bound).
 */
