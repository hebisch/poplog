/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/pml/src/env.p
 > Purpose:         PML: static environment
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

constant procedure  (       ;;; forward
    ml_autoload,
        ;;; for autoloading module definitions
    signature_instance,
        ;;; creates instances of signatures
    functor_instance,
        ;;; creates instances of functors
    newmlident,
        ;;; creates run-time variable record
);

vars
    ml_closure_rules = false,
        ;;; enforces closure rules on modules (exported from "System.ml")
;


/*
 *  Identifier Classes
 */

    ;;; Functors:
defclass fnc [writeable] {
    fnc_name        : full,     ;;; name
    fnc_arg         : full,     ;;; input environment
    fnc_env         : full,     ;;; output environment
    fnc_code        : full,     ;;; (pointer to) code for functor body
};

define newfnc() with_nargs 1;
    consfnc((), false, false, consref(false));
enddefine;


    ;;; Signatures:
defclass sig [writeable] {
    sig_name        : full,     ;;; name
    sig_signame     : full,     ;;; identifier
    sig_env         : full,     ;;; environment
};

define newsig =
    conssig(% false, false %);
enddefine;


    ;;; Structures:
defclass str [writeable] {
    str_name        : full,     ;;; name
    str_strname     : full,     ;;; identifier
    str_env         : full,     ;;; environment
};

define newstr =
    consstr(% false, false %);
enddefine;


    ;;; Type constructors:
defclass tycon [writeable] {
    tycon_name      : full,     ;;; name
    tycon_parent    : full,     ;;; parent structure
    tycon_function  : full,     ;;; unique identifier; typename or typealias
    tycon_cons      : full,     ;;; constructor env (= [] for abstypes)
    tycon_orig_cons : full,     ;;; original constructor env
    tycon_printer   : full,     ;;; printing procedure for the type
    tycon_datarep   : byte,     ;;; data representation
    tycon_pervasive : bool,     ;;; => pervasive
};

define newtycon =
    constycon(% false, false, [], [], false, 0, false %);
enddefine;


    ;;; Values (variables, constructors, exceptions):
defclass val [writeable] {
    val_name        : full,     ;;; name
    val_parent      : full,     ;;; parent structure
    val_type        : full,     ;;; type
    val_ident       : full,     ;;; pointer to run-time value
    val_extra       : full,     ;;; extra info (see below)
    val_usage       : byte,     ;;; reference count
    val_iscon       : bool,     ;;; => value/exn constructor
    val_isexn       : bool,     ;;; => exn constructor
    val_nonexpansive: bool,     ;;; => var bound in non-expansive context
    val_pervasive   : bool,     ;;; => pervasive
};

define newval(/* id, */ iscon, isexn) with_nargs 3;
    lvars iscon, isexn;
    consval(/* id, */ false, false, newmlident(), false, 0, iscon, isexn,
        false, false);
enddefine;

define newcon =
    newval(% true, false %);
enddefine;

define newexn =
    newval(% true, true %);
enddefine;

define newvar =
    newval(% false, false %);
enddefine;

define iscon(item);
    lvars item;
    isval(item) and val_iscon(item);
enddefine;

define isexn(item);
    lvars item;
    isval(item) and val_isexn(item);
enddefine;

define isvalcon(item);
    lvars item;
    isval(item) and val_iscon(item) and not(val_isexn(item));
enddefine;

define isvar(item);
    lvars item;
    isval(item) and not(val_iscon(item));
enddefine;

define val_isvalcon(val);
    lvars val;
    val_iscon(val) and not(val_isexn(val));
enddefine;

define val_isvar =
    val_iscon <> not;
enddefine;

constant procedure (
    val_datarep     = val_extra,    ;;; data rep of cons
    val_printer     = val_extra,    ;;; printer for exns
    val_overloading = val_extra,    ;;; overloading for vars
    val_arity,                      ;;; forward in "vmml.p"
    val_value,
);

lconstant
    MAX_USAGE = 2,
        ;;; largest *interesting* usage count in values
;


    ;;; Type variables
defclass tyvar [writeable] {
    tyvar_name      : full,     ;;; name
    tyvar_type      : full,     ;;; vartype
};

define newtyvar =
    constyvar(% false %);
enddefine;


    ;;; Operators:
defclass opr [writeable] {
    opr_name        : full,     ;;; name
    opr_fixity      : full,     ;;; fixity: "infix", "infixr", "nonfix"
    opr_prec        : full,     ;;; precedence: 0-9
};

constant
    nonfix_opr = consopr("nop", "nonfix", -1),
    procedure newopr = consopr,
;


/*
 *  Generic operations on environment entries (identifiers)
 */

/*
;;; NB: THIS DOESN'T APPLY TO ALL IDENTIFIERS!!!
defclass pretend entry {
    entry_name      : full,     ;;; name
    entry_parent    : full,     ;;; optional parent structure name
};
*/

constant macro (
    entry_name      = "fast_front",     ;;; any entry
    entry_parent    = "fast_back",      ;;; not functors or signatures
);

;;; print_entry:
;;;     prints an environment entry. Printing procedures for different
;;;     datakeys are assigned in "print_bindings.p"

define print_entry(entry);
    lvars   entry, full = false;
    dlocal  pr = ml_pr;
    if isboolean(entry) then ((), entry) -> (entry, full) endif;
    lvars print = class_print(datakey(entry));
    if print == syspr then
        ;;; not properly assigned -- do the minimum
        printf('<%p %p>', [% dataword(entry), entry_name(entry) %]);
    else
        print(entry, full);
    endif;
enddefine;

;;; unbound_error:
;;;     reports an unbound identifier

define lconstant unbound_error(msg, id, linenum);
    lvars msg, id, linenum;
    ml_error('unbound %S\n\t%p\n', [^msg ^id], popfilename, linenum);
enddefine;

;;; rebind_error:
;;;     reports the rebinding of an identifier within a declaration

define lconstant rebind_error(msg, id, linenum);
    lvars msg, id, linenum;
    ml_error('multiple binding for %S\n\t%p\n', [^msg ^id], popfilename,
        linenum);
enddefine;


/*
 *  Global Environment
 */

;;; Initial sizes for global environments: chosen as the smallest power
;;; of 2 sufficient to contain all built-in names
lconstant
    N_FNCS  = 16,
    N_SIGS  = 16,
    N_STRS  = 16,
    N_TYCS  = 16,
    N_VALS  = 256,
    N_OPRS  = 32,
;

;;; make_global_env:
;;;     creates a new expandable property for use as a global env.

define lconstant make_global_env(size);
    lvars size;
    newanyproperty([],size,1,(size*7) div 8,false,false,"perm",false,false);
enddefine;

;;; Global environments:
#_IF not(DEF global_opr_env)
constant procedure (
    global_fnc_env = make_global_env(N_FNCS),
    global_sig_env = make_global_env(N_SIGS),
    global_str_env = make_global_env(N_STRS),
    global_tyc_env = make_global_env(N_TYCS),
    global_val_env = make_global_env(N_VALS),
    global_opr_env = make_global_env(N_OPRS),
);
#_ENDIF

;;; clear_global_env:
;;;     zap everything in the environment: useful for stand-alone saved
;;;     images

define clear_global_env();
    clearproperty(global_fnc_env);
    clearproperty(global_sig_env);
    clearproperty(global_str_env);
    clearproperty(global_tyc_env);
    clearproperty(global_val_env);
    clearproperty(global_opr_env);
enddefine;

;;; global_entries:
;;;     return a list of all the entries in a global env which satisfy
;;;     predicate p. Results are in no particular order.

define lconstant global_entries(env, p);
    lvars env, procedure p;
    [%
        appproperty(env, procedure(k, v);
            lvars k, v;
            if p(v) then v endif;
        endprocedure);
    %];
enddefine;

define global_fncs =
    global_entries(% global_fnc_env, identfn %);
enddefine;

define global_sigs =
    global_entries(% global_sig_env, identfn %);
enddefine;

define global_strs =
    global_entries(% global_str_env, identfn %);
enddefine;

define global_tycons =
    global_entries(% global_tyc_env, identfn %);
enddefine;

define global_vals =
    global_entries(% global_val_env, identfn %);
enddefine;

define global_cons =
    global_entries(% global_val_env, iscon %);
enddefine;

define global_exns =
    global_entries(% global_val_env, isexn %);
enddefine;

define global_vars =
    global_entries(% global_val_env, isvar %);
enddefine;

;;; Global lookup:

lvars
    closed_env  = false,
        ;;; when <true> only pervasives are accessible in the global env
;

define global_fnc(id) -> fnc;
    lvars id, fnc;
    unless global_fnc_env(id) ->> fnc then
        if ml_autoload(id) then
            global_fnc_env(id) -> fnc;
        endif;
    endunless;
enddefine;

define updaterof global_fnc =
    updater(global_fnc_env)(%%);
enddefine;

define global_sig(id) -> sig;
    lvars id, sig;
    unless global_sig_env(id) ->> sig then
        if ml_autoload(id, true) then
            global_sig_env(id) -> sig;
        endif;
    endunless;
enddefine;

define updaterof global_sig =
    updater(global_sig_env)(%%);
enddefine;

define global_str(id) -> str;
    lvars id, str;
    unless global_str_env(id) ->> str then
        if ml_autoload(id) then
            global_str_env(id) -> str;
        endif;
    endunless;
enddefine;

define updaterof global_str =
    updater(global_str_env)(%%);
enddefine;

define global_tycon(id) -> tycon;
    lvars id, tycon;
    if global_tyc_env(id) ->> tycon then
        if closed_env and not(tycon_pervasive(tycon)) then
            false -> tycon
        endif;
    endif;
enddefine;

define updaterof global_tycon =
    updater(global_tyc_env)(%%);
enddefine;

define global_val(id) -> val;
    lvars id, val;
    if global_val_env(id) ->> val then
        if closed_env and not(val_pervasive(val)) then
            false -> val;
        endif;
    endif;
enddefine;

define updaterof global_val =
    updater(global_val_env)(%%);
enddefine;

define global_con(/* id */) -> val with_nargs 1;
    lvars val = global_val(/* id */);
    if val and not(val_iscon(val)) then false -> val endif;
enddefine;

define global_exn(/* id */) -> val with_nargs 1;
    lvars val = global_val(/* id */);
    if val and not(val_isexn(val)) then false -> val endif;
enddefine;

define global_var(/* id */) -> val with_nargs 1;
    lvars val = global_val(/* id */);
    if val and val_iscon(val) then false -> val endif;
enddefine;

define global_opr =
    global_opr_env(%%);
enddefine;

define updaterof global_opr(opr, id);
    lvars opr, id;
    if opr_fixity(opr) == "nonfix" then false -> opr endif;
    opr -> global_opr_env(id);
enddefine;


/*
 *  Local Environment
 */

;;; The local environment records bindings arising during a single topdec.
;;; Each environment is organised as a sequence of "layers", with one layer
;;; corresponding to each declaration processed.
;;; A layer is a pair -- [flag | entry-list] -- where -flag- must be one of:
;;;     <false>     --- the layer is complete;
;;;     "hidden"    --- the layer is incomplete and non-recursive, so
;;;                     should be ignored in lookups;
;;;     a list      --- the layer is incomplete but recursive: the list
;;;                     is a list of forward references

;;; open_layer:
;;;     add an empty layer with the given flag to -env-

define lconstant open_layer(/* flag, */ env) with_nargs 2;
    lvars env;
    conspair(conspair(/* flag, */, []), env);
enddefine;

;;; close_layer:
;;;     completes the most recent layer in -env- by assigning <false> to
;;;     the flag field. Returns the list of entries declared in the layer

define lconstant close_layer(/* env */) with_nargs 1;
    lvars layer = Front(/* env */);
    false -> Front(layer);
    Back(layer);
enddefine;

;;; resolve_forwards:
;;;     resolve any forward references in the most recent layer of -env-

define lconstant resolve_forwards(/* env, */ fix) with_nargs 2;
    lvars layer, forwards, fix;
    Front(/* env */) -> layer;
    ;;; extract any forward references (parsetree nodes)
    Front(layer) -> forwards;
    ;;; close this layer
    false -> Front(layer);
    ;;; fix up the references
    app(forwards, fix);
enddefine;

;;; sort_env:
;;;     flattens out a layered local env,
;;;     sorting entries into alphabetical order

define lconstant insert_entry(entry, entries) -> entries;
    lvars p, id, p1 = false, entry, entries;
    entry_name(entry) -> id;
    entries -> p;
    until p == [] or alphabefore(id, entry_name(Front(p))) do
        p -> p1;
        Back(p) -> p;
    enduntil;
    if p == [] or entry_name(Front(p)) /== id then
        ;;; insert the new entry
        conspair(entry, p) -> if p1 then Back(p1) else entries endif;
    endif;
enddefine;

define lconstant sort_env(env) -> entries;
    lvars entry, entries = [], env;
    until env == [] do
        For entry in Back(Destpair(env) -> env) do
            insert_entry(entry, entries) -> entries;
        endfor;
    enduntil;
enddefine;

;;; lookup:
;;;     looks up an identifier in a list of entries

define lookup(id, entries);
    lvars entry, id, entries;
    For entry in entries do
        returnif(entry_name(entry) == id)(entry);
    endfor;
    false;
enddefine;

;;; Local environments:
lvars
    local_fnc_env = [],
    local_sig_env = [],
    local_str_env = [],
    local_tyc_env = [],
    local_val_env = [],
    local_opr_env = [],
;


/*
 *  Structure Environments
 */

constant procedure lookup_str;      ;;; forward

defclass structenv [writeable] {
    structenv_strname   : full,     ;;; identifying structure name
    structenv_signame   : full,     ;;; optional signature name
    structenv_strs      : full,     ;;; sub-structure env
    structenv_tycons    : full,     ;;; type constructor env
    structenv_vals      : full,     ;;; value env
};

;;; build_structenv:
;;;     package a set of layered local envs into a single structure env

define lconstant build_structenv(strs, tycons, vals);
    lvars val, strs, tycons, vals;
    sort_env(vals) -> vals;
    for val in vals do
        MAX_USAGE -> val_usage(val);
    endfor;
    consstructenv(false, false, sort_env(strs), sort_env(tycons), vals);
enddefine;

;;; long_lookup:
;;;     looks up a long identifier.
;;;     -structenv_field- determines in which component of the last
;;;     structure environment the name should be sought.
;;;     All structures in the path must be defined.

define long_lookup(longid, structenv_field);
    lvars longid, id, str, structenv_field;
    destlongid(longid) -> longid -> id;
    unless lookup_str(id) ->> str then
        unbound_error('structure name', id, ml_linenum);
    endunless;
    until isword(longid) do
        fast_destpair(longid) -> longid -> id;
        unless lookup(id, structenv_strs(str_env(str))) ->> str then
            unbound_error('structure name', id, ml_linenum);
        endunless;
    enduntil;
    lookup(longid, structenv_field(str_env(str)));
enddefine;


/*
 *  Binding Occurrences
 */

;;; enter:
;;;     adds an entry to the most recent layer of a local env.
;;;     Multiple bindings of the same name are forbidden.

define lconstant enter(entry, env);
    lvars layer, entry, env;
    Front(env) -> layer;
    lookup(entry_name(entry), Back(layer)); ;;; return any existing entry
    conspair(entry, Back(layer)) -> Back(layer);
enddefine;

;;; Adding entries to the local env:

define declare_fnc(/* id, */ linenum) -> fnc with_nargs 2;
    lvars fnc = newfnc(/* id */), linenum;
    if enter(fnc, local_fnc_env) then
        rebind_error('functor name', fnc_name(fnc), linenum);
    endif;
enddefine;

define declare_sig(/* id, */ linenum) -> sig with_nargs 2;
    lvars sig = newsig(/* id */), linenum;
    if enter(sig, local_sig_env) then
        rebind_error('signature name', sig_name(sig), linenum);
    endif;
enddefine;

define declare_str(/* id, */ linenum) -> str with_nargs 2;
    lvars str = newstr(/* id */), linenum;
    if enter(str, local_str_env) then
        rebind_error('structure name', str_name(str), linenum);
    endif;
enddefine;

define declare_tycon(/* id, */ linenum) -> tycon with_nargs 2;
    lvars tycon = newtycon(/* id */), linenum;
    if enter(tycon, local_tyc_env) then
        rebind_error('type constructor', tycon_name(tycon), linenum);
    endif;
enddefine;

define declare_con(/* id, */ linenum) -> con with_nargs 2;
    lvars con = newcon(/* id */), linenum;
    if enter(con, local_val_env) then
        rebind_error('constructor', val_name(con), linenum);
    endif;
enddefine;

define declare_exn(/* id, */ linenum) -> exn with_nargs 2;
    lvars exn = newexn(/* id */), linenum;
    if enter(exn, local_val_env) then
        rebind_error('exception constructor', val_name(exn), linenum);
    endif;
enddefine;

define declare_var(/* id, */ linenum) -> var with_nargs 2;
    lvars var = newvar(/* id */), linenum;
    if enter(var, local_val_env) then
        rebind_error('variable', val_name(var), linenum);
    endif;
enddefine;

define declare_oprs(ids, fixity, prec);
    lvars id, ids, fixity, prec;
    For id in ids do
        conspair(newopr(id, fixity, prec), local_opr_env) -> local_opr_env;
    endfor;
enddefine;


/*
 *  Applied Occurrences
 */

;;; local_lookup:
;;;     looks up a short identifier in a layered local environment.
;;;     May return an entry if one can be found, an extendable layer if one
;;;     occurs first, or else <false>

define local_lookup(id, env, extendable);
    lvars layer, entry, id, extendable, env;
    For layer in env do
        if Front(layer) == "hidden" then
            ;;; ignore it
        elseif lookup(id, Back(layer)) ->> entry then
            ;;; found an entry -- return it
            return(entry);
        elseif extendable and Front(layer) then
            ;;; extendable layer -- return that
            return(layer);
        endif;
    endfor;
    false;
enddefine;

define lookup_fnc(id) -> fnc;
    lvars id, fnc;
    unless local_lookup(id, local_fnc_env, false) ->> fnc then
        global_fnc(id) -> fnc;
    endunless;
enddefine;

define lookup_sig(id) -> sig;
    lvars id, sig;
    unless local_lookup(id, local_sig_env, false) ->> sig then
        global_sig(id) -> sig;
    endunless;
enddefine;

define lookup_str(id);
    lvars id, str;
    if not(isword(id)) then
        long_lookup(id, structenv_strs);
    elseif local_lookup(id, local_str_env, false) ->> str then
        str;
    else
        global_str(id);
    endif;
enddefine;

define lookup_tycon(id);
    lvars id, tycon;
    if not(isword(id)) then
        long_lookup(id, structenv_tycons);
    elseif local_lookup(id, local_tyc_env, true) ->> tycon then
        tycon;
    else
        global_tycon(id);
    endif;
enddefine;

define lookup_val(id, extendable) -> val;
    lvars id, val, extendable;
    if isword(id) then
        unless local_lookup(id, local_val_env, extendable) ->> val then
            global_val(id) -> val;
        endunless;
    else
        long_lookup(id, structenv_vals) -> val;
    endif;
enddefine;

define lookup_con(/* id */) -> val with_nargs 1;
    lvars val = lookup_val(/* id, */ false);
    if val and not(val_iscon(val)) then false -> val endif;
enddefine;

define lookup_exn(/* id */) -> val with_nargs 1;
    lvars val = lookup_val(/* id, */ false);
    if val and not(val_isexn(val)) then false -> val endif;
enddefine;

define lookup_var =
    lookup_val(% true %);
enddefine;

define lookup_opr(id) -> opr;
    lvars id, opr;
    if lookup(id, local_opr_env) ->> opr then
        if opr_fixity(opr) == "nonfix" then false -> opr endif;
    else
        global_opr(id) -> opr;
    endif;
enddefine;

;;; get_entry:
;;;     like -lookup-, but gives an error on failure

define lconstant get_entry(id, linenum, lookup, msg) -> entry;
    lvars id, lookup, entry, linenum, msg;
    unless lookup(id) ->> entry then
        unbound_error(msg, id, linenum);
    endunless;
enddefine;

define get_fnc =
    get_entry(% lookup_fnc, 'functor name' %);
enddefine;

define get_sig =
    get_entry(% lookup_sig, 'signature name' %);
enddefine;

define get_str =
    get_entry(% lookup_str, 'structure name' %);
enddefine;

define get_tycon =
    get_entry(% lookup_tycon, 'type constructor' %);
enddefine;

define get_con =
    get_entry(% lookup_con, 'constructor' %);
enddefine;

define get_exn =
    get_entry(% lookup_exn, 'exception constructor' %);
enddefine;

define get_var =
    get_entry(% lookup_var, 'variable' %);
enddefine;

;;; set_entry:
;;;     overwrites the identifier in the first field of a node with its
;;;     environment entry. No forward references are allowed, so the
;;;     identifier must be already defined.

define lconstant set_entry(node, get);
    lvars node, get;
    get(first(node), nodeline(node)) -> first(node);
enddefine;

define set_fnc =
    set_entry(% get_fnc %);
enddefine;

define set_sig =
    set_entry(% get_sig %);
enddefine;

define set_str =
    set_entry(% get_str %);
enddefine;

define set_con =
    set_entry(% get_con %);
enddefine;

define set_exn =
    set_entry(% get_exn %);
enddefine;

;;; set_tycon, set_var:
;;;     allow for forward references within recursive bindings

define set_tycon(tyexp);
    lvars tycon, tyexp;
    if not(lookup_tycon(first(tyexp)) ->> tycon) then
        ;;; not found, and no incomplete recursive environments
        unbound_error('type constructor', first(tyexp), nodeline(tyexp));
    elseif istycon(tycon) then
        tycon -> first(tyexp);
    else
        ;;; incomplete recursive environment -- add a forward reference
        conspair(tyexp, Front(tycon)) -> Front(tycon);
    endif;
enddefine;

define set_var(e);
    lvars var, e;
    if not(lookup_var(first(e)) ->> var) then
        ;;; not found, and no incomplete recursive environments
        unbound_error('variable', first(e), nodeline(e));
    elseif isval(var) then
        var -> first(e);
        unless val_usage(var) == MAX_USAGE then
            val_usage(var) fi_+ 1 -> val_usage(var);
        endunless;
    else
        ;;; incomplete recursive environment -- add a forward reference
        conspair(e, Front(var)) -> Front(var);
    endif;
enddefine;


/*
 *  Type Variables
 */

lvars
    tyvar_pool = [],
    local_tyvar_env = [],
;

define declare_tyvar(/* id, */ linenum) -> tyvar with_nargs 2;
    lvars tyvar = newtyvar(/* id */), linenum;
    if enter(tyvar, local_tyvar_env) then
        rebind_error('type variable', tyvar_name(tyvar), linenum);
    endif;
enddefine;

define lookup_tyvar(/* id */) with_nargs 1;
    local_lookup(/* id, */ local_tyvar_env, true);
enddefine;

define get_tyvar(id) -> tyvar;
    lvars id, tyvar;
    unless lookup(id, tyvar_pool) ->> tyvar then
        newtyvar(id) -> tyvar;
        conspair(tyvar, tyvar_pool) -> tyvar_pool;
    endunless;
enddefine;

;;; set_tyvar:
;;;     an occurrence of a type variable may be an implicit declaration
;;;     if the current layer allows it

define set_tyvar(tyexp);
    lvars id, tyvar, tyexp;
    first(tyexp) -> id;
    if not(lookup_tyvar(id) ->> tyvar) then
        ;;; not found, and not extendable
        unbound_error('type variable', id, nodeline(tyexp));
    elseif istyvar(tyvar) then
        ;;; found
        tyvar -> first(tyexp);
    else
        ;;; not found, but env is extendable
        get_tyvar(id) -> first(tyexp);
        conspair(first(tyexp), Back(tyvar)) -> Back(tyvar);
    endif;
enddefine;


/*
 *  Scope Management
 */

lvars
    scope_stack = [],
        ;;; stack of local environments/layers used for maintaining scope
;

;;; begin/end_scope:
;;;     save/restore current local envs to establish a local scope.
;;;     Functor, signature and tyvar environments can be ignored.

define begin_scope();
    {%  local_str_env,
        local_tyc_env,
        local_val_env,
        local_opr_env,
    %}, conspair((), scope_stack) -> scope_stack;
enddefine;

define end_scope();
    explode(Destpair(scope_stack) -> scope_stack)
        -> local_opr_env
        -> local_val_env
        -> local_tyc_env
        -> local_str_env;
enddefine;

;;; chop_scope:
;;;     deletes a scope out of sequence (for locals and abstypes).
;;;     The section between the last two stacked positions is cut from
;;;     each environment

define lconstant chop(l, l1, l2);
    lvars l, l1, l2;
    returnif(l1 == l2)(l);
    returnif(l == l1)(l2);
    l;  ;;; return this
    until Back(l) == l1 do Back(l) -> l enduntil;
    l2 -> Back(l);
enddefine;

define chop_scope();
    lvars e1, e2;
    Destpair(Destpair(scope_stack)) -> scope_stack -> e2 -> e1;
    chop(local_str_env, Subscrv(1, e1), Subscrv(1, e2)) -> local_str_env;
    chop(local_tyc_env, Subscrv(2, e1), Subscrv(2, e2)) -> local_tyc_env;
    chop(local_val_env, Subscrv(3, e1), Subscrv(3, e2)) -> local_val_env;
    chop(local_opr_env, Subscrv(4, e1), Subscrv(4, e2)) -> local_opr_env;
enddefine;

;;; begin/end_modbind:
;;;     for the RHS of a structure, signature or functor binding.
;;;     Enforces closure rules if required.

define begin_modbind();
    conspair(closed_env, scope_stack) -> scope_stack;
    begin_scope();
    if ml_closure_rules then
        true -> closed_env;     ;;; hides non-pervasive global references
        [] -> local_tyc_env;    ;;; hides local references
        [] -> local_val_env;
    endif;
enddefine;

define end_modbind();
    end_scope();
    Destpair(scope_stack) -> scope_stack -> closed_env;
enddefine;

;;; begin/end_rule:
;;;     establish scope for variables occurring in the pattern part of
;;;     a rule or clause.

define begin_rule();
    open_layer(false, local_val_env) -> local_val_env;
enddefine;

define end_rule();
    Back(local_val_env) -> local_val_env;
enddefine;


;;; begin/end_valdec, begin/end_fundec:
;;;     scoping for non-recursive and completely recursive (i.e. "fun")
;;;     variable declarations. An extendable layer is added to the tyvar
;;;     env for type variables scoped at this declaration.

define lconstant open_var_layer(/* flag */) with_nargs 1;
    open_layer(/* flag, */ local_val_env) -> local_val_env;
    open_layer(true, local_tyvar_env) -> local_tyvar_env;
enddefine;

define lconstant close_var_layer(flag);
    lvars flag;
    ;;; resolve any forward references in local_val_env
    if flag == [] then resolve_forwards(local_val_env, set_var) endif;
    ;;; close current tyvar and var layers, returning any declared entries
    Back(Destpair(local_tyvar_env) -> local_tyvar_env),
    close_layer(local_val_env);
enddefine;

define begin_valdec =
    open_var_layer(% "hidden" %);
enddefine;

define end_valdec =
    close_var_layer(% "hidden" %);
enddefine;

define begin_fundec =
    open_var_layer(% [] %);
enddefine;

define end_fundec =
    close_var_layer(% [] %);
enddefine;


;;; begin/end_valrec:
;;;     scope for variables occurring in the "rec" part of a val declaration.
;;;     Only called between begin/end_valrec.

define begin_valrec();
    lvars layer;
    ;;; add a dummy recursive var layer *above* the current one to catch
    ;;; forward references
    Front(local_val_env) -> layer;
    conspair([],[]) -> Front(local_val_env);
    conspair(layer, local_val_env) -> local_val_env;
    ;;; save any non-recursive bindings already made
    conspair(Back(layer), scope_stack) -> scope_stack;
enddefine;

define end_valrec();
    lvars entries, nonrec_entries;
    ;;; recover any non-recursive bindings
    Destpair(scope_stack) -> scope_stack -> nonrec_entries;
    ;;; expose the previous dummy recursive layer and add the recursive
    ;;; bindings to it
    Back(Destpair(local_val_env) -> local_val_env) -> entries;
    chop(entries, nonrec_entries, []) -> Back(Front(local_val_env));
    ;;; fix up any forward references
    resolve_forwards(local_val_env, set_var);
    ;;; add the non-recursive bindings back in
    entries nc_<> nonrec_entries -> Back(Front(local_val_env));
enddefine;


;;; begin/end_typebind:
;;;     create an explicit scope for type variables

define begin_typebind();
    open_layer(false, local_tyvar_env) -> local_tyvar_env;
enddefine;

define end_typebind();
    Back(local_tyvar_env) -> local_tyvar_env;
enddefine;


;;; begin/end_typedec, begin/end_datatypedec:
;;;     scope for non-recursive and recursive type constructor declarations
;;;     plus any associated constructor bindings

define lconstant open_tycon_layer(/* flag */) with_nargs 1;
    ;;; add an appropriate layer to the local tycon env
    open_layer(/* flag, */ local_tyc_env) -> local_tyc_env;
    ;;; add a layer to the local val env
    open_layer(false, local_val_env) -> local_val_env;
enddefine;

define lconstant close_tycon_layer(flag);
    lvars flag;
    ;;; resolve any forward references
    if flag == [] then resolve_forwards(local_tyc_env, set_tycon) endif;
    ;;; close current tycon and con layers, returning any declared entries
    close_layer(local_tyc_env), close_layer(local_val_env);
enddefine;

define begin_typedec =
    open_tycon_layer(% "hidden" %);
enddefine;

define end_typedec();
    close_tycon_layer("hidden") -> /* cons */;
enddefine;

define begin_datatypedec =
    open_tycon_layer(% [] %);
enddefine;

define end_datatypedec =
    close_tycon_layer(% [] %);
enddefine;


;;; begin/end_withtype:
;;;     called only from within a datatype (abstype) declaration.
;;;     The "withtype" declaration is non-recursive, but the preceding
;;;     datatype declaration may have forward references to it. These are
;;;     concealed on the stack for the duration of the withtype, and the
;;;     environment layer made "hidden".

define begin_withtype();
    ;;; save any forward references from the datatype dec on the stack
    conspair(Front(Front(local_tyc_env)), scope_stack) -> scope_stack;
    ;;; close the datatype dec, but copy its entries to a new "hidden" layer
    conspair(conspair("hidden", close_layer(local_tyc_env)), local_tyc_env)
        -> local_tyc_env;
enddefine;

define end_withtype();
    ;;; replace the datatype layer with the "hidden" layer
    Destpair(local_tyc_env) -> local_tyc_env -> Front(local_tyc_env);
    ;;; restore the forward references from the stack
    Destpair(scope_stack) -> scope_stack -> Front(Front(local_tyc_env));
enddefine;


;;; begin/end_with:
;;;     called only after a begin_/end_datatypedec pair, for processing the
;;;     body of an abstype declaration.
;;;     A copy of the datatype tycon layer is made for export from the
;;;     abstype, containing the same set of tycons, but with their
;;;     constructors hidden.
;;;     The original layer and the original con layer are used throughout
;;;     the "with" part and then replaced at the end.

define begin_with();

    define lconstant abstract(tycon) -> tycon;
        lvars tycon;
        unless tycon_cons(tycon) == [] then
            copy(tycon) -> tycon;
            [] -> tycon_cons(tycon);
        endunless;
    enddefine;

    ;;; save the current tycon and con layers on the stack
    conspair(Front(local_val_env), scope_stack) -> scope_stack;
    conspair(Front(local_tyc_env), scope_stack) -> scope_stack;
    ;;; return abstract copies of the tycons
    map(Back(Front(local_tyc_env)), abstract);
enddefine;

define end_with(/* tycons */) with_nargs 1;
    ;;; update the saved tycon layer with the newly abstract copy
    /* tycons */ -> Back(Destpair(scope_stack) -> scope_stack);
    ;;; empty the saved con layer
    [] -> Back(Destpair(scope_stack) -> scope_stack);
enddefine;


;;; begin/end_exceptiondec:
;;;     scope for exception constructors

define begin_exceptiondec();
    open_layer("hidden", local_val_env) -> local_val_env;
enddefine;

define end_exceptiondec();
    close_layer(local_val_env);
enddefine;


;;; begin/end_struct:
;;;     for generative structure and signature bindings. Everything declared
;;;     within the struct is parcelled up into a structure environment.

define begin_struct();
    begin_scope();
enddefine;

define end_struct();
    lvars se = local_str_env, te = local_tyc_env, ve = local_val_env;
    end_scope();
    build_structenv(
        chop(se, local_str_env, []),
        chop(te, local_tyc_env, []),
        chop(ve, local_val_env, []));
enddefine;

;;; begin/end_structuredec:
;;;     scoping for structure declarations

define begin_structuredec();
    open_layer("hidden", local_str_env) -> local_str_env;
enddefine;

define end_structuredec();
    close_layer(local_str_env);
enddefine;

;;; begin/end_signaturedec:
;;;     scoping for signature declarations

define begin_signaturedec();
    open_layer("hidden", local_sig_env) -> local_sig_env;
enddefine;

define end_signaturedec();
    close_layer(local_sig_env);
enddefine;

;;; begin/end_functordec:
;;;     scoping for functor declarations

define begin_functordec();
    open_layer("hidden", local_fnc_env) -> local_fnc_env;
enddefine;

define end_functordec();
    close_layer(local_fnc_env);
enddefine;


;;; open, pervasive:
;;;     open a structure environment, adding its bindings to the current
;;;     local env. -pervasive- marks all entries as pervasive, i.e visible
;;;     anywhere

define open(strenv);
    lvars strenv;
    conspair(conspair(false, structenv_strs(strenv)), local_str_env)
        -> local_str_env;
    conspair(conspair(false, structenv_tycons(strenv)), local_tyc_env)
        -> local_tyc_env;
    conspair(conspair(false, structenv_vals(strenv)), local_val_env)
        -> local_val_env;
enddefine;

define pervasive(strenv);
    lvars entry, strenv;
    open(strenv);
    ;;; mark entries as pervasive
    For entry in structenv_tycons(strenv) do
        true -> tycon_pervasive(entry);
    endfor;
    For entry in structenv_vals(strenv) do
        true -> val_pervasive(entry);
    endfor;
enddefine;

;;; globalise, globalise_bindings:
;;;     copy from local to global env

define globalise(entry);
    lvars entry;
    if isval(entry) then
        entry -> global_val(entry_name(entry));
    elseif istycon(entry) then
        entry -> global_tycon(entry_name(entry));
    elseif isstr(entry) then
        entry -> global_str(entry_name(entry));
    elseif issig(entry) then
        entry -> global_sig(entry_name(entry));
    elseif isfnc(entry) then
        entry -> global_fnc(entry_name(entry));
    elseif isopr(entry) then
        entry -> global_opr(entry_name(entry));
    else
        mishap(entry, 1, 'ENVIRONMENT ENTRY NEEDED');
    endif;
enddefine;

define globalise_bindings(env);
    lvars i, env;

    define lconstant globalise_layer(layer);
        lvars layer;
        app(Back(layer), globalise);
    enddefine;

    for i to 5 do   ;;; fncs, sigs, strs, tycons, vals
        revapp(env(i), globalise_layer);
    endfor;
    revapp(env(6), globalise);  ;;; oprs -- not layered
enddefine;


/*
 *  Guarding the Environment
 */

lvars
    local_env_record = writeable {%
        local_fnc_env,
        local_sig_env,
        local_str_env,
        local_tyc_env,
        local_val_env,
        local_opr_env,
        local_tyvar_env,
        tyvar_pool,
        scope_stack,
        closed_env,
    %},
;

define save_local_env();
    local_fnc_env,
    local_sig_env,
    local_str_env,
    local_tyc_env,
    local_val_env,
    local_opr_env,
    local_tyvar_env,
    tyvar_pool,
    scope_stack,
    closed_env,
    fill(local_env_record);
enddefine;

define active local_env =
    save_local_env(%%);
enddefine;

define updaterof active local_env() with_nargs 1;
    save_local_env() -> ;
    explode(->> local_env_record)
        -> closed_env
        -> scope_stack
        -> tyvar_pool
        -> local_tyvar_env
        -> local_opr_env
        -> local_val_env
        -> local_tyc_env
        -> local_str_env
        -> local_sig_env
        -> local_fnc_env;
enddefine;

define init_local_env();
    writeable {% repeat 9 times [] endrepeat, false %};
enddefine;


/*
 * Top level declarations
 */

define begin_topdec();
    init_local_env() -> local_env;
enddefine;

define end_topdec();
    lvars val, layer;
    For layer in local_val_env do
        For val in Back(layer) do
            MAX_USAGE -> val_usage(val);
        endfor;
    endfor;
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul  4 1995
        Made identifier classes writeable by default and added other
        writeable declarations
--- Robert John Duncan, Dec 20 1994
        Run-time information from the val structure moved into mlident to
        simplify sharing, etc.
--- Robert John Duncan, Nov 24 1994
        Sectionised. Added clear_global_env.
--- Robert John Duncan, Oct 24 1994
        Changes to error reporting: rebind and unbound error routines moved
        into here
--- Robert John Duncan, Sep 27 1991
        Added -orig_cons- field to tycons
--- Simon Nichols, Jun 26 1991
        Changes to -set_var- and -build_structenv- to manage usage counts
        in values. Added -begin_topdec- and -end_topdec-.
--- Robert John Duncan, Mar 18 1991
        Disabled closure rules by default.
--- Robert John Duncan, Feb 11 1991
        Amalgamated exceptions, constructors and variables into a single
        "value" class.
--- Robert John Duncan, Feb  4 1991
        Integrated operators properly.
        Removed "undeclared" field from constructors.
        New active variable -local_env- does all necessary guarding of the
        environment.
        Changed to use -defclass-.
        Some name changes and reorganisation.
--- Robert John Duncan, Jan 14 1991
        Quick fix to scoping of value constructors -- rebinding a type
        shouldn't hide its constructors. For now, just make -undeclare_cons-
        do nothing; a tidier solution can wait.
--- Simon Nichols, Jun 21 1990
        Changes to support optimisation of tupled functions. A var now
        contains a -var_tupled- field, initialised to 0 in -newvar-.
--- Rob Duncan, Apr  9 1990
        Changed -guard_env- to reset all environments to nil: this is
        essential for recursive compilation!
--- Rob Duncan, Sep 12 1989
        Changed -lookup_sig- and -lookup_fnc- to refer to the local env,
        to allow for sequences of signature/functor declarations at top
        level.
        Changed -newfnc- to place a reference in the -fnc_code- field so
        that the field can be shared between functor instances.
 */
