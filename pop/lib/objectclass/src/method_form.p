/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/method_form.p
 > Purpose:         Objectclass: method definition syntax
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

section $-objectclass;

include pop11_flags;

;;; Dynamically bound variables holding the method name & call/update
;;; mode.
vars MethodName = false;
vars MethodMode = false;

;;; Lists of input and output locals
vars InLocals  = [];    ;;; NB: in reverse order
vars OutLocals = [];

;;; This variable is dlocalised to a property-table.  This allows
;;; various internal constructs to stash information for later use.
vars CurrentProperties = false;


;;; -- Representation for procedure locals --------------------------------

;;; local_name, local_idprops, local_class:
;;;     a procedure local is a 3-vector {^name ^idprops ^class}

define local_name(local);
    subscrv(1, local);
enddefine;
;;;
define local_idprops(local);
    subscrv(2, local);
enddefine;
;;;
define local_class(local);
    subscrv(3, local);
enddefine;


;;; -- Shared slots and "if_needed" methods -------------------------------

define make_shared_slot(name, defproc, defval) -> slot_p;
    lvars slot_p = sysCLOSURE(
        if defproc then
            "ident computed_shared_slot", writeable {^false ^defproc ^name},
        else
            "ident shared_slot", writeable consref(defval),
        endif, 1);
    ;;; need to set pdnargs for benefit of Popc
    1 -> slot_p.pdnargs;
    2 -> slot_p.updater.pdnargs;
enddefine;

define lconstant Make_if_needed(name, initial);
    newanyproperty([], 20, 1, false, false, false, "tmparg", false,
        sysCLOSURE("ident if_needed", initial, name, 2));
enddefine;

define check_ilocals(ilocals, keyword);
    lvars ilocals, keyword;
    if keyword == "IF_NEEDED" or keyword == "SHARED_SLOT" then
        unless ilocals.length == 1 and local_class(ilocals(1)) do
            mishap('One and only one typed input local allowed', []);
        endunless;
    endif;
enddefine;

define lconstant Cnvt_action( keyword, action ) -> action;
    if keyword == "IF_NEEDED" then
        Make_if_needed(MethodName, action) -> action;
    elseif keyword == "SHARED_SLOT" then
        make_shared_slot(MethodName, action, false) -> action;
    endif;
enddefine;


;;; -- define :method -------------------------------------------------------

;;; The define-hook -- sort of user defineable.  Agreed to put this in
;;; after a conversation with Jonathan Cunningham.  Definitely a good
;;; idea. Just a stub at present.
define vars procedure define_hook( keyword, name ); lvars keyword, name;
enddefine;

;;; Get_class:
;;;     read a class name following ":"

define lconstant Get_class() -> class;
    lvars item = readitem();
    unless isword(item)
    and isclass(currentval(derive_key_name(item)) ->> class)
    then
        mishap('CLASS NAME NEEDED', [^item]);
    endunless;
enddefine;

;;; Get_pdargs:
;;;     read input or output arguments from a procedure header; may have
;;;     type constraints if allow_constraints is true

define lconstant Get_pdargs(allow_constraints) -> locals;
    lvars locals = [];
    lvars brack = pop11_try_nextreaditem("(");
    repeat
        lvars savepl = proglist;
        lvars (name, idprops, class) = (readitem(), false, false);
        if fast_lmember(name, [lvars dlvars dlocal nonlocal]) then
            ;;; prototype feature no longer supported
            warning('NON-STANDARD DECLARATION IGNORED', [^name]);
            readitem() -> name;
        endif;
        if name == "procedure" or isnumber(name) then
            ;;; idprops
            (readitem(), name) -> (name, idprops);
        endif;
        if isword(name) and isconstant(name)
        and is_syntax_word(name) and name /== "$-"
        then
            ;;; constant syntax can't be local, so must be terminator
            quitloop;
        elseunless isword(sys_read_path(name, false, false) ->> name) then
            ;;; surely an error? will be picked up on next pass
            quitloop;
        endif;
        if pop11_try_nextreaditem(":") then
            unless allow_constraints then
                mishap('TYPE CONSTRAINT NOT ALLOWED HERE', [^name :
                    ^(readitem())]);
            endunless;
            Get_class() -> class;
        endif;
        pop11_try_nextreaditem(",").erase;
        conspair(
            {^name ^idprops ^(if allow_constraints then class endif)},
            locals) -> locals;
    endrepeat;
    savepl -> proglist;
    if brack then pop11_need_nextreaditem(")").erase endif;
enddefine;

;;; get_locals:
;;;     read input and output locals for a method or similar; inputs may
;;;     have type constraints if allow_constraints is true

define get_locals(allow_constraints) -> (ilocals, olocals);
    lvars ilocals = Get_pdargs(allow_constraints);
    lvars olocals = [];
    while pop11_try_nextreaditem("->") do
        fast_ncrev(Get_pdargs(false)) nc_<> olocals -> olocals;
    endwhile;
enddefine;

;;; Synth_method_name:
;;;     synthesises a meaningful name for a method part, i.e. the method
;;;     signature, useful for debugging

define lconstant Synth_method_name(name, ilocals, mode);
    consstring(#|
        if mode == UCALL_MODE then explode("->") endif;
        dest_characters(name), `(`;
        revapplist(ilocals,
            procedure(x);
                lvars class = local_class(x);
                class and dest_characters(class_name(class)) or `_`, `,`;
            endprocedure);
        if dup() == `,` then erase() endif, `)`;
    |#);
enddefine;

;;; comp_method:
;;;     compile a method part; props is the suggested pdprops, InLocals
;;;     and OutLocals have been previously read from the define header

define comp_method(closer, props, InLocals, OutLocals) -> proc;
    dlocal InLocals, OutLocals;
    dlocal CurrentProperties = newproperty([], 8, false, "perm");
    ;;; set up standard procedure header
    define local_decl(x);
        if x.local_idprops then x.local_idprops endif;
        x.local_name;
    enddefine;
    [%  revapplist(InLocals, local_decl);
        unless OutLocals == [] then
            "->", applist(OutLocals, local_decl);
        endunless;
     % ^^proglist] -> proglist;
    ;;; compile it, with oldvar compatibility turned off so that
    ;;; undeclared arguments are always lvars
    dlocal pop_pop11_flags = pop_pop11_flags &&~~ POP11_OLD_VARS;
    lvars proc = pop11_comp_procedure(closer, MethodName, props);
    if proc.isclosure then
        ;;; can't handle lexical closures
        mishap('METHOD CAPTURING LEXICAL VARIABLE', [^MethodName]);
    endif;
    if CurrentProperties("invokes_call_next_method") then
        set_invokes_call_next_method( MethodMode, proc );
    endif;
enddefine;

;;; method_form:
;;;     implements define_method and similar forms

define method_form(keyword, closing_keyword);
    lvars mode = pop11_try_nextreaditem("updaterof").flag_to_mode;
    lvars decl = read_identspec(default_default_idspec());
    lvars name = readitem();
    ;;; declare a generic procedure with this name
    define_hook(keyword, name);
    mode_identspec_declare(decl, mode)(name, "procedure");
    bind_method(name, mode);
    ;;; read input/output locals with type constraints
    lvars (ilocals, olocals) = get_locals(true);
    check_ilocals(ilocals, keyword);
    ;;; compile method part: always done in popc-mode, because we can't
    ;;; cope with generics compiled half one way and half another
    dlocal pop_pas_mode = OBJECTCLASS_IN_POPC;
    dlocal MethodName = name, MethodMode = mode;
    lvars props = Synth_method_name(name, ilocals, mode);
    lvars action = comp_method(closing_keyword, props, ilocals, olocals);
    ;;; add method to generic
    update_method(
        Cnvt_action(keyword, action),
        mode,
        maplist(ilocals, local_class),
        currentval(name));
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec  7 1995
        Changed comp_method to disable the +oldvar compile-mode flag, so
        that undeclared arguments will always be lvars; no longer a need
        for pop_oc_v*ars_default.
--- Robert John Duncan, Nov 28 1995
        Revised method_form, etc. to use pop11_comp_procedure so that
        method definitions are as close as possible to ordinary procedure
        definitions: argument declarations in the define header are no
        longer supported, and the default declaration is the same as for
        other procedures. Added pop_oc_v*ars_default so that the "lvars"
        default can be restored for old programs.
--- Robert John Duncan, Oct 27 1995
        Changed make_shared_slot to handle both kinds of slot and to use
        sysCLOSURE; likewise Make_if_needed, renamed from Make_slot.
--- Robert John Duncan, Oct  4 1995
        Popc changes
;;; -------------------------------------------------------------------------
;;; Modified, 31/8/93, sfk
;;;     *   Removed the define :method <name>; syntax.  Replaced
;;;         by define :generic <name>.
;;;     *   Made define_method autoloadable.  This is a fairly
;;;         irrelevant change that I made for symmetry.  It is sensible
;;;         in principle -- but makes no change in practice.
;;; -------------------------------------------------------------------------
;;; Modified, 4/6/93, sfk
;;;     *   Changed the mode value for invokes_call_next_method to use
;;;         CALL_MODE and UCALL_MODE.
;;; -------------------------------------------------------------------------
;;; Modified, 30/05/93, sfk
;;;     *   Extensively rewritten to reflect the new compilation
;;;         strategy for classes.
;;; -------------------------------------------------------------------
;;; Modified, 20/04/93, sfk
;;;     *   Uses the read_olocals utility, shared by the define:pd form.
;;; -------------------------------------------------------------------------
;;; Modified, 15/04/93, sfk
;;;     *   Changed spurious "GENERIC" prefix on error messages
;;; -------------------------------------------------------------------------
;;; Modified, 6/12/92, sfk
;;;     *   Cleaned up declaration of :define_form method.
;;; -------------------------------------------------------------------------
;;; Modified 4/10/92, sfk
;;;     *   Ensured that MethodName was dynamically bound to method_form.
;;;         This wasn't correct previously and the error message for
;;;         using call_next_method outside of a procedure was
;;;         inevitably incorrect.
;;;     *   Cleaned up spelling errors inside comments.
;;; -------------------------------------------------------------------------
 */
