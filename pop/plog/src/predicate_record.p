/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/predicate_record.p
 > Purpose:         Prolog: recording predicate declarations
 > Author:          Robert John Duncan, May  5 1993 (see revisions)
 > Documentation:
 > Related Files:
 */


section prolog;

constant
    procedure ( alias, unwrapped, is_special_functor, cons_special_closure,
        is_undefined_closure, cons_undefined_closure, is_dynamic,
        proc_clauses, );

vars
    procedure ( prolog_invisible, );

;;; ========================================================================

vars
    prolog_pas_mode = false,
        ;;; set to "prologc" for stand-alone compilation
;

;;; Type converters for predicate records:

define lconstant bit1(n);
    lvars n;
    if n == 0 then false else true endif;
enddefine;
;;;
define updaterof bit1(v);
    lvars v;
    if v then 1 else 0 endif;
enddefine;

define lconstant bit2(n);
    lvars n;
    if n == 0 then false elseif n == 1 then true else "undef" endif;
enddefine;
;;;
define updaterof bit2(v);
    lvars v;
    if v == false then 0 elseif v == "undef" then 2 else 1 endif;
enddefine;

defclass predicate_record [writeable] {
    pred_ident      : full,
    pred_alias      : full,
    pred_functor    : full,
    pred_arity      : full,
    pred_tag        : full,
    pred_system     : 2#bit2,
    pred_dynamic    : 2#bit2,
    pred_notext     : 2#bit2,
    pred_hidden     : 1#bit1,
    pred_spied      : 1#bit1,
};

;;; new_predicate_record:
;;;     returns a new predicate record with attribute information left
;;;     undefined

define new_predicate_record(name, fn, arity);
    lvars name, fn, arity;
    conspredicate_record(
        false,      ;;; pred_ident
        name,       ;;; pred_alias
        fn,         ;;; pred_functor
        arity,      ;;; pred_arity
        false,      ;;; pred_tag
        "undef",    ;;; pred_system
        "undef",    ;;; pred_dynamic
        "undef",    ;;; pred_notext
        false,      ;;; pred_hidden
        false,      ;;; pred_spied
    );
enddefine;

;;; predicate_table:
;;;     maps predicate identifiers to predicate records

define predicate_table =
    newanyproperty([], 512, 1, 410, false, false, "tmparg", false, false);
enddefine;

;;; pred_spec:
;;;     returns functor name and arity from a predicate record

define pred_spec(pred);
    lvars pred;
    pred_functor(pred), pred_arity(pred);
enddefine;

;;; pred_declare:
;;;     ensure that a predicate has an associated identifier

define pred_declare(pred);
    lvars pred, const, id;
    ;;; default declaration
    if pred_system(pred) == true
    or pred_system(pred) == "undef" and prolog_pas_mode and prolog_syspredicate
    then
        true
    else
        false
    endif -> const;
    unless (pred_ident(pred) ->> id)
    and identtype(id) == "procedure"
    and (not(const) or isconstant(id))
    then
        lvars name = pred_alias(pred);
        sysSYNTAX(name, "procedure", const or id and isconstant(id));
        sysGLOBAL(name);
        identof(name) -> id;
    endunless;
    id -> pred_ident(pred);
    if isconstant(id) then true -> pred_system(pred) endif;
    pred -> predicate_table(id);
enddefine;

;;; pred_idval:
;;;     raw mapping from predicate records to procedure values. In
;;;     normal mode this works through the idval of the predicate
;;;     identifier, but it's redefined by POPC to do something
;;;     different. Note that the value of a predicate must always be a
;;;     procedure, so this can safely return <false> to indicate that
;;;     the value's not yet been set.

define vars pred_idval(pred);
    lvars pred, proc;
    if (pred_ident(pred) ->> proc)
    and isprocedure(idval(proc) ->> proc)
    and not(isundef(proc))
    then
        proc;
    else
        false;
    endif;
enddefine;
;;;
define updaterof pred_idval(proc, pred);
    lvars proc, pred, id;
    unless pred_ident(pred) ->> id then
        pred_declare(pred);
        pred_ident(pred) -> id;
    endunless;
    proc -> idval(id);
enddefine;

;;; pred_valof:
;;;     standard mapping from predicate records to procedure values.
;;;     Similar to pred_idval but returns the unwrapped value and always
;;;     returns a procedure, even if it's only an undef closure created
;;;     on the fly.

define pred_valof(pred) -> proc;
    lvars pred, proc;
    if pred_idval(pred) ->> proc then
        unwrapped(proc) -> proc;
    else
        cons_undefined_closure(pred_spec(pred)) -> proc;
    endif;
enddefine;
;;;
define updaterof pred_valof(proc, pred);
    lvars proc, pred;
    proc -> unwrapped(pred_idval(pred)) -> pred_idval(pred);
enddefine;

;;; pred_raw_valof:
;;;     like pred_valof, but without ignoring invisible wrappers

define pred_raw_valof with_nargs 1;
    dlocal prolog_invisible = not;
    pred_valof();
enddefine;
;;;
define updaterof pred_raw_valof with_nargs 2;
    dlocal prolog_invisible = not;
    -> pred_valof();
enddefine;

;;; pred_isdefined:
;;;     like predicate_isdefined, but working on a predicate record. It
;;;     returns the unwrapped value.

define pred_isdefined(pred) -> proc;
    lvars pred, proc;
    if (pred_idval(pred) ->> proc)
    and is_undefined_closure(unwrapped(proc) ->> proc)
    then
        false -> proc;
    endif;
enddefine;

;;; pred_has_clauses:
;;;     like predicate_has_clauses, but working on a predicate record

define pred_has_clauses(pred);
    lvars pred, proc, clauses;
    unless pred_isdefined(pred) ->> proc then
        false;
    elseunless is_dynamic(proc) ->> clauses then
        proc_clauses(proc) /== [];
    else
        Back(clauses) /== [];
    endunless;
enddefine;

;;; special_record:
;;;     returns a predicate record for a "special" predicate. These should
;;;     always be defined, so if there's no record already, create one.

define special_record(fn, arity) -> pred;
    lvars fn, arity, pred;
    lvars name = alias(fn, arity);
    lvars wid = word_identifier(name, pop_section, "undef");
    lvars id = isdefined(wid);
    unless id
    and identtype(id) == "procedure"
    and isconstant(id)
    and isglobal(id)
    then
        sysSYNTAX(wid, "procedure", true);
        sysGLOBAL(wid);
        identof(wid) -> id;
    endunless;
    unless id == identof(name) then
        id -> identof(name);
    endunless;
    unless predicate_table(id) ->> pred then
        new_predicate_record(name, fn, arity) -> pred;
        id -> pred_ident(pred);
        true -> pred_system(pred);
        true -> pred_notext(pred);
        false -> pred_dynamic(pred);
        pred -> predicate_table(id);
    endunless;
    unless pred_isdefined(pred) then
        cons_special_closure(fn, arity) -> pred_valof(pred);
    endunless;
enddefine;

;;; predicate_record:
;;;     get a record for predicate fn/arity. If there isn't one in the
;;;     table and create is <true> then return a newly-created one.

define predicate_record(fn, arity, create) -> pred;
    lvars fn, arity, create, pred = false;
    if is_special_functor(fn, arity) then
        special_record(fn, arity) -> pred;
    else
        lvars name = alias(fn, arity), id;
        if isdefined(name) ->> id then
            predicate_table(id) -> pred;
        endif;
        if not(pred) and create then
            new_predicate_record(name, fn, arity) -> pred;
            if id then
                ;;; return the same record next time
                id -> pred_ident(pred);
                pred -> predicate_table(id);
                ;;; fill in missing declaration attributes
                if isconstant(id) then true -> pred_system(pred) endif;
                lvars proc;
                if pred_isdefined(pred) ->> proc then
                    if is_dynamic(proc) then
                        true -> pred_dynamic(pred);
                    else
                        false -> pred_dynamic(pred);
                        proc_clauses(proc) /== [] -> pred_notext(pred);
                    endif;
                endif;
            endif;
        endif;
    endif;
enddefine;

;;; app_predicates:
;;;     apply a procedure to every predicate which is defined in the
;;;     current section and not hidden

define app_predicates(p);
    dlvars procedure p;
    appproperty(
        predicate_table,
        procedure(id, pred);
            lvars id, pred;
            if isdefined(pred_alias(pred)) == pred_ident(pred)
            and pred_isdefined(pred)
            and not(pred_hidden(pred))
            then
                p(pred);
            endif;
        endprocedure);
enddefine;

;;; app_named_predicates:
;;;     like app_predicates, but restricted to predicates having a given
;;;     functor name

define app_named_predicates(fn, p);
    dlvars fn, procedure p;
    appproperty(
        predicate_table,
        procedure(id, pred);
            lvars id, pred;
            if pred_functor(pred) == fn
            and isdefined(pred_alias(pred)) == pred_ident(pred)
            and pred_isdefined(pred)
            and not(pred_hidden(pred))
            then
                p(pred);
            endif;
        endprocedure);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 16 1993
        Added prolog_pas_mode as an indicator of compilation mode (normal
        or standalone).
 */
