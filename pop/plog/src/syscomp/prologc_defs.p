/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/syscomp/prologc_defs.p
 > Purpose:         Customising the Prolog system for standalone compilation
 > Author:          Robert John Duncan, Jul 13 1993 (see revisions)
 > Documentation:
 > Related Files:
 */

section prolog;

;;; Redefine pred_idval to record predicate definitions in a property
;;; table, only assigned to identifiers at the end of the file to allow
;;; for redefinitions
lvars procedure prologc_pred_table;

define lconstant prologc_pred_idval(pred) with_props pred_idval;
    lvars pred, entry = prologc_pred_table(pred);
    entry and Front(entry);
enddefine;
;;;
define updaterof prologc_pred_idval(proc, pred) with_props pred_idval;
    lvars proc, pred;
    conspair(proc, word_identifier(pred_alias(pred), current_section, true))
        -> prologc_pred_table(pred);
enddefine;

;;; Redefine prolog_load to do extra POPC work
lconstant procedure save_prolog_load = prolog_load;

define vars prolog_load();
    dlocal
        prolog_pas_mode     = "prologc",
        prolog_syspredicate = not(DEF prolog_debugging),
        prolog_no_clauses   = not(DEF prolog_debugging),
        prolog_tags         = DEF prolog_debugging,
        pop_debugging       = DEF prolog_debugging,
        popgctrace          = DEF prolog_debugging,
    ;

    define dlocal prologc_pred_table =
        newanyproperty([], 64, 1, 56, false, false, "perm", false, false);
    enddefine;

    dlocal pred_idval = prologc_pred_idval;

    ;;; start off with no preconceptions
    clearproperty(predicate_table);

    ;;; do the load
    save_prolog_load();

    ;;; output definitions
    appproperty(
        prologc_pred_table,
        procedure(pred, entry);
            lvars pred, entry, (proc, wid) = destpair(entry);
            lvars dynamic = is_dynamic(proc);
            ;;; mark as a Prolog identifier
            pas_declare_perm(identof(wid), if dynamic then `D` else `S` endif);
            ;;; output the last assigned definition for the predicate
            unless is_undefined_closure(proc)
            or is_special_functor(pred_spec(pred))
            or dynamic and back(dynamic) == []  ;;; declared, but no clauses
            then
                sysPASSIGN(proc, wid);
                ;;; add the predicate record to the table
                sysPUSHQ(pred), sysPUSHQ(pred_ident(pred)),
                    sysUCALL("ident predicate_table");
            endunless;
        endprocedure);
    sysEXECUTE();
enddefine;

;;; Record assignments to incremental properties
define prologc_property_update(val, arg, prop_name);
    lvars val, arg, prop_name;
    sysPUSHQ(val), sysPUSHQ(arg), sysUCALL(prop_name);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug 16 1993
        Added call to pas_declare_perm to mark Prolog identifiers; this
        allows default definitions to be generated automatically by poplink.
        Set prolog_pas_mode while compiling.
--- Robert John Duncan, Aug  4 1993
        Disabled prolog_tags by default
 */
