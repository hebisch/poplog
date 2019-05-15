/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/modules.p
 > Purpose:         Prolog: module system
 > Author:          Rob Duncan, Aug  1 1989 (see revisions)
 > Documentation:   PLOGHELP * MODULES
 */


section prolog;

constant
    procedure ( alias, predicate_record, pred_declare, call\/1, ),
;

;;; ========================================================================

vars
    prolog_modules = [],    ;;; module stack
;

;;; push_module, pop_module:
;;;     manipulate the module stack

define lconstant push_module() with_props false;
    current_section :: prolog_modules -> prolog_modules;
enddefine;

define lconstant pop_module() with_props false;
    if prolog_modules == [] then
        mishap(0, 'NOT IN A MODULE');
    endif;
    Destpair(prolog_modules) -> (current_section, prolog_modules);
enddefine;

;;; check_module_name:
;;;     checks that -name- is a valid module name, i.e. a word (atom).

define lconstant check_module_name(/* name */) -> name with_props false;
    lvars name = prolog_deref(/* name */);
    unless isword(name) then
        mishap(name, 1, 'ATOM NEEDED');
    endunless;
enddefine;

;;; get_module:
;;;     turns a module path into a section

define lconstant get_module(/* path, */ create) -> sect with_props false;
    lvars name, create, sect = current_section, path = prolog_deref(/* path */);
    if prolog_checkspec(path, "$-", 1) then
        pop_section -> sect;
        prolog_arg(1, path) -> path;
    endif;
    while prolog_checkspec(path, "$-", 2) do
        prolog_args(path) -> path -> name;
        section_subsect(check_module_name(name), sect, create) -> sect;
    endwhile;
    section_subsect(check_module_name(path), sect, create) -> sect;
enddefine;

;;; declare_global:
;;;     export the predicate fn/arity all the way up to top section. Relies
;;;     on the implicit global status of identifiers to make it global
;;;     everywhere.

define declare_global(fn, arity) with_props false;
    lvars fn, arity;
    lvars name = alias(fn, arity), sect;
    dlocal current_section;
    while section_supersect(current_section) ->> sect do
        section_export(name);
        sect -> current_section;
    endwhile;
enddefine;

;;; declare_import, declare_export:
;;;     import/export the predicate fn/arity into/from this section.

define lconstant get_supersect() -> sect with_props false;
    lvars sect;
    unless section_supersect(current_section) ->> sect then
        mishap(0, 'NOT IN A MODULE');
    endunless;
enddefine;

define declare_import(fn, arity) with_props false;
    lvars fn, arity, pred;
    ;;; an imported name must be declared in the outer section
    procedure;
        dlocal current_section = get_supersect();
        predicate_record(fn, arity, true) -> pred;
        pred_declare(pred);
    endprocedure();
    section_import(pred_alias(pred));
enddefine;

define declare_export(fn, arity) with_props false;
    lvars fn, arity;
    ;;; check we're in a module
    get_supersect() -> ;
    ;;; export the predicate name (needn't be declared)
    section_export(alias(fn, arity));
enddefine;

;;; module/0:
;;;     enter top-section

define module\/0();
    push_module();
    pop_section -> current_section;
    chain(prolog_apply_continuation);
enddefine;

;;; module/1:
;;;     enter a named module

define module\/1(/* path */);
    lvars sect = get_module(/* path, */ true);
    push_module();
    sect -> current_section;
    chain(prolog_apply_continuation);
enddefine;

;;; endmodule/0, endmodule/1:
;;;     leave the current module

define endmodule\/0();
    pop_module();
    chain(prolog_apply_continuation);
enddefine;

define endmodule\/1(path);
    lvars path, sect = current_section;
    pop_module();
    unless get_module(path, false) == sect then
        mishap(path, 1, 'NOT IN THIS MODULE');
    endunless;
    chain(prolog_apply_continuation);
enddefine;

;;; prolog_module/1:
;;;     unify -X- with the name of the current module, or fail if at top
;;;     level

define prolog_module\/1(X);
    lvars X, name = section_name(current_section);
    if name and prolog_unify(X, name) then
        chain(prolog_apply_continuation);
    endif;
enddefine;

;;; current_module/1:
;;;     generates all existing sub-modules of the current module

define current_module\/1(/* M */);
    lvars sects, M = prolog_deref(/* M */);
    maplist(section_subsect(current_section), section_name) -> sects;
    if isword(M) then
        if fast_lmember(M, sects) then
            chain(prolog_apply_continuation);
        endif;
    elseif isprologvar(M) then
        SAVE;
        while sects /== [] do
            prolog_assign(M, Destpair(sects) -> sects);
            prolog_apply_continuation();
            RESTORE;
        endwhile;
    endif;
enddefine;

;;; $-/1, $-/2:
;;;     call a goal in a given module.

define restore_section(current_section);
    dlocal  current_section;
    prolog_apply_continuation();
enddefine;

define \$\-\/1(/* G */);
    dlocal  current_section;
    prolog_push_continuation(current_section, "ident restore_section", 2);
    pop_section -> current_section;
    call\/1(/* G */);
enddefine;

define \$\-\/2(/* name, */ G);
    lvars   name = check_module_name(/* name */), G;
    dlocal  current_section;
    prolog_push_continuation(current_section, "ident restore_section", 2);
    section_subsect(name, current_section, true) -> current_section;
    call\/1(G);
enddefine;

/*
 *  See also:
 *      "declpreds.p" for definitions of global/1, import/1, export/1
 */

endsection;     /* prolog */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Jul 15 1993
        Changes to the way predicates are declared. All predicates are now
        assumed to be global in the Pop sense (i.e downwards visible) so
        delcaring a global predicate just means pushing it up to the top
        section. Abandoned the use of define-forms for predicates.
--- Simon Nichols, Apr  7 1992
        Removed -make_declared-, replacing uses of it with -prolog_predof-,
        which now takes predicate name and arity as arguments.
--- Robert John Duncan, Jun 24 1991
        Renamed define forms.
--- Rob Duncan, Apr  9 1990
        Took out -guard_module- as being too incompatible with existing
        programs.
        Extended 'module' and 'endmodule' to allow pathnames like
            :- module $-foo$-baz.
--- Rob Duncan, Aug  8 1989
        New file based on library modules, but with many improvements and
        extensions.
 */
