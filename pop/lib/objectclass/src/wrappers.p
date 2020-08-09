/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/wrappers.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

section $-objectclass;

;;; -- Compiling wrappers -------------------------------------------------

define lconstant Check_wrapper_args(which, args, n);
    unless length(args) == n then
        mishap(
            if length(args) < n then
                'TOO FEW ARGUMENTS FOR WRAPPER PROCEDURE'
            else
                'TOO MANY ARGUMENTS FOR WRAPPER PROCEDURE'
            endif,
            [^which ( ^^args )]);
    endunless;
enddefine;

;;; compile a wrapper as a procedure expression
define compile_any_wrapper( expr ); lvars expr;
    lvars n = #| sysEXEC_COMPILE( exprcomp(% expr %), false ) |#;
    if n < 1 then
        mishap( 'NO RESULT FROM WRAPPER EXPRESSION', [^expr] )
    elseif n > 1 then
        mishap( 'TOO MANY RESULTS FROM WRAPPER EXPRESSION', [^expr] )
    elseunless dup().isprocedure then
        erase();
        mishap( 'NON-PROCEDURE RESULT FROM WRAPPER EXPRESSION', [^expr] );
    endif;
enddefine;

;;; for cons and new wrappers:
;;;     on cons(x) do <expr> ==>
;;;         on cons do procedure() -> x; ()() -> x; <expr> endprocedure
define lconstant compile_cons_wrapper(which, idecs, ilocals, expr);
    Check_wrapper_args(which, ilocals, 1);
    lvars decl = idecs(1), x = ilocals(1);
    sysPROCEDURE(false, 1);
        ;;; apply constructor from stack
        sysCALLS(0);
        ;;; pop result
        decl(x), sysPOP(x);
        ;;; initialise
        exprcomp(expr);
        ;;; push result
        sysPUSH(x);
    sysENDPROCEDURE();
enddefine;

;;; for access and update wrappers:
;;;     on access(x,p) do <expr> ==>
;;;         on access do procedure(x,p); <expr>; p(x); endprocedure
define lconstant compile_access_wrapper(which, idecs, ilocals, expr);
    Check_wrapper_args(which, ilocals, 2);
    lvars (xdecl, pdecl) = idecs.dl, (x, p) = ilocals.dl;
    sysPROCEDURE(false, which == "access" and 2 or 3);
        ;;; pop arguments
        pdecl(p), sysPOP(p), xdecl(x), sysPOP(x);
        ;;; update first
        if which == "update" then sysPUSH(x), sysCALL(p) endif;
        ;;; user code
        exprcomp(expr);
        ;;; access last
        if which == "access" then sysPUSH(x), sysCALL(p) endif;
    sysENDPROCEDURE();
enddefine;

;;; for destroy wrappers:
;;;     on destroy(x) do <expr> ==>
;;;         on destroy do procedure(x,p); <expr>; p(x); endprocedure;
define lconstant compile_destroy_wrapper(which, idecs, ilocals, expr);
    Check_wrapper_args(which, ilocals, 1);
    lvars decl = idecs(1), x = ilocals(1);
    sysPROCEDURE(false, 2);
        ;;; save super destroyer to temporary
        lvars p = sysNEW_LVAR(); sysPOP(p);
        ;;; pop item
        decl(x), sysPOP(x);
        ;;; user code
        exprcomp(expr);
        ;;; call super
        sysPUSH(x), sysCALL(p);
    sysENDPROCEDURE();
enddefine;
;;;
;;; this extra cons wrapper adds a destroy proc to the destroy property
define make_add_to_destroy_wrapper(destroy_p);
    sysPROCEDURE(false, 1);
        ;;; apply constructor from stack
        sysCALLS(0);
        ;;; pop result
        lvars x = sysNEW_LVAR(); sysPOP(x);
        ;;; add destroy action
        sysPUSHQ(destroy_p), sysPUSH(x), sysUCALL("ident destroy_action");
        ;;; push result
        sysPUSH(x);
    sysENDPROCEDURE();
enddefine;

define compile_wrapper(which, idecs, ilocals, expr);
    if not(idecs) then
        compile_any_wrapper(expr)
    elseif which == "cons" or which == "new" then
        compile_cons_wrapper(which, idecs, ilocals, expr);
    elseif which == "access" or which == "update" then
        compile_access_wrapper(which, idecs, ilocals, expr);
    elseif which == "destroy" then
        compile_destroy_wrapper(which, idecs, ilocals, expr);
    else
        mishap('UNSUPPORTED WRAPPER TYPE', which);
    endif;
enddefine;

;;; -- Wrapper tables -----------------------------------------------------

define class_cons_wrapper =
    newanyproperty(
        [], 20, 1, false,
        false, false, "tmparg",
        false, false);
enddefine;

define class_new_wrapper =
    copy(class_cons_wrapper);
enddefine;

define class_access_wrapper =
    copy(class_cons_wrapper);
enddefine;

define class_destroy_wrapper =
    copy(class_cons_wrapper);
enddefine;

define class_update_wrapper(k);
    lvars access_p = class_access_wrapper(k);
    access_p and updater(access_p);
enddefine;
;;;
define updaterof class_update_wrapper(update_p, k);
    returnunless(update_p);
    lvars access_p = class_access_wrapper(k);
    unless access_p then
        apply(%%) ->> access_p -> class_access_wrapper(k);
    endunless;
    update_p -> updater(access_p);
enddefine;

;;; build up a list of wrappers in inheritance order
define all_wrappers(class, class_wrapper);
    [%  appsupers(class,
            procedure(class);
                lvars wrapper = class_wrapper(class);
                if wrapper then wrapper endif;
            endprocedure);
    %];
enddefine;

vars
    wrapper_names = [
        cons
        new
        access
        update
        destroy
    ],
    wrapper_types = [
        ^class_cons_wrapper
        ^class_new_wrapper
        ^class_access_wrapper
        ^class_update_wrapper
        ^class_destroy_wrapper
    ];

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct 26 1995
        Radical changes to support improved syntax for wrappers, to add
        update and destroy wrappers, and to respect changes in the
        inheritance algorithm. Wrapper tables now contain just the single
        wrapper associated with each class; use all_wrappers to generate the
        full, ordered set.
;;; -------------------------------------------------------------------------
;;; Modified, 2/7/93, sfk
;;;     *   Corrected defect in the treatement of updaters in
;;;         plant_call_accessor.
;;; -------------------------------------------------------------------------
;;; Modified, 11/04/92, sfk
;;;     *   Altered call to seq_to_closure.
;;; -------------------------------------------------------------------------
;;; Modified, 10/12/92, sfk
;;;     *   Removed superfluous code in singletonclass revision,
;;;         at version 5.05.
;;; -------------------------------------------------------------------------
;;; Modified, 9/12/92, jonm & sfk
;;;     *   Improved performance for wrappers of apply.
;;; -------------------------------------------------------------------------
 */
