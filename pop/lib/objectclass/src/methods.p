/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/methods.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Methods --------------------------------------------------------------
;;; Methods are either "linked" or "unlinked".  Methods will automatically
;;; re-link on the first use and un-link on the first potential change.
;;; Un-linked methods are distinguished by a separate secret identifier
;;; held in the pdprops of their pdpart.

compile_mode :pop11 +strict;

section $-objectclass =>
    isgeneric
    newgeneric
;

;;; forward declarations.
constant procedure (
    Do_unlink_method,
    unlink_method,
    relink_method,
    isclass
);

;;; We can exploit the method table to produce an app_all_methods -- it's
;;; not nice, but compactness is a moderately important virtue here.
;;;
define app_all_methods( p );
    lvars p;    ;;; known to be dlvars by compiler.
    appproperty(
        method_table,
        procedure();
            erase();
            p();
        endprocedure
    )
enddefine;

;;; Returns true if "m" is a method.
sysunprotect( "isgeneric" );
define global isgeneric( m ); lvars m;
    m.method_table and true
enddefine;
sysprotect( "isgeneric" );

define is_traced_generic( m ); lvars m;
    m.isclosure and
    m.pdpart == systrace and
    m.datalength == 4 and
    (frozval( 1, m ) ->> m).isgeneric and
    m
enddefine;

define detrace_generic( m ) -> m; lvars m, x;
    while m.is_traced_generic ->> x do x -> m endwhile;
enddefine;

define Check_arity( action, mode, classes, generic ); lvars action, mode, classes, generic;

    unless action.isprocedure or action.isxslot do
        mishap( 'ocimp: ADDING INVALID METHOD PART TO METHOD PROCEDURE', [^generic] )
    endunless;

    lvars field = check_mode( mode ) == UCALL_MODE and uArityMethodTable or cArityMethodTable;
    lvars a = generic.method_table.field;
    lvars nargs = action.part_pdnargs;
    
    ;;; [ check_arity: a = ^a, nargs = ^nargs, mode = ^mode ] =>
    
    if a == "unbound" then
        ;;; No checking required.  We will force the arity of the generic
        ;;; to the new value.
        nargs -> generic.method_table.field;
        set_pdnargs_mode( nargs, generic, mode );
    elseif a == "variadic" then
        ;;; No checking required.  It is allowed to have any arity.
    elseif a.isinteger then
        if not( a ==# nargs ) then
            mishap( 'ocmpa: ARITY OF METHOD PARTS DO NOT AGREE', [^generic] )
        endif;
        lvars excess = length( classes ) - nargs;
        if excess > 0 then
            repeat nargs times
                classes.tl -> classes
            endrepeat;
            repeat excess times
                if classes.dest -> classes then
                    mishap( 'oceta: METHOD PART HAS EXCESS TYPED ARGUMENTS (incorrect with_nargs?)', [^generic] )
                endif
            endrepeat
        endif;
        set_pdnargs_mode( a, generic, mode );
    else
        internal_error()
    endif;
enddefine;

define check_classes( classes ); lvars classes;
    lvars c;
    for c in classes do
        ;;; NB: must perform the "not(c)" test first -- this is because of
        ;;; fail_generic which has to be introduced very early, before
        ;;; everything works.
        unless not( c ) or c.isclass do
            mishap( 'CLASS NEEDED', [^c] )
        endunless
    endfor;
enddefine;

define update_method( action, mode, classes, generic );
    lvars action, mode, classes, generic;

    generic.detrace_generic -> generic;
    Check_arity( action, mode, classes, generic );
    check_classes( classes );

    unlink_method( generic );
    lvars path = classes.destlist.consvector;
    lvars MT = generic.method_table;

    updateMethodTable( action, mode, path, MT );
    if mode.check_mode == CALL_MODE then
        ;;; part_updater is a variant of updater that is able to
        ;;; respect slots.
        lvars u = action.part_updater;
        if u then
            Check_arity( u, UCALL_MODE, classes, generic );
            updateMethodTable( u, UCALL_MODE, path, MT )
        endif
    endif;
enddefine;

;;; Note that since methods work by having an updateable pdpart, they must
;;; be registered as writeable closures.
;;;
sysunprotect( "newgeneric" );

define set_method_properties( name, M ); lvars name, M;
    name -> pdprops( M );
    newMethodTable() -> method_table( M );
    Do_unlink_method( M );
enddefine;

define global newgeneric( name ) -> M; lvars name, M;
    newindirect( identfn ) -> M;
    set_method_properties( name, M );
enddefine;

sysprotect( "newgeneric" );


;;; -- Dependents -----------------------------------------------------------

;;; Hanging off some methods is a list of unlinking dependencies.  These
;;; are procedures that need to be run whenever methods are unlinked or
;;; relinked.  They are held in records with the unlinker, relinker,
;;; and data.
;;;
constant procedure method_dependents =
    newanyproperty(
        [], 8, 1, false,
        false, false, "tmparg",
        [], false
    );

defclass dependent {
    Dependent_unlinker,
    Dependent_relinker,
    Dependent_data
};

define_lconstant_procedure Unlink_dependents( m ); lvars m;
    lvars d;
    for d in m.method_dependents do
        Dependent_unlinker( d )( d.Dependent_data.explode, m )
    endfor;
enddefine;

define relink_dependents( m ); lvars m;
    lvars d;
    for d in m.method_dependents do
        Dependent_relinker( d )( d.Dependent_data.explode, m )
    endfor;
enddefine;

define add_dependent( dec, rec, data, m ); lvars dec, rec, data, m;
    consdependent( dec, rec, data ) :: method_dependents( m )
        -> method_dependents( m )
enddefine;



;;; -- Class Free Methods ---------------------------------------------------
;;;
;;; If a method refers to no classes then it is class-free and doesn't
;;; participate in the relinking issues.
;;;

define is_class_free_method( M ); lvars M;
    not( any_class_of_method_satisfies( M, identfn ) )
enddefine;


;;; -- Unlinking ------------------------------------------------------------

;;; declare the secret identifer for unlinked methods.
lconstant default_method_id = 'default_method';

;;; This procedure is used in the form Default_method( myself ) to fill
;;; in the pdpart of unlinked methods.  When invoked, it ensures that
;;; the method is linked then executes it.
;;;
define_lconstant_procedure Default_method( myself ) -> m; lvars myself, m;
    procedure();
        relink_method( myself );
        chain( myself )
    endprocedure -> m;
    procedure();
        relink_method( myself );
        lvars u = updater( myself );
        if u then
            chain( u )
        else
            mishap( 'ocnu: NO UPDATER FOR METHOD', [^m] )
        endif
    endprocedure -> updater( m );
    default_method_id -> pdprops( m );
enddefine;

;;; isunlinked_method recognises when a method is currently unlinked.
;;; This is needed to avoid unncessary recompilations of methods.  Method
;;; recompilation is moderately expensive, so we can't afford that.
define isunlinked_method( m ); lvars m;
    m.isgeneric and
    m.indirect_pdpart.isclosure and
    m.indirect_pdpart.pdprops == default_method_id
enddefine;

define Do_unlink_method( M ); lvars M;
    Default_method( M ) -> indirect_pdpart( M );
    updater( indirect_pdpart( M ) ) -> indirect_updater( M );
    pdprops( M ) -> pdprops( indirect_updater( M ) );
    Unlink_dependents( M );
enddefine;

define unlink_method( m ); lvars m;
    unless m.isunlinked_method do
        if trace_matches( "link" ) then
            lvars name = m.pdprops.recursive_front;
            if newgeneric.iscaller then
                printf( ';;; creating method %p\n', [^name] )
            else
                printf( ';;; UNlinking method %p\n', [^name] )
            endif
        endif;
        Do_unlink_method( m )
    endunless;
enddefine;


;;; We often only want to unlink a method if it depends on a class.
;;; This happens when a method mentions the class or any of the
;;; ancestors of C.  In this particular case, we are only concerned
;;; about dependencies on supers.
;;;
define unlink_all_methods_of_supers( CS ); lvars CS;
    unless all_classed_methods_are_unlinked do

        ;;; Construct a recognisers for all relevant classes.
        lvars procedure affected_class = newproperty( [], 32, false, "perm" );
        lvars C;
        for C in CS do
            appsupers(
                C,
                procedure( x ); lvars x;
                    true -> affected_class( x )
                endprocedure
            );
        endfor;

        lvars seen_linked_method = false;
        procedure( M ); lvars M;
            unless M.isunlinked_method do
                if any_class_of_method_satisfies( M, affected_class ) then
                    unlink_method( M )
                elseunless M.is_class_free_method then
                    true -> seen_linked_method;
                endif
            endunless
        endprocedure.app_all_methods;

        ;;; Note this crafty optimisation.  Since we are iterating
        ;;; over all methods we might as well take the opportunity
        ;;; to note if they are all unlinked.
        ;;;
        not( seen_linked_method ) -> all_classed_methods_are_unlinked;
    endunless
enddefine;

define unlink_all_methods();
    unlink_method.app_all_methods;
    true -> all_classed_methods_are_unlinked;
enddefine;

endsection;

;;; -------------------------------------------------------------------------
;;; Modified, 15/04/93, sfk
;;;     *   Added explicit sys(un)protect for exported identifiers.
;;; -------------------------------------------------------------------------
;;; Modified, 25/05/93, sfk
;;;     *   Added more sophisticated unlinking & detection of
;;;         "total-unlink" condition.
;;; -------------------------------------------------------------------------

/* --- Revision History ---------------------------------------------------
--- Integral Solutions Ltd (Julian Clinton), Mar  4 1994
        Added Steve Knight's correction for method arg checking in
        Check_arity.
--- Modified, Mar 7th 2003, Steve Knight
        set_pdnargs_mode now used to get better values for pdnargs
        for tracing.
 */
