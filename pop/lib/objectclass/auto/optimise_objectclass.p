/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/optimise_objectclass.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode :pop11 +strict;

uses objectclass;

section $-objectclass => optimise_objectclass;

define lconstant procedure opt_methods();
    relink_method.app_all_methods;
enddefine;

define lconstant procedure opt_isas();
    appisas( procedure( i ); lvars i; 0.i.erase endprocedure )
enddefine;

sysunprotect( "method_table" );
;;; Irreversibly trash the method table.
define lconstant opt_irreversible( unchecked ); lvars unchecked;

    ;;; Note that this MUST be run before we trash the methods_table since
    ;;; app_all_methods requires the methods table to persist.
    ;;;
    define lconstant fix_methods();
        procedure( m ); lvars m;
            erase( nonwriteable m )
        endprocedure.app_all_methods
    enddefine;

    define lconstant destroy_syntax_words();
        syscancel( "define_method" );
        syscancel( "define_objectclass" );
        syscancel( "define_extantclass" );
        syscancel( "define_mixin" );
        syscancel( "define_singletonclass" );
        syscancel( "define_if_needed_method" );
        syscancel( "define_shared_slot_method" );
        syscancel( "define_object" );
        syscancel( "object" );
    enddefine;

    define lconstant destroy_run_time_tables();
        procedure() with_props method_table;
            mishap( 'Method table has been destroyed', [] )
        endprocedure -> method_table;
    enddefine;

    if can_trash_method_table or unchecked then
        fix_methods();
        destroy_syntax_words();
        destroy_run_time_tables();
    else
        warning( 'Method table may still be in use, not trashed', [] )
    endif;
enddefine;
sysprotect( "method_table" );

define lconstant opt_zap_methods();
    app_all_methods(
        procedure( m ); lvars m;
            false -> pdprops( m );
        endprocedure
    )
enddefine;

define lconstant opt_zap_method_parts();

    define lconstant zap( e ); lvars e;
        if e.isEntry and isprocedure( actionEntry( e ) ) then
            false -> pdprops( actionEntry( e ) )
        endif;
    enddefine;

    procedure( m ); lvars m;
        lvars mt = m.method_table;
        appdata( mt.cEntriesMethodTable, zap );
        appdata( mt.uEntriesMethodTable, zap );
    endprocedure.app_all_methods
enddefine;

lconstant this_section = current_section;
define lconstant opt_zap_objectclass();
    section_cancel( this_section, true );
enddefine;

lconstant allowed_keywords =
    [
        all delivery_time irreversible isas methods unchecked
        zap_methods zap_method_parts zap_objectclass_section
    ];

;;; Since old classes can only be garbage collected when methods & isas are
;;; unlinked, we must perform an explicit garbage collection here.
;;;
define global vars procedure optimise_objectclass( keywords ); lvars keywords;
    lvars Do = newproperty( [], 20, false, "perm" );

    define lconstant Add( k ); lvars k;
        unless lmember( k, allowed_keywords ) do
            warning( 'Invalid keyword', [^k] )
        endunless;
        returnif( Do( k ) );
        k -> Do( k );
        if k == "all" then
            Add( "methods" );
            Add( "isas" );
        elseif k == "irreversible" then
            Add( "all" )
        elseif k == "delivery_time" then
            applist( delete( "unchecked", allowed_keywords ), Add )
        endif;
    enddefine;

    applist( keywords.islist and keywords or [^keywords], Add );

    ;;; Let's try to scavenge all the defunct classes.  These classes
    ;;; can only be garbage-collected while the owning methods are
    ;;; unlinked.  So we unlink and garbage-collect!
    unlink_method.app_all_methods;
    unlink_all_isas();
    sysgarbage();

    if Do( "methods" ) then opt_methods() endif;
    if Do( "isas" ) then opt_isas() endif;
    if Do( "zap_methods" ) then opt_zap_methods() endif;
    if Do( "zap_method_parts" ) then opt_zap_method_parts() endif;
    if Do( "zap_objectclass" ) then opt_zap_objectclass() endif;
    if Do( "irreversible" ) then opt_irreversible( Do( "unchecked" ) ) endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
