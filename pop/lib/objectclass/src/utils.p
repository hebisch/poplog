/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/utils.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

;;; -- Utilities ------------------------------------------------------------

section $-objectclass;

define internal_error();
    mishap( 'ocie: OBJECTCLASS INTERNAL ERROR', [] );
enddefine;

define internal_fault( msg ); lvars msg;
    mishap( 'ocif: OBJECTCLASS INTERNAL FAULT', [^msg] )
enddefine;

;;; Used as boolean values for modes. Can be tested using "==".
constant
    CALL_MODE  = "CALL_MODE",
    UCALL_MODE = "UCALL_MODE"
;

define flag_to_mode( f ); lvars f;
    f and UCALL_MODE or CALL_MODE
enddefine;

define apply_mode( x, m ); lvars x, m;
    if m == CALL_MODE then
        x
    elseif m == UCALL_MODE then
        x.updater
    else
        internal_fault( 'apply_mode: inconsistent mode value' )
    endif
enddefine;

;;; Useful only as a transitional device from boolean modes to
;;; procedure valued modes.
;;;
define check_mode( m ) -> m; lvars m;
    unless m == CALL_MODE or m == UCALL_MODE do
        internal_fault( 'inconsistent mode value' )
    endunless
enddefine;

define check_flag( f ) -> f; lvars f;
    unless f == "CALLQ" or f == "PUSHQ" do
        internal_fault( 'inconsistent call/push flag' )
    endunless
enddefine;

define property_range( prop ); lvars prop;
    [%
        fast_appproperty(
            prop,
            procedure( k, v ); lvars k, v;
                v
            endprocedure
        )
    %]
enddefine;

define property_domain( prop ); lvars prop;
    [% fast_appproperty( prop, erase ) %]
enddefine;

define currentval( w ); lvars w;
    sys_current_val(w);
enddefine;

define isrecordkey( K ); lvars K;
    K.class_field_spec.islist
enddefine;

define exprcomp( E ); lvars E;
    dlocal proglist_state = proglist_new_state( E );
    ;;; this looks a bit weird, but using pop11_comp_expr_to forces the
    ;;; expression to be well-formed, and putting <termin> in a list
    ;;; forces the condition popclosebracket /== popclosebracket_exec to
    ;;; allow lblocks
    pop11_comp_expr_to( #_<[^termin]>_# ).erase;
enddefine;

define check_datakey( x, k ) -> x; lvars x, k;
    unless x.datakey == k do
        mishap( 'ITEM WITH KEY ' sys_>< class_dataword( k ) sys_>< ' NEEDED',
            [^x] )
    endunless
enddefine;

;;; Detect_suspend is used in the following idiom:
;;;     vars flag;
;;;     define foo();
;;;         dlocal flag = false;
;;;         dlocal 0 % detect_suspend( dlocal_context, ident flag ) %;
;;;         ...
;;;     enddefine;
;;;
define detect_suspend( context, id ); lvars context, id;
    if context == 3 then
        ;;; We have intercepted a suspend.
        true -> idval( id );
    endif
enddefine;
;;;
;;; And it must have an updater.
;;;
define updaterof detect_suspend( context, id ); lvars context, id;
enddefine;

define warn_obsolete( message ); lvars message;
    warning( 'OBSOLETE LIBRARY (SUPERCEDED BY ' sys_>< message sys_>< ')', [] );
enddefine;

define macro define_lconstant_procedure;
    "define";
    if pop_debugging /== true then
        "lconstant", "procedure"
    endif;
enddefine;

define check_one( n ); lvars n;
    unless n == 1 do
        if n < 1 then
            mishap( 'CHECK ONE: Too few results', [] )
        elseif n > 1 then
            mishap( 'CHECK ONE: Too many results', [] )
        endif
    endunless
enddefine;

define all_the_same_in_list( L ); lvars L;
    if null( L ) then
        true
    else
        lvars i, item = dest( L ) -> L;
        for i in L do
            returnunless( i == item )( false )
        endfor;
        true
    endif
enddefine;

define filterout( L, pred ); lvars L, procedure pred;
    [%
        until null( L ) do
            lvars i = fast_destpair( L ) -> L;
            unless pred( i ) do i endunless
        enduntil
    %]
enddefine;

define filterin( L, pred ); lvars L, procedure pred;
    [%
        until null( L ) do
            lvars i = fast_destpair( L ) -> L;
            if pred( i ) then i endif
        enduntil
    %]
enddefine;

define filterout_lmember( x, y ); lvars x, y, i;
    expandlist( y ) -> y;
    [%
        until null( x ) do
            unless fast_lmember( fast_destpair( x ) -> x ->> i, y ) then
                i
            endunless
        enduntil
    %]
enddefine;

define at_least_length( x, n ); lvars x, n;
    if n <= 0 then
        true
    elseif null( x ) then
        false
    else
        chain( tl(x), n-1, at_least_length )
    endif
enddefine;

define more_than_one( x ); lvars x;
    not( x.null ) and not( x.fast_back.null )
enddefine;

define exactly_one( x ); lvars x;
    not( x.null ) and null( fast_back( x ) )
enddefine;

define revconslist( n ); lvars n;
    if fi_check( n, 0, false ) == 0 then
        []
    else
        lvars ans = conspair( (), [] );
        lvars h = ans;
        fast_repeat n fi_- 1 times
            conspair( (), [] ) ->> back( h ) -> h
        endrepeat;
        ans;
    endif;
enddefine;

define revdl( L ); lvars L;
    unless null( L ) do
        lvars ( h, t ) = fast_destpair( L );
        revdl( t );
        h
    endunless;
enddefine;

define revapplist(L, P);
    unless null(L) then
        revapplist(fast_back(L), P);
        P(fast_front(L));
    endunless;
enddefine;

;;; Need a memoisation capability for preventing recalculation of old values.
;;; This one is suitable for 1-input, 1-output functions & actually checks
;;; on that.
define memofn( f ); lvars procedure f;
    newanyproperty(
        [], 20, 1, false,
        false, false, "tmparg",
        false,
        procedure( k, p ); lvars k, p;
            check_one(#| f( k ) |#) ->> p( k )
        endprocedure
    )
enddefine;

define applists_n( n, p ); lvars n, procedure p;
    lvars lists = conslist( n );
    repeat
        lvars m = lists, j = 0;
        until m == [] do
            lvars i = fast_front( m );
            if null( i ) then
                erasenum( j );
                quitloop( 2 )
            endif;
            j fi_+ 1 -> j;
            fast_destpair( i ) -> fast_front( m );
            fast_back( m ) -> m;
        enduntil;
        p();
    endrepeat;
    sys_grbg_list( lists );
enddefine;

;;; Used to enable call_next_method to be used in update mode.
define null_instruction( item ); lvars item;
enddefine;

define updaterof null_instruction( item ); lvars item;
enddefine;

define sysSUBSCR_STACK( n ); lvars n;
    if n == 1 then
        sysPUSHS( undef )
    else
        sysSWAP( 1, n );
        sysPUSHS( undef );
        sysSWAP( 2, n + 1 );
    endif;
enddefine;

;;; like consclosure, but takes a procedure name rather than a value and
;;; uses the VM to generate it so that in Popc we can create closures of
;;; procedures in the runtime library
define sysCLOSURE(n);
    lvars args = consvector(n), p = ();
    sysEXEC_COMPILE(
        procedure();
            sysPUSH(p), appdata(args, sysPUSHQ), sysPUSHQ(n),
                sysCALL("consclosure");
        endprocedure,
        false);
enddefine;

define andlist( L, pred ); lvars L, procedure pred;
    lvars bool = true;
    until null( L ) do
        unless pred( fast_destpair( L ) -> L ) ->> bool do
            return( false )
        endunless
    enduntil;
    return( bool );
enddefine;

define orlist( L, pred ); lvars L, procedure pred;
    lvars bool = false;
    until null( L ) do
        if pred( fast_destpair( L ) -> L ) ->> bool do
            return( bool )
        endif
    enduntil;
    return( false );
enddefine;

;;; -popfilename- is protected in Prolog and SML.
lvars prot = "popfilename".isprotected;
#_IF prot
sysunprotect( "popfilename" );
#_ENDIF

define plant_and_execute( p ); dlvars p;
    dlocal popfilename = false;     ;;; inhibits debugger.
    procedure();
        p();
        sysEXECUTE();
    endprocedure.sysCOMPILE
enddefine;

#_IF prot
sysprotect( "popfilename" );
#_ENDIF

;;; Compiles a default procedure.  The default procedure has the
;;; responsibility of ensuring that only one argument is returned and
;;; binding the "magic" lexical variable -myself-.
;;;
define compile_defproc( fname, E ); lvars fname, E;
    sysPROCEDURE( false, 1 );
        sysLVARS( "myself", 0 );
        sysPOP( "myself" );
        lvars n = sysNEW_LVAR();
        sysCALL( "stacklength" );
        sysPOP( n );
        exprcomp( E );
        sysCALL( "stacklength" );
        sysPUSH( n );
        sysPUSHQ( fname );
        sysCALL( "ident check_slot_init" );
    sysENDPROCEDURE();
enddefine;

define set_pdnargs_raw( n, p );
    ;;; p must not be a protected closure, must be in the heap.
    if p.isprocedure and p.isinheap and p.isclosure /== 1 then
        if n.isword then 0 else n endif -> pdnargs( p )
    endif
enddefine;

define set_pdnargs_mode( n, p, mode );
    if mode == CALL_MODE then
        set_pdnargs_raw( n, p )
    elseif mode == UCALL_MODE then
        set_pdnargs_raw( n, p.updater )
    else
        internal_error()        
    endif
enddefine;

;;; allow definition and redefinition of protected identifiers

include vm_flags;

define :define_form protected;
    lvars protected_id = false;
    dlvars procedure save_pop11_define_declare = pop11_define_declare;
    define dlocal pop11_define_declare(id, globl_p, decl_p, idprops);
        dlocal pop_vm_flags = pop_vm_flags || VM_NOPROT_PVARS;
        save_pop11_define_declare(id, globl_p, decl_p, idprops);
        ;;; remember name defined
        id -> protected_id;
        ;;; disable for nested defines
        save_pop11_define_declare -> pop11_define_declare;
    enddefine;
    nonsyntax define();
    if protected_id
    and isident(sys_current_ident(protected_id)) == "perm"
    then
        sysprotect(protected_id);
    endif;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 28 1995
        Added revapplist
--- Robert John Duncan, Oct 26 1995
        Moved compile_w*rapper to wrappers.p. Added sysCLOSURE and
        define_protected. Fixed exprcomp to allow for lblocks.
--- Robert John Duncan, Oct  4 1995
        Popc changes
--- Steve Leach, 7th March 2003
        Changed set_pdnargs to set_pdnargs_mode.  Now takes a mode
        parameter and hence sets the arity of the updater appropriately.
        Added isclosure check for protected closures, too.
 */
