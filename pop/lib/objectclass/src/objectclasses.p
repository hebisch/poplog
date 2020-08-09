/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/objectclasses.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- OBJECTCLASSES --------------------------------------------------------
;;; Objectclasses are either special KEYS or MIXINS.  Both of these are
;;; special by virtue of their entries in the fields_of_class and
;;; supers_of_class tables.

compile_mode :pop11 +strict;

section $-objectclass =>
    apply_instance
    isclass
    ismixinclass
    isobjectclass
    issingleton
    issingletonclass
    mixinclass_key
    class_name
    name_of_object
    pop_oc_print_level
    pop_oc_print_loop_check
    print_instance
    singleton_key
    singletonclass_key
    sys_print_instance
    fast_add_superclass
    =_instance
    hash_instance
;

;;; Forward declaration:
;;;     Unlink all the isa-predicates.
constant procedure unlink_all_isas;


;;; -- mixins & singletonclasses -----------------------------------------------

sysunprotect( "ismixinclass" );
;;; Objectclasses are either mixins or (specific) keys.
defclass mixinclass {
    mixinclass_name : full
};
sysprotect( "ismixinclass" );
;;;
;;; mixins should be unique
nonop == -> class_=(mixinclass_key);

define issingletonclass_table =
    newproperty( [], 20, false, "tmparg" )
enddefine;

sysunprotect( "issingletonclass" );
define global issingletonclass( x ); lvars x;
    issingletonclass_table( x )
enddefine;
sysprotect( "issingletonclass" );

define global issingleton( x ); lvars x;
    x.datakey.issingletonclass_table
enddefine;

define Initialise_singletonclass( sc ); lvars sc;
    lvars item = do_class_new( sc, false )();
    item -> issingletonclass_table( sc );
    item -> class_example( sc );
enddefine;


;;; -- isclass/isobjectclass ----------------------------------------------
;;; Determine whether an object (typically a key) is a class or objectclass
;;; (i.e, excluding mixins and singletons). In this case (unlike methods)
;;; it is proper to overload the role of the fields_of_class table.  Every
;;; objectclass (key or mixin) must have a non-false entry in this table.
;;; There is no point in wasting space duplicating this meaning.

define :protected isobjectclass(key);
    key.iskey and               ;;; exclude mixins
    key.fields_of_class and
    true                        ;;; convert to boolean
enddefine;

define checkr_objectclass(item) -> item;
    unless isobjectclass(item) then
        mishap('ocon: OBJECTCLASS NEEDED', [^item]);
    endunless;
enddefine;

define :protected isclass( key );
    key.ismixinclass and "mixin" or
    key.issingletonclass and "singleton" or
    key.fields_of_class and "object"
enddefine;

define check_class(item);
    unless isclass(item) then
        mishap('occn: CLASS NEEDED', [^item]);
    endunless;
enddefine;


;;; -- class_name --------------------------------------------------

define :protected class_name( c ); lvars c;
    if c.iskey then
        class_dataword( c )
    elseif c.ismixinclass then
        c.mixinclass_name
    else
        check_class( c );
    endif;
enddefine;


;;; -- The Generic Class Methods --------------------------------------------

;;; This is a vector of pairs.  This gives a good compromise in space
;;; of data vs. space consumed by the interpreting procedure Set_generics.
vars generic_methods = {};

define lconstant Add_generic(name, proc);
    ;;; when called from Popc we must get the Popc-generated value of
    ;;; the name: this dlocal affects the call to sys_current_val
    dlocal pop_pas_mode = OBJECTCLASS_IN_POPC;
    lvars p = conspair(sys_current_val(name), proc);
    {^p ^^generic_methods} -> generic_methods;
enddefine;

define lconstant Set_generics( class ); lvars class;
    appdata(
        generic_methods,
        procedure( m ); lvars m;
            front( m ) -> back( m )( class )
        endprocedure
    )
enddefine;


;;; -- Printing Objectclasses -----------------------------------------------

#_IF DEF OBJECTCLASS_IN_POPC

define :method print_instance( x ); lvars x;
    ;;; do minimal printing: we won't have access to fields_of_class,
    ;;; etc., at run-time but we don't want to use raw field accessors
    ;;; because they may subvert any access wrappers
    cucharout( `<` ), pr( class_dataword(datakey(x)) ), cucharout( `>` );
enddefine;
;;;
Add_generic("print_instance", class_print);

#_ELSE

;;; This section has been significantly revised to cope with
;;;     1.  pop_pr_level
;;;     2.  pop_oc_print_level
;;;     3.  infinite print loops
;;;
define :method print_instance( x ); lvars x;
    lvars k = datakey( x );
    cucharout( `<` );
    pr( class_name( k ) );
    lvars f, n = 0;
    for f in fields_of_class( k ) do
        lvars v = class_access( n + 1 ->> n, k )( x );
        lvars m = xslot_identity( f );
        cucharout( ` ` );
        if m.isprocedure then
            pr( recursive_front( pdprops( m ) ) );
        else
            pr( m );
        endif;
        cucharout( `:` );
        pr( v );
    endfor;
    cucharout( `>` );
enddefine;

;;; Initially, -pop_oc_print_level- places no restrictions on
;;; the depth of printing.  User-defineable: should be an integer or
;;; false.
;;;
global vars pop_oc_print_level = false;

;;; This tells us whether or not to look for infinite loops.
;;; Note that we cannot avoid First_sys_print_instance because if the
;;; programmer messes around with pop_oc_print_loop_check at
;;; run-time we would end up hacking the value of -sys_print_instance_seen-
;;; and corrupting the entire system.
;;;
global vars pop_oc_print_loop_check = true;

;;; This variable is used to inhibit infinite print loops.
vars sys_print_instance_seen = false;

define Rest_sys_print_instance( x ); lvars x;
    dlocal pop_oc_print_level;
    if
        pop_pr_level <= 0 or
        pop_oc_print_level and
        pop_oc_print_level <= 0
    then
        cucharout( `<` );
        pr( class_name( x.datakey ) );
        appdata( ' ...>', cucharout )
    elseunless pop_oc_print_loop_check then
        if pop_oc_print_level then
            pop_oc_print_level - 1 -> pop_oc_print_level;
        endif;
        chain( x, print_instance )
    elseif fast_lmember( x, sys_print_instance_seen ) then
        appdata( '(LOOP <', cucharout );
        pr( class_name( x.datakey ) );
        appdata( '>)', cucharout );
    else
        conspair( x, sys_print_instance_seen ) -> sys_print_instance_seen;
        if pop_oc_print_level then
            pop_oc_print_level - 1 -> pop_oc_print_level;
        endif;
        chain( x, print_instance )
    endif
enddefine;

lvars was_suspend;
define First_sys_print_instance( x ); lvars x;
    dlocal was_suspend = false;
    dlocal 0 % detect_suspend( dlocal_context, ident was_suspend ) %;
    dlocal sys_print_instance_seen = [];
    Rest_sys_print_instance( x );
    unless was_suspend do
        ;;; If there has been no suspend it is safe to deallocate the
        ;;; seen-list.
        sys_grbg_list( sys_print_instance_seen );
    endunless;
enddefine;

define :protected sys_print_instance( x ); lvars x;
    chain(
        x,
        if sys_print_instance_seen then
            Rest_sys_print_instance
        else
            First_sys_print_instance
        endif
    )
enddefine;

Add_generic("sys_print_instance", class_print);

#_ENDIF


;;; -- A default apply_instance method --------------------------------------

define :method apply_instance( x ); lvars x;
    mishap( 'ocai: APPLY_INSTANCE NOT DEFINED FOR THIS ITEM', [^x] )
enddefine;
;;;
Add_generic("apply_instance", class_apply);


;;; -- Default Equality method ----------------------------------------------

define :method =_instance( x, y ); lvars x, y;
    sys_=( x, y )
enddefine;
;;;
Add_generic("=_instance", class_=);


;;; -- Default Hashing Method -----------------------------------------------

    ;;; rely on the fact that all record keys use the same hashing
    ;;; algorithm
lconstant procedure Record_hash = class_hash(conskey("dummy", []));

define :method hash_instance( x ); lvars x;
    Record_hash( x )
enddefine;
;;;
Add_generic("hash_instance", class_hash);


;;; -- Class Creators -------------------------------------------------------

define lconstant procedure Make_key( name, slot_names, specs, attribs );
    lvars name, slot_names, specs, attribs;
    lvars k = conskey( name, specs, attribs );
    k -> key_of_dataword(name);
    name.derive_construct_name -> pdprops(class_cons(k));
    name.derive_dest_name -> pdprops(class_dest(k));
    lvars n, i;
    for n, i in slot_names, [% k.explode %] do
        n -> pdprops( i )
    endfor;
    return( k );
enddefine;

define lconstant procedure Unlink_and_check( supers, field_list, extern_posn ) -> specs; lvars field_list, specs, extern_posn, supers;
    ;;; Ensure the field list consists of valid field specs.
    lvars specs = maplist( field_list, xslot_field_spec );
    applist( specs, check_spec );

    if extern_posn then
        [%
            lvars i, n = 0;
            for i in specs do
                n + 1 -> n;
                if n == extern_posn then
                    ">->"
                endif;
                i
            endfor;
        %] -> specs;
        if 0 >= extern_posn or n < extern_posn then
            mishap( '>-> (external position) out of range', [] )
        endif
    endif;

    unlink_all_methods_of_supers( supers );
    unlink_all_isas();
enddefine;

;;; check that the inheritance order can be resolved
define lconstant Check_inheritance(class);
    class_precedence_list(class) -> ;
enddefine;

define lconstant procedure Set_class( class, supers, field_list, wrap_list, extern_posn ); lvars class, supers, field_list, wrap_list, extern_posn;
    extern_posn -> class_extern_posn( class );
    field_list -> fields_of_class( class );
    supers -> supers_of_class( class );
    ;;; ensure that the combination of supers doesn't introduce ambiguity
    Check_inheritance(class);
    ;;; ... before we add links back from supers to this new class
    lvars s;
    for s in supers do
        add_inf( class, s )
    endfor;

    ;;; Note that we cut off as soon as the wrap-list is exhausted.
    lvars v, w;
    for v, w in wrap_list, wrapper_types do
        v -> w(class);
    endfor;
enddefine;

define lconstant procedure Field_names( field_list ); lvars field_list;
    maplist( field_list, xslot_name )
enddefine;

;;;
;;; For createobjectclass, a field_list is a list of fields, where a
;;; field is one of two things.
;;;
;;;     (a) a slot with 4 fields
;;;         1.  method
;;;         2.  a key spec
;;;         3.  default value
;;;         4.  default proc
;;; OR  (b) a class field of the form
;;;         1.  class (key, mixin, singletonclass)
;;;
;;; For subsequent class-creators, namely newobjectclass, newmixinclass,
;;; newsingletonclass, and newextantclass, the type-(b) fields are
;;; flattened out to become type-(a).
;;;

define newobjectclass( name, supers, field_list, wrap_list, attribs, extern_posn ) -> class; lvars name, supers, field_list, wrap_list, attribs, class, extern_posn;
    lvars specs = Unlink_and_check( supers, field_list, extern_posn );
    lvars names = Field_names( field_list );
    Make_key( name, names, specs, attribs ) -> class;
    Set_generics( class );
    Set_class( class, supers, field_list, wrap_list, extern_posn );
enddefine;

define newmixinclass( name, supers, field_list, wrap_list, attribs, extern_posn ) -> class; lvars name, supers, field_list, wrap_list, attribs, class, extern_posn;
    Unlink_and_check( supers, field_list, extern_posn ).erase;
    consmixinclass( name ) -> class;
    Set_class( class, supers, field_list, wrap_list, extern_posn );
    ;;; It is not necessary to update the slot methods for mixins, since
    ;;; their slots can never be instantiated.
enddefine;

define newsingletonclass( name, supers, field_list, wrap_list, attribs, extern_posn ) -> class; lvars name, supers, field_list, wrap_list, attribs, class, extern_posn;
    lvars specs = Unlink_and_check( supers, field_list, extern_posn );
    lvars names = Field_names( field_list );
    ;;; Works for all other objectclasses.
    Make_key( name, names, specs, attribs ) -> class;
    Set_generics( class );
    Set_class( class, supers, field_list, wrap_list, extern_posn );
    Initialise_singletonclass( class );
enddefine;

define newextantclass( class, supers, field_list, wrap_list, attribs, extern_posn ) -> class; lvars class, supers, field_list, wrap_list, attribs, class, extern_posn;
    lvars specs = Unlink_and_check( supers, field_list, extern_posn );

    lvars cspecs = class_field_spec( class );
    unless
        cspecs = specs or
        not( cspecs ) or
        specs == [] and not( cspecs.islist )    ;;; allow vectorclasses -- ianr's suggestion.
    do
        mishap(
            'EXTANTCLASS: incompatible specs',
            [found ^specs expecting ^cspecs]
        )
    endunless;

    Set_class( class, supers, field_list, wrap_list, extern_posn );
enddefine;


;;; -- Class Adoption -------------------------------------------------------

;;; The ORPHAN gets given a foster parent -- provided that the candidate
;;; parent can pass a simple test.  It may add any new slots to the child
;;; class.
;;;
define global fast_add_superclass( foster_parent, orphan ); lvars foster_parent, orphan;
    check_class(foster_parent);
    check_class(orphan);
    if lmember( orphan, supers_of_class( foster_parent ) ) then
        mishap( 'ADOPTION WOULD CREATE INHERITANCE LOOP', [^foster_parent ^orphan] )
    endif;
    lvars fp_slots = maplist( fields_of_class( foster_parent ), xslot_identity );
    lvars or_slots = maplist( fields_of_class( orphan ), xslot_identity );
    lvars s;
    for s in fp_slots do
        unless lmember( s, or_slots ) do
            mishap( 'ocipca: INVALID PARENT FOR CLASS ADOPTION', [^foster_parent ^orphan] )
        endunless;
    endfor;
    lvars os = supers_of_class( orphan );
    unless lmember( foster_parent, os ) do
        [^^os ^foster_parent] -> supers_of_class( orphan );
        add_inf( orphan, foster_parent );
        ;;; changing inheritance means recomputing the class precedence list
        clear_class_precedence_list( orphan );

        lvars cw, need_to_relink_constructors = false;
        for cw in wrapper_types do
            unless all_wrappers( foster_parent, cw ).null do
                true -> need_to_relink_constructors;
                quitloop;
            endunless
        endfor;

        if need_to_relink_constructors then
            ;;; This is a very indicriminate form of relinking.
            relink_all_class_construct();
            relink_all_class_new();
        endif;

        unlink_all_methods();
        unlink_all_isas();
    endunless;
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- Robert Duncan, Jun 12 1996
        Changed Make_key to set the pdrops of class cons & dest procedures
--- Robert John Duncan, Oct 27 1995
        Changed to use new define_protected for protected identifiers.
--- Robert John Duncan, Oct 26 1995
        Changes for new wrappers. Made mixinclass_key use == for equality
        to make mixins unique.
--- Robert John Duncan, Oct 13 1995
        Added check_class, checkr_objectclass
--- Robert John Duncan, Oct 12 1995
        Added a check that the inheritance order (class precedence list)
        can be resolved for a new class. Changed fast_add_superclass to
        clear the existing class precedence list for the adopted class.
--- Robert John Duncan, Oct  4 1995
        Popc changes
--- Robert John Duncan, May 26 1995
        Exported =_instance and hash_instance. Corrected the definition
        of hash_instance to use Record_hash as the default.
--- Integral Solutions Ltd, Sep  1 1994 (Julian Clinton)
        Added SFK's fix to make ObjectClass work with key_of_dataword.
;;; -------------------------------------------------------------------------
;;; Modified, 01/07/93, sfk
;;;     *   Added -fast_add_superclass- to support class-adoption.
;;; -------------------------------------------------------------------------
;;; Modified, 16/04/93, sfk
;;;     *   Removed an erroneous sys_grbg_list from print_object.  In the
;;;         process I stopped it allocating any store, so it is now
;;;         garbage-free.
;;;     *   Arranged for sys_print_object to reclaim the store in almost
;;;         all circumstances.  The only situation in which it can't is
;;;         when there is a suspend (or consproc_to) between the store
;;;         allocation and deallocation.  This is automatically detected.
;;; -------------------------------------------------------------------------
;;; Modified, 15/04/93, sfk
;;;     *   Printing of objectclasses has been changed to respect
;;;         -pop_pr_level-, a new variable -pop_oc_print_level-,
;;;         and to inhibit infinite print loops.
;;;     *   Added explicit protections to exported identfiers.
;;; -------------------------------------------------------------------------
;;; Modified, 10/12/92, sfk
;;;     *   Singletonclasses completely revised to be in line with
;;;         objectclasses.
;;;     *   Changed defective error message for class_name.
;;;     *   Allowed class_name to work on all keys.
;;; -------------------------------------------------------------------------
;;; Modified, 8/10/92, sfk
;;;     *   Incorrect test for vectors in extantclass code corrected.
;;; -------------------------------------------------------------------------
;;; Modified, 22/10/92, sfk
;;;     *   Eliminated use of "initialise" for singletons.
;;; -------------------------------------------------------------------------
 */
