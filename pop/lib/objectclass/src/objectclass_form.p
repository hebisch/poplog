/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/objectclass_form.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- objectclass_form -----------------------------------------------------
;;; This defines the basis for the define forms for objectclasses,
;;; namely
;;;     define :objectclass
;;;     define :mixinclass
;;;     define :singletonclass
;;;     define :extantclass
;;;

section $-objectclass;


;;; -- Inheritance check ----------------------------------------------------

;;; Although it is not possible to construct a loop in the hierarchy, it is
;;; possible to TRY to do so.  This procedure, invoked by objectclass_form,
;;; prints out a warning in that situation.
define Check_self_inheritance( newclass, oldclass ); lvars newclass, oldclass;
    lvars c;
    for c in all_supers_of_class( newclass ) do
        if c == newclass then
            ;;; this can arise during the construction of extantclasses.
            mishap( 'OBJECTCLASS: trying to build inheritance loop', [^newclass] )
        elseif c == oldclass then
            warning( 'OBJECTCLASS: inheriting from old-version of class', [^newclass] )
        endif;
    endfor;
enddefine;

;;; Returns a reason for why classes New and Old are different or false if Old
;;; is as good as New.  This is used in order to decide whether or not to
;;; replace the old class Old by the newly created New.
;;;
;;; We would like to reuse Old provided that it has the right properties.
;;; This is determined by New and Old being both mixins or both objectclasses,
;;; having the same names, having the same EXPANDED set of fields (doesn't
;;; matter if they weren't the same before expansion), and the same set of
;;; supers, AND having the same class attributes!
;;;
define Differ_classes( New, Old ); lvars New, Old;
    if not( pop_oc_reuse ) then
        'not reusing classes'
    elseunless Old.isclass then
        'creating it'
    elseunless datakey( New ) == datakey( Old ) do
        'class-types differ'
    elseunless class_name( New ) == class_name( Old ) do
        'names differ'
    elseunless fields_of_class( Old ) = fields_of_class( New ) then
        'fields differ'
    elseunless supers_of_class( Old ) = supers_of_class( New ) then
        'supers differ'
    elseunless class_attributes( Old ) = class_attributes( New ) then
        'attributes differ'
    elseunless class_cons_wrapper( Old ) = class_cons_wrapper( New ) then
        'cons-wrappers differ'
    elseunless class_new_wrapper( Old ) = class_new_wrapper( New ) then
        'new-wrappers differ'
    elseunless class_access_wrapper( Old ) = class_access_wrapper( New ) then
        'access-wrappers differ'
    elseunless class_destroy_wrapper( Old ) = class_destroy_wrapper( New ) then
        'destroy-wrappers differ'
    else
        false
    endif
enddefine;



;;; -- Creating the class ---------------------------------------------------

;;; Should we replace the old objectclass with the newly constructed one?
;;; If the fields & supers are identical, we might as well reuse the old one.
define Replace_old_with_new( newclass, oldclass ); lvars newclass, oldclass;
    lvars reason = Differ_classes( newclass, oldclass );
    if reason then
        true -> oldclass.isobsolete;
        newclass
    else
        oldclass
    endif;
    if trace_matches( "replace" ) then
        if dup() == oldclass then
            printf( ';;; Keeping old key for class %p\n', [% oldclass %] )
        else
            printf(
                ';;; Replacing key for class %p because %p\n',
                [% oldclass, reason %]
            )
        endif
    endif;
    if
        reason and
        oldclass.isclass and
        pop_oc_sensitive_methods
    then
        newclass -> upgrade_table( oldclass );
        unless all_classed_methods_are_unlinked do
            upgrade_methods( true )
        elseif trace_matches( "upgrade" ) then
            printf( ';;; Delaying upgrade of %p\n', [^newclass] )
        endunless;
    endif;
enddefine;


;;; Canonise_fields removes later duplicates from the list of
;;; fields.
;;;
define Canonise_fields( list ); lvars list;
    lvars extern_posn = false;
    lvars procedure seen = newproperty( [], 20, false, "perm" );
    lvars procedure default_isset = newproperty( [], 20, false, "perm" );
    [%
        lvars s, posn = 0;
        for s in list do
            if s.iscslot then
                lvars si = xslot_identity( s );
                lvars x;
                if seen( si ) ->> x then
                    unless default_isset( si ) do
                        s.xslot_default_value -> x.xslot_default_value;
                        s.xslot_field_spec -> x.xslot_field_spec;
                        s.xslot_default_proc -> x.xslot_default_proc;
                    endunless
                else
                    s.copy ->> seen( si );
                    posn + 1 -> posn;
                endif;
                true -> default_isset( si );
            elseif s.isclass then
                lvars class = s;
                lvars t;
                for t in class.fields_of_class do
                    lvars ti = xslot_identity( t );
                    unless seen( ti ) do
                        t.copy ->> seen( ti );
                        posn + 1 -> posn;
                    endunless;
                endfor;
                ;;; The external position is sensitive to inheritance.
                extern_posn or class.class_extern_posn -> extern_posn;
            elseif s == ">->" then
                ;;; The next position is the external position.
                posn + 1 -> extern_posn;
            else
                internal_error()
            endif
        endfor;
    %];
    extern_posn;
enddefine;

;;; Used in lib declare_objectclass, worst luck ...
;;;
define makeobjectclass( name, fields, class_type, attribs, wrap_list, oldclass ) -> class;
    lvars name, fields, attribs, class, wrap_list, supers, class_type, oldclass;

    lvars supers = filterin( fields, isclass );
    lvars ( field_list, extern_posn ) = fields.Canonise_fields;

    if class_type == "extantclass" then
        newextantclass( oldclass, supers, field_list, wrap_list, attribs, extern_posn )
    elseif class_type == "objectclass" then
        newobjectclass( name, supers, field_list, wrap_list, attribs, extern_posn )
    elseif class_type == "singletonclass" then
        newsingletonclass( name, supers, field_list, wrap_list, attribs, extern_posn )
    elseif class_type == "mixinclass" then
        newmixinclass( name, supers, field_list, wrap_list, attribs, extern_posn )
    else
        mishap( 'INVALID CLASS TYPE', [^class_type] )
    endif -> class;

    lvars f, m;
    for f in fields do
        if f.iscslot and ( f.xslot_identity ->> m ).isgeneric then
            update_method( f, CALL_MODE, [^class], m );
        endif;
    endfor;
enddefine;

define makeobjectclass_with_checking( name, fields, class_type, attribs, wrap_list, oldclass ); lvars oldclass, name, fields, attribs, wrap_list, class_type;
    lvars newclass = makeobjectclass( name, fields, class_type, attribs, wrap_list, oldclass );

    Check_self_inheritance( newclass, oldclass );   ;;; Check circularity.

    if class_type == "extantclass" then
        oldclass
    else
        Replace_old_with_new( newclass, oldclass )
    endif
enddefine;


;;; -- Create Class Value ---------------------------------------------------

define create_class_value( name, attribs, fields, class_type ); lvars name, attribs, fields, class_type;

    define lconstant push_fields();
        [%  lvars f;
            for f in fields do
                lvars t = f(1);
                if t == "SLOT" then
                    lvars identity = currentval( f(3) );
                    lvars defproclist = f(6);
                    lvars defproc =
                        defproclist and compile_defproc( f(3), f(6) );
                    newcslot( identity, f(4), f(5), defproc );
                elseif t == "ISA" then
                    lvars k = currentval( f(2) );
                    if k.isclass then
                        k
                    else
                        mishap( 'VARIABLE NOT BOUND TO CLASS', [% f(2) %] )
                    endif
                elseif t == ">->" then
                    t
                endif
            endfor;
        %];
    enddefine;

    define lconstant push_wrap_list();
        [%  lvars w;
            for w in wrapper_names do
                lvars i, seen = false;
                for i in fields do
                    if i(1) == w then
                        if seen then
                            mishap( 'REPEATED WRAPPER DEFINITION', [^w] )
                        else
                            true -> seen;
                            compile_wrapper( explode(i) )
                        endif
                    endif;
                endfor;
                unless seen do
                    false
                endunless
            endfor;
        %];
    enddefine;

    makeobjectclass_with_checking(
        name,
        push_fields(),
        class_type,
        attribs,
        push_wrap_list(),
        currentval( derive_key_name( name ) )
    )
enddefine;


;;; -- Define the Variables Associated with the Class -----------------------

define assign_class_slots( class, fields );
    lvars f;
    for f in fields do
        if f(1) == "SHARED_SLOT" then
            lvars (name, defval, defproc) = (f(3), f(4), f(5));
            if defproc then compile_defproc(name, defproc) -> defproc endif;
            update_method(
                make_shared_slot(name, defproc, defval),
                CALL_MODE, [^class], currentval(name));
        endif
    endfor;
enddefine;

define assign_adoptions( class, fields ); lvars class, fields;
    lvars f;
    for f in fields do
        if f(1) == "ADOPTING" then
            lvars orphan = f(2).currentval;
            fast_add_superclass( class, orphan );
        endif;
    endfor;
enddefine;

define assign_ako( class, name ); lvars class, name;
    lvars ako_name = derive_ako_name( name );
    lvars proc = class_isa( class );
    if isprotected( ako_name ) then
        sysunprotect( ako_name );
        sysPASSIGN( proc, ako_name );
        sysprotect( ako_name );
    else
        sysPASSIGN( proc, ako_name )
    endif;
enddefine;

define assign_class_form( class, name, fields, class_type ); lvars class, name, fields, class_type;

    ;;; Assign the key.
    unless class_type == "extantclass" then
        sysPASSIGN( class, derive_key_name( name ) )
    endunless;

    assign_class_slots( class, fields );

    ;;; Declare the AKO recogniser.
    assign_ako( class, name );

    ;;; Declare the associated procedures.
    if class_type == "singletonclass" then
        sysPASSIGN( class_example( class ), name )
    elseunless class_type == "mixinclass" then
        if class_type == "extantclass" then
            if class.isrecordkey then
                sysPASSIGN( class.class_new, name.derive_new_name );
            endif
        else
            sysPASSIGN( class.class_construct, name.derive_construct_name );
            sysPASSIGN( class.class_dest, name.derive_dest_name );
            sysPASSIGN( class.class_new, name.derive_new_name );
        endif
    endif;

    ;;; Perform any adoptions.
    assign_adoptions( class, fields );
enddefine;


;;; -- Declaring Definitions ------------------------------------------------

define declare_ako( declare, name ); lvars declare, name;
    lvars ako_name = derive_ako_name( name );

    lvars prot = isprotected( ako_name );

    if prot then
        if trace_matches( "cancel" ) then
            printf( ';;; Unprotecting %p\n', [^ako_name] )
        endif;
        sysunprotect( ako_name )
    endif;

    if isconstant( ako_name ) then
        if trace_matches( "cancel" ) then
            printf( ';;; Cancelling %p\n', [^ako_name] )
        endif;
        syscancel( ako_name )
    endif;

    declare( ako_name, "procedure" );

    if prot then
        sysprotect( ako_name )
    endif;
enddefine;


define declare_class_form( name, def_idspec, fields, class_type );
    lvars name, def_idspec, fields, class_type;

    lvars declare = identspec_declare( def_idspec );

    ;;; Declare the key.
    lvars keyname = derive_key_name( name );
    if class_type == "extantclass" then
        ;;; there should be an existing key
        lvars key = currentval( keyname );
        ;;; and we need to zap those properties that won't be
        ;;; automatically reset
        clear_infs( key );
        clear_class_precedence_list( key );
    else
        declare( keyname, 0 )
    endif;

    ;;; Bind all the slots to methods.
    lvars f;
    for f in fields do
        lvars t = f(1);
        if t == "SLOT" or t == "SHARED_SLOT" then
            bind_method( f(3), CALL_MODE )
        endif
    endfor;

    ;;; Declare the AKO recogniser.
    declare_ako( declare, name );

    ;;; Declare the associated procedures.
    if class_type == "singletonclass" then
        declare( name, 0 );
    elseunless class_type == "mixinclass" then
        if class_type == "extantclass" then
            lvars oldclass = currentval( derive_key_name( name ) );
            if oldclass.isrecordkey then
                declare( name.derive_new_name, "procedure" );
            endif
        else
            declare( name.derive_construct_name, "procedure" );
            declare( name.derive_dest_name, "procedure" );
            declare( name.derive_new_name, "procedure" );
        endif
    endif;
enddefine;


;;; -- Parsing Class Definitions -------------------------------------------

;;; Returns the default value and default procedure for a given slot.
;;;
define Get_defaults( fname, spec ); lvars fname, spec;

    define lconstant literal( x ); lvars x;
        x.isnumber or
        x.isstring
    enddefine;

    define lconstant literal_expr_coming();

        at_least_length( proglist, 2 ) and
        proglist(2) == ";" and
        literal( proglist( 1 ) ) or

        at_least_length( proglist, 4 ) and
        proglist(4) == ";" and
        proglist(1) == """ and proglist(3)

    enddefine;

    define lconstant Spec_default( spec ); lvars spec;
        spec == "full" and undef or
        spec == "exptr" and null_external_ptr or
        0;
    enddefine;

    if pop11_try_nextreaditem( "=" ) then
        if literal_expr_coming() then
            plant_and_execute( pop11_comp_expr );
            false
        else
            Spec_default( spec );
            exprread();
            ;;; pop11_need_nextreaditem( ";" ).erase;
        endif
    else
        if pop11_try_nextreaditem( "==" ) then
            plant_and_execute( pop11_comp_expr );
        else
            Spec_default( spec );
        endif;
        false
    endif
enddefine;

define Get_instance_slots( default_decl ); lvars default_decl;
    repeat
        lvars idspec = read_identspec( default_decl );
        lvars declare = identspec_declare( idspec );
        lvars fieldname = readitem();
        unless fieldname.isword do
            mishap( 'ocsn: INVALID NAME FOR SLOT', [^fieldname] )
        endunless;
        declare( fieldname, "procedure" );
        lvars spec = get_spec() or "full";
        lvars ( defval, defproc ) = Get_defaults( fieldname, spec );
        {SLOT ^idspec ^fieldname ^spec ^defval ^defproc};
        quitif( pop11_need_nextreaditem( [; ,] ) == ";" );
    endrepeat
enddefine;

define Get_wrappers();
    lvars t = pop11_need_nextreaditem( wrapper_names );
    lvars idecs = false, ilocals = false;
    if pop11_try_nextreaditem("(") then
        [] ->> idecs -> ilocals;
        until pop11_try_nextreaditem(")") do
            lvars (D, L) = read_local();
            pop11_try_nextreaditem(",").erase;
            [^^idecs ^D] -> idecs;
            [^^ilocals ^L] -> ilocals;
        enduntil;
    endif;
    pop11_need_nextreaditem( "do" ).erase;
    lvars expr = exprread();
    pop11_need_nextreaditem( ";" ).erase;
    {^t ^idecs ^ilocals ^expr}
enddefine;

define Get_shared_slots( default_decl ); lvars default_decl;
    repeat
        lvars idspec = read_identspec( default_decl );
        lvars decl = identspec_declare( idspec );
        lvars cslotname = readitem();
        unless cslotname.isword do
            mishap( 'ocsn: INVALID NAME FOR CLASS SLOT', [^cslotname] )
        endunless;
        decl( cslotname, "procedure" );
        lvars ( defval, defproc ) = Get_defaults( cslotname, "full" );

        {SHARED_SLOT ^idspec ^cslotname ^defval ^defproc };
        quitif( pop11_need_nextreaditem( [; ,] ) == ";" )

    endrepeat;
enddefine;

define procedure Get_isa( type ); lvars type;
    until pop11_try_nextreaditem( ";" ) do
        unless pop11_try_nextreaditem( "," ) do
            lvars supername = readitem();
            unless supername.isword do
                mishap( 'ocscn: SUPER-CLASS NAME NOT A WORD', [^supername] )
            endunless;
            {^type % derive_key_name( supername ) %}
        endunless;
    enduntil;
enddefine;

define Get_fields( ddecl, closer ); lvars ddecl, closer;
    lconstant isa_keyword = [isa is];
    [%
        until pop11_try_nextreaditem( closer ) do
            if pop11_try_nextreaditem( isa_keyword ) then
                Get_isa( "ISA" )
            elseif pop11_try_nextreaditem( "adopting" ) then
                Get_isa( "ADOPTING" )
            elseif pop11_try_nextreaditem( "shared_slot" ) then
                Get_shared_slots( ddecl )
            elseif pop11_try_nextreaditem( "on" ) then
                Get_wrappers()
            elseif pop11_try_nextreaditem( ">->" ) then
                { >-> }
            elseif pop11_try_nextreaditem( "slot" ) then
                Get_instance_slots( ddecl )
            else
                pop11_need_nextreaditem( "slot" ).erase;    ;;; raise correct error
                internal_error();
            endif;
        enduntil;
    %]
enddefine;


define Read_attributes();
    if proglist.null then
        mishap( 'ocicd: CLASS DEFINITION IS INCOMPLETE', [] )
    else
        lvars attribs, item = proglist.hd;
        if item == "[" then
            readitem().erase;
            pop11_exec_compile( nonsyntax [, false )
        elseif item.islist then
            readitem()          ;;; Will return ``item''! Sneaky!
        else
            []
        endif -> attribs;
        if pop_oc_writeable_default then
            unless pop_oc_writeable_default == "writeable"
            or pop_oc_writeable_default == "nonwriteable"
            then
                mishap('pop_oc_writeable_default: INVALID VALUE',
                    [^pop_oc_writeable_default]);
            elseunless fast_lmember("writeable", attribs)
            or fast_lmember("nonwriteable", attribs)
            then
                pop_oc_writeable_default :: attribs -> attribs;
            endunless;
        endif;
        consvector(destlist(attribs));
    endif;
enddefine;

;;; Optional semi-colon or mandatory bracket.
;;; This procedure is the syntactic constructor for new classes.  As such it
;;; is one of the most important syntax words in the system.
;;;
define objectclass_form( class_type, opening_keyword, closing_keyword ); lvars class_type, closing_keyword, opening_keyword;
    lvars def_idspec = read_identspec( default_default_idspec() );
    lvars name = readitem();
    lvars attribs = Read_attributes();
    read_optional_semi( opening_keyword );
    lvars fields = Get_fields( def_idspec, closing_keyword );
    declare_class_form( name, def_idspec, fields, class_type );
    lvars class = create_class_value( name, attribs, fields, class_type );
    assign_class_form( class, name, fields, class_type );
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Oct 26 1995
        Changed to support new wrapper syntax
--- Robert John Duncan, Oct 12 1995
        Changed declare_class_form to clear the class precedence list for
        an extant class
--- Robert John Duncan, Oct  9 1995
        Made Read_attributes sensitive to pop_oc_writeable_default
--- Robert John Duncan, Oct  4 1995
        Popc changes
--- Integral Solutions Ltd, Aug 30 1994 (Julian Clinton)
        Added Steve Knight's fix for field slot updates.
;;; -------------------------------------------------------------------------
;;; Modified, 8/10/93, JJC
;;;     *   Removed redundant code for supporting previous versions of
;;;         objectclass.
;;;     *   Removed pop_oc_v600_recognisers code (assign_recogniser
;;;         and declare_recogniser) run when this is false.
;;; -------------------------------------------------------------------------
;;; Modified, 21/09/93, JJC
;;;     *   Removed lconstant from Get_isa (needed by SIBAL until
;;;         proper interface to accessing slots provided).
;;; -------------------------------------------------------------------------
;;; Modified, 01/07/93, sfk
;;;     *   Moved some utility procedures & variables into readutils.p.
;;; -------------------------------------------------------------------------
;;; Modified, 04/06/93, sfk
;;;     *   Uses new (U)CALL_MODE values.
;;;     *   Allows use of ``is XXX'' rather than ``isa XXX'' inside
;;;         class definitions.
;;; -------------------------------------------------------------------------
;;; Modified, 30/5/93, sfk
;;;     *   Complete rewrite in order to implement lconstant classes.
;;; -------------------------------------------------------------------------
 */
