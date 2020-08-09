/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/method_table.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Method Table ---------------------------------------------------------
;;;
;;; The method table is where methods keep the information about
;;; their parts.  Methods are linked from this information.
;;;

compile_mode :pop11 +strict;

section $-objectclass;

;;; -- Parts and Sorting Them -> Inheritance --------------------------------

;;; Every entry in the method table is a collection of classes, represented
;;; as a temporary vector, and a procedure.
defclass Entry [writeable] {
    classesEntry    : full,     ;;; tmpvec of classes
    actionEntry     : full,     ;;; a procedure
};

define_lconstant_procedure LockEntry( e ); lvars e;
    e.classesEntry.locktmpvec
enddefine;

define_lconstant_procedure UnlockEntry( e ); lvars e;
    e.classesEntry.unlocktmpvec
enddefine;


;;; -- Entry Tables ---------------------------------------------------------

lconstant procedure List_check = identfn(% () %);
procedure( L ); lvars L;
    if L.islist then
        L
    else
        internal_fault( 'method table entries not a list' )
    endif;
endprocedure -> List_check.updater;

lconstant procedure Arity_check = identfn(% () %);
procedure( A ); lvars A;
    if A == "variadic" or A == "unbound" or A.isinteger then
        A
    else
        internal_fault( 'arity of table invalid' )
    endif;
endprocedure -> Arity_check.updater;

defclass MethodTable [writeable] {
    cArityMethodTable   : full # Arity_check,
    cEntriesMethodTable : full # List_check,    ;;; [ Entry( tmpvec( class|false ), proc ) ]
    uArityMethodTable   : full # Arity_check,
    uEntriesMethodTable : full # List_check     ;;; --- ditto ----
};

constant fieldsOfMethodTable = [^cEntriesMethodTable ^uEntriesMethodTable];

define newMethodTable();
    consMethodTable( "unbound", [], "unbound", [] )
enddefine;

;;; Lock the entries of the method table.  Deleted any melted entries.
;;;
define lockMethodTable( MT ); lvars MT;
    lvars e, f;
    for f in fieldsOfMethodTable do
        [%
            for e in f( MT ) do
                if LockEntry( e ) then
                    e                   ;;; hasn't melted yet -> keep
                endif
            endfor;
        %] -> f( MT )
    endfor
enddefine;

define unlockMethodTable( MT ); lvars MT;
    applist( cEntriesMethodTable( MT ), UnlockEntry );
    applist( uEntriesMethodTable( MT ), UnlockEntry );
enddefine;

;;; -- Method Table ---------------------------------------------------------

;;; Given a method, return the method table with which they are associated.
;;;
constant procedure method_table =
    newanyproperty(
        [], 64, 1, false,
        false, false, "tmparg",
        false, false
    );


;;; Allocate a method table for m if necessary, then return its method table.
;;;
define method_entry( path, table, procedure field ); lvars path, table, procedure field;
    lvars result = false;

    lockMethodTable( table );

    lvars p;
    for p in field( table ) do
        lvars d = classes_differ( p.classesEntry, path );
        unless d then
            p -> result;
            quitloop;
        endunless;
    endfor;

    unlockMethodTable( table );
    return( result );
enddefine;

define truncate_path( path ) -> path; lvars path;
    repeat
        lvars N = length( check_datakey( path, vector_key ) );
        quitif( N == 0 );
        quitif( path( N ) );
        {% path.explode.erase %} -> path;
    endrepeat;
enddefine;

define updateMethodTable( action, mode, path, table ); lvars action, mode, path, table;
    lvars field = check_mode( mode ) == UCALL_MODE and uEntriesMethodTable or cEntriesMethodTable;
    path.truncate_path -> path;
    if action then
        lvars e = method_entry( path, table, field );
        if e then
            action -> actionEntry( e );
        else
            lvars classes = constmpvec(#| path.explode |#);
            consEntry( classes, action ) :: field( table ) -> field( table );
        endif;
    else
        ;;; Remove entry from method table.
        lockMethodTable( table );
        lvars classes = path.destlist.consvector;
        [%
            lvars p;
            for p in field( table ) do
                if classes_differ( p.classesEntry, classes ) do
                    p
                endif
            endfor;
        %] -> field( table );
        unlockMethodTable( table );
    endif;
enddefine;


;;; -- Iterating over the classes refered to by a method --------------------

define any_class_of_method_satisfies( M, pred ); lvars M, pred;
    lvars mt = method_table( M );
    lvars f;
    for f in fieldsOfMethodTable do
        lvars ents = f( mt );
        lvars e;
        for e in ents do
            lvars c;
            for c in [% explode_tmpvec( classesEntry( e ) ) %] do
                unless c.isundef do
                    returnif( pred( c ) )( true )
                endunless;
            endfor;
        endfor;
    endfor;
    return( false )
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, May 26 1995
        Added [writeable] attribute to class definitions
;;; -------------------------------------------------------------------------
;;; Modified, 11/04/93, sfk
;;;     *   Enhanced updateMethodTable to be able to remove entries
;;;         for a given path.
;;; -------------------------------------------------------------------------
;;; Modified, 10/12/92, sfk
;;;     *   Added upgrade_method to cope with pop_oc_sensitive_methods
;;; -------------------------------------------------------------------------
 */
