/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/slots.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Slots are enhanced field specs ---------------------------------------

compile_mode :pop11 +strict;

section $-objectclass;

section slots
    fields_of_class
    internal_error
=>
    newcslot            ;;; allocate a new caller-mode-slot
    newuslot            ;;; create an updater-mode-slot (based on caller-slot)
    isxslot             ;;; is either a caller-  or updater-mode-slot
    iscslot             ;;; is a caller-mode-slot
    isuslot             ;;; is an updater-mode-slot
    xslot_name          ;;; the name associated with a slot
    xslot_posn          ;;; position of slot in a class
    xslot_identity      ;;; identity of slot
    xslot_field_spec    ;;; the field-spec used by the slot in keys
    xslot_default_value ;;; the default value for newXXX
    xslot_default_proc  ;;; default procedure for newXXX
;

defclass cslot [writeable] {
    identity,
    field_spec,
    default_value,
    default_proc
};

identof( "conscslot" ) -> identof( "newcslot" );

defclass uslot {
    uslot_slot
};

identof( "consuslot" ) -> identof( "newuslot" );

define xslot( a ); lvars a;
    if a.iscslot then
        a
    elseif a.isuslot then
        a.uslot_slot
    else
        internal_error()
    endif;
enddefine;

vars procedure (
    xslot_identity      = xslot <> identity,
    xslot_field_spec    = xslot <> field_spec,
    xslot_default_proc  = xslot <> default_proc,
    xslot_default_value = xslot <> default_value
);

define xslot_name( x ); lvars x;
    lvars id = xslot_identity( x );
    while id.isprocedure do
        id.pdprops -> id
    endwhile;
    return( id )
enddefine;

define xslot_posn( c, slf ); lvars c, slf;
    slf.xslot -> slf;
    lvars s, n = 0;
    for s in fields_of_class( c ) do
        n + 1 -> n;
        if s.identity == slf.identity then
            return( n )
        endif
    endfor;
    internal_error();
enddefine;

define isxslot( x ); lvars x;
    x.iscslot or x.isuslot
enddefine;

endsection;

endsection;


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, May 26 1995
        Added [writeable] attribute to cslot class definition
;;; -------------------------------------------------------------------------
;;; Modified 15/11/92 -- sfk
;;;     *   Completely rewrote in order to change the names of the
;;;         exported identifiers.  I had made a number of daft errors based
;;;         on the confusingly similar names I had chosen.  I have also
;;;         hidden the "harmful" identifiers from the rest of the package.
;;;     *   Made the accessors work equally well for updater-mode-slots
;;;         as caller-mode-slots.  The point of this is that updater-mode-
;;;         slots no longer have to be dereferenced.  That in turn means
;;;         that I don't have to mess around with an update-mode
;;;         parameter in loads of silly places.
;;; -------------------------------------------------------------------------
;;; Modified 27/9/92 -- sfk
;;;     *   Altered the lvars "self" to "slf" because it clashes with
;;;         the flavours library.
;;; -------------------------------------------------------------------------
 */
