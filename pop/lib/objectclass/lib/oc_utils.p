/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/lib/oc_utils.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

;;; A collection of general utilities used by autoloadable procedures
;;; that aren't part of the core ObjectClass system.  Furthermore they
;;; aren't exported to the user level (otherwise they could have their
;;; own library).

section $-objectclass;

#_IF not(DEF pop_max_int)
uses int_parameters;
#_ENDIF

define sort_by( L, field, cmp ); lvars L, procedure field, procedure cmp;
    syssort(
        L,
        procedure( x, y ); lvars x, y;
            cmp( field( x ), field( y ) )
        endprocedure
    )
enddefine;

define app_all_classes( p ); lvars p;
    applist( property_domain( fields_of_class ), p )
enddefine;

;;; This is the obvious numbering scheme -- a class C is given a number
;;; greater than the numbers of its parents.  This definition is
;;; neatly recursive and this is reflected in the implementation below.
;;;
define get_hierarchy_numbering() -> table;
    lvars table =
        newanyproperty(
            [], 64, 1, false,
            false, false, "perm",
            false,
            procedure( k, p ); lvars k, p;
                applist( 0, supers_of_class( k ), table <> max ) + 1 ->> p( k )
            endprocedure
        );
enddefine;

define inferiors( c ); lvars c;
    lvars infs = infs_of_class( c );
    infs and property_domain( infs ) or []
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
