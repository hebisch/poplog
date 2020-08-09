/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/class_attributes.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
;;; -- Return Sorted List of Attributes of a Class --------------------------

compile_mode :pop11 +strict;

section $-objectclass;

#_IF pop_internal_version < 142000

lconstant attrs =
    syssort(
        [ external_deref external_ptr external_ptr_props writeable prop_entry ],
        alphabefore
    );

define class_attributes( k ); lvars k;
    k.iskey and
    maplist(
        attrs,
        procedure( a ); lvars a;
            class_attribute( k, a )
        endprocedure
    ) or
    []
enddefine;

#_ELSE

;;; MUCH more elegant in version 14.2.
define class_attributes( k ); lvars k;
    k.iskey and syssort( k.class_attribute, alphabefore ) or []
enddefine;

#_ENDIF

endsection;

;;; -------------------------------------------------------------------------
;;; Modified, 3/12/92, sfk & jonm
;;;     *   More elegant definition of -class_attributes- available
;;;         in version 14.2 onwards.
;;; -------------------------------------------------------------------------
