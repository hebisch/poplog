/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/objectclass/src/derive.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
section $-objectclass;

define derive_ako_name( name ); lvars name;
    "is" <> name;
enddefine;

define derive_recogniser_name( name ); lvars name;
    "is" <> name
enddefine;

define derive_key_name( name ); lvars name;
    name <> "_key"
enddefine;

define derive_new_name( name ); lvars name;
    "new" <> name
enddefine;

define derive_construct_name( name ); lvars name;
    "cons" <> name
enddefine;

define derive_dest_name( name ); lvars name;
    "dest" <> name
enddefine;


endsection;

;;; -------------------------------------------------------------------------
;;; Modified, 8/10/93, jjc
;;;     *   Removed redundant code for supporting previous versions of
;;;         objectclass (pop_oc_v600_recognisers code run when this
;;;         is false).
;;; -------------------------------------------------------------------------
