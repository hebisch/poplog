/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/objectclass/auto/KEY_SLOT.p
 > Purpose:         Objectclass file
 > Author:          Steve Knight, HP Labs, 1992-1993 (see revisions)
 > Documentation:   HELP OBJECTCLASS
 > Related Files:
 */
compile_mode:pop11 +strict;

uses objectclass;

;;; Code donated by Ian Rogers

;;; KEY_SLOT <class-name> <method-name>
;;;     This arranges for the named method to act as a non-inheriting
;;;     class-variable.  In other words, sub-classes act as if the class
;;;     variables are copied rather than shared.
;;;
;;;     It does this by creating a property, called "class_<method-name>"
;;;     which is used to go from the class to the value.

section;

define macro KEY_SLOT class meth_name;
    lvars
        class meth_name,
        prop_name = "class_" <> meth_name;

    pop11_try_nextitem(";") -> ;
    [
        define ^prop_name =
            newassoc([]);
        enddefine;

        define:method ^meth_name (o: ^class );
            ^prop_name (datakey(o))
        enddefine;

        define:method updaterof ^meth_name (o: ^class );
            -> ^prop_name (datakey(o))
        enddefine;
    ].dl
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 21 1995
        Added: uses objectclass
 */
