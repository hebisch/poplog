/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.x/x/src/xt_version.p
 > Purpose:         X Toolkit - version number definition
 > Author:          Roger Evans, Jul  5 1988 (see revisions)
 > Documentation:
 > Related Files:
 */

#_INCLUDE 'xt_declare.ph'


    ;;; The following constants are simple-valued, and must be vars in AIX
    ;;; because the stupid assembler/object file format doesn't support
    ;;; global symbols with absolute values.

#_IF DEF AIX

;;; defined in xt_impl.ph
vars XtVersion = _XtVersion;

;;; defined in xt_declare.ph
vars XtSpecificationRelease = _XtSpecificationRelease;


#_ELSE

;;; defined in xt_impl.ph
constant XtVersion = _XtVersion;

;;; defined in xt_declare.ph
constant XtSpecificationRelease = _XtSpecificationRelease;


#_ENDIF



/* --- Revision History ---------------------------------------------------
--- John Gibson, May  8 1998
        Added AIX modification
 */
