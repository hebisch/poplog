/* --- Copyright University of Sussex 1991. All rights reserved. ----------
 > File:            C.all/lib/auto/FIELDOFFSET.p
 > Purpose:         Macro to produce offset of field in structure <typespec>
 > Author:          John Gibson, Jul 29 1990 (see revisions)
 > Documentation:   REF *DEFSTRUCT
 > Related Files:   LIB *TYPESPEC_UTILS
 */
compile_mode:pop11 +strict;

;;; Usage:  FIELDOFFSET( <typespec>, <fieldname> )

section;

sysunprotect("FIELDOFFSET");

define :inline global FIELDOFFSET(spec=typespec, field=item);
    #_< exacc ^int (exacc[@] spec null_external_ptr.field) >_#
enddefine;

sysprotect("FIELDOFFSET");

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 16 1991
        Rewritten using new define :inline capabilities
 */
