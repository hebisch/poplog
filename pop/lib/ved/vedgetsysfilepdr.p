/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/vedgetsysfilepdr.p
 > Purpose:         Doc types property
 > Author:          John Gibson, Nov  4 1995
 > Documentation:   REF * DOCUMENTATION
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

include ved_declare.ph;
include sysdefs.ph;

;;; Property mapping documentation types to access procedure
;;; (declared as incremental property in lib/include/ved_declare.ph
;;;  -- next assignment marks this as the basic initialisation)

define vars vedgetsysfilepdr =
    newproperty(

        [[DOC       ved_doc]
         [HELP      ved_help]
         [INCLUDE   ved_showinclude]
         [LIB       ved_showlib]
% #_IF DEF UNIX
         [MAN       ved_man],
         [UNIX      ved_man]
#_ENDIF %
         [PLOGHELP  ved_ploghelp]
         [REF       ved_ref]
         [SHOWLIB   ved_showlib]
         [SRC       ved_src]
         [TEACH     ved_teach]],

    16, false, "perm")
enddefine;

endsection;
