/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XAW_EXLIBS.p
 > Purpose:         External library list for Athena widgetset
 > Author:          John Gibson, Mar 31 1993 (see revisions)
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

include sysdefs.ph;

section;

vars XAW_EXLIBS =

#_IF DEF UNIX
    [% ident XLINK_EXLIBDIRS, '-lXaw', ident XLINK_EXT_EXLIBS,
            ident XLINK_EXLIBFILES %]

#_ELSEIF DEF POPC_SYSDEFS_LOADED or systranslate('POP_XAW_EXLIBS')
    ['==POP_XAW_EXLIBS'];

#_ELSE
    mishap("Athena", 1, 'WIDGET SET NOT SUPPORTED');
#_ENDIF

;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Sep 10 1996
        Changed to use idents in the UNIX list
 */
