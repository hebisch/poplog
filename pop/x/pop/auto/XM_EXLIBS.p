/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XM_EXLIBS.p
 > Purpose:         External library list for Motif widgetset
 > Author:          John Gibson, Mar 31 1993
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

include sysdefs.ph;

section;

vars XM_EXLIBS =

#_IF DEF popxlink_motif
    ;;; image linked against Motif, so XLINK_EXLIBS should contain
    ;;; everything we need
    XLINK_EXLIBS

#_ELSEIF DEF POPC_SYSDEFS_LOADED or (systranslate('POP_XM_EXLIBS') and not(DEF popxlink_openlook))
    ['==POP_XM_EXLIBS']

#_ELSE
    mishap("Motif", 1, 'WIDGET SET NOT SUPPORTED');

#_ENDIF

;

endsection;
