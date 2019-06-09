/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XOL_EXLIBS.p
 > Purpose:         External library list for OpenLook widgetset
 > Author:          John Gibson, Mar 31 1993
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

section;

vars XOL_EXLIBS =

#_IF DEF popxlink_openlook
    ;;; image linked against OpenLook, so XLINK_EXLIBS should contain
    ;;; everything we need
    XLINK_EXLIBS

#_ELSEIF DEF POPC_SYSDEFS_LOADED or (systranslate('POP_XOL_EXLIBS') and not(DEF popxlink_motif))
    ['==POP_XOL_EXLIBS']

#_ELSE
    mishap("OpenLook", 1, 'WIDGET SET NOT SUPPORTED');

#_ENDIF

;

#_IF DEF SUN and pop_runtime
    unless systranslate('OPENWINHOME') then
        warning('environment variable OPENWINHOME not set', []);
    endunless;
#_ENDIF

endsection;
