/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XM_VERSION.p
 > Purpose:         Define Motif widgetset version number
 > Author:          John Gibson, Mar 27 1993
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

#_IF DEF popxlink_motif and DEF XLINK_VERSION
    lconstant version = XLINK_VERSION;
#_ELSE
    lconstant (, version, ) = sys_translate_exlibs('POP_XM_EXLIBS');
    unless version then
        mishap(0, 'NO MOTIF VERSION (XM_VERSION) AVAILABLE')
    endunless;
#_ENDIF

constant $-XM_VERSION = version;
