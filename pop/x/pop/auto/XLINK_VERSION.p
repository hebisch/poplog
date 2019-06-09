/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.x/x/pop/auto/XLINK_VERSION.p
 > Purpose:         Define widgetset version numbers
 > Author:          John Gibson, Mar 27 1993
 > Documentation:
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING  ;;; for compile-time use only

;;; Default if not defined via poplink
vars $-XLINK_VERSION =  #_IF     DEF popxlink_motif     XM_VERSION
                        #_ELSEIF DEF popxlink_openlook  XOL_VERSION
                        #_ELSE                          1000
                        #_ENDIF
