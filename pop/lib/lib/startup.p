/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/startup.p
 > Purpose:         Code for inclusion in the "startup.psv" saved image
 > Author:          Robert John Duncan, Aug 28 1991 (see revisions)
 > Documentation:
 > Related Files:   C.unix/com/mkstartup, C.vms/com/mkstartup.com
 */
compile_mode :pop11 +strict;

#_TERMIN_IF DEF POPC_COMPILING

/***********************************************************************

    This file lists any libraries to be included in the "startup"
    saved image, $popsavelib/startup.psv.

    The file is loaded by the command script $popcom/mkstartup.

    System managers can customise the startup image by providing a
    local version of this file as $poplocal/local/lib/startup.p

 **********************************************************************/

#_IF DEF XLINK_TYPE

;;; Poplog has been linked with X Window System support:
;;; include any X-specific libraries here

uses popxlib;

#_IF DEF vedprocess
    ;;; Standard Ved is defined
    #_IF DEF popxlink_motif or DEF popxlink_openlook
        ;;; GUI is available so make PUI+XVed available also
        uses poplog_ui;
    #_ELSE
        ;;; just load vanilla XVed
        uses xved;
    #_ENDIF
#_ENDIF

#_ENDIF

;;; Include general libraries here

uses popuseslist;
uses poppackagelist;
uses objectclass;
uses prwarning;
uses emacsreadline;

#_IF DEF vedprocess
uses vedset;
uses ved_autosave;

#_ENDIF


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 17 1993
        Made it just load vanilla XVed if Motif/OpenLook not available
--- Simon Nichols, May 11 1993
        Poplog linked with X if XLINK_TYPE defined
--- John Gibson, Nov 15 1992
        Added uses vedset (since vedset moved out of core system)
--- Adrian Howard, Jul 20 1992
        Now loads PUI instead of XVed
--- Robert John Duncan, May 15 1992
        Made LIB XVED conditional on standard VED being defined
--- Robert John Duncan, Oct 21 1991
        Added LIB XVED
 */
