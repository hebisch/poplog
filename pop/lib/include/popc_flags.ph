/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/include/popc_flags.ph
 >  Purpose:        Definitions of flags used by POPC through the active
 >                  variable -pop_popc_flags-
 >  Author:         John Gibson, May 31 1989
 >  Related Files:  LIB COMPILE_MODE
 */

#_TERMIN_IF DEF POPC_FLAGS_INCLUDED

;;; N.B. -compile_mode- is a POP-11 syntax construct for compile-time
;;; manipulation of the flags defined in this file; the 'compile_mode:popc'
;;; option corresponding to SETTING each flag appears on the right after
;;; its definition.

section;

iconstant macro (

    POPC_SYSPOP_MODE            = 2:1e0,        ;;; +syspop
            /*  If set, the current file uses sysPOP11.
            */

    POPC_NONWRITEABLE_DEFAULT   = 2:1e1,        ;;; +wrdflt
            /*  When set, makes the default for structures be nonwriteable
                (i.e. for structures not otherwise marked as writeable or
                nonwriteable, individually or by class key). If not set, the
                default is writeable.
            */

    POPC_WRITEABLE_CLOSURES     = 2:1e2,        ;;; +wrclos
            /*  When set, makes (user-created) closures writeable (except where
                individually marked nonwriteable). If not set, all closures
                default to nonwriteable (as for ordinary procedures).
            */

    );

iconstant POPC_FLAGS_INCLUDED = true;

endsection;
