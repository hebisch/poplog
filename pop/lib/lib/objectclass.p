/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/lib/objectclass.p
 > Purpose:         Objectclass: load core sources
 > Author:          Robert John Duncan, Nov 20 1995
 > Documentation:   HELP * OBJECTCLASS
 > Related Files:   LIB * OBJECTCLASS_HELP
 */
compile_mode:pop11 +strict;

#_TERMIN_IF DEF objectclass or DEF POPC_COMPILING

;;; setup searchlists
uses objectclass_help;

section $-objectclass;

pop11_compile(objectclass_dir dir_>< 'objectclass.p');

endsection;     /* $-objectclass */

;;; for uses
vars $-objectclass = true;
