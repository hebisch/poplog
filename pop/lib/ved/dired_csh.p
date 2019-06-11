/* --- Copyright University of Sussex 1988.  All rights reserved. ---------
 > File:           C.unix/lib/ved/dired_csh.p
 > Purpose:        Used with VED_DIRED, to obey shell command
 > Author:         Aaron Sloman, Oct 1 1988
 > Documentation:   HELP *DIRED, *DIRED.SHORT
 > Related Files:   LIB *VED_DIRED
 */

#_TERMIN_IF DEF POPC_COMPILING

/*
dired_csh is associated with flag -$ in ved_dired
*/

uses ved_dired;

section $-dired => dired_csh;

define global dired_csh(dummy, command, file, dummy);
    ;;; Obey shell command, but replace '%' in the command with the file
    lvars dummy, command, file;
    diredpipein(command, file);
    dired_setup();
enddefine;

endsection;
