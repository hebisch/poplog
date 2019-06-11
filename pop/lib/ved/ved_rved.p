/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_rved.p
 > Purpose:         To read in a file from a remote machine, using rsh
 > Author:          Aaron Sloman, Jul 31 1988 (see revisions)
 > Documentation:   HELP * PIPEUTILS
 > Related Files:   SHOWLIB * VED_RSH, * VEDPIPEIN, * VED_SH, * PIPEIN
 */


/*
ved_rved
A VED command for reading a file from another machine.
 <ENTER> rved <machine> <file>

NB though the file is read, it can't be written (at present)
*/

section;

global vars show_output_on_status;  ;;; used in vedpipein

uses ved_rsh;

define global ved_rved;
    lvars args=sysparse_string(vedargument);
    dlocal show_output_on_status = false,
        ved_rsh_indentstep = vedindentstep; ;;; used in ved_rsh
    veddo('rsh '  sys_>< args(1) sys_>< ' cat ' sys_>< args(2))
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  7 1992
        LIB * VED_RSH changed to work on all systems
--- Aaron Sloman, Nov  5 1989
    Altered to use ved_remsh on Berkeley-type machines
--- Aaron Sloman, Jun 30 1989
    made ved_rsh_vedindentstep = vedindentstep
--- Aaron Sloman, May  9 1989
    Made sure show_output_on_status is declared
--- Aaron Sloman, Dec  4 1988
    Made show_output_on_status false locally, so that stuff always goes
    into a temporary file.
 */
