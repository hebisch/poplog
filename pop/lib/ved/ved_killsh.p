/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:           C.unix/lib/ved/ved_killsh.p
 > Purpose:        kill current sh communicating process
 > Author:         A.Sloman Aug 1986 - based on ved_killcsh.p (see revisions)
 > Documentation:  HELP * IMSH, HELP * IMCSH HELP * CSH_COMPILE
 > Related Files:  LIB * SH_COMPILE, LIB * VED_IMSH
 */
compile_mode :pop11 +strict;

section $-lib => ved_killsh;

uses sh_compile;

define vars ved_killsh;
    if sh_send_process.isliveprocess and sh_pid.isinteger then
        sh_send(false);
        'sh process killed'
    else
        'No live sh process';
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, May 22 1996
        Changed section to $-lib
--- John Gibson, Dec  1 1992
        Now in shell_compile section
 */
