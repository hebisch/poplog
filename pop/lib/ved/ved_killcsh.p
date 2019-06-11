/* --- Copyright University of Sussex 1996.  All rights reserved. ---------
 > File:           C.unix/lib/ved/ved_killcsh.p
 > Purpose:        kill current csh communicating process
 > Author:         Tom Khabaza, Sep 27 1983 (see revisions)
 > Documentation:  HELP * CSH_COMPILE, HELP * IMCSH
 > Related Files:  LIB * CSH_COMPILE LIB * VED_IMCSH
 */
compile_mode :pop11 +strict;

/* adapted without change from ved_killdcl, roger evans 8/83 */

uses csh_compile;

section $-lib => ved_killcsh;

define vars ved_killcsh;
    if csh_send_process.isliveprocess and csh_pid.isinteger then
        csh_send(false);
        'csh process killed'
    else
        'No live csh process';
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, May 22 1996
        Changed section to $-lib
--- John Gibson, Dec  1 1992
        Now in shell_compile section
--- Robert John Duncan, Nov  5 1991
        Now loads LIB * CSH_COMPILE for 'csh_' variable declarations.
 */
