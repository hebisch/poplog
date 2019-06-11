/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_remsh.p
 > Purpose:         Invoke REMSH on a remote machine. Output into a ved buffer
 > Author:          Aaron Sloman, Nov  5 1989 (see revisions)
 > Documentation:   HELP * PIPEUTILS . See also below
 > Related Files:   LIB * VEDPIPEIN, * VED_RSH
 */
compile_mode :pop11 +strict;

/*
Alternative name for ved_rsh -- see HELP * PIPEUTILS for full details.

<ENTER> remsh <machine> <command>
    Obeys the command on the machine specified, and reads the
    result into a temporary VED file.

For more details see *VEDPIPEIN, including the effect of the global flag
-show_output_on_status-.

*/

uses ved_rsh;

section;

vars
    ved_remsh_command,
        ;;; Pathname of remsh command
;

define vars ved_remsh;
    unless isstring(ved_remsh_command) then
        unless sys_search_unix_path('remsh', systranslate('$PATH'))
                ->> ved_remsh_command
        then
            vederror('REMSH: command not found');
        endunless;
    endunless;
    vedremoteshell(ved_remsh_command, sys_fname_nam(ved_remsh_command));
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  7 1992
        Changed not to rely on a fixed pathname
--- Robert John Duncan, Jun 25 1992
        Moved into C.unix, so no longer cancels "ved_rsh"
 */
