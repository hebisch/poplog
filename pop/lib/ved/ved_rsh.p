/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_rsh.p
 > Purpose:         Invoke RSH on a remote machine. Output into a ved buffer
 > Author:          Aaron Sloman, Apr 19 1988 (see revisions)
 > Documentation:   HELP * PIPEUTILS . See also below
 > Related Files:   LIB * VEDPIPEIN, * VED_SH, * VED_CSH, * VED_RVED,
 >                  LIB * VED_REMSH
 */
compile_mode:pop11 +strict;

/*
<ENTER> rsh <machine> <command>
    Obeys the command on the machine specified, and reads the
    result into a temporary VED file.

For more details see *VEDPIPEIN, including the effect of the global flag
-show_output_on_status-.

*/

section;

uses vedpipein;

global vars
    ved_rsh_indentstep = 8,
        ;;; Formatting rsh output with tabs
        ;;; This is altered in ved_rved
    ved_rsh_command,
        ;;; Pathname of rsh command to run
;

define vedremoteshell(command_file, command_name);
    lvars loc,
        command_file, ;;; path name of command
        command_name;

    lconstant arglist= writeable [0 0 '-n' 0];

    ;;; parse the argument
    if locchar(`\s`,1,vedargument) ->> loc then
        substring(1, loc fi_- 1, vedargument) -> arglist(2);
        allbutfirst(loc, vedargument) -> arglist(4);
    else
        vederror(lowertoupper(command_name) >< ' <machine> <command>')
    endif;

    command_name -> arglist(1);
    vedpipein(
        command_file,
        arglist,
        systmpfile(false, command_name, nullstring),
        procedure;
            false ->> vedcompileable ->> vedwriteable
                ->> vedbreak -> vednotabs;
            ved_rsh_indentstep -> vedindentstep;    ;;; keep formatting right
        endprocedure,
        true,                       ;;; i.e. display the file
        copy(vedcommand))           ;;; header for output file
enddefine;

define vars ved_rsh;
    unless isstring(ved_rsh_command) then
        ;;; Try /usr/ucb/rsh first; if that fails, look for 'remsh' in
        ;;; case there's a restricted shell lurking somewhere, and
        ;;; finally look for 'rsh' in the standard places
        unless sys_search_unix_path('rsh', '/usr/ucb')
            or sys_search_unix_path('remsh', systranslate('$PATH'))
            or sys_search_unix_path('rsh', systranslate('$PATH'))
                ->> ved_rsh_command
        then
            vederror('RSH: command not found');
        endunless;
    endunless;
    vedremoteshell(ved_rsh_command, sys_fname_nam(ved_rsh_command));
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  7 1992
        Changed not to rely on fixed pathnames: should now work on all
        systems.
--- Aaron Sloman, Nov  5 1989
    Defined ved_rsh in terms of vedremoteshell, so that the same procedure
    could be used to define ved_remsh for HPUX and other system V Poplogs.
--- Aaron Sloman, Jun 30 1989
    Introduced ved_rsh_indentstep to control indentation
--- Aaron Sloman, Jan 14 1989 removed vednullstring
 */
