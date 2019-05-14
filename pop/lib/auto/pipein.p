/*  --- Copyright University of Sussex 1994.  All rights reserved. ---------
 >  File:           C.unix/lib/auto/pipein.p
 >  Purpose:        reading characters in from a unix command
 >  Author:         Aaron Sloman 1/1/1988 (see revisions)
 >  Documentation:  to follow. See below
 >  Related Files:  LIB * PIPEOUT, * VEDPIPEIN, * VED_SH, *VED_CSH, * VED_RSH
 */
compile_mode :pop11 +strict;

/*
{Partly modelled on lib pipeout}
pipein(<string:C>,<list:A>,<boolean:B>) -> <device|repeater:R>;

Uses the command name C and list of argument strings A to generate a
sub-process, and returns either a device record for a pipe from that
process (if B is false), or a character repeater from the pipe (if B is
not false).

Example

    pipein('/bin/csh', ['/bin/csh' '-ce' 'who'], true) -> charrep;

Runs the command 'who' in a subshell, and returns a character repeater
for the ouput, as does

    pipein('/bin/who', ['/bin/who' ],true) -> charrep;

Note that if the final argument is false, and a device is returned, then
the pipe is created with mode "line", i.e. sysread will return a
line of output at a time (terminated with newline character).

*/

section;

/*  Spawn a process to run the command with args.
    Return either a readable device or a character repeater, for the
    output of the process. If -flag- is false return the device.
*/
define pipein(command, args, flag) -> result;
    lvars command, args, flag, din, dout, result;
    ;;; Make the pipe.
    syspipe(if flag then false else "line" endif) -> (dout, din);
    if sys_vfork(false) then        ;;; false = don't want to wait for child
        ;;; parent - get characters from pipe
        sysclose(dout);
        if flag then discin(din) else din endif -> result
    else
        ;;; child - execute the command to produce the output
        sysclose(din);
        dout -> popdevout;
        dout -> popdeverr;          ;;; ensure you get error messages
        sysexecute(command, args, false);
        mishap(command, args, 2, 'pipein: COMMAND NOT FOUND ??');
    endif
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 21 1994
        Now uses new sys_vfork(false) which makes a wait for the child
        unnecessary. Hence removed p*ipein_child_pid.
--- James Goodlet, Jan 15 1992 - added -p*ipein_child_pid- in response to
        BR jamesg.31
--- Rob Duncan, Apr  4 1989
        Changed to use -sys*vfork- unconditionally, as this is always available
        (although it may be the same as -sys*fork-)
 */
