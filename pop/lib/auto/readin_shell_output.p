/* --- Copyright University of Birmingham 1999. All rights reserved. ------
 > File:            $poplocal/local/auto/readin_shell_output.p
 > Purpose:         Run a shell command and make a list strings
                    corresponding to the output.
 > Author:          Aaron Sloman, Feb 14 1999
 > Documentation:   HELP * READIN_SHELL_OUTPUT
 > Related Files:   LIB * RUN_UNIX_PROGRAM, LIB * PIPEIN
                    LIB * SYS_OBEY_LINEREP, LIB * SYS_POPEN
 */

/*
;;; tests
readin_shell_output('uname -rv', '/bin/sh')=>;
readin_shell_output('who', '/bin/sh')==>;
vars list = readin_shell_output('who', '/bin/sh'); applist(list,npr);
vars list = readin_shell_output('w', '/bin/sh'); applist(list,npr);
vars list = readin_shell_output('finger axs', '/bin/sh'); applist(list,npr);

*/

section;

compile_mode :pop11 +strict;

define readin_shell_output(command, shell) -> list;

    lvars
        header, file,
        arglist=[0 '-ce' 0],
        arglist, repeater, num;

    ;;; if shell false then get default shell
    unless shell then
        systranslate('SHELL') -> shell;
        unless shell then
            '/bin/sh' -> shell
        endunless;
    endunless;

    unless isstring(shell) then
        mishap(shell,1,'SHELL PATH NAME NEEDED')
    endunless;

    shell -> arglist(1);
    command -> arglist(3);
    line_repeater(pipein(shell, arglist, false), sysstring) -> repeater;

    ;;; read in all the output from the sub-process
    lvars line;
    [%
        until (repeater() ->> line) == termin do line enduntil
    %] -> list;
enddefine;

endsection;
