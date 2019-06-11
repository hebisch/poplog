/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_csh.p
 > Purpose:         Run a CSH command, and read output into a VED buffer
 > Author:          Aaron Sloman, Jul 31 1988 (see revisions)
 > Documentation:   HELP * PIPEUTILS, HELP * SHELL
 > Related Files:   SHOWLIB * VEDGENSHELL, * VEDPIPEIN, * PIPEIN
 */
compile_mode :pop11 +strict;

/*
ved_csh
A VED command for running a C-shell command and reading the output
into a temporary VED file.

 <ENTER> csh <command>

Runs the /bin/csh with the <command> and reads in any output.

*/

section;

define vars ved_csh;
    vedgenshell('/bin/csh',copy(vedcommand));
enddefine;

endsection;
