/* --- Copyright University of Sussex 1988. All rights reserved. ----------
 > File:            C.unix/lib/ved/ved_sh.p
 > Purpose:         Run a sh command and read output into a VED buffer
 > Author:          Aaron Sloman, Jul 31 1988 (see revisions)
 > Documentation:   HELP * PIPEUTILS, HELP * SHELL
 > Related Files:   LIB * VEDGENSHELL, LIB * VEDPIPEIN, LIB * PIPEIN
 */
compile_mode :pop11 +strict;


/*
ved_sh
A VED command for running a shell command and reading the output
into a temporary VED file.
 <ENTER> sh <command>

Runs /bin/sh with the <command> as argument and reads any output
into a VED file.

*/

section;

define vars ved_sh;
        vedgenshell('/bin/sh',copy(vedcommand));
enddefine;

endsection;
