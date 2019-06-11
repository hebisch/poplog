/* --- Copyright University of Birmingham 2005. All rights reserved. ------
 > File:            $poplocal/local/auto/vedgenshell.p
 > Purpose:         See below
 > Author:          Aaron Sloman, Feb  3 1999 (see revisions)
 > Documentation:
 > Related Files:
 */

/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.unix/lib/ved/vedgenshell.p
 > Purpose:         Run a shell command and store output in VED file
 > Author:          Aaron Sloman, Apr 19 1988 (see revisions)
 > Documentation: HELP * PIPEUTILS, * PIPEIN
 > Related Files: LIB * PIPEIN, * VEDPIPEIN, * VED_SH, * VED_CSH
 */
compile_mode :pop11 +strict;

/*
vedgenshell(<false|string:C>,<false|string:H>);
    C is either false or the pathname of a shell, e.g. '/bin/sh' or
    '/bin/csh'. If it is false then the shell defaults to the result of
    systranslate('SHELL').
    If H is a string then it is inserted as a header in the output
    file.

    -vedargument- is used to define the arguments to be given to the
    shell, e.g. a command to be run.
    If there are no arguments then an interactive sub-shell is spawned,
    otherwise -vedpipein- is used to run the shell, and any output is
    read back into a temporary ved file.

See * VEDPIPEIN for more details

Used to define VED_SH, VED_CSH

*/

section;

uses vedpipein;

lvars tmpfilecount=0;

define vedgenshell(shell,header);
    lvars shell,header, file;
    lconstant
        tcshflag = '-c',
        otherflag = '-ce',
        arglist=[0 0 0];


    ;;; if shell false then get default shell
    unless shell then
        systranslate('SHELL') -> shell
    endunless;
    unless isstring(shell) then
        mishap(shell,1,'SHELL PATH NAME NEEDED')
    endunless;
    if vedargument = nullstring then
        ;;; if no argument then run an interactive shell
        pr(newline);
        chain(shell,sysobey);
    else
        shell -> arglist(1);
        if issubstring('tcsh', shell) then
            ;;; Necessary for tcsh on linux, but not necessary on solaris
            ;;; However, seems to work on solaris
            tcshflag else otherflag
        endif -> arglist(2);
        vedargument -> arglist(3);
        ;;; avoid vedopen - it searches directories?
        tmpfilecount fi_+ 1 -> tmpfilecount;
        vedpipein(
            shell,
            arglist,
            vedopen('/tmp/vdsh' sys_>< tmpfilecount sys_>< 'x' sys_>< poppid) ->> file,
            procedure;
                false ->> vedcompileable ->> vedwriteable
                    ->> vedbreak -> vednotabs;
                8 -> vedindentstep; ;;; for output of "ls" etc.
            endprocedure,
            vedediting,                 ;;; i.e. display the file
            header)                 ;;; header for output file
    endif;
    unless vedusedsize(file(3)) < 3 then
        vededit(file, vedhelpdefaults);
    endunless;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan 11 2005
        Moved into $usepop/pop/lib/ved
--- Aaron Sloman, Jul 27 2002
        Changed to use '-c' instead of '-ce' for tcsh on linux.
--- Aaron Sloman, Feb  3 1999
    Changed to use current value of vedediting.
    Changed to use result of vedpipein, so that if vedediting is false the file is
    still available.
--- Aaron Sloman, Jan 14 1989 removed vednullstring
--- Aaron Sloman, Apr 24 1988
    Changed not to use -systmpfile-, as it uses -readable-, which takes
    time
 */
