/* --- Copyright University of Birmingham 2005. All rights reserved. ------
 > File:            $poplocal/local/auto/sys_popen.p
 > Purpose:         Simulate the effect of popen in read mode
 > Author:          Aaron Sloman, Feb 19 1997 (see revisions)
 > Documentation:   MAN * POPEN
 > Related Files:   HELP * RUN_UNIX_PROGRAM
 */

/*

HELP SYS_POPEN                                     Aaron Sloman Feb 1997

sys_popen(command, args) -> (pipe, ref)
sys_popen(command, args, org) -> (pipe, ref)

read_pipe(dev, string, len, ref) -> found;

Given a command (a string) and a list of strings providing arguments for
the command, sys_popen uses the command and its arguments to spawn a
unix process. It returns a pipe from which the output of the process can
be read, and a Pop-11 reference which initially contains false, but
after the unix process dies the reference contains the exit status of
the process at the end (a number).

The optional third argument can be either false, in which case the pipe
is optimised for reading individual characters or "line" in which case
each call of sysread (or read_pipe) reads in only characters up to the
next newline. (See REF * SYSIO, * syspipe).

The library also defines the following procedure

read_pipe(dev, string, len, ref) -> found;

This takes a device (i.e. a pipe for reading, the first result returned
by sys_popen), a string into which to read characters from the device,
the maximum number of characters to read at a time (usually the length
of the string), and a reference, i.e. the second result produced by
sys_popen. The first three arguments are the same as those for sysread.

The result, found, can have one of the following values

    false:      meaning the process has finished.
    0:          meaning the process has not yet finished, but there
                are no characters in the pipe
    an integer: meaning that number of characters has been read from
                the pipe into the string.


EXAMPLES

;;; first define a convenient procedure to run sys_popen then
use read_pipe repeatedly to read from the pipe.

define test_popen(command, args);
    ;;; Run the command (a string) with the arguments (a list of strings)
    ;;; Format is as for run_unix_program

    lvars (indev, ref) = sys_popen(command, args);

    lvars n;
    repeat;
    quitunless(read_pipe(indev, sysstring, sysstringlen, ref) ->> n);
        ;;; next command will print nothing if n == 0
        printf(substring(1,n,sysstring));
    endrepeat;
    sysclose(indev);
enddefine;


;;; Examples
test_popen('who', []);

test_popen('w', []);

test_popen('top', ['-b' '-n' '0']);

test_popen('ps', ['-auxw']);

test_popen('/bin/csh', ['-c' 'ls -ld  ~/.*']);

test_popen('finger', ['axs' 'rmp']);


;;; This one should print nothing, but will not hang forever
;;; waiting.
test_popen('csh', [ '-c' 'echo -n']);


*/

compile_mode :pop11 +strict;
section;

define vars read_pipe(dev, string, len, ref) -> num;
    ;;; return either number of characters in the pipe,
    ;;; or false if the process has finished and there's
    ;;; nothing left to read
    lvars n;

    ;;; Wait for up to half a second to see if any input is ready?
    sys_device_wait(dev, [], [], 5e5) -> (n, , );

    if n then
        ;;; sys_input_waiting(dev) ->> n) then
        ;;; characters waiting

        sysread(dev, string, len) -> num;

    else
        ;;; Nothing in pipe. See if process is dead,
        ;;; chance to run
        if cont(ref) then ;;; it has finished
            false -> num
        else
            0 -> num;       ;;; Alive, but no characters to read
        endif
    endif;
enddefine;

define lconstant report_finished(pid, status, ref);
    ;;; this procedure partially applied to a reference will
    ;;; be used as the ast_p argument for sys_fork
    status -> cont(ref)
enddefine;

define global sys_popen(name, args) -> (parent_get, ref);
    ;;; check whether final "org" argument is provided
    lvars org, ref = consref(false) ;

    if islist(args) then
        "line" -> org;  ;;; default is to read a line at a time
    else
        name, args ->(name, args, org)
    endif;

    lvars child_devout, parent_get, path;

    sysfileok(name) -> path;
    unless path = nullstring
    or fast_subscrs(1, path) == `/` do
        sys_search_unix_path(path, systranslate('PATH')) -> path;
        unless path do
            mishap(name, 1, 'COMMAND NOT FOUND')
        endunless
    endunless;

    /* Check out <args>, from run_unix_program, WHY?? */
    path :: maplist(args, $-lisp$-get_simple_string) -> args;

    syspipe(org) -> (child_devout, parent_get);

    ;;; Give sys_fork an ast_p argument, a procedure that will
    ;;; do false -> cont(ref) when the child process dies.
    lvars pid;
    if (sys_fork(false, report_finished(%ref%)) ->> pid) then
        ;;; parent
    else
        ;;; child
        sysclose(popdevin);
        child_devout ->> popdevout -> popdeverr;
        sysclose(parent_get);
        sysexecute(path, args, false);
        ;;; should never return from this, but just in case....
        fast_sysexit()
    endif;

enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan 15 2005
        Changed examples to work on linux
--- Aaron Sloman, Feb 14 1999
    Declared pid. Why wasn't it decleared previously?
 */
