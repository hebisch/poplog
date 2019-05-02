#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.unix/lisp/modules/run-unix-program.lsp
 | Purpose:         Utilities for running Unix programs from Lisp
 | Author:          John Williams, Mar 30 1990 (see revisions)
 | Documentation:   HELP * RUN-UNIX-PROGRAM
 | Related Files:   C.unix/lib/auto/run_unix_program.p
 |#

(cl:provide :run-unix-program)

(cl:in-package :poplog)

(export '(run-unix-program))

(pop11)


section $-lisp;

lisp_compile_mode;

uses run_unix_program;


define cl_run_unix_prog(name, args, input, output, errs, wait);
    lvars pid, status;

    define lconstant Checkr_stream_arg(arg, procedure field, key);
        lvars dev;
        if arg == @:STREAM then
            true
        elseif arg == nil then
            false
        else
            field(arg) -> dev;
            if isident(dev) then
                idval(dev) -> dev
            endif;
            if isdevice(dev) then
                dev
            else
                lisp_error('Unsuitable ~S stream for RUN-UNIX-PROGRAM',
                            [^key ^arg])
            endif
        endif
    enddefine;

    checkr_filename(name) -> name;
    Checkr_stream_arg(input, stream_source, @:INPUT) -> input;
    Checkr_stream_arg(output, stream_dest, @:OUTPUT) -> output;
    if errs == @:OUTPUT then
        1
    else
        Checkr_stream_arg(errs, stream_dest, @:ERROR-OUTPUT)
    endif -> errs;

    run_unix_program(name, args, input, output, errs, pop_true(wait))
        -> pid -> status -> errs -> output -> input;

    if isdevice(input) or isdevice(output) then
        make_stream(output and discin(output),
                    input and discout(input))
    else
        nil
    endif;
    if isdevice(errs) then
        make_stream(discin(errs), false)
    else
        nil
    endif;
    lisp_true(status);
    pid
enddefine;


define cl_kill_pid(pid, signum);
    defaults signum 9;
    lisp_true(sys_send_signal(pid, signum))
enddefine;


lisp_export(cl_run_unix_prog,   @SYS:RUN-UNIX-PROGRAM,  [6 6 4]);
lisp_export(cl_kill_pid,        @KILL-PID,              [1 2 1]);


endsection;


/* Now define RUN-UNIX-PROGRAM */

lisp

(in-package :poplog)

(setq *constant-functions* t)


(defun RUN-UNIX-PROGRAM (name &key args
                                    input output error-output
                                    (if-input-does-not-exist    :error)
                                    (if-output-exists           :error)
                                    (if-error-output-exists     :error)
                                    (wait t))
    (unless (or (eq input :stream)
                (eq input nil)
                (streamp input))
            (setq input (open input
                            :direction :input
                            :if-does-not-exist if-input-does-not-exist)))
    (unless (or (eq output :stream)
                (eq output nil)
                (streamp output))
            (setq output (open output
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists if-output-exists)))
    (unless (or (eq error-output :stream)
                (eq error-output :output)
                (eq error-output nil)
                (streamp error-output))
            (setq error-output (open error-output
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists if-error-output-exists)))
    (sys:run-unix-program name args input output error-output wait))


#| --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Apr  3 1995
        stream_source & stream_dest instead of str_dev_in/out.
 |#
