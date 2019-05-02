#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/modules/process.lsp
 | Purpose:         Lisp interface to the POPLOG process mechanism
 | Author:          John Williams, Nov 24 1986 (see revisions)
 | Documentation:   HELP * PROCESS
 | Related Files:
 |#

(cl:provide :process)

(cl:in-package :poplog)

(export '(*process*
          make-process
          make-process-to
          copy-process
          run-process
          suspend-process
          ksuspend-process
          resume-process
          kresume-process
          process-p
          live-process-p))

(pop11)


section $-lisp;

lisp_compile_mode;

vars process_next_pid = 0;

defprop process_pid;

lconstant macro (
    ASSIGN_PS
        =   [if ps == true then current_process -> ps endif],
    ASSIGN_PID
        =   [process_next_pid fi_+ 1 ->> process_next_pid -> process_pid(ps)],
        );


define active current_process;
    lisp_true(pop_current_process)
enddefine;


define updaterof active current_process() with_nargs 1;
    -> read_only_variable(ident pop_current_process, @*PROCESS*)
enddefine;


define make_process(fn, args) -> ps;
    consproc(destlist(args), make_pop11_procedure(fn, false)) -> ps;
    ASSIGN_PID;
enddefine;


define make_process_to(fn) -> ps;
    consprocto(0, checkr_function(fn)) -> ps;
    ASSIGN_PID;
enddefine;


define process_p(ps);
    ASSIGN_PS;
    isprocess(ps) and process_pid(ps) or true
enddefine;


define copy_process(ps) -> ps;
    ASSIGN_PS;
    isliveprocess(ps) ->;   ;;; check process
    copy(ps) -> ps;
    ASSIGN_PID;
enddefine;


define run_process(ps, vals);
    ASSIGN_PS;
    runproc(destlist(vals), ps)
enddefine;


define suspend_process(ps, vals);
    ASSIGN_PS;
    suspend(destlist(vals), ps)
enddefine;


define ksuspend_process(ps, vals);
    ASSIGN_PS;
    ksuspend(destlist(vals), ps)
enddefine;


define resume_process(ps, res_ps, vals);
    ASSIGN_PS;
    resume(destlist(vals), ps, res_ps)
enddefine;


define kresume_process(ps, res_ps, vals);
    ASSIGN_PS;
    kresume(destlist(vals), ps, res_ps)
enddefine;


define live_process_p(ps);
    ASSIGN_PS;
    isliveprocess(ps)
enddefine;


lisp_export(make_process,       @MAKE-PROCESS,          [1 ? 1]);
lisp_export(make_process_to,    @MAKE-PROCESS-TO,       "boolean");
lisp_export(copy_process,       @COPY-PROCESS,          [1 1 1]);
lisp_export(run_process,        @RUN-PROCESS,           [1 ? ?]);
lisp_export(suspend_process,    @SUSPEND-PROCESS,       [1 ? ?]);
lisp_export(ksuspend_process,   @KSUSPEND-PROCESS,      [1 ? ?]);
lisp_export(resume_process,     @RESUME-PROCESS,        [2 ? ?]);
lisp_export(kresume_process,    @KRESUME-PROCESS,       [2 ? ?]);
lisp_export(process_p,          @PROCESS-P,             "boolean");
lisp_export(live_process_p,     @LIVE-PROCESS-P,        "boolean");

lispsynonym(@*PROCESS*,         "current_process");


endsection;


;;; Back to Lisp for Revision History!
lisp


#| --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
 |#
