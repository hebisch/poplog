#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/modules/prolog.lsp
 | Purpose:         Access to Prolog from Lisp
 | Author:          John Williams, Apr 30 1987 (see revisions)
 | Documentation:   HELP * PROLOG
 | Related Files:   LIB * PLOG_GOALS.P
 |#

(cl:provide :prolog)

(cl:in-package :poplog)

(export '(
        make-plog-atom
        make-plog-goal
        make-plog-term
        make-plog-var
        read-plog-atom
        read-plog-goal
        read-plog-goal-closer
        read-plog-var
        plog-goal-call
        do-plog
        ))

(pop11)


/** Export some basic utilities to Lisp *************************************/

section;

lisp_compile_mode;

procedure();
    dlocal % item_chartype(`@`) % = 3;
    useslib("plog_goals");
endprocedure();

endsection;


section $-lisp;

nonmac lisp -> prolog_command("LISP");

switch_lisp_to(% "prolog" %) -> magic_word_handler(@PROLOG);

lisp_export(magic_word_handler(@PROLOG),
            @PROLOG,
            [0 0 0]);

lisp_export(procedure(functor, args);
                lvars arity;
                destlist(args) -> arity;
                prolog_maketerm(functor, arity)
            endprocedure,
            @MAKE-PLOG-TERM,
            [1 ? 1]);

lisp_export(procedure(term, env);
                consplog_goal(term, env, false)
            endprocedure,
            @MAKE-PLOG-GOAL,
            [2 2 1]);

lisp_export(sym_to_word,
            @MAKE-PLOG-ATOM,
            [1 1 1]);

lisp_export(prolog_newvar,
            @MAKE-PLOG-VAR,
            [0 0 1]);

lisp_export(class_apply(plog_goal_key),
            @PLOG-GOAL-CALL,
            "boolean");

sys_syspr -> lisp_class_print(prologterm_key);
sys_syspr -> lisp_class_print(prologvar_key);

endsection;


/** Reading Prolog goals ****************************************************/

lisp

(in-package :poplog)

(setq *constant-functions* t)

(defvar *PLOGTEMPVARS* 0)
(defvar *PLOGLETLIST* 0)
(defvar *PLOGVARLIST* 0)


(defun READ-PLOG-ATOM (stream char)
    `(make-plog-atom ',(read stream t nil t)))


(defun READ-PLOG-GOAL (stream char &aux term)
    (if (eq *plogtempvars* 0)
        (let (*plogtempvars* *plogletlist* *plogvarlist*)
            (setq term (read-delimited-list #\} stream t))
            `(let* ,*plogletlist*
                (make-plog-goal
                    (make-plog-term ,@term)
                    (list ,@*plogvarlist*))))
        (progn
            (setq term (read-delimited-list #\} stream t))
            `(make-plog-term ,@term))))


(defun READ-PLOG-GOAL-CLOSER (stream char)
    (error "Unexpected closing ~:C" char))


(defun READ-PLOG-VAR (stream char &aux sym var)
    (if (eq *plogtempvars* 0)
        (error "Read macro ~:C not used within { ... } syntax" char))
    (setq sym (read stream t nil t))
    (unless (setq var (getf *plogtempvars* sym))
            (setq var (gensym "PV"))
            (setq *plogtempvars* `(,sym ,var ,@*plogtempvars*))
            (setq *plogletlist* `((,var (make-plog-var)) ,@*plogletlist*))
            (setq *plogvarlist* `(',sym ,var ,@*plogvarlist*)))
    var)


(set-macro-character #\$ #'read-plog-atom)
(set-macro-character #\{ #'read-plog-goal)
(set-macro-character #\} #'read-plog-goal-closer)
(set-macro-character #\? #'read-plog-var)


(pop11)


/** (DO-PLOG goal [vars] . body) *********************************************/

section $-lisp;

lisp_compile_mode;


define Do_plog(forms, nresults);
    lvars goal, symlist, flag, body;

    if endp(forms) then
        program_error('Missing Prolog goal in DO-PLOG form', [])
    endif;
    fast_destpair(forms) -> (goal, forms);

    if endp(forms) then
        ;;; Optimise for once only invocation
        lispSET_STKLEN(nresults, 1) -> nresults;
        compile_form(goal, 1);
        sysCALL("destplog_goal");
        sysERASE(1);
        sysERASE(1);
        sysPUSHQ(prolog_invoke);
        sysCALLQ(prolog_barrier_apply);
        lispTRUE();
        lispRESET_STKLEN(nresults);
        return;
    endif;

    fast_destpair(forms) -> (symlist, forms);

    compile_form(goal, 1);
    lispNEW_VTOK() -> goal;
    lispPOP(goal);

    lispNEW_VTOK() -> flag;
    lispPUSHQ(nil);
    lispPOP(flag);

    lispFCOMPILE(
        false,
        @FUNCTION,
        procedure();
            lvars lab, sym, lab1, lab2;
            parse_body(forms, false, true) -> (forms, , );
            lispPUSHQ(true);
            lispPOP(flag);
            lispBLOCK(nil, nresults);
            sysNEW_LABEL() -> lab;
            applist(symlist, lispLOCAL);
            fast_for sym in symlist do
                sysNEW_LABEL() -> lab1;
                sysNEW_LABEL() -> lab2;
                lispPUSHQ(sym);
                sysPUSH("current_plog_goal_env");
                sysCALLQ(list_assoc);
                sysOR(lab1);
                sysGOTO(lab2);
                sysLABEL(lab1);
                sysCALLQ(prolog_full_deref);
                lispPOP(sym);
                sysLABEL(lab2);
            endfast_for;
            Progn(forms, 0);
            sysGOTO(lab);
            lispENDBLOCK();
            sysPUSHQ(true);
            sysPUSHQ(1);
            sysCALLQ(suspend);
            sysLABEL(lab);
        endprocedure) -> body;

    lispPUSH(goal);
    lispPUSHQ(body);
    sysCALL("plog_goal_invoke");
    sysERASE(1);

    lispPUSHN(flag, nresults);
enddefine;


lisp_export(Do_plog, @DO-PLOG, "special");


endsection;


/* Back to Lisp so the revision notes are read properly! */

lisp


#| --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 12 1995
        program_error instead of mishap in Do_plog.
--- John Williams, Jul 11 1994
        Fixed for changed lispBLOCK & lispENDBLOCK.
--- John Williams, Apr 23 1992
        Read macros now used named functions, not lambda expressions
--- John Williams, Jul 18 1990
        Revised for new LIB SUBSYSTEM
--- John Williams, Jun 19 1989
        Fixed read macros to create Prolog vars at run-time, not read-time
--- John Williams, Mar  4 1988
        (1) Localises -item_chartype- of `@` while compiling Prolog system
        (2) Defines Prolog macro -LISP-
 |#
