#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/modules/storeutils.lsp
 | Purpose:         Lisp interface to POPLOG storeage management utilities
 | Author:          John Williams, Mar 14 1988 (see revisions)
 | Documentation:   HELP * STOREUTILS, REF * SYSTEM
 | Related Files:
 |#

(cl:provide :storeutils)

(cl:in-package :poplog)

(export
    '(*gc-copy* *gc-hook* *gc-ratio* *gc-time* *gc-verbose*
      *current-store-size* *min-store-size* *max-store-size*
      gc gc-object lock-heap unlock-heap))

(pop11)


section $-lisp;

lisp_compile_mode;


define active gc_hook;
    if pop_after_gc == identfn then
        nil
    elseif pdpart(pop_after_gc) == lisp_apply then
        frozval(1, pop_after_gc)
    else
        pop_after_gc
    endif
enddefine;


define updaterof active gc_hook(fn);
    if fn == nil then
        identfn
    else
        lisp_apply(% checkr_function(fn), 0, 0 %)
    endif -> pop_after_gc
enddefine;


vars active (
    gc_copy
        =   boolean_variable(% ident pop_gc_copy %),
    gc_ratio
        =   integer_range_variable(% ident popgcratio, 1, 64, @*GC-RATIO* %),
    gc_verbose
        =   boolean_variable(% ident popgctrace %),
    );


define active max_store_size;
    popmemlim
enddefine;


define updaterof active max_store_size(n);
    if isdecimal(n) or isratio(n) then
        intof(n) -> n
    endif;
    if isbiginteger(n) then
        pop_max_int -> n
    endif;
    pop_true(n) -> popmemlim
enddefine;


define gc_object(item);
    if ispair(item) then
        chain(true, item, sys_grbg_list)
    else
        nil
    endif
enddefine;


define lock_heap(flag);
    defaults flag true;
    if flag /== nil then
        sysgarbage()
    endif;
    chain(sys_lock_heap)
enddefine;


define unlock_heap(flag);
    defaults flag true;
    sys_unlock_heap();
    if flag /== nil then
        sysgarbage()
    endif
enddefine;


define timeform(form, n) -> (t, gc);
    lvars p, looptime;

    sysPROCEDURE(0, false);
    compile_form(form, 0);
    sysENDPROCEDURE() -> p;

    /* Calculate time taken by loop code */
    check_positive(n) ->;
    systime() -> t;
    fast_repeat n times
        fast_apply(procedure(); endprocedure)
    endfast_repeat;
    systime() fi_- t -> looptime;

    /* Now run the procedure n times */
    popgctime -> gc;
    systime() -> t;
    fast_repeat n times
        fast_apply(p)
    endfast_repeat;

    /* Calculate results */
    ((systime() fi_- t) fi_- looptime) / 100.0s0 -> t;
    (popgctime fi_- gc) / 100.0s0 -> gc;
    max(t, decimal_0) -> t;
    max(gc, decimal_0) -> gc;
enddefine;


/* Export to Lisp */

lispsynonym(@*CURRENT-STORE-SIZE*,  "popmemused");
lispsynonym(@*GC-COPY*,             "gc_copy");
lispsynonym(@*GC-HOOK*,             "gc_hook");
lispsynonym(@*GC-RATIO*,            "gc_ratio");
lispsynonym(@*GC-TIME*,             "popgctime");
lispsynonym(@*GC-VERBOSE*,          "gc_verbose");
lispsynonym(@*MAX-STORE-SIZE*,      "max_store_size");
lispsynonym(@*MIN-STORE-SIZE*,      "popminmemlim");

lisp_export(sysgarbage,     @SYSTEM:DO-GC,  [0 0 0]);
lisp_export(gc_object,      @GC-OBJECT,     [1 1 1]);
lisp_export(lock_heap,      @LOCK-HEAP,     [0 1 0]);
lisp_export(unlock_heap,    @UNLOCK-HEAP,   [0 1 0]);
lisp_export(timeform,       @SYS:TIME,      [2 2 2]);

endsection;


/* Back to Lisp */

lisp

(in-package :poplog)

(setq *constant-functions* t)


(defun GC (&key
            ((:copy *gc-copy*) *gc-copy*)
            ((:hook *gc-hook*) *gc-hook*)
            ((:verbose *gc-verbose*) *gc-verbose*))
    (sys::do-gc))


(fmakunbound 'time)


(defmacro TIME (form &optional n)
    (if n
        `(multiple-value-bind
            (#1=#:cpu #2=#:gc)
            (sys:time ',form ,n)
            (format *trace-output*
                "~&CPU TIME: ~,2F seconds~%GC  TIME: ~,2F seconds~%"
                #1# #2#)
            ,form)
        `(let* ((#2#  *gc-time*)
                (#1#  (get-internal-run-time)))
            (multiple-value-prog1
                ,form
                (setq #1# (- (get-internal-run-time) #1#)
                      #2# (- *gc-time* #2#)
                      #1# (- #1# #2#))
                (format *trace-output*
                    "~&CPU TIME: ~,2F seconds~%GC  TIME: ~,2F seconds~%"
                    (/ #1# internal-time-units-per-second)
                    (/ #2# internal-time-units-per-second))))))



#| --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Dec  1 1993
        *max-store-size* now copes with big integers, floats, and ratios.
--- John Williams, Aug 23 1993
        Missing procedure sys:time now defined.
--- John Williams, Apr 21 1992
        *max-store-size* now a -boolean_variable-
--- John Williams, Dec 11 1990
        Revised for new active -popmemlim-
 |#
