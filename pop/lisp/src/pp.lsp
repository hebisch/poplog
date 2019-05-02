#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/pp.lsp
 | Purpose:         Common Lisp Pretty Printer
 | Author:          John Williams, Oct 16 1995
 | Documentation:   CLtL, ch 27
 | Related Files:   C.all/lisp/src/pp.p, C.all/lisp/src/pp-dispatch.p
 |#


(defun PPRINT-DISPATCH (object &optional (table *print-pprint-dispatch*))
    (let ((fn (sys:pprint-dispatch object table)))
        (if fn
            (values fn t)
            (values
                #'(lambda (stream object &aux (*print-pretty* nil))
                    (sys:write object stream nil nil)
                    (values))
                nil))))


(set-pprint-dispatch 't 'sys:pprint-any 1 nil)


;;;


(defmacro PPRINT-LOGICAL-BLOCK ((ss list &key prefix suffix per-line-prefix)
                                &body body)
    (if (eq ss t)
        (setq ss '*terminal-io*)
        (if (eq ss nil)
            (setq ss '*standard-output*)))

    `(let ((#1=#:stream ,ss) (#2=#:list ,list))
        (if (listp #2#)
            (sys:pprint-logical-block
                #1# #2# ,prefix ,suffix ,per-line-prefix
                (function
                    (lambda ()
                        ; Note: called twice if *PRINT-CIRCLE* is T
                        ,@body)))
            (sys:write #2# #1# nil nil))
        nil))


(defun PPRINT-NEWLINE (kind &optional stream)
    (sys:pprint-newline kind stream)
    nil)


(defun PPRINT-INDENT (rel n &optional stream)
    (sys:pprint-indent rel n stream)
    nil)


(defun PPRINT-TAB (kind colnum colinc &optional stream)
    (sys:pprint-tab kind colnum colinc stream)
    nil)


(defmacro PPRINT-POP ()
    `(sys:pprint-pop))


(defmacro PPRINT-EXIT-IF-LIST-EXHAUSTED ()
    `(sys:pprint-test-exit))


;;;


(defun SYS:PPRINT-LIST (mode *standard-output* list colon-p &aux item)
    (pprint-logical-block
      (nil list :prefix (and colon-p "(") :suffix (and colon-p ")"))
        (pprint-exit-if-list-exhausted)
        (pprint-indent :current 0)
        (loop
            (setq item (pprint-pop))
            (if (consp item)
                (sys:pprint-list mode *standard-output* item colon-p)
                (write item))
            (pprint-exit-if-list-exhausted)
            (write-char #\Space)
            (pprint-newline mode)))
    (values))


(defun PPRINT-FILL (stream list &optional (colon-p t) at-sign-p)
    (sys:pprint-list :fill stream list colon-p))


(defun PPRINT-LINEAR (stream list &optional (colon-p t) at-sign-p)
    (sys:pprint-list :linear stream list colon-p))


(defun PPRINT-TABULAR (*standard-output* list
                        &optional (colon-p t) at-sign-p (size 16)
                        &aux item)
    (pprint-logical-block
      (nil list :prefix (and colon-p "(") :suffix (and colon-p ")"))
        (pprint-exit-if-list-exhausted)
        (loop
            (setq item (pprint-pop))
            (if (consp item)
                (pprint-tabular nil item colon-p at-sign-p size)
                (write item))
            (pprint-exit-if-list-exhausted)
            (pprint-tab :section-relative 1 size)
            (pprint-newline :fill)))
    (values))
