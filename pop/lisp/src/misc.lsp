#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/misc.lsp
 | Purpose:         Miscellaneous Common Lisp functions
 | Author:          John Williams, Feb  1 1988 (see revisions)
 | Documentation:   CLtL, p438-443
 | Related Files:   C.all/lisp/src/time.p
 |#


(defun APPLYHOOK (&rest args)
    (sys:warn-wi
        "Poplog does not have an interpreter: APPLYHOOK not implemented"
        (cons 'applyhook args)))


(defun FIND-ALL-SYMBOLS (string &aux list)
    (if (symbolp string)
        (setq string (symbol-name string)))
    (dolist (pkg (list-all-packages) list)
            (multiple-value-bind
                (sym found) (find-symbol string pkg)
                (if found (pushnew sym list)))))


(defun APROPOS (string &optional pkg)
    (flet
        ((pr-sym (sym pkg &aux (*package* (or pkg #.(find-package "EMPTY"))))
            (prin1 sym))
         (sym< (x y &aux (px (symbol-package x)) (py (symbol-package y)))
            (if (eq px py)
                (string< x y)
                (string<
                    (if px (package-name px) "#:")
                    (if py (package-name py) "#:")))))
        (dolist (sym (sort (apropos-list string pkg) #'sym<) (values))
            (pr-sym sym pkg)
            (if (and (boundp sym) (not (keywordp sym)))
                (format t " (value: a ~S)" (type-of (symbol-value sym))))
            (if (fboundp sym)
                (if (special-operator-p sym)
                    (princ " (special-form)")
                    (if (macro-function sym)
                        (princ " (macro)")
                        (princ " (function)"))))
            (terpri))))


(defun COMPILE (name &optional def)
    (if def
        (progn
            (setq def (coerce def 'function))
            (if name
                (progn
                    (setf (fdefinition name) def)
                    name)
                def))
        (if (fboundp name)
            name
            (error 'undefined-function :operation 'compile :name name))))


(defun COMPILE-FILE (&rest args)
    (let ((*package *package*))
        (sys:warn-wi
            "Poplog is incrementally compiled: doing nothing"
            (cons 'compile-file args))))


(defvar *COMPILE-FILE-PATHNAME* nil)
(defvar *COMPILE-FILE-TRUENAME* nil)
(defvar *COMPILE-PRINT* nil)
(defvar *COMPILE-VERBOSE* nil)

(export '(*compile-file-pathname* *compile-file-truename*
          *compile-print* *compile-verbose*)
        "COMMON-LISP")


(defun SAVELISP (file &key init lock share)
    (sys:savelisp file init (or lock share) share))


(defmacro STEP (form)
    (warn "Poplog does not have an interpreter: STEP not implemented")
    form)


(defmacro TIME (form)
    `(let* ((#1=#:cpu  (get-internal-run-time)))
        (multiple-value-prog1
            ,form
            (setq #1# (- (get-internal-run-time) #1#))
            (format *trace-output*
                "~&CPU TIME: ~,2F seconds~%"
                (/ #1# internal-time-units-per-second)))))


(defmacro TRACE (&rest forms)
    `(sys:trace ',forms))


(defmacro UNTRACE (&rest forms)
    `(sys:untrace ',forms))


(defmacro WITH-COMPILATION-UNIT (options &body forms)
    (warn "WITH-COMPILATION-UNIT same as PROGN in Poplog")
    `(progn ,@forms))



#| --- Revision History ---------------------------------------------------
--- John Williams, Jun  6 1995
        Added dummy version of WITH-COMPILATION-UNIT.
--- John Williams, Jun  5 1995
        Deleted function COMPILE-FILE-PATHNAME ('cos not in Steele 1990).
        Added variables *compile-file-pathname*, *compile-file-truename*,
        *compile-print*, and *compile-verbose*.
--- John Williams, Apr 12 1995
        Added COMPILE-FILE-PATHNAME.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Jun  7 1994
        EMPTY package now created with DEFPACKAGE.
--- John Williams, Apr 26 1994
        COMPILE upgraded for Steele 1990 p677.
        Non-inlining by TRACE now done in Pop.
 |#
