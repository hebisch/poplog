#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/control.lsp
 | Purpose:         Common Lisp control macros
 | Author:          John Williams, March 4 1987 (see revisions)
 | Documentation:   CLtL, ch 6 & 7
 | Related Files:
 |#


(defun sys:genvar () (gensym "V"))

;;; Conditionals

(export 'otherwise)     ; an alternative to T in various macros


(defmacro WHEN (test &rest actions)
    `(if ,test (progn ,@actions)))


(defmacro UNLESS (test &rest actions)
    `(if ,test nil (progn ,@actions)))


(defmacro COND (&optional ((test &rest actions) '(t nil)) &rest clauses)
    (if (endp clauses)
        (if (endp actions)
            `(values ,test)
            (if (eq test t)
                `(progn ,@actions)
                `(if ,test (progn ,@actions))))
        (if (endp actions)
            `(or ,test (cond ,@clauses))
            `(if ,test (progn ,@actions) (cond ,@clauses)))))


(defmacro TYPECASE (keyform &rest clauses &aux keyvar)
    (labels
        ((expand-clauses (clauses &aux (type (caar clauses)))
            (if (endp clauses)
                nil
                (if (or (eq type t) (eq type 'otherwise))
                    (if (endp (cdr clauses))
                       `(progn ,@(cdar clauses))
                        (sys:program-error
                            "~A clause not the last in TYPECASE form" type))
                   `(if (typep ,keyvar ',type)
                        (progn ,@(cdar clauses))
                       ,(expand-clauses (cdr clauses)))))))
        (if (atom keyform)
            (progn
                (setq keyvar keyform)
                (expand-clauses clauses))
            (progn
                (setq keyvar (sys:genvar))
                `(let ((,keyvar ,keyform))
                     ,(expand-clauses clauses))))))


(defmacro CASE (keyform &rest clauses &aux keyvar)
    (labels
        ((expand-clauses (clauses &aux (keys (caar clauses)))
            (if (endp clauses)
                nil
                (if (or (eq keys t) (eq keys 'otherwise))
                    (if (endp (cdr clauses))
                       `(progn ,@(cdar clauses))
                        (sys:program-error
                            "~A clause not the last in CASE form" keys))
                    (if (listp keys)
                       `(if (member ,keyvar ',keys)
                            (progn ,@(cdar clauses))
                           ,(expand-clauses (cdr clauses)))
                       `(if (eql ,keyvar ',keys)
                            (progn ,@(cdar clauses))
                           ,(expand-clauses (cdr clauses))))))))
    (if (atom keyform)
        (progn
            (setq keyvar keyform)
            (expand-clauses clauses))
        (progn
            (setq keyvar (sys:genvar))
            `(let ((,keyvar ,keyform))
                 ,(expand-clauses clauses))))))


;;; "Progs"

(defmacro PROG1 (result-form &body forms)
    `(multiple-value-prog1 (values ,result-form) ,@forms))


(defmacro PROG2 (form result-form &body forms)
    `(progn ,form (prog1 ,result-form ,@forms)))


(defmacro PROG (varslist &body #(body decs))
    `(block nil (let ,varslist ,@decs (tagbody ,@body))))


(defmacro PROG* (varslist &body #(body decs))
    `(block nil (let* ,varslist ,@decs (tagbody ,@body))))


;;; Iteration

(defmacro RETURN (&rest results)
    `(return-from nil ,@results))


(defmacro LOOP (&body forms)
    `(block nil (tagbody #1=#:tag ,@forms (go #1#))))


(defmacro DOLIST ((var list &optional result) &body #(forms decs))
    `(block nil
        (let* ((#1=#:list ,list) ,var)
            (declare (inline pop))
            ,@decs
            (tagbody
                #2=#:tag
                (if #1#
                    (progn
                        (setq ,var (pop #1#))
                        (tagbody ,@forms)
                        (go #2#))))
             ,@(if result `((setq ,var nil)))
             ,result)))


(defmacro DOTIMES ((var limit &optional result) &body #(forms decs))
    `(block nil
        (let* ((#1=#:limit ,limit) (,var 0))
            ,@decs
            (tagbody
                #2=#:tag
                (if (,(if (sys:fast) 'sys:fix< '<) ,var #1#)
                    (progn
                        (tagbody ,@forms)
                        (setq ,var (,(if (sys:fast) 'sys:fix+ '+) ,var 1))
                        (go #2#))))
            ,result)))


(defun sys:check-do-vars (forms type &aux form var inits steps)
    (loop
        (if (null forms)
            (return))
        (if (atom forms)
            (sys:program-error
                "Malformed var/init/step specifier in ~S form" type forms))
        (setq form (car forms) forms (cdr forms))
        (if (atom form)
            (setq inits `(,.inits ,form))
            (progn
                (setq var (car form)
                      form (cdr form)
                      inits `(,.inits (,var ,(car form)))
                      form (cdr form))
                (if (consp form)
                    (setq steps `(,.steps ,var ,(car form))
                          form (cdr form)))
                (if (consp form)
                    (sys:program-error
                        "Excess forms in ~S var/init/step specifier"
                        type form)))))
    (values inits steps))


(defmacro DO (vars (end-test &rest results) &body #(body decs))
    (multiple-value-bind
      (inits steps)
      (sys:check-do-vars vars 'do)
        `(block nil
            (let ,inits
                (declare (inline psetq))
                ,@decs
                (tagbody
                    #1=#:tag
                    (if ,end-test
                        nil
                        (progn
                            (tagbody ,@body)
                            (psetq ,@steps)
                            (go #1#))))
                ,@results))))


(defmacro DO* (vars (end-test &rest results) &body #(body decs))
    (multiple-value-bind
      (inits steps)
      (sys:check-do-vars vars 'do*)
        `(block nil
            (let* ,inits
                ,@decs
                (tagbody
                    #1=#:tag
                    (if ,end-test
                        nil
                        (progn
                            (tagbody ,@body)
                            (setq ,@steps)
                            (go #1#))))
                ,@results))))


;;; Multiple value handling

(defmacro MULTIPLE-VALUE-LIST (form)
    `(multiple-value-call 'list ,form))


(defmacro MULTIPLE-VALUE-BIND (varslist values-form &body body)
    `(multiple-value-call
        #'(lambda (&optional ,@varslist &rest sys:temp) ,@body)
        ,values-form))


(defmacro MULTIPLE-VALUE-SETQ (varslist values-form &aux arglist setlist temp)
    (dolist (var varslist)
        (setq temp (sys:genvar))
        (setq arglist (cons temp arglist))
        (setq setlist (cons var (cons temp setlist))))
    `(multiple-value-call
        #'(lambda (&optional ,@(sys:nreverse-list arglist) &rest sys:temp)
            (setq ,@setlist))
        ,values-form))


(defmacro NTH-VALUE (n form)
    (if (eq n 0)
        `(values ,form)
        `(let ((#1=#:n ,n))
            (multiple-value-call #'sys:nth-value ,form #1#))))


;;; Miscellaneous

(defun MACROEXPAND (form &optional env &aux flag new-form new-flag)
    (loop
        (multiple-value-setq (new-form new-flag)
                             (macroexpand-1 form env))
        (if new-flag
            (setq form new-form flag new-flag)
            (return)))
    (values form flag))


(defun COMPILER-MACROEXPAND (form &optional env &aux flag new-form new-flag)
    (loop
        (multiple-value-setq (new-form new-flag)
                             (compiler-macroexpand-1 form env))
        (if new-flag
            (setq form new-form flag new-flag)
            (return)))
    (values form flag))


(defmacro DECLAIM (&rest forms)
    `(progn
        ,@(mapcar #'(lambda (form) `(proclaim ',form)) forms)
        (values)))



#| --- Revision History ---------------------------------------------------
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        Added NTH-VALUE (Steele 2 p184).
--- John Williams, Feb 24 1994
        LOCALLY is now a special form, also added macro DECLAIM
            (Steele 1990 p221-3).
 |#
