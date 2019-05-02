#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/errors.lsp
 | Purpose:         Common Lisp error signalling macros
 | Author:          John Williams, May 29 1987 (see revisions)
 | Documentation:   CLtL, p435-437
 | Related Files:   C.all/lisp/src/errors.p
 |#


(defun ABORT (&optional condition &aux r)
    (if (setq r (find-restart 'abort condition))
        (invoke-restart r)))


(defun CONTINUE (&optional condition &aux r)
    (if (setq r (find-restart 'continue condition))
        (invoke-restart r)))


(defun MUFFLE-WARNING (&optional condition &aux r)
    (if (setq r (find-restart 'muffle-warning condition))
        (invoke-restart r)))


(defun STORE-VALUE (value &optional condition &aux r)
    (if (setq r (find-restart 'store-value condition))
        (invoke-restart r value)))


(defun USE-VALUE (value &optional condition &aux r)
    (if (setq r (find-restart 'use-value condition))
        (invoke-restart r value)))


(defmacro IGNORE-ERRORS (&body forms)
    `(block #1=#:ignore-errors
        (handler-bind
          ((error #'(lambda (c) (return-from #1#(values nil c)))))
            ,@forms)))


(defvar *DEBUGGER-HOOK* nil)

(export '*DEBUGGER-HOOK* "COMMON-LISP")


(defun INVOKE-DEBUGGER (poplog:*debugger-condition* &key caller)
    (symbol-macrolet ((c poplog:*debugger-condition*))
        (if (functionp *debugger-hook*)
            (let* ((save *debugger-hook*) (*debugger-hook* nil))
                (funcall save c save)))
        (sys:invoke-debugger
            (concatenate 'string
                "(" (string-capitalize (or caller (type-of c))) ")"))))


(defun BREAK (&optional (datum "") &rest args &aux caller)
    (if (consp datum)
        ;; Poplog specific hack for Trace Entry/Exit Breaks
        (setq caller (cdr datum) datum (car datum)))
    (restart-case (invoke-debugger
                    (sys:checkr-condition 'simple-condition datum args)
                    :caller caller)
        (continue () :report "Return from break"))
    nil)


(defconstant SYS:INTERRUPT-CONDITION
    (make-condition 'simple-condition :format-string "Keyboard interrupt"))


(defparameter POPLOG:*INTERRUPT*
    #'(lambda ()
        (if (sys:break-ok *break-on-interrupts*)
            (if (find-restart 'continue sys:interrupt-condition)
                (invoke-debugger sys:interrupt-condition :caller :interrupt)
                (restart-case
                    (invoke-debugger sys:interrupt-condition :caller :interrupt)
                    (continue () :report "Continue computation")))
            (setlisp))))


(defun SYS:REPORT-CONDITION (condition &optional (iserr t))
    (sys:report-error
        condition
        nil
        iserr
        (sys:break-ok (if iserr poplog:*break-on-errors*
                                poplog:*break-on-warnings*))))


(defun WARN (datum &rest args)
    (let ((cond (sys:checkr-condition 'simple-warning datum args)))
        (restart-case (signal cond)
            (muffle-warning ()
                :report "Return from WARN"
                (return-from warn nil)))
        (if (sys:report-condition cond nil)
            (restart-case (invoke-debugger cond :caller :warning)
                (continue () :report "Return from break"))))
    nil)


(defun ERROR (datum &rest args)
    (let ((cond (sys:checkr-condition 'simple-error datum args)))
        (signal cond)
        (if (sys:report-condition cond)
            (invoke-debugger cond :caller 'error)))
    (sys:try-quit-to-top-level)
    nil)  ;; Dummy result so that ERROR has nresults 1


(defun CERROR (cstring datum &rest args)
    (let ((cond (sys:checkr-condition 'simple-error datum args)))
        (restart-case (signal cond)
            ;; Allow early exit from cerror if user sets a handler for cond
            (continue () (return-from cerror nil)))
        (if (sys:report-condition cond)
            (progn
                (restart-case (invoke-debugger cond :caller 'cerror)
                    (continue ()
                        :report (lambda (s) (apply #'format s cstring args))))
                nil)
            (sys:try-quit-to-top-level))))


(defvar POPLOG::*ASSERT-PLACES* nil)

(defvar POPLOG::*TEST-FORM* nil)

(defvar POPLOG::*TEST-VALUE* nil)

(export
    '(poplog::*assert-places*
      poplog::*test-form*
      poplog::*test-value*)
    :poplog)


(defun SYS:READ-VALUE (&optional (stream *debug-io*)
                                 (poplog:*read-prompt* "New value: "))
       (values (eval (read stream))))


(defun SYS:LIST-READ-VALUE ()
    (list (sys:read-value)))


(defun SYS:DO-ECASE-CLAUSES (clauses block type errform
                             &aux (key (caar clauses)))
    (if (or (eq key t) (eq key 'otherwise))
        (sys:program-error "Misplaced ~A clause" key (car clauses)))
    (if (endp clauses)
        errform
        (let ((test (if type 'typep (if (listp key) 'member 'eql))))
            (if block
                `(if (,test poplog:*test-value* ',key)
                    (return-from ,block (progn ,@(cdar clauses)))
                    ,(sys:do-ecase-clauses (cdr clauses) block type errform))
                `(if (,test poplog:*test-value* ',key)
                    (progn ,@(cdar clauses))
                    ,(sys:do-ecase-clauses (cdr clauses) block type errform))))))


(defmacro ECASE (key &rest clauses)
    `(let ((poplog:*test-form* ',key)
           (poplog:*test-value* ,key))
        ,(sys::do-ecase-clauses
            clauses nil nil
            `(sys:type-error poplog:*test-value*
                             '(member ,@(mapcar #'car clauses))))))


(defmacro ETYPECASE (key &rest clauses &aux err)
    `(let ((poplog:*test-form* ',key)
           (poplog:*test-value* ,key))
        ,(sys::do-ecase-clauses
            clauses nil t
            `(sys:type-error poplog:*test-value*
                             '(or ,@(mapcar #'car clauses))))))


(defun SYS:DO-CHECK-TYPE (place value type &optional string)
    (restart-case
        (error
            (if string
                (make-condition 'simple-type-error
                    :datum value
                    :expected-type type
                    :format-string string
                    :format-arguments (list value))
                (make-condition 'type-error
                    :datum value
                    :expected-type type)))
        (store-value (new-value)
            :report
            (lambda (#1=#:stream)
                (format #1# "Supply a new value for ~S" place))
            :interactive
            sys:list-read-value
            ; body
            new-value)))


(defmacro CCASE (key &rest clauses &aux (block (gensym "B")))
    (let ((items (mapcar #'car clauses)))
        `(block ,block
            (let ((poplog:*test-form* ',key)
                  (poplog:*test-value* ,key))
                 (tagbody
                    #1=#:tag
                    ,(sys::do-ecase-clauses
                        clauses block nil
                        `(progn
                            (setq poplog:*test-value*
                                (sys:do-check-type
                                    poplog:*test-form*
                                    poplog:*test-value*
                                    '(member ,@items)))
                            (setf ,key poplog:*test-value*)))
                    (go #1#))))))


(defmacro CTYPECASE (key &rest clauses &aux (block (gensym "B")))
    (let ((items (mapcar #'car clauses)))
        `(block ,block
            (let ((poplog:*test-form* ',key)
                 (poplog:*test-value* ,key))
                (tagbody
                    #1=#:tag
                    ,(sys::do-ecase-clauses
                        clauses block t
                        `(progn
                            (setq poplog:*test-value*
                                (sys:do-check-type
                                    poplog:*test-form*
                                    poplog:*test-value*
                                    '(or ,@items)))
                            (setf ,key poplog:*test-value*)))
                    (go #1#))))))


(defmacro CHECK-TYPE (place type &optional string)
    `(let ((poplog:*test-form* ',place)
           (poplog:*test-value* ,place))
        (tagbody #1=#:tag
            (unless (typep poplog:*test-value* ',type)
                (setq poplog:*test-value*
                    (sys:do-check-type
                        poplog:*test-form* poplog:*test-value*
                        ',type ,string))
                (setf ,place poplog:*test-value*)
                (go #1#)))))


(defmacro ASSERT (test-form &optional (places)
                    (datum (format nil "Assertion ~S failed" test-form))
                    &rest args)
    `(let ((poplog:*test-form* ',test-form)
           (poplog:*test-value* nil)
           (poplog:*assert-places* ',places))
        (tagbody
            #1=#:tag
            (unless ,test-form
                (restart-case
                    (error (sys:checkr-condition
                                'simple-error ,datum (list ,@args)))
                    ,@(mapcar
                        #'(lambda (place)
                            `(,(gensym 'assert) (#2=#:new-value)
                                :report
                                ,(format nil "Supply a new value for ~S" place)
                                :interactive
                                sys:list-read-value
                                ; body
                                (setf ,place #2#)))
                        places)
                    (continue ()
                        :report "You will be prompted for new values"
                        ,@(mapcan
                            #'(lambda (place)
                                `((format *debug-io*
                                    "~%The value of ~S is ~S~%"
                                    ',place ,place)
                                  (if (y-or-n-p "Change it ?")
                                      (setf ,place (sys:read-value)))))
                            places)))
                    (go #1#)))))



#| --- Revision History ---------------------------------------------------
--- John Williams, Nov 28 1995
        poplog:interrupt only creates continue restart if none such
        current exists.
--- John Williams, Jul 18 1995
        break can now be given a (datum . caller) cons as first argument.
--- John Williams, May  1 1995
        CERROR now makes CONTINUE restart available around call to SIGNAL.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Feb 27 1995
        Changes for the Condition system (Steele 1990 ch 29).
--- John Williams, Feb  9 1995
        Added primitive version of IGNORE-ERRORS.
--- John Williams, Jun  7 1994
        Changes for POPLOG package.
--- John Williams, May 17 1988
        Fixed bugreport johnw.129
 |#
