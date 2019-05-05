#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/conditions.lsp
 | Purpose:         Common Lisp condition handling system
 | Author:          John Williams, Feb 27 1995 (see revisions)
 | Documentation:   Steele 1990, ch 29
 | Related Files:   C.all/lisp/src/errors.lsp, C.all/lisp/src/errors.p
 |#


;;; Creating condition handlers

(defvar SYS:*HANDLER-BINDINGS* nil)

(defvar SYS:*SPECIAL-BLOCKS* nil)

(defvar SYS:*SPECIAL-BLOCK-TEMP* nil)


(defmacro SYS:SPECIAL-BLOCK (name &body body)
    `(let ((sys:*special-blocks* (cons ',name sys:*special-blocks*)))
        (block ,name ,@body)))


(defmacro SYS:RETURN-FROM-SPECIAL-BLOCK (name &body body)
    `(if (setq sys:*special-block-temp* (sys:eq-member ',name
                                                       sys:*special-blocks*))
         (progn
            (setq sys:*special-blocks* sys:*special-block-temp*)
            (return-from ,name (progn ,@body)))
         (sys:control-error
            "Cannot exit from non-enclosing block ~S" ',name)))


(defun SYS:MAKE-HANDLER-BIND-FORM (new-bindings body)
    `(let ((sys:*handler-bindings* (cons (list ,@new-bindings) sys:*handler-bindings*)))
            ,@body))


(defmacro HANDLER-BIND ((&rest bindings) &body body)
    (sys:make-handler-bind-form
          (loop
            for b in bindings
            unless (sys:2-list b) do
                (sys:program-error "Malformed (type handler) binding in HANDLER-BIND form" b)
            end
            collect `(cons ',(car b) ,(cadr b)))
        body))


(defmacro HANDLER-CASE (expression &rest cases
                        &aux binding bindings (b (gensym "HANDLER-CASE-")))
    (setq bindings
        (loop
            for c in cases
            if (and (consp c) (eq (car c) :no-error))
            do (progn
                    (setq expression
                        `(multiple-value-call #'(lambda ,@(cdr c)) ,expression))
                    (loop-finish))
            else
            do (destructuring-bind
                (type (&optional (var '#:dummy)) &rest body)
                c
                  (setq binding
                    `(cons ',type
                           #'(lambda (,var)
                                (sys:return-from-special-block ,b ,@body)))))
            end
            collect binding))

    `(sys:special-block ,b
        ,(sys:make-handler-bind-form bindings (list expression))))


(defmacro DEFINE-CONDITION (name parent-types &optional slots &rest options)
    (let (doc report)
        (dolist (option options)
            (if (sys:2-list option)
                (case (car option)
                    (:documentation
                        (setq doc (cadr option)))
                    (:report
                        (setq report (cadr option)))
                    (t
                        (sys:program-error
                            "Unrecognised option in DEFINE-CONDITION form"
                            option)))
                (sys:program-error
                    "DEFINE-CONDITION option not a 2-list" option)))
        `(let ((#1=#:class
                 (defclass ,name ,(or parent-types '(condition))
                    ,slots
                    ,@(if doc `((:documentation ,doc))))))
            ,@(if report
                `((defmethod PRINT-OBJECT ((#2=#:condition ,name) #3=#:stream)
                    (if *print-escape*
                        (call-next-method)
                        ,(if (stringp report)
                             `(write-string ,report #3#)
                             `(funcall #',report #2# #3#))))))
            #1#)))


;;; Define built-in condition types

(defclass CONDITION () ())


(define-condition SIMPLE-CONDITION (condition)
    ((:string :initarg :format-string :initform nil
        :accessor simple-condition-format-control)
     (:args :initarg :format-arguments :initform ()
        :accessor simple-condition-format-arguments))
    (:report
        (lambda (c s &aux string)
            (if (setq string (slot-value c :string))
                (apply #'format s string (slot-value c :args))
                (call-next-method)))))


(define-condition SERIOUS-CONDITION (condition))


(define-condition ERROR (serious-condition))


(defconstant SYS:MISHAP-SLOTS
    '((:message   :initform ""  :initarg :message)
      (:involving :initform nil :initarg :involving)))


(defun SYS:REPORT-MISHAP (stream message involving)
    (let ((*print-escape* t))
        (apply #'format stream
            "~@?~^~I~:@_INVOLVING:  ~:I~@{~:W ~:_~}" message involving)))


(defun SYS:MISHAP-REPORTER (condition stream)
    (sys:report-mishap
        stream
        (slot-value condition :message)
        (slot-value condition :involving)))


(define-condition POPLOG:MISHAP (error)
    #.sys:mishap-slots
    (:report sys:mishap-reporter))


(define-condition SIMPLE-ERROR (simple-condition error))

(define-condition PARSE-ERROR (error))

(define-condition ARITHMETIC-ERROR (error)
    ((:operation :initarg :operation :accessor arithmetic-error-operation)
     (:operands :initarg :operands :accessor arithmetic-error-operands))
    (:report
        (lambda (c s)
            (sys:report-mishap s "Error in arithmetic operation ~S"
                (cons (slot-value c :operation) (slot-value c :operands))))))


(define-condition DIVISION-BY-ZERO (arithmetic-error)
    ((:operation :initarg :operation :initform '/))
    (:report
        (lambda (c s)
            (apply #'format
                    s "Cannot divide ~S by ~S" (slot-value c :operands)))))


(define-condition FLOATING-POINT-OVERFLOW (arithmetic-error)
    ()
    (:report
        (lambda (c s)
            (sys:report-mishap s "Floating-point overflow"
                               (slot-value c :operands)))))


(define-condition FLOATING-POINT-UNDERFLOW (arithmetic-error))

(define-condition FLOATING-POINT-INEXACT (arithmetic-error))

(define-condition FLOATING-POINT-INVALID-OPERATION (arithmetic-error))


(define-condition CELL-ERROR (error)
    ((:name :initarg :name :accessor cell-error-name)
     (:operation :initarg :operation :initform :access)
     (:description :initform "cell")
     (:object :initform nil))
    (:report
        (lambda (c s &aux (obj (slot-value c :object)))
            (sys:report-mishap s
                "Cannot ~(~A~) ~A ~S"
                (list* (slot-value c :operation)
                       (slot-value c :description)
                       (slot-value c :name)
                       (if obj (list obj)))))))


(define-condition UNBOUND-VARIABLE (cell-error)
    ((:description :initform "unbound variable" :allocation :class)
     (:object :initform nil :allocation :class)))


(define-condition UNDEFINED-FUNCTION (cell-error)
    ((:description :initform "undefined function" :allocation :class)
     (:object :initform nil :allocation :class)))

(define-condition POPLOG:APPLYING-SPECIAL-FORM (undefined-function)
    ((:description :initform "special form")))


(define-condition UNBOUND-SLOT (cell-error)
    ((:description :initform "unbound slot" :allocation :class)
     (:object :initarg :object)))


(define-condition POPLOG:MISSING-SLOT (cell-error)
    ((:description :initform "non-existent slot" :allocation :class)
     (:object :initarg :object)))


(define-condition POPLOG:REDEFINE-ERROR (error)
    ((:name :initarg :name)
     (:type :initarg :type))
    (:report
        (lambda (c s &aux (type (slot-value c :type)))
            (sys:report-mishap s
                (if (eq type 'setf)
                    "Redefining ~(~A~) method for ~S"
                    "Redefining ~(~A~) ~S")
                (list type (slot-value c :name))))))


(define-condition PROGRAM-ERROR (error)
    #.sys:mishap-slots
    (:report sys:mishap-reporter))


(define-condition CONTROL-ERROR (error)
    #.sys:mishap-slots
    (:report sys:mishap-reporter))

(define-condition PRINT-NOT-READABLE (error)
    #.sys:mishap-slots
    (:report sys:mishap-reporter))

(define-condition FILE-ERROR (error)
    #.`((pathname :initarg :pathname :accessor file-error-pathname)
        ,@sys:mishap-slots)
    (:report sys:mishap-reporter))


(define-condition PACKAGE-ERROR (error)
    #.`((package :initarg :package :accessor package-error-package)
        ,@sys:mishap-slots)
    (:report
        (lambda (c s)
            (let ((*package* #.(find-package "EMPTY")))
                (sys:mishap-reporter c s)))))


(define-condition STREAM-ERROR (error)
    #.`((stream :initarg :stream :accessor stream-error-stream)
        ,@sys:mishap-slots)
    (:report sys:mishap-reporter))

(define-condition READER-ERROR (parse-error stream-error))


(define-condition END-OF-FILE (stream-error)
    ((:message :initform "Unexpected end of input")
     (:involving :initform nil))
    (:report
        (lambda (c s)
            (sys:report-mishap s
                (slot-value c :message)
                (if (slot-boundp c 'stream)
                    (list (slot-value c 'stream)))))))


(define-condition POPLOG:CLOSED-STREAM (stream-error)
    ((:message :initarg :message)
     (:operation :initform 'read :initarg :operation))
    (:report
        (lambda (c s)
            (if (slot-boundp c :message)
                (sys:report-mishap s (slot-value c :message)
                                     (slot-value c :involving))
                (sys:report-mishap s "Cannot ~(~A~) closed stream"
                                   (list (slot-value c :operation)
                                         (slot-value c 'stream)))))))


(define-condition TYPE-ERROR (error)
    ((:datum :initarg :datum :accessor type-error-datum)
     (:type :initarg :expected-type :accessor type-error-expected-type))
    (:report
     (lambda (c s)
        (let ((item (slot-value c :datum))
              (type (slot-value c :type)))
            (if (atom type)
                (sys:report-mishap s "~@(~A~) needed" (list type item))
                (case (car type)
                    (or
                        (sys:report-mishap s
                            "~@(~{~#[~;~S needed~;~S or ~:;~S, ~]~}~)"
                            (list (cdr type) item)))
                    (member
                        (sys:report-mishap s
                            "Object eql to ~{~#[~;~S needed~;~S or ~:;~S, ~]~}"
                            (list (cdr type) item)))
                    (t
                        (sys:report-mishap s
                            "Object of type ~S needed"
                            (list type item)))))))))


(define-condition POPLOG:BAD-NAME-ERROR (type-error)
    ((:type :initform 'symbol)
     (:kind :initarg :kind :initform 'variable))
    (:report
        (lambda (c s &aux (kind (slot-value c :kind)))
            (if (eq kind 'package)  ;;; yukky kludge
                (setf (slot-value c :type) '(or string symbol)))
            (sys:report-mishap s "Invalid ~(~A~) name"
                               (list kind (slot-value c :datum))))))


(define-condition SIMPLE-TYPE-ERROR (simple-condition type-error))


(define-condition STORAGE-CONDITION (serious-condition)
    ((:message :initarg :message :initform "Storage problem"))
    (:report
        (lambda (c s) (write-string (slot-value c :message) s))))


(define-condition WARNING (condition))

(define-condition STYLE-WARNING (warning))

(define-condition POPLOG:WARNING-WITH-INVOLVING (warning)
    #.sys:mishap-slots
    (:report
        (lambda (c s)
            (apply #'format s "~@?~^~%;;; Involving: ~@{ ~:S~}"
                (slot-value c :message) (slot-value c :involving)))))


(define-condition SIMPLE-WARNING (simple-condition warning))


;;; The function SIGNAL


(defvar *BREAK-ON-SIGNALS* nil)

(export '*BREAK-ON-SIGNALS* "COMMON-LISP")


(defun MAKE-CONDITION (type &rest slot-inits)
    (unless (subtypep type 'condition)
        (error "The type ~S is not a sub-type of CONDITION" type))
    (values (apply #'make-instance type slot-inits)))


(defconstant SYS:ROM-STORAGE-CONDITION
    (make-condition 'storage-condition :message "Run out of memory"))


(defconstant SYS:RLE-STORAGE-CONDITION
    (make-condition 'storage-condition :message "Recursion level exceeded"))


(defun SYS:CHECKR-CONDITION (default-type datum args)
    (typecase datum
        (string
            (make-condition default-type
                :format-string datum :format-arguments args))
        (function
            ; from formatter
            (make-condition default-type
                :format-string datum :format-arguments args))
        (symbol
            (apply #'make-condition datum args))
        (class
            (apply #'make-condition datum args))
        (condition
            (if args
                (cerror "Ignore the excess arguments"
                    "Can not supply extra args when datum is a condition"))
            datum)
        (t
            (sys:type-error datum '(or string symbol class condition)))))



(defun SIGNAL (datum &rest args)
    (let ((cond (sys:checkr-condition 'simple-condition datum args)))
        (if (and *break-on-signals* (typep cond *break-on-signals*))
            (break cond))
        (let ((bindings sys:*handler-bindings*))
            (locally (declare (inline car cdr))
                (loop
                    (unless (consp bindings) (return))
                    (setq list (car bindings) bindings (cdr bindings))
                    (let ((sys:*handler-bindings* bindings))
                        (dolist (b list)
                            (if (typep cond (car b))
                                (funcall (cdr b) cond))))))))
    nil)


;;; Restarts


(in-package "SYSTEM")

(cl:defstruct LISP:RESTART
    name
    function
    (test-function #'sys:always-t)
    interactive-function
    report-function)

(cl:in-package "COMMON-LISP")

(setf (symbol-function 'cl:restart-name) (symbol-function 'sys:restart-name))


(defmethod PRINT-OBJECT ((r restart) stream &aux f)
    (if (or *print-escape* (not (setq f (sys:restart-report-function r))))
        (call-next-method)
        (funcall f stream)))


(defvar SYS:*RESTART-BINDINGS* nil)


(defmacro RESTART-BIND (bindings &body body)
    (let ((make-restart-forms
           (loop for (name function . keys&vals) in bindings
                 collect `(sys:make-restart
                            :name ',name :function ,function ,@keys&vals))))
        `(let ((sys:*restart-bindings*
                (cons (list ,@make-restart-forms) sys:*restart-bindings*)))
            ,@body)))


(defconstant SYS:CONDITION-RESTART-TABLE
    (make-hash-table :test 'eq :size 64
        :rehash-size 20 :rehash-threshold 1 :weak :key))


(defmacro SYS:CONDITION-RESTARTS (condition)
    `(gethash ,condition sys:condition-restart-table))


(defmacro SYS:SET-CONDITION-RESTARTS (new condition)
    ;; save on code size by not using SETF
    `(sys:update-gethash ,new ,condition sys:condition-restart-table))


(defmacro WITH-CONDITION-RESTARTS (condition-form restarts-form &body body)
    `(let ((#1=#:condition ,condition-form)
           (#2=#:restarts ,restarts-form))
        (let ((#3=#:save (sys:condition-restarts #1#)))
            (unwind-protect
                ;; main form
                (progn
                    (sys:set-condition-restarts (append #2# #3#) #1#)
                    ,@body)
                ;; clean-up form
                (sys:set-condition-restarts #3# #1#)))))


(defmacro SYS:APPLICABLE-RESTART (restart &optional (condition nil))
    `(funcall (sys:restart-test-function ,restart) ,condition))


(defmacro SYS:DO-RESTARTS ((var &optional name) &body loop-clause)
    `(let ((#1=#:name ,name))
        (loop
            named sys:do-restarts
            for #2=#:list in sys:*restart-bindings* do
            (loop
                for ,var in #2#
                ,@(if name
                    `(if (eq (restart-name ,var) #1#)
                      do ,@loop-clause)
                    loop-clause)))))


(defun COMPUTE-RESTARTS (&optional condition)
    (if condition
        (loop
            for r in (sys:condition-restarts condition)
            if (sys:applicable-restart r condition)
               collect r)
        (sys:do-restarts (r)
            if (sys:applicable-restart r)
            collect r)))


(defun FIND-RESTART (restart-id &optional condition &aux c)
    (flet ((bad-arg (x)
            (error 'simple-type-error
                :format-string "Illegal argument to FIND-RESTART: ~S"
                :format-arguments (list x)
                :expected-type '(or (and symbol (not null)) restart)
                :datum x)))
        (typecase restart-id
            (null
                (bad-arg restart-id))
            (symbol
                (if condition
                    (dolist (r (sys:condition-restarts condition))
                        (if (sys:applicable-restart r condition)
                            (return-from find-restart r)))
                    (sys:do-restarts (r restart-id)
                        if (sys:applicable-restart r)
                        do (return-from sys:do-restarts r))))
            (restart
                (sys:do-restarts (r)
                    if (eq r restart-id)
                    do (return-from sys:do-restarts r)))
            (t
               (bad-arg restart-id)))))


(defun INVOKE-RESTART (restart-id &rest args &aux r)
    (unless (setq r (find-restart restart-id))
        (error "No restart named ~S" restart-id))
    (apply (sys:restart-function r) args))


(defun INVOKE-RESTART-INTERACTIVELY (restart-id &aux r f)
    (unless (setq r (find-restart restart-id))
        (error "No restart named ~S" restart-id))
    (if (setq f (sys:restart-interactive-function r))
        (apply (sys:restart-function r) (funcall f))
        (funcall (sys:restart-function r))))


(defmacro WITH-SIMPLE-RESTART ((name format-string &rest format-args)
                               &body body)
    `(block #1=#:simple-restart
        (restart-bind
            ((,name
              #'(lambda () (return-from #1# (values nil t)))
              :report-function
              #'(lambda (#2=#:stream)
                    (format #2# ,format-string ,@format-args))))
            ,@body)))


(defmacro RESTART-CASE (form &rest cases
                        &aux bindings (b (gensym "RESTART-CASE-")) s)
    (setq bindings
        (loop
            with test and inter and report and binding
            for c in cases do
                (destructuring-bind (name arglist &rest rest) c
                    (loop
                        (case (car rest)
                            (:test
                                (setq test (cadr rest) rest (cddr rest)))
                            (:interactive
                                (setq inter (cadr rest) rest (cddr rest)))
                            (:report
                                (setq report (cadr rest) rest (cddr rest))
                                (if (stringp report)
                                    (setq report
                                        `(lambda (stream)
                                            (write-string ,report stream)))))
                            (t (return))))
                    (setq binding
                        `(,name
                          #'(lambda ,arglist
                                (sys:return-from-special-block ,b ,@rest))
                          ,@(if test `(:test-function #',test))
                          ,@(if inter `(:interactive-function #',inter))
                          ,@(if report `(:report-function #',report))
                         )))
                collect binding))

    (if (and (consp form)
             (sys:eq-member (setq s (car form))
                            '(signal invoke-debugger error cerror warn)))
        `(sys:special-block ,b
            (let* ((#1=#:condition
                   ,(if (eq s 'cerror) (third form) (second form))))
                (restart-bind ,bindings
                    (with-condition-restarts
                      #1#
                      (car sys:*restart-bindings*)
                        (,s
                         ,@(if (eq s 'cerror) (list (second form)))
                         #1#
                         ,@(if (eq s 'cerror) (cdddr form) (cddr form)))))))
        `(sys:special-block ,b
            (restart-bind ,bindings ,form))))



#| --- Revision History ---------------------------------------------------
--- John Williams, May 15 1995
        handler-case & restart-case now use `special blocks'.
--- John Williams, May 10 1995
        Fixed bug in signal, and added new macro sys:set-condition-restarts.
--- John Williams, Mar 30 1995
        end-of-file and poplog:closed-stream condition types added.
--- John Williams, Mar 20 1995
        Association of restarts with a condition now implemented via the
        hash-table sys:condition-restart-table.
--- John Williams, Mar 17 1995
        Added slots and report methods for arithmetic-error (and sub-types),
        and storage-condition.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Mar  2 1995
        sys:applicable-restart now uses eq.
 |#
