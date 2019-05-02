#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/defs.lsp
 | Purpose:         DEFMACRO, DEFUN, DEFTYPE, DEFSETF, DEFVAR, etc.
 | Author:          John Williams, Feb 26 1986 (see revisions)
 | Documentation:   CLtL, cp 5 & 7
 | Related Files:   C.all/lisp/src/defun.p, C.all/lisp/src/defsetf.p
 |#

(proclaim '(special sys::temp))

(proclaim '(ftype (function (&optional t &rest t) null) break))
(proclaim '(ftype (function (t &rest t) null) error warn))
(proclaim '(ftype (function (t t &rest t) null) cerror))


;;; First need to hand build the macro DEFMACRO !

(sys::update-macro-function
    (eval
        `(function
            ((defmacro . macro) (form env)
                ,(sys::make-destructuring-form
                    'form
                    'env
                    nil
                    'defmacro
                    '(name lambda-list &body #(body decs doc))
                    '()
                    '`((locally
                        (declare (ftype macro ,name))
                        (sys::update-macro-function
                            (function
                                ((,name . macro) (#1=#:callform #2=#:env)
                                    ,@(if doc `(,doc))
                                    ,(sys::make-destructuring-form
                                        '#1# '#2# nil
                                        name lambda-list decs body)))
                            ',name)
                        ',name))))))
    'DEFMACRO)


(defmacro SYS:PROGRAM-ERROR (message &rest involving)
    `(error 'program-error
        :message ,message
        ,@(if involving `(:involving (list ,@involving)))))


(defmacro SYS:CONTROL-ERROR (message &rest involving)
    `(error 'control-error
        :message ,message
        ,@(if involving `(:involving (list ,@involving)))))


(defmacro SYS:TYPE-ERROR (got expecting)
    `(error 'type-error :datum ,got :expected-type ,expecting))


(defmacro SYS:WARN-WI (message &rest involving)
    `(warn 'poplog:warning-with-involving
         :message ,message
         ,@(if involving `(:involving (list ,@involving)))))



(defmacro DESTRUCTURING-BIND (lamlist form &body #(body decs))
    (sys::make-destructuring-form form nil nil nil lamlist decs body))


(defmacro LAMBDA (&whole form) `(function ,form))


(defmacro AND (&rest forms)
    (labels ((expand-and (forms)
                (if (endp (cdr forms))
                    (car forms)
                    `(if ,(car forms) ,(expand-and (cdr forms))))))
        (if (endp forms) t
            (expand-and forms))))


(defmacro OR (&rest forms)
    (labels ((expand-or (forms)
                (if (endp (cdr forms))
                    (car forms)
                    `(if (setq sys::temp ,(car forms))
                         sys::temp
                        ,(expand-or (cdr forms))))))
        (expand-or forms)))


(defmacro DEFUN (name lambda-list &body body)
    `(locally
        (declare (ftype function ,name))
        (sys::update-fdefinition
            (function (,name ,lambda-list ,@body))
            ',name)
        ',name))


(defmacro DEFTYPE (name lambda-list &body #(body decs doc))
    `(progn
        (proclaim '(declaration ,name))
        (sys::update-type-expander
            ,(if (and (null lambda-list)
                      (null (cdr body))
                      (constantp (car body)))
                    (car body)
                    `(function
                        ((,name . type) (#1=#:typespec)
                            ,@(if doc `(,doc))
                            ,(sys::make-destructuring-form
                                '#1# nil ''*
                                name lambda-list decs body))))
            ',name)
        ',name))


(defmacro DEFSETF (name second &rest rest)
    (if (and second (symbolp second))
        (let (doc)
            (if rest
                (if (simple-string-p (car rest))
                    (setq doc (car rest))
                    (sys:program-error
                        "Excess forms after ~S in DEFSETF form" second rest)))
            `(progn
                (sys::update-setf-expander ',second ',name)
                ,@(if doc `((setf (documentation ',name 'setf) ',doc)))
                ',name))
        (destructuring-bind
          (lambda-list (store-variable) &body body)
          `(,second ,@rest)
            `(progn
                (sys::update-setf-expander
                    (cons
                        'defsetf
                        (function
                            ((,name . setf) (,store-variable ,@lambda-list)
                                ,@body)))

                    ',name)
                ',name))))


(defmacro DEFINE-SETF-METHOD (name lambda-list &body #(body decs doc))
    `(progn
        (sys::update-setf-expander
            (cons
                'define-setf-method
                (function
                    ((,name . setf) (#1=#:callform #2=#:env)
                        ,@(if doc `(,doc))
                        ,(sys::make-destructuring-form
                            '#1# '#2# nil
                            name lambda-list decs body))))
            ',name)
        ',name))


(defmacro DEFINE-COMPILER-MACRO (name lambda-list &body #(body decs doc))
    `(progn
        (sys::update-compiler-macro-function
            (function
                ((,name . compiler-macro) (#1=#:callform #2=#:env)
                     ,@(if doc `(,doc))
                     ,(sys::make-destructuring-form
                         '#1# '#2# nil
                         name lambda-list decs body)))
            ',name)
        ',name))


(defmacro DEFVAR (name &optional (value nil value-p) (doc nil doc-p))
    `(progn
        (proclaim '(special ,name))
        ,@(if value-p `((unless (boundp ',name) (set ',name ,value))))
        ,@(if doc-p `((setf (documentation ',name 'variable) ',doc)))
        ',name))


(defmacro DEFPARAMETER (name value &optional (doc nil doc-p))
    `(progn
        (proclaim '(special ,name))
        (set ',name ,value)
        ,@(if doc-p `((setf (documentation ',name 'variable) ',doc)))
        ',name))


(defmacro DEFCONSTANT (name value &optional (doc nil doc-p))
    `(progn
        (proclaim `(poplog:constant ,',name ,,value))
        ,@(if doc-p `((setf (documentation ',name 'variable) ',doc)))
        ',name))


;;; Miscellaneous utilites

(defun LISP-IMPLEMENTATION-VERSION () pop11:lispversion)


(defun LISP-IMPLEMENTATION-TYPE ()
    #.(concatenate 'string pop11:poptitle pop11:lisptitle))


(defun SYS:2-LIST (l)
    (declare (inline cdr))
    (and (consp l) (consp (setq l (cdr l))) (null (cdr l))))



#| --- Revision History ---------------------------------------------------
--- John Williams, Mar 15 1995
        Added macros for signalling typed errors.
--- John Williams, Feb 27 1995
        Added forward declarations of BREAK, CERROR, ERROR, and WARN.
--- John Williams, Jan  5 1995
        Moved definition of SYS:2-LIST into here.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Feb 24 1994
        DEFVAR, DEFPARAMETER, and DEFCONSTANT no longer evaluate the
        documentation string form, if supplied (Steele 1990 p87).
 |#
