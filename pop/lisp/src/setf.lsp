#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/setf.lsp
 | Purpose:         Common Lisp generalised variables
 | Author:          John Williams, Feb 26 1986 (see revisions)
 | Documentation:   CLtL, p93-107
 | Related Files:   C.all/lisp/src/setf.p, C.all/lisp/src/defs.lsp
 |#


(defmacro SYS:CAN-USE-SETQ (place)
    `(and (symbolp ,place) (not (sys:symbol-macro-p ,place))))


(defun SYS:MAKE-LET-LIST (vars vals &optional end-var end-val)
    (if (null vars)
        (if end-var
            (list (list end-var end-val))
            nil)
        (cons (list (car vars) (car vals))
              (sys:make-let-list (cdr vars) (cdr vals) end-var end-val))))


(defmacro PUSH (item place &environment env)
    (if (sys:can-use-setq place)
        `(setq ,place (cons ,item ,place))
        (multiple-value-bind
          (vars vals stores store-form access-form)
          (get-setf-method place env)
            (setq vars (sys:make-let-list
                            vars vals (car stores)
                            `(cons ,item ,access-form)))
            `(let* ,vars ,store-form))))


(defmacro PSETQ (&rest args &aux var vars setqs)
    (loop
        (if (endp args)
            (return `(let* ,(sys:nreverse-list vars) (setq ,@setqs) nil)))
        (if (atom (cdr args))
            (sys:program-error
                "Missing value for ~S in PSETQ form" (car args)))
        (setq var (gensym "G"))
        (push (list var (cadr args)) vars)
        (push var setqs)
        (push (car args) setqs)
        (setq args (cddr args))))


(defun SYS:MAKE-SETF-FORM (place value env &optional (return-value t)
                           &aux usym)
    (flet
        ( (No-Side-Effects (arglist)
             (dolist (arg arglist t)
                 (unless (or (and (atom arg) (not (sys:symbol-macro-p arg)))
                         (and (consp arg) (eq (car arg) 'quote)))
                     (return-from No-Side-Effects nil))))
         )
        (if (sys:can-use-setq place)
            `(setq ,place ,value)
            (if (and (setq usym (sys:setf-is-updater place))
                     (no-side-effects (cons value (cdr place))))
                (if return-value
                    `(progn
                        (,usym ,value ,@(cdr place))
                        ,value)
                    `(,usym ,value ,@(cdr place)))
                (multiple-value-bind
                  (vars vals stores store-form)
                  (get-setf-method-multiple-value place env)
                    (if (null (cdr stores))
                        (setq stores (car stores)))
                    `(let* ,(sys:make-let-list vars vals stores value)
                        ,store-form))))))


(defmacro SETF (&rest args &environment env)
    (labels
        ( (do-setf (args)
            (declare (inline cddr))
            (if (endp args)
                nil
                (if (atom (cdr args))
                    (sys:program-error "Missing value for ~S in SETF form"
                                       (car args))
                    (cons
                        (sys:make-setf-form
                            (car args) (cadr args) env (null (cddr args)))
                        (do-setf (cddr args))))))
        )
        (cons 'progn (do-setf args))))


(defmacro PSETF (&optional (place nil p-supp) (value nil v-supp) &rest rest
                 &environment env
                 &aux bindings store-forms)
    (if (null rest)
        (if (and p-supp v-supp)
            (sys:make-setf-form place value env)
            (if p-supp
                (sys:program-error "No value to assign to ~S in PSETF form"
                                   place)
                nil))
        (loop
            (multiple-value-bind
              (vars vals stores store-form access-form)
              (get-setf-method-multiple-value place env)
                (if (null (cdr stores))
                    (setq stores (car stores)))
                (setq bindings
                      (nconc bindings
                             (sys:make-let-list vars vals stores value)))
                (push store-form store-forms))
            (if (endp rest)
                (return
                    `(let* ,bindings
                        ,@(sys:nreverse-list store-forms)
                        nil)))
            (setq place (car rest) rest (cdr rest))
            (if (endp rest)
                (sys:program-error "No value to assign to ~S in PSETF form"
                                   place))
            (setq value (car rest) rest (cdr rest)))))


(defmacro DEFINE-MODIFY-MACRO (name lambda-list function
                                    &optional doc-string
                                    &aux args (rest '(())))
    (do ((ll lambda-list (cdr ll))
         (arg nil))
        ((endp ll))
        (setq arg (car ll))
        (cond
            ((eq arg '&optional)
                nil)
            ((eq arg '&rest)
                (setq rest (cdr ll)) (return))
            ((sys:eq-member arg lambda-list-keywords)
                (sys:program-error "Misplaced lambda-list keyword: ~A" arg))
            ((symbolp arg)
                (push arg args))
            ((consp arg)
                (push (car arg) args))
            (t  (return))))
    (setq args (sys:nreverse-list args))

    `(defmacro ,name (#1=#:place ,@lambda-list &environment #2=#:env)
        ,@(if doc-string `(,doc-string))
        (if (sys:can-use-setq #1#)
            `(setq ,#1# (,',function ,#1# ,,@args ,@,@rest))
            (multiple-value-bind
              (vars vals stores store-form access-form)
              (get-setf-method #1# #2#)
                `(let* ,(sys:make-let-list
                                vars vals (car stores)
                                `(,',function ,access-form ,,@args ,@,@rest))
                    ,store-form)))))


(define-modify-macro INCF (&optional (delta 1)) +)

(define-modify-macro DECF (&optional (delta 1)) -)


(defmacro PUSHNEW (item place &rest keys &environment env)
    (if (sys:can-use-setq place)
        `(setq ,place (adjoin ,item ,place ,@keys))
        (multiple-value-bind
          (vars vals stores store-form access-form)
          (get-setf-method place env)
            (setq vars (sys:make-let-list
                            vars vals (car stores)
                            `(adjoin ,item ,access-form ,@keys)))
            `(let* ,vars ,store-form))))


(defmacro POP (place &environment env)
    (if (sys:can-use-setq place)
        `(prog1 (car ,place) (setq ,place (cdr ,place)))
        (multiple-value-bind
          (vars vals stores store-form access-form)
          (get-setf-method place env)
            (setq stores (car stores))
            `(let* ,(sys:make-let-list vars vals stores access-form)
                (prog1
                    (car ,stores)
                    (setq ,stores (cdr ,stores))
                    ,store-form)))))


(defmacro REMF (place key &environment env
                          &aux (plist '#:plist)
                               (keyvar '#:key)
                               (bool '#:bool))
    (if (sys:can-use-setq place)
       `(multiple-value-bind
          (,plist ,bool)
          (sys:remf ,place ,key)
            (setq ,place ,plist)
            ,bool)
        (multiple-value-bind
          (vars vals stores store-form access-form)
          (get-setf-method place env)
            (setq stores (car stores))
            (setq vars (sys:make-let-list
                            (append vars (list keyvar stores))
                            (append vals (list key access-form))))
            `(let* ,vars
                (multiple-value-bind
                  (,stores ,bool)
                  (sys:remf ,stores ,keyvar)
                    ,store-form
                    ,bool)))))


(defmacro SHIFTF (&rest args &environment env)
    (if (or (endp args) (endp (cdr args)))
        (sys:program-error "SHIFTF expects at least 2 arguments"))
    (let ((leftmost (gensym)))
        (do ((a args (cdr a))
             (bindings nil)
             (store-forms nil)
             (stores-on-left leftmost))
            ((endp (cdr a))
              (push (list stores-on-left (car a)) bindings)
             `(let* ,(sys:nreverse-list bindings)
                 ,@(sys:nreverse-list store-forms)
                 ,leftmost))
            (multiple-value-bind
              (vars vals stores store-form access-form)
              (get-setf-method-multiple-value (car a) env)
                (do ((v vars (cdr v))
                     (f vals (cdr f)))
                    ((null v))
                    (push (list (car v) (car f)) bindings))
                (push (list stores-on-left access-form) bindings)
                (push store-form store-forms)
                (setq stores-on-left (if (cdr stores) stores (car stores)))))))


(defmacro ROTATEF (&rest args &environment env)
    (if (endp args) (return-from rotatef nil))
    (if (endp (cdr args)) (return-from rotatef `(progn ,(car args) nil)))
    (do ((a args (cdr a))
         (bindings nil)
         (store-forms nil)
         (stores-on-left nil)
         (fix-me nil))
        ((endp a)
         (rplaca fix-me stores-on-left)
         `(let* ,(sys:nreverse-list bindings)
             ,@(sys:nreverse-list store-forms) nil))
        (multiple-value-bind
          (vars vals stores store-form access-form)
          (get-setf-method-multiple-value (car a) env)
            (do ((v vars (cdr v))
                 (f vals (cdr f)))
                ((null v))
                (push (list (car v) (car f)) bindings))
            (push (list stores-on-left access-form) bindings)
            ;; We don't know the stores variable for the last form yet,
            ;; so fake it for the first access-form and fix it at the end.
            (unless fix-me (setq fix-me (car bindings)))
            (push store-form store-forms)
            (setq stores-on-left (if (cdr stores) stores (car stores))))))


;;; Some Setf methods for built in functions

(defsetf SUBSEQ (seq start &optional end) (new)
    `(progn (replace ,seq ,new :start1 ,start :end1 ,end) ,new))


(define-setf-method LDB (byte-spec int &environment env
                                       &aux (spvar (gensym "B"))
                                            (store (gensym "S")))
    (multiple-value-bind
      (vars vals stores store-form access-form)
      (get-setf-method int env)
        (values
            (cons spvar vars)
            (cons byte-spec vals)
            (list store)
            `(let ((,(car stores) (dpb ,store ,spvar ,access-form)))
                ,store-form
                ,store)
            `(ldb ,spvar ,access-form))))


(define-setf-method MASK-FIELD (byte-spec int &environment env
                                              &aux (spvar (gensym "B"))
                                                   (store (gensym "S")))
    (multiple-value-bind
      (vars vals stores store-form access-form)
      (get-setf-method int env)
        (values
            (cons spvar vars)
            (cons byte-spec vals)
            (list store)
            `(let ((,(car stores) (deposit-field ,store ,spvar ,access-form)))
                ,store-form
                ,store)
            `(mask-field ,spvar ,access-form))))


(define-setf-method GETF (plist key &optional default &environment env
                            &aux (keyvar (gensym "K"))
                                 (defvar (gensym "D"))
                                 (store (gensym "S")))
    (multiple-value-bind
      (vars vals stores store-form access-form)
      (get-setf-method plist env)
        (values
            `(,@vars ,keyvar ,@(if default `(,defvar)))
            `(,@vals ,key ,@(if default `(,default)))
            `(,store)
            `(let* ((,(car stores) (sys:putf ,access-form ,keyvar ,store)))
                ,store-form
                ,store)
            `(getf ,access-form ,keyvar ,@(if default `(,defvar))))))


(proclaim '(ftype function subst))      ; 'cos forward referenced

(define-setf-method THE (type place &aux store &environment env)
    (multiple-value-bind
      (vars vals stores store-form access-form)
      (get-setf-method place env)
        (setq store (car stores))
        (values
            vars
            vals
            stores
            (subst `(the ,type ,store) store store-form :test #'eq)
            `(the ,type ,access-form))))


(defun SYS:STORE-FORM-IS-UPDATER-CALL (store-form store &aux temp)
    ;; Recognises store-forms of the form
    ;;  (progn (sys:func <store> <args>) <store>)
    ;; And returns the cadr of the store-form
    (and (eq (car store-form) 'progn)
         (consp (setq temp (car (setq store-form (cdr store-form)))))
         (symbolp (car temp))
         (eq (symbol-package (car temp)) #.(find-package "SYSTEM"))
         (eq (car (setq store-form (cdr store-form))) store)
         (null (cdr store-form))
         temp))


(define-setf-method APPLY (fn &rest args &aux temp &environment env)
    (if (and (consp fn)
             (eq (car fn) 'function)
             (eq (length fn) 2)
             (symbolp (cadr fn)))
        (setq fn (cadr fn))
        (sys:program-error "Form like #'NAME needed for SETF of APPLY" fn))
    (multiple-value-bind
      (vars vals stores store-form access-form)
      (get-setf-method (cons fn args) env)
        (if (setq temp (sys:store-form-is-updater-call
                            store-form (car stores)))
            (values
                vars
                vals
                stores
                `(progn (apply #',(car temp) ,@(cdr temp)) ,(car stores))
                `(apply #',(car access-form) ,@(cdr access-form)))
            (if (eq (car (last access-form)) (car (last store-form)))
                (values
                    vars
                    vals
                    stores
                    `(apply #',(car store-form) ,@(cdr store-form))
                    `(apply #',(car access-form) ,@(cdr access-form)))
                (sys:program-error
                    "No setf method for (APPLY #'~S ...)" fn)))))


(define-setf-method VALUES (&rest places &environment env
                            &aux var-list val-list store-vars
                                 store-forms access-forms)
    (dolist (place places)
        (multiple-value-bind
          (vars vals stores store-form access-form)
          (get-setf-method-multiple-value place env)
            (setq var-list (append var-list vars))
            (setq val-list (append val-list vals))
            (push (if (cdr stores) stores (car stores)) store-vars)
            (push store-form store-forms)
            (push access-form access-forms)))
    (setq store-vars (sys:nreverse-list store-vars))
    (values
        var-list
        val-list
        store-vars
        `(progn ,@(sys:nreverse-list store-forms) ,@store-vars)
        `(values ,@(sys:nreverse-list access-forms))))



#| --- Revision History ---------------------------------------------------
--- John Williams, Jun 30 1995
        sys:make-setf-form now uses sys:setf-is-updater.
--- John Williams, Jun 13 1995
        SETF and PSETF now do the optimising that the inline versions did.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Apr 26 1994
        Changes for symbol macros.
--- John Williams, Nov  1 1993
        Removed setf method for CHAR-BIT (as per Steele 1990).
 |#
