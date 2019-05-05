#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/clos.lsp
 | Purpose:         Clos primitives defined in Lisp
 | Author:          John Williams, Apr 26 1994 (see revisions)
 | Documentation:
 | Related Files:   C.all/lisp/src/clos.p, C.all/lisp/src/methods.p
 |#

(in-package "COMMON-LISP")

(export '(
    call-next-method change-class class-of
    defclass defgeneric defmethod documentation
    generic-function
    initialize-instance
    allocate-instance make-instance
    next-method-p
    slot-boundp slot-value
    with-accessors with-slots

    add-method
    class-name
    compute-applicable-methods
    ensure-generic-function
    find-class find-method function-keywords
    make-instances-obsolete
    no-applicable-method no-next-method
    print-object
    reinitialize-instance remove-method
    shared-initialize slot-exists-p slot-makunbound slot-missing slot-unbound
    update-instance-for-different-class update-instance-for-redefined-class

    call-method
    define-method-combination
    invalid-method-error
    method-combination-error method-qualifiers

    standard-object structure-object
    class standard-class built-in-class structure-class
    generic-function standard-generic-function
    method standard-method
    ))


(defun sys:capture-initform (form)
    (if (constantp form)
        `(constantly ,form)
        `(function (lambda () (values ,form)))))


#| DEFCLASS |#

(defun sys:enclose-initforms (slots &aux item slot newslot newslots)
    (dolist (slot slots)
        (if (atom slot)
            (push `(list ',slot) newslots)
            (progn
                (setq item (pop slot) newslot nil)
                (push `(quote ,item) newslot)
                (loop
                    (if (atom slot) (return))
                    (setq item (pop slot))
                    (push `(quote ,item) newslot)
                    (when (eq item :initform)
                          (setq item (pop slot))
                          (push (sys:capture-initform item)
                                newslot)))
                (push `(list ,@(sys:nreverse-list newslot)) newslots))))
    `(list ,@(sys:nreverse-list newslots)))


(defun sys:enclose-default-initargs (args &aux item result)
    (loop
        (if (endp args) (return))
        (setq item (pop args))
        (push `(quote ,item) result)
        (if (endp args)
            (sys:program-error "Missing value form in :DEFAULT-INITARGS list")
            (setq item (pop args)))
        (push (sys:capture-initform item) result))
    `(list ,@(sys:nreverse-list result)))


(defmacro DEFCLASS (name supers slots &rest options
                                      &aux default-initargs doc meta)
    (flet ((dup-err (old option)
            (sys:program-error
                "Duplicate ~A option in DEFCLASS form"
                (car option) option)))
        (dolist (option options)
            (if (consp option)
                (case (car option)
                    (:metaclass
                        (if meta
                            (dup-err meta option)
                            (setq meta (cadr option))))
                    (:documentation
                        (if doc
                            (dup-err doc option)
                            (setq doc (cadr option))))
                    (:default-initargs
                        (if default-initargs
                            (dup-err default-initargs option)
                            (setq default-initargs (cdr option))))
                    (otherwise
                        (sys:program-error
                            "Unrecognised DEFCLASS option" option)))
                (sys:program-error "DEFCLASS option not a list" option))))
    `(sys:define-class
        ',name
        ',(or meta 'standard-class)
        ',supers
        ,(sys:enclose-initforms slots)
        ,(sys:enclose-default-initargs default-initargs)
        ',doc
        t))


#| Create some standard metaclasses |#

(defclass STANDARD-OBJECT (t) ())

(defclass STRUCTURE-OBJECT (t) () (:metaclass structure-class))


(defclass CLASS () (pop11:class))

(defclass STANDARD-CLASS (class) ())

(defclass BUILT-IN-CLASS (class) ())

(defclass STRUCTURE-CLASS (class) ())


(defclass METHOD () (pop11:method))

(defclass STANDARD-METHOD (method) ())


#| DEFMETHOD |#

(defun sys:parse-spec-lamlist (lamlist &aux var p rq-args p-specs)
    (loop
        (if (endp lamlist)
            (return))
        (setq var (car lamlist))
        (if (sys:eq-member var lambda-list-keywords)
            (return))
        (if (consp var)
            (progn
                (unless (sys:2-list var)
                        (sys:program-error
                            "Malformed parameter specializer name" var))
                (setq p (cadr var) var (car var))
                (if (consp p)
                    (progn
                        (unless (and (eq (car p) 'eql) (sys:2-list p))
                                (sys:program-error
                                    "Malformed EQL parameter specializer" p))
                        (push (sys:capture-initform (cadr p))
                                p-specs))
                    (push `(quote ,p) p-specs)))
            (push '(quote t) p-specs))
        (push var rq-args)
        (pop lamlist))
    (dolist (var rq-args)
        (push var lamlist))
    (values lamlist (sys:nreverse-list p-specs)))


(defmacro SYS:MAKE-METHOD-BODY (name lamlist qualifiers
                                &body #(body decs doc))
    `(function
        (lambda ,lamlist
            ,@(if doc `(,doc))
            ,@decs
            (macrolet
                ( (call-method (m next) `(sys:call-method ,m ,next)) )
                (flet ( (next-method-p () (sys:next-method-p)) )
                    (block ,name
                        ,@(if (or (eq (car qualifiers) :before)
                                (eq (car qualifiers) :after))
                            `(,@body (values))
                            body)))))))


(defun SYS:DEFMETHOD-INTERNAL (name source env args ensure-gfn-p)
    (let* ((block-name (if (sys:2-list name) (cadr name) name))
           (qualifiers (if (atom (car args)) (list (pop args)) nil))
           (spec-lamlist (pop args))
           (body args))
        (multiple-value-bind
            (lamlist p-specs)
            (sys:parse-spec-lamlist spec-lamlist)
            `(progn
                ,(if ensure-gfn-p
                    `(sys:ensure-generic-function
                        ',name ',source ',env ',lamlist nil nil)
                    nil)
                (sys:define-method
                    (function ,name)
                    ',source
                    ',qualifiers
                    ',lamlist
                    (list ,@p-specs)
                    (sys:make-method-body
                        ,block-name ,lamlist ,qualifiers ,@body))))))


(defmacro DEFMETHOD (name &rest args &environment env)
    (sys:defmethod-internal name 'defmethod env args t))


#| DEFGENERIC and co. |#

(defconstant SYS:GENERIC-FUNCTION-OPTIONS
                '(:argument-precedence-order
                  declare
                  :documentation
                  :method-combination
                  :generic-function-class
                  :method-class))


(defun ENSURE-GENERIC-FUNCTION (fname &rest options
                                      &key lambda-list
                                           documentation
                                           environment
                                           (source nil))
    (remf options ':lambda-list)
    (remf options ':documentation)
    (remf options ':environment)
    (remf options ':source)
    (sys:ensure-generic-function
        fname source environment lambda-list documentation options))


(defun sys:split-gfn-args (args &aux option options methods done)
    (dolist (item args)
        (unless (consp item)
            (sys:program-error "Generic function option must be a list" item))
        (setq option (car item))
        (if (eq option :method)
            (push (cdr item) methods)
            (if (sys:eq-member option sys:generic-function-options)
                (if (sys:eq-member option done)
                    (sys:program-error
                        "Duplicate generic function option ~S" option)
                    (progn
                        (unless (eq option 'declare)    ; Not used in Poplog
                            (push option options)
                            (push (cadr item) options))
                        (push option done)))
                (sys:program-error
                    "Unrecognised generic function option ~S" option))))
    (values (sys:nreverse-list methods) (sys:nreverse-list options)))


(defmacro DEFGENERIC (name lamlist &rest args)
    (multiple-value-bind
      (methods options)
      (sys:split-gfn-args args)
        `(prog1
            (ensure-generic-function
                ',name :source 'defgeneric :lambda-list ',lamlist ,@options)
            ,@(mapcar
                #'(lambda (method)
                    (sys:defmethod-internal
                        name 'defgeneric nil method nil))
                methods))))


(defmacro GENERIC-FUNCTION (&rest args)
    `(defgeneric nil ,@args))


#| Definitions of built in generic functions |#

(defgeneric NO-APPLICABLE-METHOD (gfn &rest args)
    (:method
        ((gfn t) &rest args)
            (sys:control-error "No applicable method" gfn args)))


(defgeneric CHANGE-CLASS (instance new-class)
    (:method
        ((instance STANDARD-OBJECT) (new-class STANDARD-CLASS))
            (sys:change-class instance new-class))
    (:method
        ((instance T) (new-class SYMBOL))
            (sys:change-class instance (find-class new-class))))


(defgeneric CLASS-NAME (class)
    (:method
        ((class CLASS))
            (sys:pop11-class-name (slot-value class 'pop11:class))))


(defgeneric (setf CLASS-NAME) (new-value class)
    (:method
        (new-value (class CLASS))
            (setf (sys:pop11-class-name (slot-value class 'pop11:class))
                    new-value)))


(defgeneric SLOT-MISSING (class object slot-name operation
                            &optional new-value)
    (:method
        ((class T) object slot-name operation &optional new-value)
            (error 'poplog:missing-slot :name slot-name :object object)))


(defgeneric SLOT-UNBOUND (class instance slot-name)
    (:method
        ((class T) instance slot-name)
            (error 'unbound-slot :name slot-name :object instance)))


(defgeneric ALLOCATE-INSTANCE (class &rest initargs)
    (:method
        ((class STANDARD-CLASS) &rest initargs)
            (sys:allocate-instance class))
    (:method
        ((class STRUCTURE-CLASS) &rest initargs)
            (sys:allocate-structure (class-name class) initargs)))


(defgeneric SHARED-INITIALIZE (instance slot-names &rest initargs)
    (:method
        ((instance STANDARD-OBJECT) slot-names &rest initargs)
            (sys:shared-initialize instance slot-names initargs)))


(defgeneric INITIALIZE-INSTANCE (instance &rest initargs)
    (:method
        ((instance STANDARD-OBJECT) &rest initargs)
            (apply #'shared-initialize instance t initargs)))


(defgeneric MAKE-INSTANCE (class &rest initargs)
    (:method
        ((class STANDARD-CLASS) &rest initargs)
            (apply #'initialize-instance
                (apply #'allocate-instance class initargs)
                initargs))
    (:method
        ((class SYMBOL) &rest initargs)
            (apply #'initialize-instance
                (apply #'allocate-instance (find-class class) initargs)
                initargs)))


(defgeneric REINITIALIZE-INSTANCE (instance &rest initargs)
    (:method
        ((instance STANDARD-OBJECT) &rest initargs)
            (apply #'shared-initialize instance nil initargs)))


(defgeneric MAKE-INSTANCES-OBSOLETE (class)
    (:method
        ((class STANDARD-CLASS))
            (sys:make-instances-obsolete class)
            class)
    (:method
        ((class SYMBOL))
            (make-instances-obsolete (find-class class))))


(defgeneric UPDATE-INSTANCE-FOR-DIFFERENT-CLASS (previous current
                                                    &rest initargs)
    (:method
        ((previous STANDARD-OBJECT) (current STANDARD-OBJECT) &rest initargs)
            (apply #'shared-initialize
                current (sys:newly-added-slots previous current) initargs)))


(defgeneric UPDATE-INSTANCE-FOR-REDEFINED-CLASS (instance added discarded
                                                    plist &rest initargs)
    (:method
        ((instance STANDARD-OBJECT) added discarded plist &rest initargs)
            (apply #'shared-initialize instance added initargs)))


(defgeneric NO-NEXT-METHOD (gfn method &rest args)
    (:method ((gfn STANDARD-GENERIC-FUNCTION) (method STANDARD-METHOD)
                &rest args)
        (sys:control-error "No next method" method args)))


(defgeneric ADD-METHOD (gfn method)
    (:method ((gfn STANDARD-GENERIC-FUNCTION) (method METHOD))
        (sys:add-method gfn method)))


(defgeneric FIND-METHOD (gfn qualifiers specializers &optional errorp)
    (:method
        ((gfn STANDARD-GENERIC-FUNCTION) qualifiers specializers
                &optional (errorp t))
            (sys:find-method gfn qualifiers specializers errorp)))


(defgeneric REMOVE-METHOD (gfn method)
    (:method
        ((gfn STANDARD-GENERIC-FUNCTION) (method METHOD))
            (sys:remove-method gfn method)))


(defgeneric METHOD-QUALIFIERS (method)
    (:method
        ((method STANDARD-METHOD))
            (sys:method-qualifiers method)))


#| TYPE-OF |#

(defun TYPE-OF (item)
    (declare (inline typep))
    (if (typep item 'standard-object)
        (let* ((class (sys:class-of-instance item))
               (name (class-name class)))
            (if (eq (find-class name nil) class)
                name
                class))
        (values (class-name (class-of item)))))


#| WITH-SLOTS and WITH-ACCESSORS |#

(defmacro WITH-SLOTS (slot-entries instance &body body)
    (let ((i (gensym "I")))
        `(let ((,i ,instance))
            (symbol-macrolet
                ,(mapcar
                    #'(lambda (slot-entry &aux var name)
                        (if (sys:2-list slot-entry)
                            (setq name (car slot-entry)
                                  var (cadr slot-entry))
                            (setq name slot-entry
                                  var slot-entry))
                        `(,var (slot-value ,i ',name)))
                    slot-entries)
                ,@body))))


(defmacro WITH-ACCESSORS (slot-entries instance &body body)
    (let ((i (gensym "I")))
        `(let ((,i ,instance))
            (symbol-macrolet
                ,(mapcar
                    #'(lambda (slot-entry &aux var accessor)
                        (if (sys:2-list slot-entry)
                            (setq accessor (car slot-entry)
                                  var (cadr slot-entry))
                            (setq accessor slot-entry
                                  var slot-entry))
                        `(,var (,accessor ,i)))
                    slot-entries)
                ,@body))))


#| DOCUMENTATION and PRINT-OBJECT |#

(defgeneric DOCUMENTATION (x &optional doc-type)
    (:method
        ((x FUNCTION) &optional doc-type)
            (sys:documentation x 'function))
    (:method
        ((x STANDARD-CLASS) &optional doc-type)
            (sys:documentation (class-name x) 'type))
    (:method
        ((x STANDARD-METHOD) &optional doc-type)
            (sys:documentation x 'method))
    (:method
        ((x STANDARD-GENERIC-FUNCTION) &optional doc-type)
            (sys:documentation (sys:gfn-name x) 'function))
    (:method
        ((x LIST) &optional doc-type)
            (sys:documentation x 'function))
    (:method
        ((x SYMBOL) &optional doc-type)
            (sys:documentation x doc-type)))


(defmacro sys:check-doc-supp (x doc-supp doc-type)
    `(if (symbolp ,x)
        (unless ,doc-supp
            (error "Second argument to DOCUMENTATION essential when first is symbol"
                ,x))
        (if ,doc-supp
            (sys:warn-wi
                "Second argument to DOCUMENTATION unnecessary unless first is symbol"
                ,x ,doc-type))))


(defmethod DOCUMENTATION :before (x &optional (doc-type nil doc-supp))
    (sys:check-doc-supp x doc-supp doc-type))


(defgeneric (SETF DOCUMENTATION) (new-value x &optional doc-type)
    (:method
        (new-value (x FUNCTION) &optional doc-type)
            (setf (sys:documentation x 'function) new-value))
    (:method
        (new-value (x STANDARD-CLASS) &optional doc-type)
            (setf (sys:documentation (class-name x) 'type) new-value))
    (:method
        (new-value (x STANDARD-METHOD) &optional doc-type)
            (setf (sys:documentation x 'method) new-value))
    (:method
        (new-value (x STANDARD-GENERIC-FUNCTION) &optional doc-type)
            (setf (sys:documentation (sys:gfn-name x) 'function) new-value))
    (:method
        (new-value (x LIST) &optional doc-type)
            (setf (sys:documentation x 'function) new-value))
    (:method
        (new-value (x SYMBOL) &optional doc-type)
            (setf (sys:documentation x doc-type) new-value)))


(defmethod (SETF DOCUMENTATION) :before (new-value x &optional
                                                    (doc-type nil doc-supp))
    (sys:check-doc-supp x doc-supp doc-type))


(defgeneric PRINT-OBJECT (object stream)
    (:method
        (object stream)
            (sys:default-print-object object stream)))


(defmethod NO-APPLICABLE-METHOD ((gfn (eql #'print-object)) &rest args)
    (apply #'sys:default-print-object args))


#| MAKE-LOAD-FORM (not very useful in Poplog 'cos no COMPILE-FILE) |#

(defgeneric MAKE-LOAD-FORM (object))


#| Dummy versions of things that aren't implemented yet |#

(defun FUNCTION-KEYWORDS (method)
    (warn 'poplog:warning-with-involving
            :message "FUNCTION-KEYWORDS not implemented yet"
            :involving (list method))
    nil)


(defmacro DEFINE-METHOD-COMBINATION (&rest args)
    (error "DEFINE-METHOD-COMBINATION not implemented yet"))


(defun INVALID-METHOD-ERROR (method format-string &rest args)
    (error "Invalid method ~S~@[: ~?~]" method format-string args))


(defun METHOD-COMBINATION-ERROR (format-string &rest args)
    (error "Method combination error: ~?" format-string args))



#| --- Revision History ---------------------------------------------------
--- John Williams, Jun  5 1995
        Added MAKE-LOAD-FORM.
--- John Williams, May 19 1995
        CALL-NEXT-METHOD is now globally fbound. NEXT-METHOD-P is defined
        as a local function, not macro. Dummy definitions of
        DEFINE-METHOD-COMBINATION and friend added.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Feb  9 1995
        Generic function option DECLARE now quietly ignored.
--- John Williams, Jan  5 1995
        Moved definition of SYS:2-LIST to C.all/lisp/src/defs.lsp.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, May 19 1994
        Removed the SLOT-MISSING method that was defined for standard
        classes.
--- John Williams, Apr 29 1994
        DOCUMENTATION now works for compiled lambda expressions.
 |#
