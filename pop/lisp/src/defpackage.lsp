#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/defpackage.lsp
 | Purpose:         Common Lisp DEFPACKAGE macro
 | Author:          John Williams, Nov 30 1993 (see revisions)
 | Documentation:   CLtL ed. 2, p270
 | Related Files:   C.all/lisp/src/packages.lsp
 |#


(defun sys:get-name (x)
    (if (symbolp x) (symbol-name x) x))


(defun sys:locate-symbol (name pname &aux p s)
    (if (and (setq p (find-package pname))
             (setq s (find-symbol name p)))
        (list s)
        (cerror "Skip this one" "No symbol called ~A in package ~A"
                    name pname)))


(proclaim '(ftype function member))     ; 'cos forward referenced


(defmacro DEFPACKAGE (name &rest options
                           &aux size nicknames
                                shadows shadowing-import-arglists
                                uselist got-uselist
                                import-arglists interns exports
                                calls-to-import calls-to-shadowing-import)

    (setq name (sys:get-name name))

    (dolist (option options)
        (unless (consp option)
            (sys:program-error "DEFPACKAGE option must be a list" option))
        (case (car option)
            (:size
                (if size
                    (sys:program-error
                        "Duplicate SIZE option in DEFPACKAGE form" option)
                    (setq size (cadr option))))
            (:nicknames
                (setq nicknames
                        (nconc nicknames
                                (mapcar #'sys:get-name (cdr option)))))
            (:shadow
                (setq shadows
                        (nconc shadows
                                (mapcar #'sys:get-name (cdr option)))))
            (:shadowing-import-from
                (push (cons
                        (sys:get-name (cadr option))
                        (mapcar #'sys:get-name (cddr option)))
                shadowing-import-arglists))
            (:use
                (setq uselist
                        (nconc uselist
                                (mapcar #'sys:get-name (cdr option)))
                      got-uselist
                        t))
            (:import-from
                (push (cons
                        (sys:get-name (cadr option))
                        (mapcar #'sys:get-name (cddr option)))
                import-arglists))
            (:intern
                (setq interns
                        (nconc interns
                                (mapcar #'sys:get-name (cdr option)))))
            (:export
                (setq exports
                        (nconc exports
                                (mapcar #'sys:get-name (cdr option)))))
            (otherwise
                (sys:program-error "Unrecognised DEFPACKAGE option" option))
            ))

    (unless size
        (setq size poplog:*default-package-size*))
    (unless got-uselist
        (setq uselist poplog:*default-package-use-list*))

    (let ((temp shadows))
        (flet ((check-sym (s)
                 (if (member s temp :test #'equal)
                     (sys:program-error "Symbol name ~A occurs twice" s)
                     (push s temp))))
            (dolist (l shadowing-import-arglists)
                (mapcar #'check-sym (cdr l)))
            (dolist (l import-arglists)
                (mapcar #'check-sym (cdr l)))
            (mapcar #'check-sym interns)
            (progn
                (setq temp interns)
                (mapcar #'check-sym exports))))

    (let ((p (gensym "P")))
        (dolist (arglist shadowing-import-arglists)
            (push
                `(shadowing-import
                    (mapcan
                        #'(lambda (x) (sys:locate-symbol x ,(car arglist)))
                        ',(cdr arglist))
                    ,p)
                calls-to-shadowing-import))
        (dolist (arglist import-arglists)
            (push
                `(import
                    (mapcan
                        #'(lambda (x) (sys:locate-symbol x ,(car arglist)))
                        ',(cdr arglist))
                    ,p)
                calls-to-import))

        `(let ((,p (find-package ',name)))
            (if ,p
                (rename-package ,p ',name ',nicknames)
                (setq ,p (make-package ',name
                            :nicknames ',nicknames
                            :use nil
                            :size ',size)))
            (shadow ',shadows ,p)
            ,@calls-to-shadowing-import
            (use-package ',uselist ,p)
            ,@calls-to-import
            ,@(mapcar #'(lambda (s) `(intern ,s ,p)) interns)
            (export
                (mapcar #'(lambda (s) (intern s ,p)) ',exports)
                ,p)
            ,p)))


(defmacro IN-PACKAGE (name &rest keys
                           &key (nicknames nil n-supp) (use nil u-supp) size)
    (if (or (and (consp name) (eq (car name) 'quote))
            (consp keys))
        ;; assume old style call to IN-PACKAGE function
        `(let* ((#1=#:n ,name) (#2=#:p (find-package #1#)))
            (setq *package*
                (if #2#
                    (progn
                        (rename-package #2# #1# ,@(if n-supp `(,nicknames)))
                        ,@(if u-supp `((use-package ,use #2#)))
                        #2#)
                    (make-package #1# ,@keys))))
        ;; assume new style call to IN-PACKAGE macro
        `(setq *package*
            (or (find-package ',name)
                (progn
                    (cerror "Create package ~S" "No package named ~S" ',name)
                    (make-package ',name))))))


(defpackage "EMPTY"
    (:size 1)
    (:use))



#| --- Revision History ---------------------------------------------------
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Sep 14 1994
        Fixed bugs in IN-PACKAGE.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Jun 14 1994
        Definition of IN-PACKAGE moved in from "packages.lsp".
--- John Williams, Jun  7 1994
        Now uses POPLOG:*DEFAULT-PACKAGE-USE-LIST*
        and POPLOG:*DEFAULT-PACKAGE-SIZE*.
 |#
