#| --- Copyright University of Sussex 1995. All rights reserved. ----------
 | File:            C.all/lisp/src/packages.lsp
 | Purpose:         Common Lisp package functions
 | Author:          John Williams, April 27 1987 (see revisions)
 | Documentation:   CLtL, chapter 11
 | Related Files:   C.all/src/lispcore.p, C.all/lisp/src/packages.p
 |#


(defun MAKE-PACKAGE (name &key nicknames
                               (use poplog:*default-package-use-list*)
                               (size poplog:*default-package-size*))
    (sys:make-package (cons name nicknames) use size))


(defun RENAME-PACKAGE (pkg name &optional nicknames)
    (sys:rename-package pkg (cons name nicknames)))


(defun EXPORT (syms &optional (pkg *package*))
    (sys:export syms pkg) t)


(defun UNEXPORT (syms &optional (pkg *package*))
    (sys:unexport syms pkg) t)


(defun IMPORT (syms &optional (pkg *package*))
    (sys:import syms pkg) t)


(defun SHADOWING-IMPORT (syms &optional (pkg *package*))
    (sys:shadowing-import syms pkg) t)


(defun SHADOW (syms &optional (pkg *package*))
    (sys:shadow syms pkg) t)


(defun USE-PACKAGE (pkgs &optional (pkg *package*))
    (sys:use-package pkgs pkg) t)


(defun UNUSE-PACKAGE (pkgs &optional (pkg *package*))
    (sys:unuse-package pkgs pkg) t)


(defmacro WITH-PACKAGE-ITERATOR ((mname pkglist &rest types) &rest forms)
    (if (endp types)
        (sys:program-error
            "No symbol-types specified in WITH-PACKAGE-ITERATOR form"))
    (let ((fn (gensym)))
        `(let ((,fn (sys:make-package-iterator ,pkglist ',types)))
            (macrolet
                ((,mname () '(funcall ,fn)))
                ,@forms))))


(defmacro DO-SYMBOLS ((var &optional (pkg *package*) result)
                        &body #(body decs))
    `(block nil
        (let (,var)
            ,@decs
            (sys:apppackage
                ,pkg
                #'(lambda (,var) ,@decs (tagbody ,@body)) t t)
            ,result)))


(defmacro DO-EXTERNAL-SYMBOLS ((var &optional (pkg *package*) result)
                               &body #(body decs))
    `(block nil
        (let (,var)
            ,@decs
            (sys:apppackage
                ,pkg
                #'(lambda (,var) ,@decs (tagbody ,@body)) nil nil)
        ,result)))


(defmacro DO-ALL-SYMBOLS ((var &optional result) &body #(body decs))
    `(block nil
        (let (,var)
            (dolist (#1=#:pkgs (list-all-packages) ,result)
                ,@decs
                (sys:apppackage
                    #1#
                    #'(lambda (,var) ,@decs (tagbody ,@body)) t nil)))))



#| --- Revision History ---------------------------------------------------
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
--- John Williams, Jun 14 1994
        Moved IN-PACKAGE to "defpackage.lsp".
--- John Williams, Jun  7 1994
        IN-PACKAGE now a macro. MAKE-PACKAGE now uses
        POPLOG:*DEFAULT-PACKAGE-USE-LIST* and POPLOG:*DEFAULT-PACKAGE-SIZE*.
--- John Williams, Dec  8 1993
        WITH-PACKAGE-ITERATOR now uses plain GENSYM.
--- John Williams, Dec  7 1993
        Added WITH-PACKAGE-ITERATOR (ala Steele 1990 p275)
 |#
