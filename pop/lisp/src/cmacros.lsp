#| --- Copyright University of Sussex 1996. All rights reserved. ----------
 | File:            C.all/lisp/src/cmacros.lsp
 | Purpose:         Create some built-in compiler macros
 | Author:          John Williams, Aug  8 1994 (see revisions)
 | Documentation:
 | Related Files:   C.all/lisp/src/inlines.p
 |#

(define-compiler-macro 1+ (x)
    `(+ ,x 1))


(define-compiler-macro 1- (x)
    `(- ,x 1))


(define-compiler-macro NOT (x)
    `(eq ,x nil))


(define-compiler-macro NULL (x)
    `(eq ,x nil))


(define-compiler-macro ENDP (&whole form x)
    (if (sys:fast)
        `(eq ,x nil)
        form))


(define-compiler-macro ZEROP (x)
    (if (sys:fast)
        `(sys:= ,x 0)
        `(= ,x 0)))


(define-compiler-macro PLUSP (x)
    `(> ,x 0))


(define-compiler-macro MINUSP (x)
    `(< ,x 0))


(define-compiler-macro ODDP (x)
    `(logtest ,x 1))


(define-compiler-macro EVENP (x)
    `(not (logtest ,x 1)))


(define-compiler-macro MAPCAR (&whole form
                                fn list &optional (list2 nil l2-supp)
                                &rest rest)
    (if rest
        form
        (if l2-supp
            `(sys:map-2-lists ,fn ,list ,list2)
            `(sys:map-1-list ,fn ,list))))


(define-compiler-macro MEMBER (&whole form item list &rest rest)
    (if rest
        form
        `(sys:eql-member ,item ,list)))


(define-compiler-macro ADJOIN (&whole form item list &rest keys)
    (if (and (atom item) (not (sys:symbol-macro-p item))
             (atom list) (not (sys:symbol-macro-p list)))
        `(if (member ,item ,list ,@keys) ,list (cons ,item ,list))
        form))


(define-compiler-macro ASSOC (&whole form item alist &rest rest)
    (if rest
        form
        `(sys:eql-assoc ,item ,alist)))


(define-compiler-macro RASSOC (&whole form item alist &rest rest)
    (if rest
        form
        `(sys:eql-rassoc ,item ,alist)))


;; Compiler macros that expand into Poplog-specific special forms

(define-compiler-macro MULTIPLE-VALUE-BIND (vars values-form &body forms)
    `(sys:&multiple-value-bind ,vars ,values-form ,@forms))


(define-compiler-macro MULTIPLE-VALUE-LIST (form)
    `(sys:&multiple-value-list ,form))


(define-compiler-macro MULTIPLE-VALUE-SETQ (&whole f vars values-form)
    (let ((v vars))
        (tagbody
            :start
            (if (atom v) (go :end))
            (if (sys:symbol-macro-p (car v))
                (return-from multiple-value-setq f))
            (setq v (cdr v))
            (go :start)
            :end))
    `(sys:&multiple-value-setq ,vars ,values-form))


(define-compiler-macro NTH-VALUE (&whole f n form)
    (if (typep n 'fixnum)
        (if (eq n 0)
            `(values ,form)
            `(sys:&nth-value ,n ,form))
        f))


(define-compiler-macro PSETQ (&rest forms)
    `(sys:&psetq ,@forms))


;;; Speed up some I/O functions

(define-compiler-macro WRITE (&whole form object &rest args)
    (if args
        form
        `(sys:write ,object nil nil nil)))


(define-compiler-macro WRITE-STRING (&whole form string &optional stream
                                                        &rest keys)
    (if (and (not keys) (typep string 'simple-base-string))
        `(sys:write-simple-string ,string ,stream)
        form))



#| --- Revision History ---------------------------------------------------
--- John Williams, Sep  5 1996
        Added plusp, minusp, oddp, evenp.
--- John Williams, Jul  7 1995
        Added adjoin.
--- John Williams, Apr  3 1995
        WRITE & WRITE-STRING now optimise no keyword case.
--- John Williams, Feb 23 1995
        MAPCAR now optimises 2 list case.
--- John Williams, Aug 25 1994
        Changes for Steele 1990 (Lisp version 1.6)
 |#
