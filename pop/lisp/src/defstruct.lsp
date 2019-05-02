#| --- Copyright University of Sussex 1994. All rights reserved. ----------
 | File:            C.all/lisp/src/defstruct.lsp
 | Purpose:         Common Lisp DEFSTRUCT macro
 | Author:          John Williams, Jun  9 1994 (see revisions)
 | Documentation:   CLtL, ch 19.
 | Related Files:   C.all/lisp/src/defstruct.p
 |#


(defmacro DEFSTRUCT (name &rest slots &aux doc options makers)
    (if (consp name)
        (setq options (cdr name) name (car name)))
    (if (and (consp slots)
            (typep (car slots) 'simple-base-string))
        (setq doc (pop slots)))
    (flet
        ((capture-initform (form)
             (if (and (constantp form) (not (functionp form)))
                 `(quote ,form)
                 `(list 'funcall (function (lambda () (values ,form)))))))
        (let (res)
            (dolist (slot slots)
                (if (consp slot)
                    (push
                        `(list*
                            ',(car slot)
                            ,(capture-initform (cadr slot))
                            ',(cddr slot))
                        res)
                    (push `(quote ,slot) res)))
            (setq slots (cons 'list (sys:nreverse-list res))))
        (flet
            ((capture-lamlist-initforms (lamlist &aux res)
                 (dolist (item lamlist)
                     (if (and (consp item) (consp (cdr item)))
                         (push `(list*
                                  ',(car item)
                                  ,(capture-initform (cadr item))
                                  ',(cddr item))
                             res)
                         (push `(quote ,item) res)))
                 (cons 'list (sys:nreverse-list res))))
            (dolist (op options)
                (if (and (consp op) (eq (car op) :constructor))
                    (if (eq (length op) 3)
                        (push
                            `(list
                                (quote ,(cadr op))
                                ,(capture-lamlist-initforms (third op)))
                            makers)
                        (push `(quote ,(cdr op)) makers))))
            (setq makers (cons 'list (sys:nreverse-list makers)))))
        `(sys:define-struct
            ',name
            ',options
            ,makers
            ',doc
            ,slots))
