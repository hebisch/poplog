#| --- Copyright University of Sussex 1993. All rights reserved. ----------
 | File:            C.all/lisp/src/arrays.lsp
 | Purpose:         Common Lisp arrays and hash tables
 | Author:          John Williams, Feb 26 1986 (see revisions)
 | Documentation:   CLtL, p286-298
 | Related Files:   C.all/lisp/src/arrays.p, C.all/lisp/src/hash.p
 |#


(defun MAKE-ARRAY (dims &key element-type
                             (initial-element nil ie-supp)
                             (initial-contents nil ic-supp)
                             adjustable
                             fill-pointer
                             (displaced-to nil d-supp)
                             (displaced-index-offset 0))
    (if (and ic-supp ie-supp)
        (error "Cannot specify both INITIAL-ELEMENT and INITIAL-CONTENTS for array"))
    (if (and d-supp (or ie-supp ic-supp))
        (error "Cannot specify initial element(s) for displaced array"))
    (sys:make-array
        dims element-type initial-element initial-contents
        adjustable fill-pointer displaced-to displaced-index-offset))



(defun ADJUST-ARRAY (array dims &key element-type
                                     (initial-element nil ie-supp)
                                     (initial-contents nil ic-supp)
                                     fill-pointer
                                     (displaced-to nil d-supp)
                                     (displaced-index-offset 0))
    (if (and ic-supp ie-supp)
        (error "Cannot specify both INITIAL-ELEMENT and INITIAL-CONTENTS for array"))
    (if (and d-supp (or ie-supp ic-supp))
        (error "Cannot specify initial element(s) for displaced array"))
    (sys:adjust-array
        array dims element-type initial-element initial-contents
        fill-pointer displaced-to displaced-index-offset))



(defun MAKE-HASH-TABLE (&key
                        (test 'eql)
                        (size 31)
                        (rehash-size nil)
                        (rehash-threshold nil)
                        (temporary nil t-supp)  ; for backward compatibility
                        (weak nil))
    (if t-supp (setq weak (and temporary :key)))
    (sys:make-hash-table test size rehash-size rehash-threshold weak))



(defmacro WITH-HASH-TABLE-ITERATOR ((mname hash-table) &rest forms)
    (let ((state (gensym)))
        `(let ((,state (sys:make-hash-table-iterator ,hash-table)))
            (macrolet
                ((,mname () '(sys:hash-table-iterator-next ,state)))
                ,@forms))))



#| --- Revision History ---------------------------------------------------
--- John Williams, Dec  9 1993
        Added WITH-HASH-TABLE-ITERATOR (ala Steele 1990 p439)
 |#
