#| --- Copyright University of Sussex 1996. All rights reserved. ----------
 | File:            C.all/lisp/src/numbers.lsp
 | Purpose:         Various numeric functions
 | Author:          John Williams, Feb 26 1986 (see revisions)
 | Documentation:   CLtL p193 ff.
 | Related Files:   C.all/lisp/src/numbers.p
 |#


(defun ZEROP (num)
    (= num 0))


(defun LOGIOR (&rest ints)
    (if (null ints)
        0
        (boole boole-ior (car ints) (apply #'logior (cdr ints)))))


(defun LOGXOR (&rest ints)
    (if (null ints)
        0
        (boole boole-xor (car ints) (apply #'logxor (cdr ints)))))


(defun LOGAND (&rest ints)
    (if (null ints)
        -1
        (boole boole-and (car ints) (apply #'logand (cdr ints)))))


(defun LOGEQV (&rest ints)
    (if (null ints)
        -1
        (boole boole-eqv (car ints) (apply #'logeqv (cdr ints)))))


(defun ISQRT (y)
    ;;; This algorithm supplied by Stephen Silver
    (if (zerop y)
        (return-from isqrt 0))
    (if (minusp y)
        (error 'simple-type-error
            :format-string "Integer >= 0 needed - got ~S"
            :format-arguments (list y)
            :datum y
            :expected-type '(integer 0 *)))
    (macrolet
        ((next (x) `(floor (+ (* ,x ,x) y) (* 2 ,x))))
        (let* (xn-2
                (xn-1 (ash 1 (ceiling (integer-length y) 2)))
                (xn (next xn-1)))
            (loop
                (setq xn-2 xn-1)
                (setq xn-1 xn)
                (setq xn (next xn))
                ;; (shiftf xn-2 xn-1 xn (next xn))
                (cond
                    ((= xn xn-1)
                     (return xn))
                    ((= xn xn-2)
                     (return (min xn xn-1))))))))


#| --- Revision History ---------------------------------------------------
--- John Williams, Sep  5 1996
        oddp and evenp now defined as closures of &&/=_0 and &&=_0 in
        lisp/src/exports and as compiler macros in lisp/src/cmacros.lsp.
--- John Williams, Mar 15 1995
        Now signals typed errors.
--- John Williams, Jul 31 1992
        New accurate version of ISQRT, from Stephen Silver
 |#
